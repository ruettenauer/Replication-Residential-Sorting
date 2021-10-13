# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Match EPRTR to municipalities

rm(list = ls())

library(plyr)
library(dplyr)
library(sf)
library(spdep)



setwd("../02_Data")


### Load ###

load("Eper_sp_v16.RData")




#################################################################
#### Create Shapefile for municipalities (Gemeindeverbaende) ####
#################################################################

gemverb.df <- read.table("Gemverb_Makro.csv", sep=",", header=T)
gem.df <- read.table("Gem-Gemverb_Makro.csv", sep=",", header=T)

gem.shape <- st_read(dsn="./vg250_ebenen_2017", layer="VG250_GEM")
gem.shape$AGS  <-  as.numeric(as.character(gem.shape$AGS))


### Drop gemfr. gebiete

gem.shape  <-  gem.shape[which(gem.shape$AGS %in% gem.df$gem17), ]


#### Reduce to Gemeindeverbaende ####

### Merge with Gemeinde makro (including Gemeindeverb. id)
gem.shape <- merge(gem.shape, gem.df, by.x="AGS", by.y = "gem17", all.x=T)

# ### Drop forests
# gem.shape <- gem.shape[!is.na(gem.shape$kennziffer),]

### Union by Gemverb. id
gemverb.shape <- aggregate(gem.shape[, "kennziffer"], by = list(kennziffer = gem.shape$kennziffer), 
                           FUN = mean)
gemverb.shape$kennziffer.1 <- NULL


# Merge with makro data
gemverb.spdf <- merge(gemverb.shape, gemverb.df, all.x=T)

# Test which are not merge
table(gemverb.df$bev17[which(!gemverb.df$kennziffer %in% gemverb.shape$kennziffer)])
# Only gemverbs without inhabitants

### Save
save(gemverb.spdf, file = "Gemeindeverbaende_Spatial_2017.RData")






##########################################
#### Drop EPER data (only use E-PRTR) ####
##########################################

eper_ger.spdf <- eper_ger.spdf[eper_ger.spdf$reportingyear>2004,]


############################################################
#### Reduce to industrial facilities and waste disposal ####
############################################################

table(eper_ger.spdf$mainiasectorname)

eper_ger_ind.spdf <- eper_ger.spdf[eper_ger.spdf$mainiasectorname != "Intensive livestock production and aquaculture", ]


### Descriptives
dupl <- which(duplicated(data.frame(eper_ger_ind.spdf[, c("facilityid", "reportingyear")])))
table(length(unique(eper_ger_ind.spdf$facilityid)))
table(eper_ger_ind.spdf$reportingyear[-dupl])
mean(table(eper_ger_ind.spdf$reportingyear[-dupl]))



##############################
#### Linear interpolation ####
##############################
# If records are missing in one year, but present before and after -> interpolate


### generate a balanced dataset containing a year for each facility, including info on first and last year

eper_ger_ind.spdf$fy <- ave(eper_ger_ind.spdf$reportingyear, eper_ger_ind.spdf$facilityid,
                        FUN = function(x) min(x))
eper_ger_ind.spdf$ly <- ave(eper_ger_ind.spdf$reportingyear, eper_ger_ind.spdf$facilityid,
                        FUN = function(x) max(x))

tmp.df <- unique(data.frame(eper_ger_ind.spdf)[, c("facilityid", "fy", "ly")]) 
tmp.df$n <- tmp.df$ly - tmp.df$fy + 1

eper_ger_bal.df <- data.frame(facilityid = rep(sort(tmp.df$facilityid), 
                                               each = length(unique(eper_ger_ind.spdf$reportingyear))))
eper_ger_bal.df$reportingyear <- rep(sort(unique(eper_ger_ind.spdf$reportingyear)), 
                                     times = length(unique(eper_ger_bal.df$facilityid)))

eper_ger_bal.df <- merge(eper_ger_bal.df, tmp.df, by = "facilityid")


### Add meta information of facilities
coords <- st_coordinates(eper_ger_ind.spdf)
eper_ger_ind.spdf$lon <- coords[, 1]
eper_ger_ind.spdf$lat <- coords[, 2]

mvars <- c("facilityid", "reportingyear", "facilityname", "mainiasectorname", "mainiaactivitycode", "mainiaactivityname",              
            "mainiasubactivitycode", "mainiasubactivityname", "streetname",                     
            "buildingnumber", "city", "postalcode", "parentcompanyname",                     
            "lat", "lon")

dupl <- which(duplicated(data.frame(eper_ger_ind.spdf[, c("facilityid", "reportingyear")])))
eper_ger_bal.df <- merge(eper_ger_bal.df, st_drop_geometry(eper_ger_ind.spdf[-dupl, mvars]),
                         by = c("facilityid", "reportingyear"), all = TRUE)


### gen missing indicator
eper_ger_bal.df$miss <- 0
eper_ger_bal.df$miss[is.na(eper_ger_bal.df$facilityname)] <- 1

eper_ger_bal.df$miss_total <- ave(eper_ger_bal.df$miss, eper_ger_bal.df$facilityid,
                                  FUN = function(x) sum(x))

table(eper_ger_bal.df$miss_total)
table(eper_ger_bal.df$miss)


### imputation rule 1 ---------------------------------------
### Facility is existent between first observation and last observation

# Indicator for operation (between first and last year)
eper_ger_bal.df$operation <- 0
eper_ger_bal.df$operation[which(eper_ger_bal.df$reportingyear >= eper_ger_bal.df$fy & 
                                  eper_ger_bal.df$reportingyear <= eper_ger_bal.df$ly)] <- 1


# Indicator for start and end
eper_ger_bal.df$start <- 0 
eper_ger_bal.df$start[which(eper_ger_bal.df$reportingyear == eper_ger_bal.df$fy &
                              eper_ger_bal.df$reportingyear != min(eper_ger_bal.df$reportingyear))] <- 1

eper_ger_bal.df$end <- 0 
eper_ger_bal.df$end[which(eper_ger_bal.df$reportingyear == (eper_ger_bal.df$ly + 1))] <- 1

# Table year for starts and ends
table(eper_ger_bal.df$reportingyear[eper_ger_bal.df$start == 1])
table(eper_ger_bal.df$reportingyear[eper_ger_bal.df$end == 1])




########################
#### Load IOER data ####
########################



load("IOER_industrial.RData")

# Transform to sf
ioer.spdf <- st_as_sf(ioer.spdf)

### Get unique facility locations
eper_ind_loc.spdf <- eper_ger_ind.spdf[, c("facilityid", "reportingyear", "lat", "lon")]
dupl <- which(duplicated(data.frame(eper_ind_loc.spdf[, c("facilityid", "lat", "lon")])))
eper_ind_loc.spdf <- eper_ind_loc.spdf[-dupl, ]

### get eper on IOER projection
eper_ind_loc.spdf <- st_transform(eper_ind_loc.spdf, st_crs(ioer.spdf))


### Reduce IOER data to shapes intersecting with facilities (2km surrounding)
# Use quadratic form to simplify
eper_ind_loc.buf <- st_buffer(eper_ind_loc.spdf, dist = 2000,
                              nQuadSegs = 1, 
                              endCapStyle = 'SQUARE')

ioer_red.spdf <- ioer.spdf[eper_ind_loc.buf, ]

### Compute host IOER for each facility location

ID <- st_intersection(eper_ind_loc.spdf, ioer_red.spdf)

# Append non-overlaping eprtr facilities
nint <- st_intersects(eper_ind_loc.spdf, ioer_red.spdf)
nint <- which(sapply(nint, length) == 0)
ID <- rbind.fill(ID, st_drop_geometry(eper_ind_loc.spdf)[nint,])


# # Save and reload
# save(ID, file = "Overlap_IOER_EPER.RData")
# 
# 
# load("Overlap_IOER_EPER.RData")


#####################################
#### Clean coordinates for EPRTR ####
#####################################
# Make location time constant

eper_var.df <- ID

# Reduce to varying coordinates
vr <- ave(eper_var.df$facilityid,
          by = eper_var.df$facilityid,
          FUN = function(x) length(x))

eper_var.df <- eper_var.df[which(vr > 1), ]

# Loop over fac ids and compute dist from first location
eper_var.df$dist <- NA
for(i in unique(eper_var.df$facilityid)){
  
  # Compute distances
  dist <- st_distance(eper_ind_loc.spdf[eper_ind_loc.spdf$facilityid == i, ])
  
  # select dist to first location
  dist <- as.numeric(dist[1,])
  
  # Add to df
  oo <- which(eper_var.df$facilityid == i)
  eper_var.df[oo, "dist"] <- dist
}

table(eper_var.df$dist)
summary(eper_var.df$dist)

### Change if dist > 0m
eper_var.df$change <- 0
eper_var.df$change[ave(eper_var.df$dist, eper_var.df$facilityid, FUN = function(x) max(x)) > 0] <- 1
table(eper_var.df$change)
View(eper_var.df[eper_var.df$change == 1,])


#### Correct coordinates ---------------------------------------

eper_var.df <- eper_var.df[eper_var.df$change == 1, ]

eper_var.df$lat_new <- NA
eper_var.df$lon_new <- NA


### Repeat last function
repeat_last = function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {
  if (!forward) x = rev(x)           # reverse x twice if carrying backward
  ind = which(!is.na(x))             # get positions of nonmissing values
  if (is.na(x[1]) && !na.rm)         # if it begins with NA
    ind = c(1,ind)                 # add first pos
  rep_times = diff(                  # diffing the indices + length yields how often
    c(ind, length(x) + 1) )          # they need to be repeated
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap  # exceeding maxgap
    if (any(exceed)) {               # any exceed?
      ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
      rep_times = diff(c(ind, length(x) + 1) ) # diff again
    }
  }
  x = rep(x[ind], times = rep_times) # repeat the values at these indices
  if (!forward) x = rev(x)           # second reversion
  x
}


### Rule 1 -----------------------------------------------------------
# If coordinate has always zero or <= 6 in total industry, replace
# replace with last available coord

eper_var.df$sum_ind <- rowSums(eper_var.df[, paste("ind_area_", c(2006:2018), sep = "")])

eper_var.df <- eper_var.df[order(eper_var.df$facilityid, eper_var.df$reportingyear), ]
for(i in unique(eper_var.df$facilityid)){
  lat <- eper_var.df$lat[eper_var.df$facilityid == i]
  lon <- eper_var.df$lon[eper_var.df$facilityid == i]
  zero <- eper_var.df$sum_ind[eper_var.df$facilityid == i]
  
  if(any(is.na(zero) | zero <= 7)){
    z <- which(is.na(zero) | zero <= 7)
    nz <- which(zero > 7)
    l <- length(nz)
    
    lat[z] <- NA
    lon[z] <- NA
    
    if(length(nz) != 0){
      latn <- repeat_last(lat)
      lonn <- repeat_last(lon)
    }else{
      latn <- NA
      lonn <- NA
    }
  }else{
    latn <- NA
    lonn <- NA
  }
  
  eper_var.df$lat_new[eper_var.df$facilityid == i] <- latn
  eper_var.df$lon_new[eper_var.df$facilityid == i] <- lonn
  
}

### Replace coordinates to include in next step
eper_var.df$lat[!is.na(eper_var.df$lat_new)] <- eper_var.df$lat_new[!is.na(eper_var.df$lat_new)]
eper_var.df$lon[!is.na(eper_var.df$lon_new)] <- eper_var.df$lon_new[!is.na(eper_var.df$lon_new)]


### Rule 2 -----------------------------------------------------------
# Use most recent location 
# This seemed to give best results by manual inspection (arbitrary for huge facilities)

eper_var.df <- eper_var.df[order(eper_var.df$facilityid, eper_var.df$reportingyear), ]
for(i in unique(eper_var.df$facilityid)){
  lat <- eper_var.df$lat[eper_var.df$facilityid == i]
  lon <- eper_var.df$lon[eper_var.df$facilityid == i]
  
  # Last non-missin
  oo1 <- which(!is.na(lat) & lat != 0)
  oo2 <- which(!is.na(lon) & lon != 0)
  oo <- intersect(oo1, oo2)
  
  latn <- lat
  lonn <- lon
  
  if(length(oo) > 0){
    l <- oo[length(oo)]
    
    latn[] <- lat[l]
    lonn[] <- lon[l]
  }

  
  eper_var.df$lat_new[eper_var.df$facilityid == i] <- latn
  eper_var.df$lon_new[eper_var.df$facilityid == i] <- lonn
  
}


### Test based on distance between old an new
coordold <- st_as_sf(eper_var.df, coords = c("lon", "lat"), 
                     crs = "+proj=longlat +datum=WGS84")
coordnew <- st_as_sf(eper_var.df, coords = c("lon_new", "lat_new"), 
                     crs = "+proj=longlat +datum=WGS84")

eper_var.df$dist_new <- as.numeric(st_distance(coordold, coordnew, by_element = TRUE))
eper_var.df$dist_newm <- ave(eper_var.df$dist_new, eper_var.df$facilityid, 
                             FUN = function(x) max(x))

summary(eper_var.df$dist_new)

# Inspect distance larger than 100m
View(eper_var.df[eper_var.df$dist_newm >= 100, ])

# Also for the large differences: it looks like this are mainly wrong information in early years
# Some changed facility according to coord, while address remained same, but address fits to newer coords.
# Thus I assume errors in earlier datasets.


##### -------------- Coords replace -------------- ##### 
### Reassign the new coordinates to facilities.

eper_newcoords.df <- unique(eper_var.df[, c("facilityid", "lat_new", "lon_new")])

### Eper balanced
eper_ger_bal.df <- merge(eper_ger_bal.df, eper_newcoords.df,
                         by = c("facilityid"), all.x = TRUE)
eper_ger_bal.df$lat[!is.na(eper_ger_bal.df$lat_new)] <-  eper_ger_bal.df$lat_new[!is.na(eper_ger_bal.df$lat_new)]
eper_ger_bal.df$lon[!is.na(eper_ger_bal.df$lon_new)] <-  eper_ger_bal.df$lon_new[!is.na(eper_ger_bal.df$lon_new)]

# distribute last, next for newly create obs
eper_ger_bal.df$lat <- ave(eper_ger_bal.df$lat, eper_ger_bal.df$facilityid,
                           FUN = function(x) repeat_last(x, forward = TRUE))
eper_ger_bal.df$lon <- ave(eper_ger_bal.df$lon, eper_ger_bal.df$facilityid,
                           FUN = function(x) repeat_last(x, forward = TRUE))
eper_ger_bal.df$lat <- ave(eper_ger_bal.df$lat, eper_ger_bal.df$facilityid,
                           FUN = function(x) repeat_last(x, forward = FALSE))
eper_ger_bal.df$lon <- ave(eper_ger_bal.df$lon, eper_ger_bal.df$facilityid,
                           FUN = function(x) repeat_last(x, forward = FALSE))

### Eper ger spdf
projold <- st_crs(eper_ger.spdf)
eper_ger.spdf <- st_transform(eper_ger.spdf, CRS("+proj=longlat +datum=WGS84"))
coords <- st_coordinates(eper_ger.spdf)
eper_ger.spdf$lon <- coords[, 1]
eper_ger.spdf$lat <- coords[, 2]
eper_ger.spdf <- merge(st_drop_geometry(eper_ger.spdf), eper_newcoords.df,
                       by = c("facilityid"), all.x = TRUE)
eper_ger.spdf$lat[!is.na(eper_ger.spdf$lat_new)] <-  eper_ger.spdf$lat_new[!is.na(eper_ger.spdf$lat_new)]
eper_ger.spdf$lon[!is.na(eper_ger.spdf$lon_new)] <-  eper_ger.spdf$lon_new[!is.na(eper_ger.spdf$lon_new)]

# distribute last, next for newly create obs
eper_ger.spdf$lat <- ave(eper_ger.spdf$lat, eper_ger.spdf$facilityid,
                           FUN = function(x) repeat_last(x, forward = TRUE))
eper_ger.spdf$lon <- ave(eper_ger.spdf$lon, eper_ger.spdf$facilityid,
                           FUN = function(x) repeat_last(x, forward = TRUE))
eper_ger.spdf$lat <- ave(eper_ger.spdf$lat, eper_ger.spdf$facilityid,
                           FUN = function(x) repeat_last(x, forward = FALSE))
eper_ger.spdf$lon <- ave(eper_ger.spdf$lon, eper_ger.spdf$facilityid,
                           FUN = function(x) repeat_last(x, forward = FALSE))

# to spatial
eper_ger.spdf <- data.frame(eper_ger.spdf)
eper_ger.spdf <- st_as_sf(eper_ger.spdf, coords = c("lon", "lat"),
                          crs = CRS("+proj=longlat +datum=WGS84"))
eper_ger.spdf <- st_transform(eper_ger.spdf, projold)



### Eper ind spdf
projold <- st_crs(eper_ger_ind.spdf)
eper_ger_ind.spdf <- merge(st_drop_geometry(eper_ger_ind.spdf), eper_newcoords.df,
                           by = c("facilityid"), all.x = TRUE)
eper_ger_ind.spdf$lat[!is.na(eper_ger_ind.spdf$lat_new)] <-  eper_ger_ind.spdf$lat_new[!is.na(eper_ger_ind.spdf$lat_new)]
eper_ger_ind.spdf$lon[!is.na(eper_ger_ind.spdf$lon_new)] <-  eper_ger_ind.spdf$lon_new[!is.na(eper_ger_ind.spdf$lon_new)]

# to spatial
eper_ger_ind.spdf <- st_as_sf(eper_ger_ind.spdf, coords = c("lon", "lat"),
                              crs = CRS("+proj=longlat +datum=WGS84"))
eper_ger_ind.spdf <- st_transform(eper_ger_ind.spdf, projold)






##### -------------- Continue with new coordinates -------------- ##### 

### Get unique facility locations
coords <- st_coordinates(eper_ger_ind.spdf)
eper_ger_ind.spdf$lon <- coords[, 1]
eper_ger_ind.spdf$lat <- coords[, 2]
eper_ind_loc.spdf <- eper_ger_ind.spdf[, c("facilityid", "reportingyear", "lat", "lon")]
dupl <- which(duplicated(st_drop_geometry(eper_ind_loc.spdf[, c("facilityid", "lat", "lon")])))
eper_ind_loc.spdf <- eper_ind_loc.spdf[-dupl, ]

# to spatial
projold <- st_crs(eper_ind_loc.spdf)
eper_ind_loc.spdf <- st_as_sf(eper_ind_loc.spdf, coords = c("lon", "lat"),
                              crs = CRS("+proj=longlat +datum=WGS84"))


### get eper on IOER projection
eper_ind_loc.spdf <- st_transform(eper_ind_loc.spdf, st_crs(ioer.spdf))

# Export
save(eper_ind_loc.spdf, file = "Eper_balanced_locations.RData")



### Compute host IOER for each facility location

ID <- st_intersection(eper_ind_loc.spdf, ioer_red.spdf)

# Append non-overlaping eprtr facilities
nint <- st_intersects(eper_ind_loc.spdf, ioer_red.spdf)
nint <- which(sapply(nint, length) == 0)
ID <- rbind.fill(ID, st_drop_geometry(eper_ind_loc.spdf)[nint,])

# This drops 5 ids which have coordinates outside of germany 

eper_ioer.df <- ID

# Drop reportingyear, as location is time constant
eper_ioer.df$reportingyear <- NULL
eper_ioer.df$lat <- NULL
eper_ioer.df$lon <- NULL

# Reshape
eper_ioer.df <- reshape(eper_ioer.df, varying = paste("ind_area_", c(2006:2018), sep = ""),
                        sep = "_area_", idvar = "facilityid", direction = "long")

names(eper_ioer.df)[which(names(eper_ioer.df) == "time")] <- "reportingyear"
names(eper_ioer.df)[which(names(eper_ioer.df) == "ind")] <- "ioer_ind"

eper_ioer.df <- eper_ioer.df[order(eper_ioer.df$facilityid, eper_ioer.df$reportingyear), ]


### Match to eper balanced df, keep all obs 

eper_bal.df <- merge(eper_ger_bal.df, eper_ioer.df,
                      by = c("facilityid", "reportingyear"), all = TRUE)








#################################################
#### Identify increases and decreases in oer ####
#################################################

eper_bal.df <- eper_bal.df[order(eper_bal.df$facilityid, eper_bal.df$reportingyear), ]

eper_bal.df$ioer_diff <- ave(eper_bal.df$ioer_ind,
                             by = eper_bal.df$facilityid,
                             FUN = function(x) x - dplyr::lag(x))


# Increase
eper_bal.df$ioer_inc <- 0
eper_bal.df$ioer_inc[eper_bal.df$ioer_diff > 0] <- 1

# Decrease
eper_bal.df$ioer_dec <- 0
eper_bal.df$ioer_dec[eper_bal.df$ioer_diff < 0] <- 1


### Increase and decrease at t, t-1, or t+1 
eper_bal.df$ioer_inc_lag1 <- ave(eper_bal.df$ioer_inc,
                                 by = eper_bal.df$facilityid,
                                 FUN = function(x) dplyr::lag(x))
eper_bal.df$ioer_inc_lead1 <- ave(eper_bal.df$ioer_inc,
                                 by = eper_bal.df$facilityid,
                                 FUN = function(x) dplyr::lead(x, default = 0))

eper_bal.df$ioer_inc_all <- eper_bal.df$ioer_inc + eper_bal.df$ioer_inc_lag1 + eper_bal.df$ioer_inc_lead1
eper_bal.df$ioer_inc_all[eper_bal.df$ioer_inc_all > 0] <- 1


eper_bal.df$ioer_dec_lag1 <- ave(eper_bal.df$ioer_dec,
                                 by = eper_bal.df$facilityid,
                                 FUN = function(x) dplyr::lag(x))
eper_bal.df$ioer_dec_lead1 <- ave(eper_bal.df$ioer_dec,
                                  by = eper_bal.df$facilityid,
                                  FUN = function(x) dplyr::lead(x, default = 0))

eper_bal.df$ioer_dec_all <- eper_bal.df$ioer_dec + eper_bal.df$ioer_dec_lag1 + eper_bal.df$ioer_dec_lead1
eper_bal.df$ioer_dec_all[eper_bal.df$ioer_dec_all > 0] <- 1




#####################################
#### Create new operation period ####
#####################################



### Rule 2 imputation ------------------------------------------------
### New indicator for start and end based on EPRTR ieor

eper_bal.df$start_ioer <- eper_bal.df$start * eper_bal.df$ioer_inc_all
eper_bal.df$end_ioer <- eper_bal.df$end * eper_bal.df$ioer_dec_all

table(eper_bal.df$start)
table(eper_bal.df$start_ioer)

table(eper_bal.df$reportingyear[eper_bal.df$start == 1])
table(eper_bal.df$reportingyear[eper_bal.df$start_ioer == 1])


table(eper_bal.df$end)
table(eper_bal.df$end_ioer)

table(eper_bal.df$reportingyear[eper_bal.df$end == 1])
table(eper_bal.df$reportingyear[eper_bal.df$end_ioer == 1])


### Operation from start to beginning (or from 2007 to 2018 otherwise)

eper_bal.df$fy_ioer <- NA
eper_bal.df$fy_ioer[which(eper_bal.df$start_ioer == 1)] <- eper_bal.df$reportingyear[which(eper_bal.df$start_ioer == 1)]
eper_bal.df$fy_ioer <- ave(eper_bal.df$fy_ioer,
                           by = eper_bal.df$facilityid,
                           FUN = function(x) min(x, na.rm = TRUE))
eper_bal.df$fy_ioer[is.infinite(eper_bal.df$fy_ioer)] <- 2007

eper_bal.df$ly_ioer <- NA
eper_bal.df$ly_ioer[which(eper_bal.df$end_ioer == 1)] <- eper_bal.df$reportingyear[which(eper_bal.df$end_ioer == 1)]
eper_bal.df$ly_ioer <- ave(eper_bal.df$ly_ioer,
                           by = eper_bal.df$facilityid,
                           FUN = function(x) min(x, na.rm = TRUE))
eper_bal.df$ly_ioer[is.infinite(eper_bal.df$ly_ioer)] <- 2018 # as with end date, use year after last reporting


### Operation between those two dates (note: end is year after last reporting year)

eper_bal.df$operation_ioer <- NA 
eper_bal.df$operation_ioer[which(eper_bal.df$reportingyear >= 2007 &
                                   eper_bal.df$reportingyear <= 2017)] <- 0 

eper_bal.df$operation_ioer[which(eper_bal.df$reportingyear >= eper_bal.df$fy_ioer &
                                   eper_bal.df$reportingyear < eper_bal.df$ly_ioer)] <- 1 #exlcude equal for ly 


eper_bal.df <- eper_bal.df[order(eper_bal.df$facilityid, eper_bal.df$reportingyear), ]





### Reduce the data frame to operation perion

eper_bal_red.df <- eper_bal.df[which(eper_bal.df$operation_ioer == 1), ]

### Descriptives
table(length(unique(eper_bal_red.df$facilityid)))
table(eper_bal_red.df$reportingyear)
mean(table(eper_bal_red.df$reportingyear))

# original
table(eper_bal_red.df$reportingyear[eper_bal.df$operation == 1])
mean(table(eper_bal_red.df$reportingyear[eper_bal.df$operation == 1]))


save("eper_bal_red.df", file = "Eper_balanced.RData")


### Save as spatial object
eper_bal_red.spdf <- st_as_sf(eper_bal_red.df, coords = c("lon", "lat"),
                              crs = CRS("+proj=longlat +datum=WGS84"))

save("eper_bal_red.spdf", file = "Eper_balanced_sp.RData")






###########################################
#### Generate longitudinal Gemverb. Df ####
###########################################

years <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

gemverb.df <- data.frame(gemverb.spdf)

for(i in years){
  if(i==2007){
    gemverb_long.df <- gemverb.df
    gemverb_long.df$reportingyear <- i
  }else{
    tmp <- gemverb.df
    tmp$reportingyear <- i
    gemverb_long.df <- rbind(gemverb_long.df, tmp)
  }
}

### Reorder
gemverb_long.df <- gemverb_long.df[, c("kennziffer", "name", "reportingyear", 
                                     names(gemverb_long.df)[-which(names(gemverb_long.df) %in% 
                                                                     c("kennziffer", "name", "reportingyear"))])]








##############################################################
#### Calculate Proportional overalp between Gem and EPRTR ####
##############################################################


# Create 1 km radius Buffer around EPER points
buff <- 1000
eper_ger_buf.spdf <- st_buffer(eper_ind_loc.spdf, dist = buff)



### Projection
gemverb.spdf <- st_transform(gemverb.spdf, st_crs(eper_ger_buf.spdf))

# Correct ring self-intersection
gemverb.spdf <- st_make_valid(gemverb.spdf)

### Ids
rownames(gemverb.spdf) <-  as.character(gemverb.spdf$kennziffer)
rownames(eper_ger_buf.spdf) <- as.character(eper_ger_buf.spdf$facilityid)


# Compute Overlay of Buffer Circle and Gemeinden

# Loop for the Overlap calculation (splits into chunks, not really necessary in this case)
for(i in seq(1, nrow(eper_ger_buf.spdf), 100)){
  if(i < (nrow(eper_ger_buf.spdf) - 100)){
    j <- i + 99
  }else{
    j <- i + (nrow(eper_ger_buf.spdf) - i)
  }
  tmp1 <- eper_ger_buf.spdf[i:j, ]
  tmp2 <- gemverb.spdf[tmp1, ]             #Simplify to speed up
  overlap.pol <- st_intersection(tmp1, tmp2, byid = TRUE)
  if(i == 1){
    overlap.mat <- data.frame(facilityid = overlap.pol$facilityid,
                              kennziffer = overlap.pol$kennziffer,
                              overlap = as.numeric(st_area(overlap.pol)))
  }else{
    overlap.mat <- rbind(overlap.mat,
                         data.frame(facilityid = overlap.pol$facilityid,
                                    kennziffer = overlap.pol$kennziffer,
                                    overlap = as.numeric(st_area(overlap.pol))))
  }
  print(i)
}

# save(overlap.mat, file = "Overlap_Matrix_2017_1km.RData")
# 
# load("Overlap_Matrix_2017_1km.RData")


### Test
table(overlap.mat$kennziffer %in% gemverb.spdf$kennziffer) # all gemid valid kennziffer?
summary(overlap.mat$overlap)


### Proportion of area (use area of buffer instead of exact circle area calculation )
(120/2) * sin( (2*pi / 120) ) * 1000^2 # for 120 points "circle"
overlap.mat$percentage_overlap <- overlap.mat$overlap / as.numeric(st_area(eper_ger_buf.spdf[1,]))





###########################################################################
#### Calculate number of facilities based on overlap and balanced data ####
###########################################################################

overlap.df <- overlap.mat

### Merge pollution to balanced data 
eper_ger_ind.spdf$totalquantity_air <- eper_ger_ind.spdf$totalquantity * eper_ger_ind.spdf$air
eper_ind_unique.df <- aggregate(data.frame(eper_ger_ind.spdf)[, c("totalquantity_air", "totalquantity_tox")],
                                by = list(facilityid = eper_ger_ind.spdf$facilityid,
                                          reportingyear = eper_ger_ind.spdf$reportingyear),
                                FUN = function(x) sum(x, na.rm = TRUE))



### Merge balanced data with annual pollution
eper_bal_red.df <- plyr::join(eper_bal_red.df, eper_ind_unique.df, 
                         by = c("facilityid", "reportingyear"), type = "left", match = "all")


### Perform linear interpolation
# Set NA if zero
eper_bal_red.df$totalquantity_air[which(eper_bal_red.df$totalquantity_air == 0)] <- NA
eper_bal_red.df$totalquantity_tox[which(eper_bal_red.df$totalquantity_tox == 0)] <- NA

# Linear interpolation (only between non-NA values)
interpolfun <- function(x, varx, vary){
  v1 <- x[, varx]
  v2 <- x[, vary]
  nona <- which(!is.na(v2))
  if(length(nona) < 2){
    res <- v2
  }else{
    res <- approx(x = v1, y = v2,
                  xout = v1, rule = 1)$y
  }
  return(res)
}

# Order
eper_bal_red.df <- eper_bal_red.df[order(eper_bal_red.df$facilityid, eper_bal_red.df$reportingyear), ]

# air
imp <- by(eper_bal_red.df, eper_bal_red.df$facilityid,
          FUN = function(x) interpolfun(x = x, varx = "reportingyear", vary = "totalquantity_air"))
eper_bal_red.df$totalquantity_air_imp <- unlist(imp)


# Tox air
imp <- by(eper_bal_red.df, eper_bal_red.df$facilityid,
          FUN = function(x) interpolfun(x = x, varx = "reportingyear", vary = "totalquantity_tox"))
eper_bal_red.df$totalquantity_tox_imp <- unlist(imp)



### Merge overlap with balanced df
overlap.df <- plyr::join(overlap.df, eper_bal_red.df, 
                        by = "facilityid", type = "left", match = "all")  


### Create proportionally weighted facilities


### Sum per Gemverb and year (exclude NAs)
overlap.df$number_ind_perc <- overlap.df$operation_ioer * overlap.df$percentage_overlap 
overlap.df$number_ind_perc <- ave(overlap.df$number_ind_perc,
                              overlap.df$kennziffer, overlap.df$reportingyear,
                                   FUN = function(x) sum(x, na.rm = T))

overlap.df$air_imp <- overlap.df$totalquantity_air_imp * overlap.df$percentage_overlap 
overlap.df$air_imp <- ave(overlap.df$air_imp,
                                  overlap.df$kennziffer, overlap.df$reportingyear,
                                  FUN = function(x) sum(x, na.rm = T))

overlap.df$air_tox_imp <- overlap.df$totalquantity_tox_imp * overlap.df$percentage_overlap 
overlap.df$air_tox_imp <- ave(overlap.df$air_tox_imp,
                                  overlap.df$kennziffer, overlap.df$reportingyear,
                                  FUN = function(x) sum(x, na.rm = T))


### Reduce to single obs per Gemverb and year
pollution.df <- overlap.df[, c("kennziffer", "reportingyear", "number_ind_perc", "air_imp", "air_tox_imp")]
pollution.df <- unique(pollution.df)



### Add to gemverb long
gemverb_long.df <- merge(gemverb_long.df, pollution.df, by = c("kennziffer", "reportingyear"), all.x = T)

### Replace NA by zero
gemverb_long.df$number_ind_perc[is.na(gemverb_long.df$number_ind_perc)] <- 0
gemverb_long.df$air_imp[is.na(gemverb_long.df$air_imp)] <- 0
gemverb_long.df$air_tox_imp[is.na(gemverb_long.df$air_tox_imp)] <- 0




###############################################################################
#### Calculate pollution and num facilities based on un-validated raw data ####
###############################################################################

overlap.df <- overlap.mat

### Merge with raw industrial data
overlap.df <- plyr::join(overlap.df, eper_ind_unique.df, 
                         by = "facilityid", type = "left", match = "all")


### Create proportionally weighted facilities
overlap.df$fac_per <- 1 * overlap.df$percentage_overlap 


### Sum per Gemverb and year (exclude NAs)
overlap.df$number_ind_perc_raw <- ave(overlap.df$fac_per,
                                  overlap.df$kennziffer, overlap.df$reportingyear,
                                  FUN = function(x) sum(x, na.rm = T))


### Total amount of pollution to air
overlap.df$air_raw <- overlap.df$totalquantity_air * overlap.df$percentage_overlap 
overlap.df$air_raw <- ave(overlap.df$air_raw,
                              overlap.df$kennziffer, overlap.df$reportingyear,
                              FUN = function(x) sum(x, na.rm = T))


### Toxicity weighted pollution
overlap.df$air_tox_raw <- overlap.df$totalquantity_tox * overlap.df$percentage_overlap 
overlap.df$air_tox_raw <- ave(overlap.df$air_tox_raw,
                              overlap.df$kennziffer, overlap.df$reportingyear,
                              FUN = function(x) sum(x, na.rm = T))


### Reduce to single obs per Gemverb and year
pollution_raw.df <- overlap.df[, c("kennziffer", "reportingyear", "number_ind_perc_raw", "air_raw", "air_tox_raw")]
pollution_raw.df <- unique(pollution_raw.df)



### Add to gemverb long
gemverb_long.df <- merge(gemverb_long.df, pollution_raw.df, by = c("kennziffer", "reportingyear"), all.x = T)

### Replace NA by zero
gemverb_long.df$number_ind_perc_raw[is.na(gemverb_long.df$number_ind_perc_raw)] <- 0
gemverb_long.df$air_raw[is.na(gemverb_long.df$air_raw)] <- 0
gemverb_long.df$air_tox_raw[is.na(gemverb_long.df$air_tox_raw)] <- 0






##############
#### Save ####
##############

### For path depdency reasons, add other industry variables (set to NA)

gemverb_long.df$number_all_perc <- NA
gemverb_long.df$number_air_perc <- NA
gemverb_long.df$number_tox_perc <- NA

save(gemverb_long.df, file = "EPER_Gemverb_2017.RData")








