# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Add USEtox toxicity weights


rm(list = ls())

library(sf)
library(plyr)
library(zoo)



setwd("../02_Daten")




####################
#### EPRTR load ####
####################

load("eper_release_ger_v16.RData") 

# Drop obs with missing coordinates
eper_ger_new.df <- subset(eper_ger.df, !is.na(eper_ger.df$lat) & !is.na(eper_ger.df$lon))

# Make spatial df
eper_ger.spdf <- st_as_sf(eper_ger_new.df, coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84")





######################################
#### Load USEtox toxicity weights ####
######################################

### Organics
head <- read.table("./USEtox_2.12/Results/USEtox_results_organics.csv", sep = ",",
                     header = F, stringsAsFactors = FALSE, skip = 1, nrows = 3)
head[1, ] <- gsub("([A-Za-z]+).*", "\\1", head[1, ])
head[2, ] <- gsub("Emission to ", "", head[2, ])
head[1, ][which(head[1, ] == "")] <- NA
head[2, ][which(head[2, ] == "")] <- NA
head[1, ] <- t(zoo::na.locf(t(head[1, ])))
head[2, ] <- t(zoo::na.locf(t(head[2, ]), na.rm = FALSE))
names <- make.names(apply(head, 2, FUN = function(x) paste0(x, collapse = "_")))
names[c(1:3)] <- c("NA", "CAS", "Name")

tox1.df <- read.csv("./USEtox_2.12/Results/USEtox_results_organics.csv", sep = ",",
                   header = F, stringsAsFactors = FALSE, skip = 4, na.strings = "n/a")
names(tox1.df) <- names
tox1.df <- tox1.df[, -1]


### InOrganics
head <- read.table("./USEtox_2.12/Results/USEtox_results_inorganics.csv", sep = ",",
                   header = F, stringsAsFactors = FALSE, skip = 1, nrows = 3)
head[1, ] <- gsub("([A-Za-z]+).*", "\\1", head[1, ])
head[2, ] <- gsub("Emission to ", "", head[2, ])
head[1, ][which(head[1, ] == "")] <- NA
head[2, ][which(head[2, ] == "")] <- NA
head[1, ] <- t(zoo::na.locf(t(head[1, ])))
head[2, ] <- t(zoo::na.locf(t(head[2, ]), na.rm = FALSE))
names <- make.names(apply(head, 2, FUN = function(x) paste0(x, collapse = "_")))
names[c(1:3)] <- c("NA", "CAS", "Name")

tox2.df <- read.csv("./USEtox_2.12/Results/USEtox_results_inorganics.csv", sep = ",",
                    header = F, stringsAsFactors = FALSE, skip = 4, na.strings = "n/a")
names(tox2.df) <- names
tox2.df <- tox2.df[, -1]


### Combine 
tox.df <- rbind(tox1.df, tox2.df)




### USe Endpoint urban air tox weights total

toxweights.df <- tox.df[, c("CAS", "Name", "Endpoint_urban.air_total")]
names(toxweights.df)[3] <- "tox_score"




######################################
#### Test for merging with E-PRTR ####
######################################


test.df <- data.frame(eper_ger.spdf)
test.df <- test.df[,which(names(test.df) %in% 
                            c("pollutantname", "pollutantcode",
                              "pollutantcas", "releasemediumcode"))]
test.df <- unique(test.df)


# Merge to tox
tox_test.df <- merge(test.df, toxweights.df, by.x = "pollutantcas", by.y = "CAS", all.x = T, all.y = F)



### Add CAS for inorganic compounds to E-PRTR

caslist <- c(
  	"Ag(I)   "        =  "14701-21-4",
  	"Al(III) "        =  "22537-23-1",
  	"As(III) "        =  "22541-54-4",
  	"AS AND COMPOUNDS"        =  "17428-41-0",
  	"Ba(II)  "        =  "22541-12-4",
  	"Be(II)  "        =  "22537-20-8",
  	"CD AND COMPOUNDS"        =  "22537-48-0",
  	"Co(II)  "        =  "22541-53-3",
  	"Cr(III) "        =  "16065-83-1",
  	"CR AND COMPOUNDS"        =  "18540-29-9",
  	"Cs(I)   "        =  "18459-37-5",
  	"CU AND COMPOUNDS"        =  "15158-11-9",
  	"Fe(II)  "        =  "15438-31-0",
  	"Fe(III) "        =  "20074-52-6",
  	"HG AND COMPOUNDS"        =  "14302-87-5",
  	"Mn(II)  "        =  "16397-91-4",
  	"Mo(VI)  "        =  "16065-87-5",
  	"NI AND COMPOUNDS"        =  "14701-22-5",
  	"PB AND COMPOUNDS"        =  "14280-50-3",
  	"Sb(III) "        =  "23713-48-6",
  	"Sb(V)   "        =  "22537-51-5",
  	"Se(IV)  "        =  "22541-55-5",
  	"Sn(II)  "        =  "22541-90-8",
  	"Sr(II)  "        =  "22537-39-9",
  	"Tl(I)   "        =  "22537-56-0",
  	"V(V)    "        =  "15121-26-3",
  	"ZN AND COMPOUNDS"        =  "23713-49-7"
)

for(i in names(caslist)){
  c <- caslist[i]
  oo <- which(eper_ger.spdf$pollutantcode == i) 
  eper_ger.spdf$pollutantcas[oo] <- c
}





################################
#### Merge with tox weights ####
################################

names(toxweights.df)[1] <- "pollutantcas"



eper_ger.spdf <- merge(eper_ger.spdf, toxweights.df[, c("pollutantcas", "tox_score")],
                       all.x = TRUE, all.y = FALSE)

summary(eper_ger.spdf$tox_score)





###############################
#### Hazard-based emission ####
###############################

# Without tox weight, assume weight of zero
eper_ger.spdf$tox_score[is.na(eper_ger.spdf$tox_score)] <- 0

# Gen dummy for tox facility
eper_ger.spdf$tox <- 0
eper_ger.spdf$tox[which(eper_ger.spdf$tox_score>0 & eper_ger.spdf$releasemediumcode=="AIR")] <- 1

# Gen dummy for air emission
eper_ger.spdf$air <- 0
eper_ger.spdf$air[eper_ger.spdf$releasemediumcode=="AIR"] <- 1


# Compute toxic air pollution, use only air pollution
eper_ger.spdf$totalquantity_tox <- eper_ger.spdf$totalquantity*eper_ger.spdf$tox_score*eper_ger.spdf$air



#### Save

save(eper_ger.spdf, file = "Eper_sp_v16.RData")




