# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Construct variables, lags and ranks


rm(list = ls())

library(plyr)
library(dplyr)
library(spdep)
library(sf)
library(tmap)
library(tmaptools)

# library(extrafont)
# loadfonts()


### Disable sientific notation for rownames
options(scipen = 999)


setwd("../02_Data")


### Load ###

load("EPER_INKAR_2017.RData")
load("Gemeindeverbaende_Spatial_2017.RData")
load("Eper_balanced_sp.RData")




###################
#### Drop 2007 ####
###################


### Drop 2007 (because of unrealistic changes between 2007 and 2008)

eper_inkar_final.spdf <- eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear != 2007, ]


#############################
#### construct variables ####
#############################


### Metropolitan area
eper_inkar_final.spdf$city<-0
eper_inkar_final.spdf$city[eper_inkar_final.spdf$typ==10] <- 1

### East Germany (kreis 11000000==Berlin)
eper_inkar_final.spdf$east<-0
eper_inkar_final.spdf$east[eper_inkar_final.spdf$wo == 2] <- 1

### Population density
eper_inkar_final.spdf$popdens<-eper_inkar_final.spdf$einwohnerdichte

### under 18
eper_inkar_final.spdf$einwohner18juj <- eper_inkar_final.spdf$einwohnerunter6jahre + eper_inkar_final.spdf$einwohnervon6bisunter18jahren




###############################
#### Standardize variables ####
###############################

vars<-c( "number_ind_perc", 
        "einkommensteuer", "gewerbesteuer", 
        "arbeitslosigkeit", "anteilarbeitsloseauslnder",
        "frauenanteil", 
        "einwohner65jahreundlter", "einwohner18juj",
        "city", "popdens")

for(i in vars){
  tmp <- scale(st_drop_geometry(eper_inkar_final.spdf[, which(names(eper_inkar_final.spdf) == i)]))
  colnames(tmp) <- paste("std_", i, sep="")
  if(i==vars[1]){
    std.df <- data.frame(tmp)
  }else{
    std.df <- cbind(std.df, data.frame(tmp))
  }
}


eper_inkar_final.spdf <- cbind(eper_inkar_final.spdf, std.df)



###########################
#### Gen spatial lag X ####
###########################

### Gen interactions first

eper_inkar_final.spdf$std_gewerbesteuer_sq <- eper_inkar_final.spdf$std_gewerbesteuer^2
eper_inkar_final.spdf$std_popdens_sq <- eper_inkar_final.spdf$std_popdens^2

### temporal lag
oo <- order(eper_inkar_final.spdf$kennziffer, eper_inkar_final.spdf$reportingyear)
eper_inkar_final.spdf <- eper_inkar_final.spdf[oo,]


eper_inkar_final.spdf$l_std_number_ind_perc <- ave(eper_inkar_final.spdf$std_number_ind_perc,
                                                 eper_inkar_final.spdf$kennziffer,
                                                 FUN=function(x) dplyr::lag(x,1))


eper_inkar_final.spdf$l_std_einkommensteuer <- ave(eper_inkar_final.spdf$std_einkommensteuer,
                                        eper_inkar_final.spdf$kennziffer,
                                        FUN=function(x) dplyr::lag(x,1))


### Second and third lag

eper_inkar_final.spdf$l2_std_number_ind_perc <- ave(eper_inkar_final.spdf$std_number_ind_perc,
                                                 eper_inkar_final.spdf$kennziffer,
                                                 FUN=function(x) dplyr::lag(x,2))
eper_inkar_final.spdf$l2_std_einkommensteuer <- ave(eper_inkar_final.spdf$std_einkommensteuer,
                                                 eper_inkar_final.spdf$kennziffer,
                                                 FUN=function(x) dplyr::lag(x,2))

eper_inkar_final.spdf$l3_std_number_ind_perc <- ave(eper_inkar_final.spdf$std_number_ind_perc,
                                                  eper_inkar_final.spdf$kennziffer,
                                                  FUN=function(x) dplyr::lag(x,3))
eper_inkar_final.spdf$l3_std_einkommensteuer <- ave(eper_inkar_final.spdf$std_einkommensteuer,
                                                 eper_inkar_final.spdf$kennziffer,
                                                 FUN=function(x) dplyr::lag(x,3))

  


### Spatial lags
vars<-c(paste("std_", vars, sep=""), "std_gewerbesteuer_sq", "std_popdens_sq",
        "l_std_einkommensteuer", "l_std_number_ind_perc", 
        "l2_std_einkommensteuer", "l2_std_number_ind_perc", "l3_std_einkommensteuer", "l3_std_number_ind_perc",
        "city")



#######################
#### Final weights ####
#######################

### Reorder data by cross-sections, then time
oo <- order(eper_inkar_final.spdf$reportingyear,eper_inkar_final.spdf$kennziffer)
eper_inkar_final.spdf <- eper_inkar_final.spdf[oo,]

# Queens links:
tmp <- eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear == 2010, ]
rownames(tmp) <- gsub("2010_", "", rownames(tmp))
eper.nb <- poly2nb(tmp)

# Extract nolink obs
nolink_ids <- as.numeric(attr(subset(eper.nb, subset = card(eper.nb) == 0), "region.id"))
unique(eper_inkar_final.spdf$name[which(eper_inkar_final.spdf$kennziffer %in% nolink_ids)])

# Impute nearest nb
coords <- st_centroid(tmp)
eper.1nb <- knn2nb(knearneigh(coords, k = 1), row.names = rownames(tmp))

eper2.nb <- eper.nb
eper2.nb[card(eper2.nb) == 0] <- eper.1nb[match(nolink_ids, attr(eper.1nb, "region.id"))]

# Weights (row standardised)
eper.lw <- nb2listw(eper2.nb, style = "W", zero.policy=TRUE)

save(eper.lw, file = "Eper_QueensNB_2017.RData")




######################
#### Spatial lags ####
######################


wx.df <- st_drop_geometry(eper_inkar_final.spdf[, c("kennziffer", "reportingyear")])

wx.df <- cbind(wx.df, (matrix(NA, nrow=nrow(wx.df), ncol=length(vars))))
names(wx.df)[-c(1:2)] <- paste("W_", vars, sep="")



# Loop over for each year
for(y in unique(eper_inkar_final.spdf$reportingyear)){
  tmp.df <- data.frame(eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear == y, ])
  rownames(tmp.df) <- tmp.df$kennziffer
  ids <- tmp.df$kennziffer
  
  # # Subset lw to years ob
  W <- listw2mat(eper.lw)
  # keep <- which(rownames(W) %in% ids)
  # W <- W[keep, keep]
  # #tmp.lw <- mat2listw(W)
  
  # Gen WX
  for(i in vars){
    wx <- NA # start with NA to avoid using last vars vector (in case of error)
    x <- tmp.df[, which(names(tmp.df)==i), drop=F]
    # x <- x[match(rownames(W), rownames(x)), ,drop=F]
    # if(any(is.na(x))){ # Drop NAs (this should be entire cities, thus allow unbalanced)
    #   r <- which(!is.na(x))
    #   x <- x[r, , drop =F]
    #   W <- W[r, r]
    # }
    wx <-  W %*% as.vector(x[match(rownames(W), rownames(x)), ])
    oo <- match(wx.df$kennziffer[wx.df$reportingyear == y], as.numeric(rownames(wx)))
    wx <- wx[oo, ]
    wx.df[wx.df$reportingyear == y, which(names(wx.df)==paste("W_", i, sep=""))] <- wx
  }
  
}

### Bind SLX
eper_inkar_final.spdf <- merge(eper_inkar_final.spdf, wx.df, by = c("kennziffer", "reportingyear"))





############################
#### Regression dataset ####
############################

eper_inkar_final.df <- st_drop_geometry(eper_inkar_final.spdf)




########################
#### Construct rank ####
########################

# Make ids numeric
eper_inkar_final.df$kennziffer <- as.numeric(as.character(eper_inkar_final.df$kennziffer))
eper_inkar_final.df$reportingyear <- as.numeric(as.character(eper_inkar_final.df$reportingyear))



########################
#### Construct rank ####
########################

W <- listw2mat(eper.lw)

## un-normalize W

W[W > 0] <- 1

W[W == 0] <- NA

### Get nbs vlues

years <- unique(eper_inkar_final.df$reportingyear)

ranks <- cbind(eper_inkar_final.df[, c("kennziffer", "reportingyear")], rank = NA)

for(i in years){
  tmp <- eper_inkar_final.df[eper_inkar_final.df$reportingyear == i,]
  oo <- match(as.numeric(rownames(W)), tmp$kennziffer)
  tmp <- tmp[oo, ]
  x <- tmp[, c("number_ind_perc_pa")]
  
  WX <- apply(W, 2, FUN = function(z) z * x)
  WX2 <- rbind(x, WX)
  
  # Make ranks per col
  res <- apply(WX2, 2, FUN = function(z) rank(z, na.last = "keep"))
  n <- apply(res, 2, FUN = function(z) length(which(!is.na(z))))
  res <- res[1, ]
  
  # # relative rank (to number of nbs)
  # res <- res/n
  
  
  # Match order and paste values
  res <- res[match(ranks$kennziffer[ranks$reportingyear == i], rownames(WX))]
  ranks[ranks$reportingyear == i, "rank"] <- res
}



eper_inkar_final.df <- merge(eper_inkar_final.df, ranks, by = c("kennziffer", "reportingyear"))



###################
#### Time lags ####
###################
eper_inkar_final.df <- eper_inkar_final.df[order(eper_inkar_final.df$kennziffer, eper_inkar_final.df$reportingyear), ] 

eper_inkar_final.df$l_rank<-ave(eper_inkar_final.df$rank,
                                eper_inkar_final.df$kennziffer,
                                FUN=function(x) dplyr::lag(x,1))

eper_inkar_final.df$l2_rank<-ave(eper_inkar_final.df$rank,
                                 eper_inkar_final.df$kennziffer,
                                 FUN=function(x) dplyr::lag(x,2))

eper_inkar_final.df$l3_rank<-ave(eper_inkar_final.df$rank,
                                 eper_inkar_final.df$kennziffer,
                                 FUN=function(x) dplyr::lag(x,3))

# Standardize
eper_inkar_final.df$std_rank <- scale(eper_inkar_final.df$rank)
eper_inkar_final.df$l_std_rank <- ave(eper_inkar_final.df$std_rank,
                                      eper_inkar_final.df$kennziffer,
                                      FUN=function(x) dplyr::lag(x,1))



### Save
save(eper_inkar_final.df, file = "EPER_INKAR_2017_reg.RData")





############################
#### Plot ranks example ####
############################
shadowtext <- function(x, y=NULL, labels, col='white', bg='black', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}




# Projection
eper_bal_red.spdf <- st_transform(eper_bal_red.spdf, st_crs(eper_inkar_final.spdf))

# Eper delete duplicates
eper <- eper_bal_red.spdf[which(!duplicated(eper_bal_red.spdf$facilityid)),]

# E.g. community 1002000
id <- 1002000
tmp <- eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear == i, ]

nbs <- unlist(eper.lw$neighbours[which(attr(eper.lw, "region.id") == id)])
ids <- c(id, unlist(attr(eper.lw, "region.id")[nbs]))

bsp <- tmp[tmp$kennziffer %in% ids,]

## Add ranks
res <- apply(WX2, 2, FUN = function(z) rank(z, na.last = "keep"))
res <- res[, which(rownames(WX2) == as.character(id)) - 1]
ord <- match(as.character(bsp$kennziffer[-which(bsp$kennziffer==id)]), rownames(WX2))
res[ord]

bsp$rank_tmp <- NA
bsp$rank_tmp[which(bsp$kennziffer==id)] <- res[1]
bsp$rank_tmp[-which(bsp$kennziffer==id)] <- res[ord]

bsp_cent <- st_centroid(bsp, byid = T)


# E.g. community 1058903
id2 <- 1058903

tmp <- eper_inkar_final.spdf[eper_inkar_final.spdf$reportingyear == i, ]

nbs2 <- unlist(eper.lw$neighbours[which(attr(eper.lw, "region.id") == id2)])
ids2 <- c(id2, unlist(attr(eper.lw, "region.id")[nbs2]))

bsp2 <- tmp[tmp$kennziffer %in% ids2,]

## Add ranks
res2 <- apply(WX2, 2, FUN = function(z) rank(z, na.last = "keep"))
res2 <- res2[, which(rownames(WX2) == as.character(id2)) - 1]
ord2 <- match(as.character(bsp2$kennziffer[-which(bsp2$kennziffer==id2)]), rownames(WX2))
res2[ord2]

bsp2$rank_tmp <- NA
bsp2$rank_tmp[which(bsp2$kennziffer==id2)] <- res2[1]
bsp2$rank_tmp[-which(bsp2$kennziffer==id2)] <- res2[ord2]

bsp2_cent <- st_centroid(bsp2, byid = T)


bsp_ges <- rbind(bsp, bsp2)




### Eper in bst 2km distance
eper_bsp <- st_as_sf(eper)
eper_bsp <- eper_bsp[st_buffer(bsp_ges, dist = 2000),]


#### Plot
bsp <- st_as_sf(bsp)

### Define colours
col1 <- c("#FBF6E7",
          "#D6C39D",
          "#B59E77",
          "#957A51",
          "#74552B")
# col1 <- rev(col1)
col2 <- c("#f5f5f5",
          "#c7eae5",
          "#80cdc1",
          "#35978f",
          "#01665e")
colr <- "#003c30"


# Plot 1
plot1 <- tm_shape(bsp_ges) +
  tm_borders(col = alpha("grey60", 00)) +
  tm_shape(bsp) +  #print
  tm_fill(col = "number_ind_perc_pa", 
          title = "Number of industrial \nfacilities per area",
          style = "kmeans", 
          n = 5,
          midpoint = NA,
          palette = col1,
          legend.format = list(scientific = TRUE, format = "f")) +
  tm_layout(main.title = "a", main.title.position = "center",
            #legend.width = 0.5, legend.height = 0.9, legend.hist.height = 0.3,
            legend.title.size = 1.2, 
            legend.text.size = 1, legend.hist.size = 1,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            main.title.fontface = "bold",
            frame=FALSE) +
  tm_shape(bsp_ges) +
  tm_borders(col = alpha("grey60", 0.6)) 
plot1

plot1 <- plot1 +
  tm_shape(eper_bsp) +
  tm_symbols(shape = 24,  
             border.col = "black", 
             col = "orange") +
  tm_shape(bsp[bsp$kennziffer == id,]) +
  tm_borders(col = "red", lwd = 2) +
  tm_shape(st_centroid(bsp)) +
  tm_text("rank_tmp", size = 3, col  = colr) +
  tm_shape(st_centroid(bsp[bsp$kennziffer == id,])) +
  tm_text("rank_tmp", size = 3, col  = "red")
plot1


# Plot2
plot2 <- tm_shape(bsp_ges) +
  tm_borders(col = alpha("grey60", 00)) +
  tm_shape(bsp2) +  #print
  tm_fill(col = "number_ind_perc_pa", 
          title = "Number of industrial \nfacilities per area",
          style = "kmeans", 
          n = 5,
          midpoint = NA,
          palette = col1,
          legend.format = list(scientific = TRUE, format = "f")) +
  tm_layout(main.title = "b", main.title.position = "center",
            #legend.width = 0.5, legend.height = 0.9, legend.hist.height = 0.3,
            legend.title.size = 1.2, 
            legend.text.size = 1, legend.hist.size = 1,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            main.title.fontface = "bold",
            frame=FALSE) +
  tm_shape(bsp_ges) +
  tm_borders(col = alpha("grey60", 0.6)) 
plot2

plot2 <- plot2 +
  tm_shape(eper_bsp) +
  tm_symbols(shape = 24,  
             border.col = "black", 
             col = "orange") +
  tm_shape(bsp2[bsp2$kennziffer == id2,]) +
  tm_borders(col = "red", lwd = 2) +
  tm_shape(st_centroid(bsp2)) +
  tm_text("rank_tmp", size = 3, col  = colr) +
  tm_shape(st_centroid(bsp2[bsp2$kennziffer == id2,])) +
  tm_text("rank_tmp", size = 3, col  = "red")
plot2


cairo_pdf(file=paste("../03_Output/", "Figure2.pdf", sep=""), width=12, height=6, bg = "white", 
          # family="CM Roman"
          )
par(mar=c(0,0,0,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
tmap_arrange(plot1, plot2)
dev.off()






