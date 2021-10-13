# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Read IOER from tifs extracted with arc gis

rm(list = ls())

library(rgeos)
library(spdep)
library(raster)
library(sf)


setwd("../02_Data")


### Get spatial raster object
y <- 2006
f <- paste("./ioer/", "ind_area_", y, ".tiff", sep = "")

ind_2006.rs <- raster(x = f)
ioer.sp <- rasterToPolygons(ind_2006.rs, n = 4, na.rm = TRUE, digits = 12)

# gen ids
ioer.sp$id <- sapply(slot(ioer.sp, "polygons"), function(x) slot(x, "ID")) 

# get centroids of polygons
centroids <- coordinates(ioer.sp)
ioer.sp$x <- centroids[,1]
ioer.sp$y <- centroids[,2]


### Read data values for each year
years <- c(2007:2018)

ioer.spdf <- ioer.sp
for(y in years){
  f <- paste("./ioer/", "ind_area_", y, ".tiff", sep = "")
  
  tmp.rs <- raster(x = f)
  tmp.df <- as.data.frame(tmp.rs, na.rm = TRUE, xy = TRUE, centroids = TRUE)
  
  ioer.spdf <- merge(ioer.spdf, tmp.df, by = c("x", "y"))
  
}

# Reorder vars

ioer.spdf <- ioer.spdf[, match(c("id", "x", "y", paste("ind_area_", c(2006:2018), sep = "")), names(ioer.spdf))]


### Save
save(ioer.spdf, file = "IOER_industrial.RData")
