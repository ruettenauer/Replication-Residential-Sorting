# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Read, combine and match INKAR data


rm(list = ls())

library(plyr)
library(dplyr)
library(sf)
library(spdep)



setwd("../02_Data")


### Load ###

load("Gemeindeverbaende_Spatial_2017.RData")
load("Eper_sp_v16.RData")
load("EPER_Gemverb_2017.RData")


# Gem Makro 

gemverb.df <- read.table("Gemverb_Makro.csv", sep = ",", header = T)
gem.df <- read.table("Gem-Gemverb_Makro.csv", sep = ",", header = T)



########################################
### Reshape inkar from original data ###
########################################


i <- dir("./inkar")

j <- i[grepl("INKAR_Gemeindeverbaende_", i)]
c <- 1

for(k in j){
  header <- as.vector(t(read.table(paste0("./inkar/", k), nrows = 1, sep = ";")[1,]))
  # Clean header
  header <- gsub(" ", "", header)
  header <- gsub("\\.", "", header)
  header <- gsub("ä", "ae", header)
  header <- gsub("ü", "ue", header)
  header <- gsub("ö", "oe", header)
  header <- iconv(header, "latin1", "ASCII", sub = "")
  
  # Combine with second row header (year)
  header2 <- as.vector(t(read.table(paste0("./inkar/", k), skip = 1, nrows = 1, sep = ";")[1,]))
  header3 <- paste(header, header2, sep = "_")
  header3 <- gsub("_NA", "", header3)
  
  # Input and rename data
  data <- read.csv(paste0("./inkar/", k), skip = 2, header = FALSE, sep = ";", quote = "\"", dec = ",", stringsAsFactors = F)
  names(data) <- header3
  data1 <- data
  
  # Correct character vars (containing thousands separator)
  vars <- which(sapply(data1, is.character))
  vars <- vars[-which(vars %in% c(1:3))]
  for(l in vars){
    data1[,l] <- gsub("\\.", "", data1[,l])
    data1[,l] <- gsub("\\,", ".", data1[,l])
    data1[,l] <- as.numeric(data1[,l])
  }
  
  
  # #Save
  # l <- paste("bearb", k, sep = "_")
  # write.table(data1, file = l, row.names = FALSE, sep = ";", dec = ".", na = ".")
  
  #Reshape
  helpvar1 <- unique(header[4:length(header)])
  helpvar2 <-  sort(unique(header2[!is.na(header2)]))
  n_vars <- length(helpvar1)
  n_times <- length(helpvar2)
  helpvar1 <- sort(rep(helpvar1, times = n_times))
  helpvar2 <- rep(helpvar2, times = n_vars)
  helpvar3 <- paste(helpvar1, helpvar2, sep = "_")
  count <- ncol(data1)+1
  for(v in helpvar3) {
    if(v %in% names(data1)) {}
    else{
      data1[,count] <- NA
      colnames(data1)[count] <- v
      count <- count+1
    }
  }
  data1 <- data1[c(colnames(data1)[1:3], sort(helpvar3))]
  
  data1 <- reshape(data1, direction = "long", varying = 4:ncol(data1), 
                 sep = "_")
  colnames(data1) <- substr(colnames(data1), 1, 30)
  l <- paste("./inkar/long_gemverb_", c, ".csv", sep = "")
  write.table(data1, file = l, row.names = FALSE, sep = ";", dec = ".", na = ".")
  
  c <- c+1
  
}




#################################
### Load and merge INKAR data ###
#################################

for(i in c(1:length(j))){
  name <- paste("./inkar/long_gemverb_", i, ".csv", sep = "")
  tmp.df <- read.table(name, sep = ";", header = T, dec = ".", na.string = ".", stringsAsFactors = F)
  # Preparation
  names(tmp.df) <- tolower(names(tmp.df))
  names(tmp.df)[which(names(tmp.df) == "time")] <- "reportingyear"
  tmp.df <- tmp.df[,-which(names(tmp.df) == "id")]
  if(i>1){
    tmp.df <- tmp.df[,-which(names(tmp.df) %in% c("raumeinheit", "aggregat"))]
  }
  # Merge to gemverb_long
  gemverb_long.df <- merge(gemverb_long.df, tmp.df, by = c("kennziffer", "reportingyear"), all.x = T)
  
}


summary(gemverb_long.df$einkommensteuer)




##########################################
#### Combine Inkar data with polygons ####
##########################################


years <- c(2007:2017)

for(k in years){
  # Subset to year
  tmp.df <- subset(gemverb_long.df, gemverb_long.df$reportingyear ==  k)
  tmp.df$geometry <- NULL
  
  # Combine Shapefile & data
  tmp2.spdf <- gemverb.spdf[, which(names(gemverb.spdf) %in% c("kennziffer"))]
  
  kreise_regio_tmp.spdf <- merge(tmp2.spdf, tmp.df, by = "kennziffer")
  
  # Make unique ids
  rownames(kreise_regio_tmp.spdf) <- paste0(k, "_", kreise_regio_tmp.spdf$kennziffer)
  
  # Combine years
  if(k ==  2007){
    eper_inkar_final.spdf <- kreise_regio_tmp.spdf
  }else{
    eper_inkar_final.spdf <- rbind(eper_inkar_final.spdf, kreise_regio_tmp.spdf)
  }
  
}




##########################################################
#### Remove  "Gemeindefreie Gebiete" (no inhabitants) ####
##########################################################

names(eper_inkar_final.spdf)[which(names(eper_inkar_final.spdf) ==  "bev17")] <- "bevoelkerung"
eper_inkar_final.spdf$bevoelkerung <- as.character(eper_inkar_final.spdf$bevoelkerung)
eper_inkar_final.spdf$bevoelkerung <- gsub(",", "",  eper_inkar_final.spdf$bevoelkerung)
eper_inkar_final.spdf$bevoelkerung <- as.numeric(eper_inkar_final.spdf$bevoelkerung)

names(eper_inkar_final.spdf)[which(names(eper_inkar_final.spdf) ==  "fl17")] <- "flaeche"



eper_inkar_final.spdf <- eper_inkar_final.spdf[eper_inkar_final.spdf$bevoelkerung > 0,]
eper_inkar_final.spdf <- eper_inkar_final.spdf[eper_inkar_final.spdf$aggregat !=  "gemfr. Gebiet",]



# ############################
# #### Generate Variables ####
# ############################
# 
# ### Air pollution
# 
# eper_inkar_final.spdf$ln_air<-log((eper_inkar_final.spdf$sum_quant_air/eper_inkar_final.spdf$flaeche)+1)
# 
# 
# eper_inkar_final.spdf$ln_tox<-log((eper_inkar_final.spdf$sum_quant_tox/eper_inkar_final.spdf$flaeche)+1)


#########################
#### Number per area ####
#########################

eper_inkar_final.spdf$number_all_perc_pa <- eper_inkar_final.spdf$number_all_perc/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$number_air_perc_pa <- eper_inkar_final.spdf$number_air_perc/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$number_tox_perc_pa <- eper_inkar_final.spdf$number_tox_perc/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$number_ind_perc_pa <- eper_inkar_final.spdf$number_ind_perc/eper_inkar_final.spdf$flaeche

eper_inkar_final.spdf$number_ind_perc_raw_pa <- eper_inkar_final.spdf$number_ind_perc_raw/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$air_raw_pa <- eper_inkar_final.spdf$air_raw/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$air_tox_raw_pa <- eper_inkar_final.spdf$air_tox_raw/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$air_imp_pa <- eper_inkar_final.spdf$air_imp/eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$air_tox_imp_pa <- eper_inkar_final.spdf$air_tox_imp/eper_inkar_final.spdf$flaeche

eper_inkar_final.spdf$ln_air_raw <- log(eper_inkar_final.spdf$air_raw + 1) 
eper_inkar_final.spdf$ln_air_tox_raw <- log(eper_inkar_final.spdf$air_tox_raw + 1) 
eper_inkar_final.spdf$ln_air_imp <- log(eper_inkar_final.spdf$air_imp + 1) 
eper_inkar_final.spdf$ln_air_tox_imp <- log(eper_inkar_final.spdf$air_tox_imp + 1) 

eper_inkar_final.spdf$ln_air_raw_pa <- log(eper_inkar_final.spdf$air_raw + 1) / eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$ln_air_tox_raw_pa <- log(eper_inkar_final.spdf$air_tox_raw + 1) / eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$ln_air_imp_pa <- log(eper_inkar_final.spdf$air_imp + 1) / eper_inkar_final.spdf$flaeche
eper_inkar_final.spdf$ln_air_tox_imp_pa <- log(eper_inkar_final.spdf$air_tox_imp + 1) / eper_inkar_final.spdf$flaeche


#### Reordering ####
eper_inkar_final.spdf <- eper_inkar_final.spdf[order(eper_inkar_final.spdf$kennziffer, 
                                                     eper_inkar_final.spdf$reportingyear),]



### Save
save(eper_inkar_final.spdf, file = "EPER_INKAR_2017.RData")










