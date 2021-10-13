# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Read EPERTR from database

rm(list = ls())

library(plyr)
library(dplyr)



setwd("../02_Data")




####################
#### EPRTR load ####
####################

# Pollutant transfer
eper1.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_pollutanttransfer.csv")

names(eper1.df)[which(names(eper1.df) == "quantity")] <- "totalquantity"
names(eper1.df)[which(names(eper1.df) == "quantityunitcode")] <- "unitcode"
names(eper1.df)[which(names(eper1.df) == "quantityunitname")] <- "unitname"

eper1.df$releasemediumcode <- "WASTE_WATER"
eper1.df$releasemediumname <- "Waste-Water"


# Waste transfer 
eper2.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_wastetransfer.csv")

names(eper2.df)[which(names(eper2.df) == "quantity")] <- "totalquantity"
names(eper2.df)[which(names(eper2.df) == "quantityunitcode")] <- "unitcode"
names(eper2.df)[which(names(eper2.df) == "quantityunitname")] <- "unitname"

# Combine Release and Transfer
eper3.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_pollutantrelease.csv")

eper_ger.df <- rbind.fill(eper3.df, eper2.df, eper1.df)
names(eper_ger.df) <- tolower(names(eper_ger.df))

names(eper_ger.df)[which(names(eper_ger.df) == "confidentialindicator")] <- "r_confidentialindicator"
names(eper_ger.df)[which(names(eper_ger.df) == "confidentialityreasoncode")] <- "r_confidentialityreasoncode"
names(eper_ger.df)[which(names(eper_ger.df) == "confidentialityreasonname")] <- "r_confidentialityreasonname"


# Merge meta
meta1.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_facilityreport.csv")
names(meta1.df) <- tolower(names(meta1.df))
eper_ger.df <- merge(eper_ger.df, meta1.df, by = "facilityreportid", all.x = TRUE, all.y = FALSE)

meta2.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_pollutantreleaseandtransferreport.csv")
names(meta2.df) <- tolower(names(meta2.df))
eper_ger.df <- merge(eper_ger.df, meta2.df, by = "pollutantreleaseandtransferreportid", all.x = TRUE, all.y = FALSE)

meta3.df <- read.csv("./E-PRTR_database_v16_csv/dbo.publish_facilityid_changes.csv")
names(meta3.df) <- tolower(names(meta3.df))
eper_ger.df <- merge(eper_ger.df, meta3.df, by = c("facilityreportid", "reportingyear"), all.x = TRUE, all.y = FALSE)


# Relabel some vars
eper_ger.df$coordinatesystemcode <- as.numeric(gsub("EPSG:", "", eper_ger.df$coordinatesystemcode, fixed = TRUE))
eper_ger.df$mainiasectorcode <- as.numeric(gsub("EPER_", "10", eper_ger.df$mainiasectorcode, fixed = TRUE))
eper_ger.df$nacemaineconomicactivitycode <- as.numeric(gsub("NACE_1.1:", "1", eper_ger.df$nacemaineconomicactivitycode, fixed = TRUE))
eper_ger.df$nacemaineconomicactivitycode <- as.numeric(gsub(".", "", eper_ger.df$nacemaineconomicactivitycode, fixed = TRUE))

eper_ger.df$eper <- ifelse(eper_ger.df$nacemaineconomicactivitycode>10000, 1, 0)


# Only germany
eper_ger.df <- eper_ger.df[eper_ger.df$countrycode == "DE", ]


### Save
save(eper_ger.df, file = "eper_release_ger_v16.RData")



