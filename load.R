## https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
library(dplyr)
library(tidyverse)
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(readxl)
## Up to date regions downloaded as shapefile: https://borders.ukdataservice.ac.uk/easy_download_data.html?data=infuse_uk_2011
## Converted to geoJSON using: https://mapshaper.org/
## Other details available at: https://census.ukdataservice.ac.uk/use-data/guides/boundary-data.aspx
##Scot data from: https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhouse-sales-prices&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fmedian

## Read in the geoJSON file downloaded from ONS using the geojsonio library:
## spdf = spatial dataframe, EW = England & Wales

## https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597
# set working directory

# Download and unzip
download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_uk_2011.zip", 
              destfile = "infuse_uk_2011.zip" , mode='wb')
unzip("infuse_uk_2011.zip", exdir = "./data/")
file.remove("infuse_uk_2011.zip")

setwd("./data/")

require(rgdal)
sp_UK <- readOGR(dsn = ".", layer = "infuse_msoa_lyr_2011")
sp_UK_simp <- gSimplify(sp_UK,
                        tol = 10,
                        topologyPreserve = TRUE)

par(mar=c(0,0,0,0))
plot(sp_UK_simp, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )
require(broom)
spdf_UK_simp <- SpatialPolygonsDataFrame(sp_UK_simp,
                                      sp_UK@data) 

## For testing, I subsetted a small segment of the data (MSOAs with ID's matching 01xxx or 02xxx)
# spdf1 <- spdf_EW[ substr(spdf_EW@data$msoa01cd,5,6) %in% c("01", "02") , ]

## Fortify the data (convert spatial data into a dataframe object) and keep track of the MSOA code (Takes a while for the whole dataset)
# spdf_fortified_1 <- tidy(spdf1, region = "msoa01cd")
spdf_UK_simp = tidy(spdf_UK_simp, region = c("geo_code", "geo_label"))

spdf_UK_simp <- merge(x = spdf_UK_simp, y = sp_UK@data[,1:2], by.x = 'id', by.y = 'geo_code')

to_Match_1 <- c('^[E]*[0-9]','^[W]*[0-9]')
spdf_EW_simp <- spdf_UK_simp[grep(paste(to_Match_1,collapse="|"), spdf_UK_simp[["id"]]), ]

## Save an object to a file in the data folder so I don't have to do that again

saveRDS(spdf_EW_simp, file = "spdf_EW_simp.rds")
saveRDS(spdf_UK_simp, file = "spdf_UK_simp.rds")

# Restore the object
spdf_EW_simp <- readRDS(file = "spdf_EW_simp.rds")
spdf_UK_simp <- readRDS(file = "spdf_UK_simp.rds")

## read MSOA data

# House price data found here: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/hpssadataset2medianhousepricebymsoaquarterlyrollingyear
# older: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/medianhousepricebymsoaquarterlyrollingyearhpssadataset02a
# Download 
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhousing%2fdatasets%2fhpssadataset2medianhousepricebymsoaquarterlyrollingyear%2fcurrent/hpssadataset2medianpricepaidbymsoa.xls", 
              destfile = "MSOA_MedHP_UK.xls" , mode='wb')

# Pull out xls sheet with all house data (sheet 1)
sheet_1 <- read_xls("MSOA_MedHP_UK.xls", sheet = '1a')
write.csv(sheet_1, file="MSOA_MedHP_UK.csv", row.names=FALSE)
data <- read.csv("MSOA_MedHP_UK.csv", skip = 5, header=T, na.strings = ":")
data <- data[,1:98]

clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

col2cvt <- 5:98
data[,col2cvt] <- sapply(data[col2cvt], clean)

require(scales)
colnames(data) <- gsub(".*\\.([[:alpha:]]{3})\\.([[:digit:]]{4})", "\\1 \\2", colnames(data))
colnames(data) <- gsub("Year ending ", "", colnames(data))
long.data <- data %>%
  gather(Quarter, Price, 5:98)
long.data[,"Quarter"] <- as.Date(paste('01', long.data[,"Quarter"]), format='%d %b %Y')

ratio_year_data <- data[,c(1:4, 72, 80, 88)]
## Merge MSOA datasets together

spdf_EW_data <- spdf_EW_simp %>%
  merge(x = . , y = ratio_year_data, by.x="id", by.y="MSOA.code") 
spdf_EW_data <- arrange(spdf_EW_data, id, order)

saveRDS(spdf_EW_data, file = "spdf_EW_data.rds")
