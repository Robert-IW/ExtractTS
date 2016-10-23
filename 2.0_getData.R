## To extract a time series for each station and depth along virtual transect
## Allows the user to select either the nearest point data or an average of the
## 5 nearest data points.

library(plyr)
library(dplyr)
library(doMC) # multi-core
require(mgcv)
library(tidyr)
library(tibble)
library(lubridate)
library(forecast)
library(xts)
library(data.table)


setwd("~/R_projects-UWC/ExtractTS/")

# Set parallel cores
doMC::registerDoMC(cores = 6)

# required functions
source("~/R_projects-UWC/ExtractTS/func_getdata.R")

# the base URL for all data
#sourceURL <-("/media/robert/Seagate Backup Plus Drive/Backup Satellite Data/SST/GHRSST/")
sourceURL <-("/media/robert/Seagate Backup Plus Drive/SST/GHRSST/")

# LOAD: get station information
load("~/R_projects-UWC/ExtractTS/Output/OutPut01_InsituRecords.RData")

# create directory strings for file locations
product <- c(
  "AVHRR_L3",
  "AVHRR-OI_L4",
  "CMC_L4",
  "G1SST_L4",
  "K10_L4",
  "MUR_L4",
  "MW-IR-v1_L4",
  "MW-IR-v4_L4",
  "ODYSSEA_L4",
  "OSTIA_L4")
# directories to save the time series
sourceTS <- c(
  "L3C/AVHRR_L3/ts/",
  "L4/AVHRR-OI/ts/",
  "L4/CMC/ts/",
  "L4/G1SST/ts/",
  "L4/K10/ts/",
  "L4/MUR/ts/",
  "L4/MW_IR_v1.0/ts/",
  "L4/MW_IR_v4.0/ts/",
  "L4/ODYSSEA_SAF/ts/",
  "L4/OSTIA/ts/")
imagesTS <- c(
  "L3C/AVHRR_L3/ts/images/",
  "L4/AVHRR-OI/ts/images/",
  "L4/CMC/ts/images/",
  "L4/G1SST/ts/images/",
  "L4/K10/ts/images/",
  "L4/MUR/ts/images/",
  "L4/MW_IR_v1.0/ts/images/",
  "L4/MW_IR_v4.0/ts/images/",
  "L4/ODYSSEA_SAF/ts/images/",
  "L4/OSTIA/ts/images/")

sensorRes <- c(4,25,20,1,10,1,9,9,10,5)       # vector of product pixel resolution

dataSources <-data.frame(product,sourceTS,imagesTS,sensorRes,stringsAsFactors = FALSE)
rm(product,sourceTS,imagesTS,sensorRes)

# For each product, plot TS for each station and get model
for (i in 3:3){#nrow(dataSources)){
  
  # create arguments for getting data into format
  prod_name <- dataSources[i,1]       #   prod_name e.g. "AVHRR_L3"
  proximity <- "Median Value"         #   proximity either "Nearest Value" or "Median Value"
  dayornight <- "0"                   #   don either "day", "night", "0"
  
  filename2 <- paste0(sourceURL,dataSources[i,2],prod_name,"_dataTSall.Rdata")
  
  # run local function to create data frame
  cat("Runnning function 'get_data'\n")
  
  dataTS <- get_data(prod_name,proximity,dayornight)
  
  dataTS$year <- as.integer(strftime(dataTS$date,"%Y"))
  dataTS$mon <- as.integer(strftime(dataTS$date,"%m"))
  dataTS$day <- as.integer(strftime(dataTS$date,"%d"))
  dataTS$d.o.y. <- as.integer(strftime(dataTS$date,"%j"))
  
  cat("Saving data\n")
  save(dataTS, file=filename2)
  
  rm(filename2,dataTS,prod_name,proximity,dayornight)
  gc()
}
rm(dataSources,station_lonlat,col_time,i,list_dates,list_insitu,row_stations,sourceURL)
