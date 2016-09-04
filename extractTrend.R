## To extract a time series and trend for a given station or for all stations
## Allows the user to select either the nearest point data or an average of the
##  5 nearest data points.

## The trend is determined using Mann-Kendall trend analysis in the 'trend' package

library(trend)
library(foreach)
library(doParallel)

# the base URL for all data
sourceURL <-("/media/robert/Seagate Backup Plus Drive/SST/GHRSST/")

# LOAD: get station information
load("~/R_projects-UWC/ExtractTS/Output/OutPut01_InsituRecords.RData")

num_st <- length(row_stations)

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

sourceTS <- c(
  "L3C/AVHRR_L3/ts",
  "L4/AVHRR-OI/ts",
  "L4/CMC/ts",
  "L4/G1SST/ts",
  "L4/K10/ts",
  "L4/MUR/ts",
  "L4/MW_IR_v1.0/ts",
  "L4/MW_IR_v4.0/ts",
  "L4/ODYSSEA_SAF/ts",
  "L4/OSTIA/ts")                             # directories to save the time series

sensorRes <- c(4,25,20,1,10,1,9,9,10,5)       # vector of product pixel resolution

dataSources <-data.frame(product,sourceTS,sensorRes,stringsAsFactors = FALSE)
rm(product,sourceTS,sensorRes)

# create a function taking arguments:
#   prod_name e.g. "AVHRR_L3"
#   stat_name e.g. "Oudekraal"
#   proximity either "nearest" or "average"
#   don either "day", "night", "0"

get_trend <- function(prod_name,stat_name,proximity,dayornight){
  
  # get index for product and station input
  idxPro <- which(dataSources[,1] == prod_name)
  fileList <- list.files(paste(sourceURL,dataSources[idxPro,2], sep = ''), recursive = FALSE)
  idxSta <- grep(stat_name, fileList)
  fileURL <- paste0(sourceURL,dataSources[idxPro,2],fileList[idxSta])
  
  dataTS <- read.csv(fileURL,stringsAsFactors = FALSE)
  dataTS <- subset(dataTS, don == dayornight)       # subset the data frame according to 'don'
  
  date.start <- as.Date(dataTS[1,1])
  date.end <- as.Date(last(dataTS[,1]))
  date.seq <- seq.Date(date.start,date.end,"day")
  timeseries <- data.frame(date=date.seq,sst=NA,dist=NA,stringsAsFactors = FALSE)
  
  cl <- makeCluster(6)
  registerDoParallel(cl, cores = 6)
    
  if (proximity=="nearest"){
    strt<-Sys.time()
    timeseries <- foreach(j = 1:nrow(timeseries),.inorder=TRUE)) %dopar% {
    #for (j in 1:nrow(timeseries)){
      try(timeseries[j,2:3] <- c(first(dataTS[dataTS$date == timeseries$date[j],4:5])))
      timeseries
    }
    stopCluster(cl)
    print(Sys.time()-strt)
    
  } else if (proximity=="average"){
    strt<-Sys.time()
    timeseries <- foreach(j = 1:nrow(timeseries),.inorder=TRUE)) %dopar% {
    #for (j in 1:nrow(timeseries)){
      try(timeseries[j,2:3] <- c(mean(dataTS[dataTS$date == timeseries$date[j],4],na.rm=TRUE),mean(dataTS[dataTS$date == timeseries$date[j],5],na.rm=TRUE)))
      timeseries
    }
    stopCluster(cl)
    print(Sys.time()-strt)
  }
  # return time series
}
