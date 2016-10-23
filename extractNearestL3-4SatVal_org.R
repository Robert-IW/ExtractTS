## To match-up in situ stations with nearest L3 or L4 satellite data for each day.
## The output is a list for each sensor (e.g. 'lst_MODISA_L3/4') of data frames for each day

## A series of data frame are created for each satellite product
## Individual station data frames are created and saved as csv for the entire time series

## Requires the csv files produced by 'extractSatSSTlevel[3C/4]_csv.R'

## Requires 'getInsituRecords.R' to be run first (or load 'OutPut01...Rdata')
## Requires 'list_dates', 'row_stations' 'col_time' from 'getInsituRecords.R'
## Calls 'callSatDataList.R'
## Calls the set of 'callSENSORSatVal.R' scripts

#################################################################################
# WARNING: Check that rounding 23:46 doesn't end up as 00:00
# anything beyond 11:45 should be rounded to 11:30
#################################################################################
library(data.table)
library(Imap)
library(ncdf4)
library(tools)
library(xts)
library(maptools) # also loads sp package
library(raster)
library(rasterVis)
#library(fossil)

# FUNCTION: get the date from the filename
getstr = function(mystring, initial.character, final.character) {
  sub(sprintf(".*%s(.*)%s.*", initial.character, final.character), "\\1", mystring)
}

# LOAD: get station information
#load("~/R_projects-UWC/ExtractTS/Output/OutPut01_InsituRecords.RData")
load("~/R_projects-UWC/ExtractTS/Output/all_samples_stations.Rdata") # created by 'bind_all_stations'
my.data <- all_samples

# Get the coordinates
#source("func_perpCont.R")

#sourceURL <-("/media/robert/Seagate Backup Plus Drive/SST/GHRSST/")
sourceURL <-("/media/robert/Seagate Backup Plus Drive/Backup Satellite Data/SST/GHRSST/")

######################################## TEST
#testURL <- ("/home/robert/R_projects-UWC/ExtractTS/Sample_csv/sample_sst/")
#outURL <- ("/home/robert/R_projects-UWC/ExtractTS/Output/")
######################################## END

num_st <- length(my.data)

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

sourceCSV <- c(
  "L3C/AVHRR_L3/csv",
  "L4/AVHRR-OI/csv",
  "L4/CMC/csv",
  "L4/G1SST/csv",
  "L4/K10/csv",
  "L4/MUR/csv",
  "L4/MW_IR_v1.0/csv",
  "L4/MW_IR_v4.0/csv",
  "L4/ODYSSEA_SAF/csv",
  "L4/OSTIA/csv/")                            # directories where csv files kept

saveTS <- c(
  "L3C/AVHRR_L3/ts/",
  "L4/AVHRR-OI/ts/",
  "L4/CMC/ts/",
  "L4/G1SST/ts/",
  "L4/K10/ts/",
  "L4/MUR/ts/",
  "L4/MW_IR_v1.0/ts/",
  "L4/MW_IR_v4.0/ts/",
  "L4/ODYSSEA_SAF/ts/",
  "L4/OSTIA/ts/")                             # directories to save the time series

sensorRes <- c(4,25,20,1,10,1,9,9,10,5)       # vector of product pixel resolution

dataSources <-data.frame(product,sourceCSV,saveTS,sensorRes,stringsAsFactors = FALSE)
rm(product,sourceCSV,saveTS,sensorRes)

for (h in 6:nrow(dataSources)) {            # for each satellite product
  
  # create empty list for all the days data
  mylist <- sapply(my.data[,1],function(x) NULL)
  
  productIndex <- h
  product <- dataSources[productIndex,1]
  sourceData <- dataSources[productIndex, 2]
  saveTS <- dataSources[productIndex, 3]
  resol <- dataSources[productIndex,4]
  
  # get a list of all the csv files
  fileURL <- list.files(paste(sourceURL, sourceData, sep = ''), full.names = TRUE)
  ################################################# TEST
  fileList <- list.files(paste(sourceURL, sourceData, sep = ''), recursive = FALSE)
  #fileList <- list.files(testURL, recursive = FALSE)
  ################################################# END
  
  ########## FOR EACH DAY 'i'
  for (i in 1183:length(fileList)){
    
    # Get a list of files for this date for each sensor
    cat(paste("Starting on day", i,"\n"))
    
    # get the date from the filename
    dateStr <- getstr(fileList[i],"_","_")
    temp_date <- as.Date(dateStr,"%Y%m%d")
    
    # check if the data file is for night or day
    if (grepl("day",fileList[i])){
      don <- "day"
    } else if (grepl("night",fileList[i])){
      don <- "night"
    } else {
      don <- "0"
    }
    
    # open the csv file
    ############################################# TEST
    #myData <- read.csv(fileURL[i])   # make sure the first column does not contain row numbers
    myData <- data.table::fread(fileURL[i])
    #myData <- read.csv(paste(testURL,fileList[i],sep = ''))
    ############################################# END
    
    # for each station get the 5 nearest data points dependent on resolution
    for (j in 1:nrow(my.data)){
      
      lonlat <- as.vector(my.data[j,2:3])
      
      # get lat and lon from in situ station coords
      stLon <- as.numeric(lonlat[1])
      stLat <- as.numeric(lonlat[2])
      
      # set the size of the pixel search area using 2x the resolution of the sensor
      # create a bounding box of at least 5x resol(km) from station (note this will be a rough estimate
      #   due to lat/lon estimates of resol (km):
      # ca. 5km latitude = 5* (1/110.574))
      # ca. 5km longitude = 5* (1/(111.320*cos(stLat))) !!! NOTE R uses radians not degress
      latEdge <- 2*resol* (1/110.574)
      lonEdge <-2*resol* (1/(111.320*cos(stLat*pi/180)))
      
      # exclude NA and remove values 99
      myData <- subset(myData,!is.na(SST))
      myData <- myData[!myData$SST == 99,]
      
      sub.df <- subset(myData, lat <= stLat+latEdge & lat >= stLat-latEdge 
                       & lon <= stLon+lonEdge & lon >= stLon-lonEdge)
      
      # if there is no data create an empty date row
      if (nrow(sub.df) == 0){
        cat(paste("No data found for station",j,"\n"))
        sub.df[1,1:3] <- NA
        sub.df  <- cbind(date = temp_date,sub.df,dist = NA,don = don)
        mylist[[j]][[i]] <- sub.df
        rm(sub.df)
        next
      } else {
        sub.df  <- cbind(date = temp_date,sub.df,dist = NA,don = don,station = my.data[j,1])       # add the date as the first column
      }
      
      # estimate the distance from the station coords
      sub.df$dist <- gdist(lat.1=sub.df$lat,
                           lon.1=sub.df$lon,
                           lat.2=stLat,
                           lon.2=stLon, units = "km")
      
      # sort the data points according to increaseing distance
      temp <- sub.df[order(sub.df$dist, decreasing=FALSE),]
      
      # select the 5 closest non NA
      sub.df <- subset(temp[1:5,])
      mylist[[j]][[i]] <- sub.df
      rm(sub.df,latEdge,lonEdge,temp)
      
    } # for each station 'j'
  } # for each day 'i'
  
  cat("Writing csv file\n")
  
  # create each station as data frame from list and save as csv
  for (k in 1:length(mylist)){
    myFrame <- do.call(rbind, c(mylist[[k]], make.row.names=FALSE))     # combine all the daily data into dataframe
    myFrame <- myFrame[!is.na(myFrame$date),]                           # remove NA rows (< 5 observations)
    myFrame <- cbind(
      depth=rownames(my.data)[k],
      product=product,
      myFrame)

    ###################################################### TEST
    filename1 <- paste(sourceURL,saveTS,product,"_",my.data[k,1],"_",rownames(my.data)[k],"_SST_timeseries_5nearest.csv",sep = '')
    #filename <- paste(outURL,saveTS,product,"_",row_stations[k],"_SST_timeseries_5nearest.csv",sep = '')
    ###################################################### END
    
    write.csv(myFrame, file = filename1, row.names = FALSE)
    rm(myFrame, filename1)
    gc()
  } # for each 'k'
} # for each 'h'
rm(j, k, x, y, mylist,sourceURL,dataSources,saveTS,product,resol,productIndex,fileURL,fileList)
