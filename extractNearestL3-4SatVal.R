## To match-up in situ stations with nearest L3 or L4 satellite data for each day.
## The output is a list for each sensor (e.g. 'lst_MODISA_L3/4') of data frames for each day

## Requires the csv files produced by 'extractSatSSTlevel[3C/4]_csv.R'

## Requires 'getInsituRecords.R' to be run first (or load 'OutPut01...Rdata')
## Requires 'list_dates', 'row_stations' 'col_time' from 'getInsituRecords.R'
## Calls 'callSatDataList.R'
## Calls the set of 'callSENSORSatVal.R' scripts

#################################################################################
# WARNING: Check that rounding 23:46 doesn't end up as 00:00
# anything beyond 11:45 should be rounded to 11:30
#################################################################################

require("Imap")
require("ncdf4")
require("tools")

# FUNCTION: get the date from the filename
getstr = function(mystring, initial.character, final.character) {
  sub(sprintf(".*%s(.*)%s.*", initial.character, final.character), "\\1", mystring)
}

sourceURL <-("/media/robert/Seagate Backup Plus Drive/SST/GHRSST/")

num_st <- length(row_stations)

sourceCSV <- c(
  "L3C/AVHRR_L3/csv/",
  "L4/AVHRR-OI/csv/",
  "L4/CMC/csv/",
  "L4/G1SST/csv/",
  "L4/K10/csv/",
  "L4/MUR/csv/",
  "L4/MW_IR_v1.0/csv/",
  "L4/MW_IR_v4.0/csv/",
  "L4/ODYSSEA_SAF/csv/",
  "L4/OSTIA/csv/")
saveTS <- c(
  "L3C/AVHRR_L3/ts/",
  "L4/AVHRR-OI/ts/",
  "L4/CMC/ts/",
  "L4/G1SST/ts/",
  "L4/K10/ts/",
  "L4/MUR/csv/",
  "L4/MW_IR_v1.0/ts/",
  "L4/MW_IR_v4.0/ts/",
  "L4/ODYSSEA_SAF/ts/",
  "L4/OSTIA/ts/")
sensorRes <- c(4,25,20,1,10,1,9,9,10,5)

dataSources <-data.frame(sourceCSV,saveTS,sensorRes,stringsAsFactors = FALSE)
rm(sourceCSV,saveTS,sensorRes)

for (h in 1:nrow(dataSources)) {
  productIndex <- h
  sourceData <- dataSources[productIndex, 1]
  saveImage <- dataSources[productIndex, 2]
  resol <- dataSources[productIndex,3]
  
    
  # get a list of all the csv files
  fileList <- list.files(paste(sourceURL, sourceData, sep = ''), recursive = FALSE)
  fileURL <- list.files(paste(sourceURL, sourceData, sep = ''), full.names = TRUE)

########## FOR EACH DAY 'i'
for (i in 1:length(fileList)){
  
  # Get a list of files for this date for each sensor
  print(paste("Starting on day", i))
  
  # get the date from the filename
  dateStr <- getstr(fileList[i],"_","_")
  temp_date <- as.Date(dateStr,"%Y%m%d")
  
  # open the csv file
  myData <- read.csv(fileURL[i])   # make sure the first column does not contain row numbers
  
  # for each station get the 5 nearest data points dependent on resolution
  for (j in 1:length(station_lonlat)){
    
    lonlat <- as.vector(station_lonlat[j,])
    
    # get lat and lon from in situ station coords
    stLon <- lonlat[1]
    stLat <- lonlat[2]
    
    # set the size of the pixel search area using 2x the resolution of the sensor
    # create a bounding box of at least 5x resol(km) from station (note this will be a rough estimate
    #   due to lat/lon estimates of resol (km):
    # ca. 5km latitude = 5* (1/110.574))
    # ca. 5km longitude = 5* (1/(111.320*cos(stLat))) !!! NOTE R uses radians not degress
    latEdge <- 2*resol* (1/110.574)
    lonEdge <-2*resol* (1/(111.320*cos(stLat*pi/180)))
    
    myData <- subset(myData,!is.na(SST))
    sub.df <- subset(myData, lat <= stLat+latEdge & lat >= stLat-latEdge 
                     & lon <= stLon+lonEdge & lon >= stLon-lonEdge)
    
    # estimate the distance from the station coords
    sub.df$dist <- gdist(lat.1=sub.df$lat,
                         lon.1=sub.df$lon,
                         lat.2=stLat,
                         lon.2=stLon, units = "km")
    
    # sort the data points according to increaseing distance
    temp <- sub.df[order(sub.df$dist, decreasing=FALSE),]
    
    # select the 5 closest non NA
    sub.df <- subset(temp[1:5,])
    rm(latEdge,lonEdge,temp)
    
    # store as csv with columns 'station','date','lon','lat','time','SST','dist'
    
  }
  
  # run the function to get a list of all the sensor files for the date
  source(paste(base_source,"callL3-4SatDataList.R", sep = ""))

  ########################################## WARNING ######################################
  # This section produces very large Rdata file. It may be better to run 1 sensor at a time
  
  ########## FOR EACH SENSOR 'j'
  for (j in 6){ #1:length(type_data)){
    type_sensor <- type_data[[j]]
    
    if (type_sensor == "AATSR"){
      source(paste(base_source,"callAATSRSatVal.R", sep = ""))
    } else if (type_sensor == "AVHRR"){
      source(paste(base_source,"callAVHRRSatVal.R", sep = ""))
    } else if (type_sensor == "MODISA"){
      source(paste(base_source,"callMODISASatVal.R", sep = ""))
    } else if (type_sensor == "MODIST"){
      source(paste(base_source,"callMODISTSatVal.R", sep = ""))
    } else if (type_sensor == "SEVIRIlo"){
      source(paste(base_source,"callSEVIRIloSatVal.R", sep = ""))
    } else if (type_sensor == "SEVIRIhi"){
      source(paste(base_source,"callSEVIRIhiSatVal.R", sep = ""))
    }
    
  } # for each sensor 'j'
} # for each day 'i'
rm(i, j, k, x, y, base_source, type_sensor, type_data, val_day, val_month, val_year, temp_date)
rm(type_data, AATSRList,AVHRRList,MODISAList,MODISTList,SEVIRIhiList,SEVIRIloList)
