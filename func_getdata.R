## Called by '2.0_getTS.R'

## The function takes:
##      product name; e.g. prod_name <- "AVHRR_L3"
##      proximity: proximity <- "nearest"/"average"
##      day or night: dayornight <- "day"/"night"/"0"

# "nearest" refers to the first and nearest sst record to the station
# "average" refers to the mean of the nearest 5 sst records

get_data <- function(prod_name,proximity,dayornight){

  # get index for product and station input
  idxPro <- which(dataSources[,1] == prod_name)
  
  # get a list of each day .csv file containing 5 nearest values at each in situ and virtual station
  fileList <- list.files(paste0(sourceURL,dataSources[idxPro,2]), recursive = FALSE, pattern = ".csv")
  fileURL <- paste0(sourceURL,dataSources[idxPro,2],fileList)
  
  # get a dataframe of all 5 nearest values for each station and date
  cat("Geting all station csv files\n")
  dataTemp <- do.call(rbind,lapply(fileURL,data.table::fread,stringsAsFactors=FALSE))   # combine all station data into single dataframe
  dataTemp <- subset(dataTemp, don == dayornight)       # subset the data frame according to 'don'

  if (proximity=="Nearest Value"){
    # get the first value for each 'location,date' combo
    cat("Getting nearest data\n")
    
    dataTS_temp01 <- ddply(dataTemp,.(station,depth,date),function(x,n) x[n,],n=1,.parallel=TRUE)
    
    # create a sequence of numbers in column num
    cat("Creating data sequence\n")
    dataTS_first <- ddply(dataTS_temp01,.(station,depth), mutate, days=seq_along(date))
    dataTS_first$date <- as.Date(dataTS_first$date)
    return(dataTS_first)
    
  } else if (proximity=="Median Value"){
    
    cat("Getting median data\n")
 
    dataTS_temp02 <- ddply(
      dataTemp,
      .(station,depth,date),
      summarise,lon=mean(lon,na.rm = TRUE),
      lat=mean(lat,na.rm = TRUE),
      sst=median(SST,na.rm = TRUE),
      dist=mean(dist,na.rm = TRUE),
      .parallel=TRUE)
    
    cat("Creating data sequence\n")
    dataTS_median <- ddply(dataTS_temp02,.(station,depth), mutate, days=seq_along(date))
    dataTS_median$date <- as.Date(dataTS_median$date)
    return(dataTS_median)
  }
}
