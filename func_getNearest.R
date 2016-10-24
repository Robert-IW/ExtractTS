get_5nearest <- function(lon,lat,station){  
  
  #cat("\nstarting subset at station",j,"\n")
  lonlat <- as.vector(c(lon,lat))
  
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
    #cat(paste("No data found for station",j,"\n"))
    sub.df <- data.frame(lon=NA,lat=NA,SST=NA)
    sub.df  <- cbind(date = temp_date,sub.df,dist = NA,don = don,statiop = station)
    #mylist[[j]][[i]] <- sub.df
    #rm(sub.df)
    return(sub.df)
    #next
  } else {
    sub.df  <- cbind(date = temp_date,sub.df,dist = NA,don = don,station = station)       # add the date as the first column
  }
  
  #cat("estimating distance\n")
  # estimate the distance from the station coords
  sub.df$dist <- gdist(lat.1=sub.df$lat,
                       lon.1=sub.df$lon,
                       lat.2=stLat,
                       lon.2=stLon, units = "km")
  
  # sort the data points according to increaseing distance
  temp <- sub.df[order(sub.df$dist, decreasing=FALSE),]
  
  #cat("selecting 5 closest\n")
  # select the 5 closest non NA
  sub.df <- subset(temp[1:5,])
  #mylist[[j]][[i]] <- sub.df
  return(sub.df)
} # for each station 'j'