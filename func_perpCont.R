# Takes the lat and long of a station and extracts station coords at depth intercals perpendicular to station
# Arguments lon, lat, depth intervals (vector), plot (logical)

# if running modified version, check line 10 and 28
require(fossil)
require(raster)
require(rasterVis)
require(geosphere)

#perp2stat <- function(lon,lat,depz,plot){
perp2stat <- function(lon,lat,perp.bear,depz,plot){  
  
  # get the location of the station and minLon,minLat;maxLon,maxLat bounding box and create extent obj
  loc <- t(as.matrix(c(lon,lat)))
  
  # crop the bathy to +/- 0.03 deg lon/lat and get the depth of the station
  locbb <- matrix(c(loc[1]+0.03,loc[2]-0.03,loc[1]-0.03,loc[2]+0.03),nrow=2,ncol=2)
  x <- extent(locbb)
  y <- crop(bathy,x)      # crop bathy map
  z <- extract(bathy,loc) # extract depth of station
  
  # sort the data according to depth differences and take bearing from nearest two
  depthz <- rasterToPoints(y, fun=NULL, spatial=FALSE)
  depthz <- cbind(depthz,(depthz[,3]-(z))^2)
  depthz <- depthz[order(depthz[,4]),]
  
  # get bearing and point along line 300 km distance
  #perp.bear <- earth.bear(depthz[2,1],depthz[2,2],depthz[3,1],depthz[3,2])-270
  perp.pnt <- destPoint(loc,perp.bear,300000)
  
  # draw perpendicular line from station point to depth
  locbb1 <- matrix(c(loc[1]+5,loc[2]-5,loc[1]-5,loc[2]+5),nrow=2,ncol=2)  # create 5deg window around station
  x1 <- extent(locbb1)  # create extent object
  y1 <- crop(bathy,x1)  # crop full bathy map
  
  # extract the depths along spatial line
  sp.lines <- SpatialLines(list(Lines(Line(rbind(loc,perp.pnt)), ID="a")))  # create spatial line from perp pnts
  vals <- extract(y1, sp.lines, cellnumbers=TRUE)[[1]]
  
  # get spatial points of the matching depth coordinates (value,x,y)
  vals = cbind(vals[,2],t(sapply(vals[,1], function(x) xyFromCell(y1, x))))
  
  # get closest point to -200, -500 and -2000m depths (return only first if multiple equal)
  dep1 <- vals[which((vals[,1]-depz[1])^2 == min((vals[,1]-depz[1])^2))[1],2:3]
  dep2 <- vals[which((vals[,1]-depz[2])^2 == min((vals[,1]-depz[2])^2))[1],2:3]
  dep3 <- vals[which((vals[,1]-depz[3])^2 == min((vals[,1]-depz[3])^2))[1],2:3]
  loc <- rbind(loc,dep1,dep2,dep3)
  
  temp <- as.data.frame(loc,row.names = c("station",as.character(depz)),stringAsFactors=FALSE)
  colnames(temp) <- c("lon","lat")
  
  if (plot){
    # return the 4 samoling station, the bearing and the 3 contour points
    temp <- list(temp,perp.bear,perp.pnt,z,depthz[1:3,1:2],y,y1)
  }
  return(temp)
}

  
  
  