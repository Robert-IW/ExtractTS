# get the coordinates of perpendicular stations at -200, -500 and -2000 m
# to manually adjusct bearing, run func_perpCont.R in initial mode, then update bearings
# and run func_perpCont.R in modified mode

source("func_perpCont.R")
#load("~/R_projects-UWC/ExtractTS/Output/OutPut01_InsituRecords.RData")

#station_lonlat <- rbind(station_lonlat,as.matrix(site_list[1,5:6]))
#station_lonlat <- rbind(station_lonlat,as.matrix(site_list[34,5:6]))
#station_lonlat <- rbind(station_lonlat,as.matrix(site_list[36,5:6]))
#row_stations <- c(row_stations,as.character(site_list[1,2]),as.character(site_list[34,2]),as.character(site_list[36,2]))

load("~/R_projects-UWC/ExtractTS/sampleStats_all.RData")
bathy <- raster("/home/robert/R/x86_64-pc-linux-gnu-library/3.3/bathy-GEBCO/gebco-southernAfrica.nc")

depz <- c(-100, -500, -2000)
plot = TRUE

allStat <- apply(as.matrix(st.pts@coords), 1, function(x) do.call(perp2stat, list(x[1],x[2],depz,plot)))

########################################
# manually adjust bearing
#perp.bearing <- allStat[[14]][[2]]
#perp.bearing

allStat[[1]][[2]] <- 251
allStat[[4]][[2]] <- 225
allStat[[9]][[2]] <- 160
allStat[[11]][[2]] <- 150
allStat[[14]][[2]] <- 136
allStat[[16]][[2]] <- 120
allStat[[17]][[2]] <- 260
allStat[[18]][[2]] <- 180
allStat[[19]][[2]] <- 165

# run the update
for (i in 1:19){
  allStat[[i]][[3]] <- destPoint(allStat[[i]][[1]][1,],allStat[[i]][[2]],300000)
}
# run modified perp2stat that uses manual bearings added to station_lonlat
bearStat <- lapply(allStat, function(x)do.call(cbind,list(x[[2]])))
t <- mapply(as.matrix,bearStat)
station_lonlatbear <- cbind(st.pts@coords,t)

allStat <- apply(station_lonlatbear, 1, function(x) do.call(perp2stat, list(x[1],x[2],x[3],depz,plot)))
########################################

# plot each station
insituStat <- lapply(allStat, function(x)do.call(cbind,list(x[[1]][1,1],x[[1]][1,2])))
t <- do.call(rbind,insituStat)
st.pts <- SpatialPoints(t)

locbb <- matrix(c(10,-40,40,-20),nrow=2,ncol=2)
x <- extent(locbb)
y <- crop(bathy,x)      # crop bathy map

# plot each sampling station
sampleStat <- lapply(allStat, function(x)do.call(cbind,list(x[[1]][2:4,1],x[[1]][2:4,2])))
sa.pts <- SpatialPoints(do.call(rbind,sampleStat))

# plot perpendicular lines
perpStat <- lapply(allStat, function(x)do.call(cbind,list(x[[3]][1,1],x[[3]][1,2])))
pr.pts <- SpatialPoints(do.call(rbind,perpStat))

# create spatial lines object from start and end point of transect
pr.lines <- vector("list", length(st.pts))
for (i in seq_along(pr.lines)) {
  pr.lines[[i]] <- Lines(list(Line(rbind(st.pts[i, ], pr.pts[i,]))), as.character(i))
}
pr.lines <- SpatialLines(pr.lines)

# subset the sample transects from visual inspection
trans <- c(1,4,9,11,14,16,17,18,19)
sa.pts.select <- SpatialPoints(do.call(rbind,sampleStat[trans]))
  
# plot stations and transect lines
contourplot(y,at=c(0,-100,depz), margin = F, labels = F) +
  layer(sp.lines(pr.lines[trans],lwd=2,colour="blue",fill="blue")) +
  layer(sp.points(st.pts,pch=21,cex=1,colour="black",fill="blue")) +
  layer(sp.points(sa.pts.select,pch=21,cex=1,colour="black",fill=c("red","yellow","green")))
