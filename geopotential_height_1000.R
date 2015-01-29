#Zonal Index Calculations
#using NARR NetCDF datafiles

library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)
library(maptools)
library(ggplot2)
library(raster)

u<-nc_open("C:/Users/Kelly/Documents/Undergrad_projects/uwnd.10m.mon.mean.nc")
h<-"C:/Users/Kelly/Documents/Undergrad_projects/uwnd.10m.mon.mean.nc"
uwnd<-brick(h,lvar=4, varname="uwnd", level=6)
#this needs to be fixed:

#plotting all of the months of July Uwind (at 10m)
uwnd.pts<-rasterToPoints(uwnd)
plot(uwnd.pts)
df <- data.frame(uwnd.pts)
#Make appropriate column headings
colnames(df) <- c("Lat", "Lon", "Uwnd_July_1980")
#uwnd by latitude profile # need to subset this data based on lat long
##########this figure is similiar to figure 3 in Booth 2006

#still need to look at the geopotential height

u<-nc_open("C:/Users/Kelly/Documents/Undergrad_projects/hgt.mon.mean.nc")
h<-"C:/Users/Kelly/Documents/Undergrad_projects/hgt.mon.mean.nc"
hgt.mon.mean<-brick(h,lvar=4, varname="hgt", level=6)

mean_wnd_profile<-aggregate(Uwnd_July_1980~Lon, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
mean_wnd<-mean_wnd_profile[1:491,2]
Lon<-mean_wnd_profile[1:491,1]
plot(mean_wnd[,1], Lon, main="uwnd long term July average by longitude")

mean_wnd_lat_Profile<-aggregate(Uwnd_July_1980~Lat, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
Lat<-mean_wnd_lat_Profile[1:349,1]
mean_wnd_lat<-mean_wnd_lat_Profile[1:349,2]
mean_wnd_lat<-mean_wnd_lat[,1]
plot(mean_wnd_lat, Lat, main="uwnd long term July average by latitude")

inputfile<-"C:/Users/Kelly/Documents/Undergrad_projects/hgt.mon.mean.nc"
n <- nc_open(inputfile)
f<- inputfile
#create a brick raster from the netcdf data
nlat<-ncvar_get(n,'lat')  # get the latitude and longitude coordinates
nlon<-ncvar_get(n,'lon')
#coordinates(hgt.ltm)
#12.2N;133.5W, 54.5N; 152.9W, 57.3N; 49.4W ,14.3N;65.1W
ymin<-min(nlat)
ymax<-max(nlat)
xmin<-min(nlon)
xmax<-max(nlon)
#hgt.mean@extent@xmin<-xmin
#hgt.mean@extent@xmax<-xmax
#hgt.mean@extent@ymax<-ymax
#hgt.mean@extent@ymin<-ymin
  
hgt.mean <- brick(f, lvar=3, varname="hgt", level=1)# this gives use July 
NAvalue(hgt.mean) <- 9e+20
#plot
plot(hgt.mean[[78]], main="1000mb geopotential height")
#crs.geo <- CRS()
prj <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-96"



mult<-(1:35.5)*12
mult.6<-mult-6
#subset hgt so that we have a rasterbrick with only july geopotential heights
july.hgt<-subset(hgt.mean, mult.6)

#pull lat long from original file
lat <- raster(inputfile, varname="lat")
lon <- raster(inputfile, varname="lon")


NAvalue(lat) <- 9e+20
NAvalue(lon) <- 9e+20


plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])
lonlat<-data.frame(lonlat)


julys<-rasterToPoints(july.hgt)
plot(julys)
points(lonlat, col = "red")

julys.lonlat<-data.frame(julys)
#julys.lonlat$x<-lonlat$X1


# Specify the lonlat as spatial points with projection as long/lat
#lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

#mycrs <- CRS("+proj=lcc +lat_1=35 +lat_2=51 +lat_0=39 +lon_0=14 +k=0.684241 +units=m +datum=WGS84 +no_defs")
#require(rgdal)
#plonlat <- spTransform(lonlat, CRSobj = mycrs)
#plonlat
#extent(plonlat)
#projection(july.hgt) <- mycrs
#extent(july.hgt)<-extent(plonlat)
# Take a look
#july.hgt
#plot(july.hgt[[1]])

#july.hgt.proj <- projectRaster(july.hgt, crs=mycrs)

#july.pts<-rasterToPoints(july.hgt)

#using merge...not sure how accurate this is
lat.lon.coords<-merge(plon, plat)
july.merge<-merge(julys.lonlat, lat.lon.coords)

years<-as.character(1979:2013)
colnames(july.merge)<-c("x", "y", years, "lon", "lat")

#created datasets where we have only 35degree N and 55 degree north data
july.merge55<-july.merge$lat >55
july.merge55<-july.merge[july.merge$lat >55,]
july.merge55<-july.merge55[july.merge55$lat <56,]
july.merge35<-july.merge[july.merge$lat >35,]
july.merge35<-july.merge35[july.merge35$lat <36,]

#there is probably a more elegant way to do this..
zonal.index<-matrix(0,37,1)
for(i in 3:37){
  zonal.index[i]<-mean(july.merge35[,i])-mean(july.merge55[,i])
}
 
zonal.index.plot<-cbind(as.numeric(years),zonal.index[3:37])
plot(zonal.index.plot, type="l", main="July Surface zonal index at 1000mb geopotential height", xlab="Year", ylab="zonal index")# plots the time series of zonal index

#for eastern zonal index
july.merge.east.35<-july.merge35[july.merge35$lon>-90,]
july.merge.east.55<-july.merge55[july.merge55$lon>-90,]
zonal.index.east<-matrix(0,37,1)
for(i in 3:37){
  zonal.index.east[i]<-mean(july.merge.east.35[,i])-mean(july.merge.east.55[,i])
}
zonal.index.E.plot<-cbind(as.numeric(years),zonal.index.east[3:37])
plot(zonal.index.E.plot, type="l", main="July Surface zonal index at 1000mb geopotential height", xlab="Year", ylab="zonal index")



#for western zonal index
july.merge.west.35<-july.merge35[july.merge35$lon<-90,]
july.merge.west.55<-july.merge55[july.merge55$lon<-90,]
zonal.index.west<-matrix(0,37,1)
for(i in 3:37){
  zonal.index.west[i]<-mean(july.merge.west.35[,i])-mean(july.merge.west.55[,i])
}

zonal.index.W.plot<-cbind(as.numeric(years),zonal.index.west[3:37])
plot(zonal.index.W.plot, type="l", main="July Surface zonal index at 1000mb geopotential height", xlab="Year", ylab="zonal index")
#next step is to get mean annual precipitation for these years on the scale of our data

plot(zonal.index.plot, type="l", main="July Surface zonal index at 1000mb geopotential height", xlab="Year", ylab="zonal index")
lines(zonal.index.E.plot, col="red")
lines(zonal.index.W.plot, col="green")
#make legend

zonal.table<-cbind(zonal.index, zonal.index.east, zonal.index.west)
colnames(zonal.table)<-c("hemispheric zonal index", "east zonal index", "west zonal index")

write.csv(zonal.table, file="geopthgt1980_2013.csv",sep=",", col.names=TRUE)

#calculate 
#subset(july.merge, subset = july.merge$lat>55 & jul  )
#mean<-aggregate(July_1979~lon, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
#writeRaster(july.hgt[[1]], filename="julyhgt.tif", overwrite=TRUE) # saves raster so we dont use memory
#mean_lon_hgt<-data.frame(mean[1:94019,2])
#mean.long.hgt<-data.frame(mean_lon_hgt$mean)
#Lon<-data.frame(mean[1:94019,1])
#plot(Lon[1:90000,],mean.long.hgt[1:90000,], main="uwnd long term July average by longitude")

#library("maps")
#map(add=TRUE, col="blue")
#plot(july.hgt[[2]])
#df2<-rasterToPoints(july.hgt[[2]])

#proj4string(hgt.ltm)=CRS("+init=epsg:4326") # set it to lat-long
#pts = spTransform(hgt.ltm,CRS(proj4string))
#Lambert.CC<-CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
              
 #               +lat_2=85.33255 +lat_1=0.897945 +lon_0=179.9 +lon_1=-179.9969")

#hgt.ltm.prj<-projectRaster(hgt.ltm, crs= Lambert.CC)

#projection(hgt.ltm.prj)<-Lambert.CC
#extent(hgt.ltm.prj)<-Lambert.CC
#envtl.all




