# This code correlates calculated zonal index to mean annual preciptitation
#http://www.esrl.noaa.gov/psd/data/gridded/data.cmap.html

library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)
library(maptools)
library(ggplot2)
library(raster)

inputfile="C:/Users/Kelly/Documents/Undergrad_projects/clim-analysis/precip.mon.mean.nc"
p<-nc_open(inputfile)
h<-inputfile
pr<-brick(h,lvar=3, varname="precip", level=6)
mult<-(1:35.5)*12
mult.6<-mult-6
#subset hgt so that we have a rasterbrick with only july geopotential heights
july.pr<-subset(pr, mult.6)
pr.map<-rasterToPoints(july.pr)
df <- data.frame(pr.map)
years<-as.character(1979:2013)
#Make appropriate column headings
colnames(df) <- c( "Latitude", "Longitude",years)

#subset the data so it matches the data used to calculate zonal index
#df.new<-df[df$Longitude > 180,]
#df.new<-df.new[df.new$Longitude < 359.9969,]
#df.new<-df.new[df.new$Latitude> 1.000001,]
#df.new<-df.new[df.new$Latitude< 84,]
df<-data.frame(df)

# this plots one of the years (1980)
ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=X1980)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(df.new$X1980, limits=c(0,26)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()
  )

#plotting is interesting, but not really all that importat for us

#convert to same longitude


#df$lon<-$Longitude
#df$lat<-$Latitude

df.byyear<-t(df)
df.byyear<-df.byyear[,colSums(is.na(df.byyear)) == 0]
df.byyear<-data.frame(df.byyear[,])
new.zonal<-c(zonal.index)
df.byyear$zonal.index<-zonal.index
df.all<-cbind(df.byyear,zonal.index)


#df.zonal<-data.frame(rep(zonal.index[1:36,],4))

cor.by.pt<-matrix(0,1, 9550)
for (i in 1:9550){
cor.by.pt[,i]<-cor(df.all[3:36,i],df.all[3:36,9550])
}
#cors<-t(point1[])
ll<-t(df.byyear[1:2,])
correlation<-cbind(ll,cor.by.pt[,1:9550])
colnames(correlation)<-c( "Latitude","Longitude", "Correlation")
correlation$Latitude<-correlation$Latitude-180
correlation<-data.frame(correlation)
#july.cor<-correlation$Latitude >55
july.cor<-correlation
#july.cor<-july.cor[july.cor$Lat <56,]
#july.cor<-july.cor[july.cor$Longitude >0,]
#july.cor<-july.cor[july.cor$lat <36,]
#plot the correlations on the map

#in july.cor, longuitude is listed first, so we get a strange looking plot
#need to fix this for rasterFromXYZ to work



rast<-rasterFromXYZ(july.cor)
plot(rast)
 

ggplot(data=july.cor, aes(x=Latitude, y=Longitude)) +
  geom_raster(aes(fill=Correlation)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(correlation$Correlation, limits=c(-1,1)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()
  )


#this is not a very useful representation of these data
#find a better way to plot and display only N. American Data

#check this out: http://gis.stackexchange.com/questions/61243/clipping-a-raster-in-r

us <- getData("GADM", country="USA", level=1)
pp.states=c("California","Oregon", "Washington","Idaho","Nevada","Arizona","Montana","Colorado","Wyoming","New Mexico","Kentucky","Tennessee","Ohio","Indiana", "Illinois", "Wisconsin", "Missouri", "Iowa", "Minnesota", "Arkansas", "Michigan", "North Dakota", "South Dakota", "Nebraska", "Kansas", "Oklahoma", "Texas")
pp = us[match(toupper(pp.states),toupper(us$NAME_1)),]
plot(rast)
plot(pp, add=TRUE)

rr <- mask(rast, pp)

#this looks strange because of the points to raster conversion...
X11(width=11)
plot(rr, xlim=c(-106.65, -75),ylim=c(24, 52), main="Correlation between Zonal Index and Precipitation " )
plot(pp, add=TRUE)
#need raster and data in same prjection
#raster coordinates
proj.84<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
proj.shape<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
projection(rast)<-proj.shape
projection(rr)<-proj.84


spplot(rast, sp.layout=list("sp.polygons", us, "STATE",
                            col=1, fill=0, lwd=1, lty=1, first=F), xlim=c(bbox(us)[1,]),
       ylim=c(bbox(us)[2,]), main="Daily standard deviation of temperature")

