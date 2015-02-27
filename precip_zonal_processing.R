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

#download worldclim data from the worldclim website
#this just has a monthly average precip, not really useful for this analysis
w=raster("prec/prec_6")
#monthly averaged data from http://www.esrl.noaa.gov/psd/data/gridded/data.unified.daily.conus.html
inputfile="C:/Users/Kelly/Documents/Undergrad_projects/clim-analysis/precip.V1.0.mon.mean.nc"
p<-nc_open(inputfile)
h<-inputfile
pr<-brick(h,lvar=3, varname="precip", level=6)


##coded out is the CMAP data
#inputfile="C:/Users/Kelly/Documents/Undergrad_projects/clim-analysis/precip.mon.mean.nc"
#p<-nc_open(inputfile)
#h<-inputfile
#pr<-brick(h,lvar=3, varname="precip", level=6)
mult<-(1:35.5)*12
mult.6<-mult-6
#subset hgt so that we have a rasterbrick with only july geopotential heights
july.pr<-subset(pr, mult.6)
pr.map<-rasterToPoints(july.pr)
df <- data.frame(pr.map)
years<-as.character(1979:2013)
#Make appropriate column headings
colnames(df) <- c( "X", "Y",years)

#subset the data so it matches the data used to calculate zonal index

df<-data.frame(df)

# this plots one of the years (1980), but strangely beacue it was from points
ggplot(data=df, aes(y=Y, x=X)) +
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
df$X<-df$X-360

#df$lon<-$Longitude
#df$lat<-$Latitude

df.byyear<-t(df)
df.byyear<-df.byyear[,colSums(is.na(df.byyear)) == 0]
df.byyear<-data.frame(df.byyear[,])
new.zonal<-c(zonal.index)
df.byyear$zonal.index<-zonal.index
df.all<-cbind(df.byyear,zonal.index)




#df.zonal<-data.frame(rep(zonal.index[1:36,],4))

cor.by.pt<-matrix(0,1, 13628)
for (i in 1:13628){
cor.by.pt[,i]<-cor(df.all[3:36,i],df.all[3:36,13628])
}
#cors<-t(point1[])
ll<-t(df.byyear[1:2,])
###fix this
correlation<-cbind(ll,cor.by.pt[,1:13627])

colnames(correlation)<-c( "x","y", "Correlation")

correlation<-data.frame(correlation)
#correlation$Latitude<-correlation$Latitude-180
#correlation$Longitude<-correlation$Longitude-180

july.cor<-correlation

#plot the correlations on the map

# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(july.cor[,1:2])
e <- e + 50 # add this as all y's are the same

r <- raster(e, ncol=120, nrow=300)
# or r <- raster(xmn=, xmx=,  ...

# you need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(july.cor[, 1:2], r, july.cor[,3], fun=mean)

x11(width=11)
plot(x, xlim=c(220,360), ylim=c(25,50))

#these plots are kinda crappy looking

#check this out: http://gis.stackexchange.com/questions/61243/clipping-a-raster-in-r

us <- getData("GADM", country="USA", level=1)
pp.states=c("California","Oregon", "Washington","Idaho","Nevada","Arizona","Montana","Colorado","Wyoming","New Mexico","Kentucky","Tennessee","Ohio","Indiana", "Illinois", "Wisconsin", "Missouri", "Iowa", "Minnesota", "Arkansas", "Michigan", "North Dakota", "South Dakota", "Nebraska", "Kansas", "Oklahoma", "Texas")
pp = us[match(toupper(pp.states),toupper(us$NAME_1)),]
plot(x)
plot(pp, add=TRUE)

rr <- mask(x, pp)

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

