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
years<-as.character(1980:2013)
#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", years)

#subset the data so it matches the data used to calculate zonal index
df.new<-df[df$Longitude > 180,]
df.new<-df.new[df.new$Longitude < 359.9969,]
df.new<-df.new[df.new$Latitude> 1.000001,]
df.new<-df.new[df.new$Latitude< 84,]
df.new<-data.frame(df.new)

# this plots one of the years (1980)
ggplot(data=df.new, aes(y=Latitude, x=Longitude)) +
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
df.new$Longitude<-df.new$Longitude-360
df.new$lon<-df.new$Longitude
df.new$lat<-df.new$Latitude

df.byyear<-t(df)
df.byyear<-df.byyear[,colSums(is.na(df.byyear)) == 0]
df.byyear<-data.frame(df.byyear[1:36,])
new.zonal<-c(zonal.index)
df.byyear$zonal.index<-zonal.index
df.all<-cbind(df.byyear,zonal.index[1:36,])


df.zonal<-data.frame(rep(zonal.index[1:36,],4)

point1<-matrix(0,1,9550)
for (i in 1:9550){
point1[,i]<-cor(df.all[3:36,i],df.all[3:36,9550])
}
cors<-t(point1[])
ll<-t(df.byyear[1:2,])
correlation<-cbind(ll,point1[,1:9549])
colnames(correlation)<-c("Longitude", "Latitude", "Correlation")

correlation<-data.frame(correlation)
#plot the correlations on the map
ggplot(data=correlation, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Correlation)) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient(correlation$Correlation, limits=c(-0.5,0.6)) +
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
