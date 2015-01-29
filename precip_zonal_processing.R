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
df.new<-df[df$Longitude<179.9928,]
df.new<-df.new[df.new$Longitude>-179.9969,]
df.new<-df.new[df.new$Latitude> 1.000001,]
df.new<-df.new[df.new$Latitude< 84,]

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