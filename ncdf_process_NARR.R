#R-Script to extract the values for the ocean surface and seafloor from CMIP5 NetCDF files. This script generates csv files that can be opened in
#Excel or any other statistical program. The resulting files contain the latitude, longitude and values for each cell at the surface and seafloor. The
#generate files do maintain the generic naming of the original file. This script will crash if the memory RAM used exceeds what is available in your
#computer. If you face this problem, you can buy more RAM or fraction the NetCDF files into single years using the script provided above for NCO.
#By Camilo Mora, Ph.D., cmora@hawaii.edu

path = "C:/Users/Kelly/Documents/Undergrad_projects/" #Path to the working folder. Change accordingly to the path of your Workingfolder.
#setwd("C:/Users/Kelly/Documents/Research_plans/MIP/CMIP5/output") #Path to the folder where R will place the CSV files. Change accordingly to the path of your OutputFolder.
library(reshape) #Loads this package. If not installed, click on “Packages” in R, click on Install packages and follow instructions
library(ncdf4) #Loads this package. If not installed, click on “Packages” in R, click on Install packages and follow instructions
Files =list.files (path)#Gets the names of all the NetCDF files in your WorkingFolder
Files=c("uwnd.10m.mon.mean.nc")
#note that the land cover Fraction data is set up differenly from the individual files
#need to fix the processing for land cover fraction

nFiles=length(Files) #Counts the number of files to be processed
nFiles
#Files="baresoilFrac_Oyr_HadGEM2-CC_historical_r1_1859-1859.nc"
#nFiles=1
for (i in 1:nFiles) #For each NetCDF file it extracts the values of the parameter at the ocean surface and seafloor
{
  nc=nc_open(paste(path,Files[i],sep = "")) #Opens NetCDF file i for processing
  #Gathers all the information to name the output CSV file
  #Variable=unlist(strsplit(Files[i], "_"))[1] #Obtains the name of the variable/parameter of the file
  #Model=unlist(strsplit(Files[i], "_"))[3] #Obtains the name of the model of the file
  #Year1=as.numeric(unlist(strsplit(unlist(strsplit(Files[i], "_"))[6], "-"))[1]) #Obtains the first year in the file
  #Experiment=unlist(strsplit(Files[i], "_"))[4] #Obtains the name of the experiment in the file
  #Gathers internal information that NetCDF file used to name the different dimensions
  Variable="uwnd"
  dLon=nc$var[[Variable]]$dim[[1]]$name #Obtains name used in the NetCDF file to refer to longitude. This name varied by model
  dLat=nc$var[[Variable]]$dim[[2]]$name #Obtains name used in the NetCDF file to refer to latitude. This name varied by model
  time=nc$var[[Variable]]$dim[[3]]$name #Obtains name used in the NetCDF file to refer to depth. This name varied by model
  uwnd=nc$var[[Variable]]$name
  
  vTime = ncvar_get(nc, time)
  vLon = ncvar_get(nc, dLon)
  vLat = ncvar_get(nc, dLat)
  vvariable=ncvar_get(nc, Variable)
  
  #Number of values in each dimension
  nTime=length(vTime)
  
  nLon=length(vLon)
  nLat=length(vLat)
  
  #Creates an empty data frame with the index coordinates for each cell. Values of the cells at each depth will be appended to this frame
  DataFrame=ncvar_get(nc, Variable, start=c(1,1,1), count=c(nLon,nLat,nTime))
  DataFrame=as.data.frame(melt(DataFrame)[,1:3])
  
  
######################how to get bare soil fraction???  
  #For each year in the NetCDF file i it extracts the values of the parameter for each cell  time a
  for (a in 1:nTime){
      Data = ncvar_get(nc, Variable, start=c(1,1,a), count=c(nLon,nLat,1))
      DatabyColumns=as.data.frame(melt(Data)[,3]) #Converts matrix to flat table
      #In some models land was represented as 1e20. This value is replaced with NA for consistency with most models.
      DatabyColumns=replace(DatabyColumns,DatabyColumns>1000000,NA)
      
      DataFrame=cbind(DataFrame,DatabyColumns)}
    #DataFrame[DataFrame==0]<-NA #Note: one model (i.e. Thetao in MRI-CGCM3) set land to zero. In that case you have to active this script, which sets land to NA
    #
    dat2<-data.frame(t(DataFrame[,]))
    dat3<-lapply(dat2,function(x) tail(x[!is.na(x)],1))
    dat4<-ifelse(sapply(dat3,length)==0,NA,dat3)
    dat5<-data.frame(unlist(dat4))
    dat6<-data.frame(DataFrame[1:3])
    #Final table with matrix coordinates and values for the surface and seafloor
    colnames(dat6)=c("x", "y", Variable)
    #This section adds the latitude and longitude coordinates for each cell and centers the coordinates between -180 and 180. One important
if(is.null(nc$var[["lat"]]$ndims)==TRUE){  
dLat=data.frame(melt(nc$var[[Variable]]$dim[[2]]$vals))
dLon=data.frame(melt(nc$var[[Variable]]$dim[[1]]$vals))
dLon=data.frame(dLon[1], value=ifelse(dLon$value<=180, dLon$value, dLon$value-360))
dLon=data.frame(dLon[1], value=ifelse(dLon$value>-180, dLon$value,360+ dLon$value)) #Center all longitudes between -180 to 180
dt7=merge(dat6,dLon,by.x="x",by.y="indices")
dt8=merge(dt7,dLat,by.x="y",by.y="indices")
colnames(dt8)=c("y", "x",Variable, "LONGUITUDE", "LATITUDE")
} 
else
{
  dLat= data.frame(melt(nc$var[[Variable]]$dim[[2]]$vals))
  dLon=data.frame(melt(nc$var[[Variable]]$dim[[1]]$vals))
  dLon=data.frame(dLon[1], value=ifelse(dLon$value>-180, dLon$value,360+ dLon$value)) #Center all longitudes between -180 to 180
  dt7=merge(dat6,dLon,by.x="x",by.y="indices")
  dt8=merge(dt7,dLat,by.x="y",by.y="indices")

  colnames(dt8)=c("y", "x", Variable, "LONGUITUDE", "LATITUDE")
}
#Saves the results keeping the original NetCDF file name. If you want the data from all depths, replace “dt8” for “DataFrame” in the script below
write.table(dt8, file = paste("C:/Users/Kelly/Documents/Research_plans/MIP/CMIP5/output",Variable,"_",Model,"_",Experiment,"_",Year1+a-1,".csv", sep = ""), sep = ",", col.names = NA)
}
proc.time() #calculates the time it took for data processing

#getting an error (probably on the land cover fraction netcdf values)

