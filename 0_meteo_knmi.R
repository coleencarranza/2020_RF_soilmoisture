##------Relative humidity------------------
library(rgdal)
#Read knmi text file for station locations
stn<-read.csv("./0_Data/meteo_rmpts_ts/KNMI_20200127.txt",sep="",
              header=TRUE,skip =4,nrows = 50)
stn<-stn[,-1]

stn$STN<-as.factor(substr(stn$STN,1,3))
#rearrange ascending
stn<-stn[order(stn$STN, decreasing = FALSE),]
colnames(stn)<-c("STN","LON","LAT","ALT","NAME")



#Read the data
data<-read.csv("./0_Data/meteo_rmpts_ts/KNMI_20200127.txt",sep=",",blank.lines.skip = TRUE,
               header=TRUE,skip =67, fill=TRUE)

colnames(data)[[1]]<-"STN"
data$STN<-as.factor(data$STN)

#function daily values per variable
daily.meteo<-function(data,x){
  met<-data[,c("STN","YYYYMMDD",x)]
  met<-met[order(met$STN,decreasing = FALSE),]
  met<-met[complete.cases(met),]
   #split per day
  met.day<-split(met, f=as.factor(met$YYYYMMDD))
  return(met.day)
}

mets<-colnames(data[,-c(1,2)])
mets.day.list<-lapply(mets, function(x) daily.meteo(data,x))



#INterpolation crop NL bdy
library(fields)
library(raster)
NLshp<-readOGR("./0_Data/shp/NLD_adm0.shp",layer = "NLD_adm0", pointDropZ = TRUE )

# 1deg~ 111km
NLr<-raster(NLshp)
res(NLr)<-1/20 #~ 5km

int.spl.nl<-function(stn, met,r,shp){
  #Subset STN points with common STN in data
  #met<-inlist[[1]]
  #stn1<-stn[which(as.numeric(stn$STN) %in% as.numeric(met$STN)),]
  stn<-stn[which(as.numeric(stn$STN) %in% as.numeric(met$STN)),]
  #convert to spatial WGS?
  coordinates(stn) <- ~LON+LAT
  crs(stn)<-CRS("+init=epsg:4326")
  
  m <- Tps(coordinates(stn), met[,3], lon.lat = TRUE, miles = FALSE, GCV = TRUE)
  tps <- interpolate(r, m)
  tps<-mask(tps,shp)
}


#Interoplate each meteo variable
tps.list<-list()
for(i in seq_along(mets.day.list)){
  inlist<-mets.day.list[[i]]
 tps.list[[i]]<-lapply(inlist, function(x) int.spl.nl(stn,x,NLr,NLshp))
}
st<-lapply(tps.list, stack)

  
#check range of results - min, max of each stack
mins<-lapply(st, function(x) cellStats(x, min, na.rm=TRUE))
maxs<-lapply(st, function(x) cellStats(x, max, na.rm=TRUE))

#plots to see
mapply(function(x,y) {plot(x, type= 'l', col = "red")
                      lines(y, col = "blue")},mins,maxs)


#Change to units same as in H1D
st[[1]]<-st[[1]]*60*60*24/1000*0.1 #to km/day
st[[2]]<-st[[2]]*0.1 #to mm
st[[3]]<-st[[3]]*0.1 #to mm
st[[4]]<-st[[4]]*0.1 #to mm
st[[5]]<-st[[5]]*0.1 #to hr
st[[5]][st[[5]] < 0] <- 0

st[[6]]<-st[[6]]/100 #to J/cm2
st[[7]]<-st[[7]]*0.1/10 #to cm
st[[7]][st[[7]] < 0] <- 0.04*0.1/10
st[[9]]<-st[[9]]*0.1 #to mm



#check range of results - min, max of each stack
mins<-lapply(st, function(x) cellStats(x, min, na.rm=TRUE))
maxs<-lapply(st, function(x) cellStats(x, max, na.rm=TRUE))


#plots to see
mapply(function(x,y) {plot(x, type= 'l', col = "red")
  lines(y, col = "blue")},mins,maxs)



#EXport
mapply(function(x,y) writeRaster(x,paste0("./3_figs/",y,".grd"), format="raster", overwrite=TRUE),st,mets)



#---------------------------Rainfall files---------------------------------------------
library(ncdf4)
library(raster)

rd.new<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"


rd<-list.files(path = "./0_Data/meteo_rmpts_ts/Rd1",pattern = "*.nc", full.names = TRUE,recursive = TRUE)
rd.names<-list.files(path = "./0_Data/meteo_rmpts_ts/Rd1",pattern = "*.nc", recursive = TRUE)
rd.names<-substr(rd.names,8,17)

rd<-rd[!duplicated(rd.names)]
rd.names<-rd.names[!duplicated(rd.names)]

rd1<-nc_open(rd[[1]])
print(rd1)

rd.list<-list()
for(i in seq_along(rd)){
  rd.list[[i]]<-raster(rd[[i]],  varname = "prediction")
}

rd.wgs<-function(x){
  crs(x)<- rd.new;x
  x<-projectRaster(x,crs = crs("+init=epsg:4326"))
  return(x)
}

rd.list<-lapply(rd.list,function(x)  rd.wgs(x))
rd.list<-setNames(rd.list,rd.names)
rd.list<-rd.list[!duplicated(names(rd.list))]
#Stack
rd.stack<-stack(rd.list)

#TO mm
rd.stack<-rd.stack*0.1


plot(max, type= 'l')
lines(min, col="red")


#export raster
writeRaster(rd.stack,"./3_figs/meteo_maps/rd.grd", format="raster", overwrite=TRUE)
