  ##---------------------Extract meteo data into Raam points-------------------
  #read 0_meteo_knmi.R first
  #read raster stacks of meteo files -- all are in WGS
  library(raster)
  library(rgdal)
  grds<-list.files(path = "./3_figs/meteo_maps",pattern = ".grd",full.names = TRUE)
  meteo<-lapply(grds, stack)
  
  
  #check range of results - min, max of each stack
  mins<-lapply(meteo, function(x) cellStats(x, min, na.rm=TRUE))
  maxs<-lapply(meteo, function(x) cellStats(x, max, na.rm=TRUE))
  
  
  
  #plots to see
  mapply(function(x,y) {plot(x, type= 'l', col = "red")
    lines(y, col = "blue")},mins,maxs)
  
  
  
  met.nam<-substr(grds,28,nchar(grds)-4)
  
  
  #read crop types
  
  
##Extract values to Raam stations - already with BOFEK/PWAN codes for each station
RM<-readOGR(dsn="./0_Data/shp/rm_pts_wgs_bofek.shp", layer = "rm_pts_wgs_bofek", pointDropZ = TRUE )
  


##------------Extract values to points------------------#
#split RM points shapefile separate points
RM.pts<-split(RM, f = as.factor(RM$Name))


#For each RM.pts, extract each variable)
meteo2rm<-function(pts){
  a<-lapply(meteo, function(x) extract(x,pts))
  b<-lapply(a, function(x) as.numeric(x))
  c<-do.call(qpcR:::cbind.na,b) #output is df with meteo variables
  c<-data.frame(c)
    return(c)
}

#already meteo in the function
met.rm<-lapply(RM.pts, function(x) meteo2rm(x))
met.rm2<-lapply(met.rm, function(x) {colnames(x)<-met.nam;x})


dts<-substr(names(meteo[[1]]),2,9)
library(xts)
met.rm2<-lapply(met.rm2, function(x) {x$date<-as.Date(dts, format  = "%Y%m%d");x})
met.rm.xts<-lapply(met.rm2, function(x) xts(x[,-c(11,5,7)],x[,11]))##Don't use RH, us RD maps - this is interpolated with more points!, Also rmove TG


#export to files
#lapply(nms.list, function(x) write.csv(metrm.df[[x]], file = paste0("./data/all_raam/meteo_rmpts_ts/",x,".csv"), row.names = FALSE))

#RUN only if no need to extrac values to points - read existing files!
#met<-list.files(path = "./data/all_raam/meteo_rmpts_ts/",pattern = ".csv", full.names = TRUE)
#met.list<-lapply(met, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
#met.list<-lapply(met.list, function(x) {x$date<-as.Date(x$date, format = "%Y-%m-%d");x})
#met.rm.xts<-lapply(met.list, function(x) xts(x[,-c(1,6,8)],x[,1]))##Don't use RH, us RD maps - this is interpolated with more points!, Also rmove TG

##round 2 decimals
met.rm.xts<-lapply(met.rm.xts, function(x) round(x,2))
lapply(met.rm.xts[[1]], plot)

#put rd in mm first - transform to cm in h1d_files_prep
met.rm.xts<-lapply(met.rm.xts, function(x) {x$rd<-x$rd*10;x})



###-----------------ADD soil moisture Data---------------------
sm.files<-list.files("./0_Data/rm_ts_all_daily",full.names = TRUE)
sm.files<-sm.files[grepl("10A|10B",sm.files)==FALSE] #remove parts R10


read.data<-function(x){
  st<-read.csv(x, header=T,stringsAsFactors = F,  sep = ",")
  colnames(st)[2:6]<-c("VWC5","VWC10","VWC20","VWC40","VWC80")
  st$date<-as.Date(st[, "date"],format = "%Y-%m-%d")
  st<-st[1:6]
  return(st)
}

sm<-lapply(sm.files, read.data)
sm.sts<-substr(sm.files,nchar(sm.files)-11,nchar(sm.files)-8)
sm<-setNames(sm, sm.sts)

sm<-lapply(sm, function(x) {x<-within(x,VWC_40zone <-(VWC5*7.5+VWC10*7.5 +VWC20*15 +VWC40*30)/60);x})
sm<-lapply(sm, function(x) {x<-within(x,VWC_80zone <-(VWC5*7.5+VWC10*7.5 +VWC20*15 +VWC40*30+VWC80*40)/100);x})
sm.list<-lapply(sm, function(x) xts(x[,-1],x[,1]))
sm.list<-lapply(sm.list, function(x) round(x,4))

##----------------------------------------Vegetation Variables LAI and NDVI - from MODIS?------------------------------------------
lai<-list.files(path = "./0_Data/meteo_rmpts_ts/MODIS_LAI",pattern = ".csv", full.names = TRUE)

read.lai<-function(x){
  st<-read.csv(x, header=T,stringsAsFactors = F,  sep = ",")
  st$Date<-strptime(st$Date, "%Y-%m-%d")
  st$Date<-as.Date(st[, "Date"],format = "%Y-%m-%d")
  colnames(st)<- c("date","LAI")
  return(st)
}
lai.list<-lapply(lai, read.lai)
names<-substr(lai,nchar(lai)-7,nchar(lai)-4)

#Subset only stations needed
lai.list<-setNames(lai.list,names)
st.nm<-names(sm)
lai.list<-lai.list[st.nm]
lai.list<-lapply(lai.list, function(x) xts(x$LAI,x$date))
lai.list<-lapply(lai.list, function(x) {colnames(x)<-"LAI";x})

##---------------------Combine in one data frame-----------------------
all.list<-list()
for(i in seq_along(met.rm.xts)){
  all.list[[i]]<-merge(sm.list[[i]],met.rm.xts[[i]],lai.list[[i]])
  all.list[[i]]<-all.list[[i]][complete.cases(all.list[[i]]$EV24),] #start with 2016
}

all.list<-lapply(all.list, function(x) data.frame(date=index(x), coredata(x)))


#-----------------Add soil and crop attrbutes-----------------------------
nms.list<-sm.sts
rm.att<-RM@data
rm.att$BOFEK2012<-as.factor(rm.att$BOFEK2012)
rm.crop<-read.csv("./0_Data/soil/Crop_types_Raam_Stations.csv", header = TRUE,stringsAsFactors = FALSE)
colnames(rm.crop)[2:5]<-2015:2018

#names of list is lost
library(lubridate)

#Only 2016-2018
crop2rm<-function(nms,rm.xts){
  #rm.xts<-all.list[[7]]
  #nms<-nms.list[[7]]
  crp<-rm.crop[grep(nms,rm.crop$Stn),3:5]
  Crop<-numeric()
for(i in 1:nrow(rm.xts)){
  if(year(rm.xts[i,"date"]) ==2016){
    Crop[i]<-crp[1]
  }else if(year(rm.xts[i,"date"]) ==2017){
    Crop[i]<-crp[2]
  }else if(year(rm.xts[i,"date"]) ==2018){
    Crop[i]<-crp[3]
  }else{
    Crop[i]<-NA
  }
}
  Crop<-unlist(Crop)
  Crop<-as.factor(Crop)
  return(Crop)
}


for(i in seq_along(all.list)){
  all.list[[i]]$Crop<-crop2rm(nms.list[[i]],all.list[[i]])
  all.list[[i]]$BOFEK<- rm.att[grep(nms.list[[i]],rm.att$Name),2]
}

##-----------------------------lag with all atmospheric variables--------------------------
library(tsModel)
all.list<-lapply(all.list, function(x) {x$VWC5_lag<-Lag(x$VWC5,1);x})
all.list<-lapply(all.list, function(x) {x$VWC5_lag3<-Lag(x$VWC5,3);x}) #McColl et al 2017 naturw
all.list<-lapply(all.list, function(x) {x$VWC5_lag40<-Lag(x$VWC5,40);x})#Orth Seneviratne 2012

##lag with all atmospheric variables
library(dplyr)
all.list.lag<-lapply(all.list, function(x) mutate_all(x, funs(lag))[,9:17])
#add "lag" to colnames
all.list.lag<-lapply(all.list.lag, function(x) {colnames(x)<-paste(colnames(x),"lag", sep = "_");x})

all.list<-Map(cbind,all.list,all.list.lag) # no groundwater!
#all.list<-lapply(all.list, function(x) {x$GW_lag<-Lag(x$cm_to_MVi,1);x}) depth to groundwater


#3 day rainfall sum
library(zoo)
all.list<-lapply(all.list, function(x) {x$rd_3day<-c(NA,NA,rollapply(x$rd, 3, sum));x})
#3 day rainfall lag
all.list<-lapply(all.list, function(x) {x$rd_3day_lag<-Lag(x$rd_3day,1);x})


#Seasonality - DOY
all.list<-lapply(all.list, function(x) {x$DOY<-as.numeric(strftime(x$date, format = "%j"));x})
all.list<-setNames(all.list,sm.sts)
  
  
#export to files
lapply(nms.list, function(x) write.csv(all.list[[x]], file = paste0("./0_Data/all_list/",x,".csv"), row.names = FALSE))

all.list<-all.list[1:4]
