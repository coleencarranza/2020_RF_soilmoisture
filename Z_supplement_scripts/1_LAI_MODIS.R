#_---------------------------Creating time series LAI from MODIS------------------------
library(gdalUtils)
library(raster)
library(rgdal)
library(MODIS)

##I already downloadd MODIS but just found out there is a MODIS package in R. COuld also do DL and processing there!
path<-"/media/coleen/DDrive/1_OWASIS/0_ExternalDATA/MODIS/MODIS_2014-2018/"

setwd(path)

# Get a list of sds names
modis<-list.files(pattern = "*.hdf")

sds <- get_subdatasets(paste0(path,'MOD15A2H.A2015361.h18v03.006.2016007231236.hdf'))
# Any sds can then be read directly using the raster function
r <- raster(sds[2])

lai.list<-list()
for (i in 1:length(modis)){
  sds <- get_subdatasets(paste0(path,modis[i]))
  lai.list[[i]]<-raster(sds[2])
}


#extract LAI to point (all four stations used)
st.pts<-readOGR("//media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/rf_controls", layer="Merge_TW_RM_Stations_bofek", pointDropZ = TRUE)

##Projection!
crs(lai.list[[1]])
crs(st.pts)

#Shapefile reprojection
st.pts1<- spTransform(st.pts, crs(lai.list[[1]]))

#Plot to see
plot(lai.list[[1]])
plot(st.pts1,add=TRUE)


#Separate points
uni<-as.vector(unique(st.pts1@data$Name))
st.list<-list()
for(i in 1:length(uni)){
  
  st.list[[i]]<-st.pts1[st.pts1@data$Name==uni[i],]
}


#Extract values to points
st.lai<-list()
st.all.lai<-list()
for(i in seq_along(st.list)){
    st<-st.list[[i]]
  for(j in seq_along(lai.list)){
    st.lai[[j]]<-as.numeric(extract(lai.list[[j]],st))##This is where extract is

  }
    st.all.lai[[i]]<-unlist(st.lai)
}


#MODIS date sequence from file name
modis.dates<-lapply(modis,function(x) extractDate(x, asDate = TRUE)[[1]])
lai.df<-as.data.frame(do.call("rbind",modis.dates))
lai.df$V1<-as.Date(lai.df$V1, format = "%Y%m%d", origin = "19700101")

dates<-lai.df$V1


#Combine dates and LAI into a data frame

lai.df<-lapply(st.all.lai, function(x) as.data.frame(cbind(dates,x)))
lai.df<-lapply(lai.df, function(x)  {colnames(x)<-c("Date","LAI");x})
lai.df<-lapply(lai.df, function(x)  {x$Date<-as.Date(x$Date, format = "%Y%m%d", origin = "19700101");x})
names(lai.df)<-uni

#Need to linearly interpolate values in between dates
#Daily time sequence from 2016-2018
library(xts)
library(zoo)
library(ts)
days.seq<-as.data.frame(seq(as.Date('2013-12-27'), as.Date('2019-01-17'), by = "days"))
colnames(days.seq)<-"days.seq"


LAI.daily<-lapply(lai.df, function(x) merge(days.seq,x, by.x = "days.seq",by.y = "Date", all.x = TRUE))


LAI.daily<-lapply(LAI.daily, function(x) x[!duplicated(x$days.seq),])
LAI.daily.xts<-lapply(LAI.daily, function(x) xts(x$LAI, x$days.seq))



#Interpolate missing values
library(imputeTS)

LAI.ts.imp<-lapply(LAI.daily.xts, function(x) na.interpolation(x,option = "linear"))


LAI.daily.int<-lapply(LAI.ts.imp, function(x) data.frame(date=index(x), coredata(x)))
for(i in seq_along(LAI.daily.int)){
  colnames(LAI.daily.int[[i]])<-c("Date", "LAI")
  write.csv(LAI.daily.int[[i]], paste0("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/rf_controls/data/MODIS_LAI/", 
                                  names(LAI.daily.int[i]), ".csv"), row.names = FALSE)
}

#Export 

