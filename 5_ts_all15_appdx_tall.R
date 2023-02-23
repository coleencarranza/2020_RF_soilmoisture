##---------------------------------Random forest-FORECASTING--r esults from caret/ranger--------------------------------------------
library(randomForest)
library(tsModel)
library(lubridate)
library(dplyr)
library(xts)
library(scales)
library(Metrics)
library(broman)

##analysis type:::
type="fcast"



#Choose which H1d run to plot:
h1d<-"da"

if(h1d=="da"){
  h1d.run<-"th_zone_DA"
  samp<-20
}else{
  h1d.run<-"th_zone"
}

#For some plot labels
if(type =="pred"){
  labs<-"Prediction"
}else if (type == "fcast"){
  labs <- "Forecasting"
}

#0. read and prep files
PrepFiles<-function(x){
  x$BOFEK <-as.factor(x$BOFEK)
  x$Crop<-as.factor(x$Crop)
  x$date <- as.Date(x$date, format = "%Y-%m-%d")
  return(x)
}

files<-list.files(path = "./0_Data/all_list", full.names = TRUE)
files<-files[grepl("10A|10B",files)==FALSE] #remove parts R10

names<-lapply(files, function(x)  substr(x,nchar(x)-7,nchar(x)-4))
all.list<-lapply(files, read.csv, header=TRUE, stringsAsFactors = FALSE)
all.list<-lapply(all.list, PrepFiles)

for(i in seq_along(all.list)){
  all.list[[i]]$names<-names[[i]]
}
#Remove other soil depths
all.list<-lapply(all.list, function(x) x[,-c(3:6,8)]) #VWC 40ave

#Rename to "zone
all.list<-lapply(all.list, function(x) {colnames(x)[3]<-"VWC_zone";x}) #VWC 40ave
#all.list<-lapply(all.list, function(x) x[complete.cases(x$VWC_zone),])

#Give names to all.list
nms<-substr(files, nchar(files)-7,nchar(files)-4)
all.list<-setNames(all.list,nms)

##READ R.data run from SLM processing PC----------
rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25.RData")
load(rf_out)

##GEt RMSEs for each run
fin_mods<-lapply(mods_tune,function(x) x$results)
best_mod<-lapply(mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)
best_mod.df
#plot RMSEs per training sample proportion




#All similar RMSE
#use model using 0.5 training tr.all[[1]]
rf_mod05<-mods_tune[[1]]
#pdf(paste("./figs/rf.mod.tol.fcast.pdf"), height=6,width=7)
plot(rf_mod05)
#dev.off()


###-----------------Other RF outputs - importance var for mod.tol! -------------------------------- 
vi<-(mod.tol$variable.importance)
vi<-sort(vi,decreasing =TRUE)
vi<-as.data.frame(vi)
colnames(vi)<-"VarImp"

#barplot
#pdf(paste("./figs/varimp_",type,".pdf"), height=7,width=4.5)
if(type=="pred"){
  titl<-"Prediction"
}else if(type=="fcast"){
  titl<-"Forecasting"
}
par(mar=c(3,6,1.2,3), mgp = c(1.95,0.7,0), tck = -0.02)
bp<-barplot(rev(vi$VarImp[1:20]),border=NA,
            col=alpha("skyblue3",0.5), horiz = TRUE, xlim = c(0,0.002),
            main =titl, xlab = "Importance")
axis(2,at=bp, labels = rev(row.names(vi)[1:20]), las=2) 
#dev.off()

#-----------------------------------#Reading H1D output --read B for longer simulation-------------------------------------
#-Read Soil moisture datasets----
Hydir<-list.dirs(path="./2_Hydrus/H1D_opt")
hyd<-list.files(path=Hydir,pattern = "Obs_Node.out", full.names = TRUE, recursive = FALSE)
hyd<-hyd[grepl("10A|10B",hyd)==FALSE] #remove parts R10

hyd.to.df<-function(x){
  z<-read.table(x,skip=10,nrows = length( readLines(x))-12,
                header=TRUE,stringsAsFactors = FALSE)
  z<-z[z$time %%1==0,c(1,3,6,9,12)] #integer for dailym,#Keep theta
  z<-z[!duplicated(z$time),]
  colnames(z)<-c("day","th5","th10","th20","th40")
  return(z)
}
hyd.df<-lapply(hyd, hyd.to.df)

#Root zone 
hyd.df<-lapply(hyd.df, function(x) {x<-within(x,th_zone <-(th5*7.5+th10*7.5 +th20*15 +th40*30)/60);x})
hyd.dates<-list.files(path= "./2_Hydrus/prep_data",pattern="*.csv", full.names = TRUE, recursive = TRUE)
hyd.to.dates<-function(x){
  x<-read.csv(x, header=TRUE, stringsAsFactors = FALSE)[,1]
  x<-as.Date(x, format = "%Y-%m-%d")
  return(x)
}
hyd.dates<-lapply(hyd.dates, hyd.to.dates)[[1]]
hyd.xts<-lapply(hyd.df, function(x) xts(x, hyd.dates)) 



#-------------------------------2. Reading H1D +DATA ASSIMILATION - Direct insertion ---------------------------------
#-Read Soil moisture datasets----
Hydir<-list.dirs(path=paste0("./2_Hydrus/DA/sample_",samp))
Hydir<-Hydir[grep("DAout",Hydir)]

#F1 - to df
hydDA.to.df<-function(x){
  z<-read.table(x,skip=10,nrows = length(readLines(x))-12,
                header=TRUE,stringsAsFactors = FALSE)
  z<-z[z$time %%1==0,c(1,3,6,9,12)] #integer for dailym,#Keep theta
  z<-z[!duplicated(z$time),]
  colnames(z)<-c("day","th5","th10","th20","th40")
  return(z)
}

#F -read all
hyd.merge<-function(x){
  hyd<-list.files(path=x,pattern = "Obs_Node", full.names = TRUE, recursive = FALSE)
  hyd.df<-lapply(hyd, hydDA.to.df)
  hyd.df<-do.call(rbind,hyd.df)
  #Ascending day
  hyd.df<-hyd.df[with(hyd.df, order(day)), ] 
  #remove dupplicate days
  #hyd.df<-hyd.df[!duplicated(hyd.df$day),]
  return(hyd.df)
  
}
#x<-Hydir[11]
#Read rm10 parts first then save as RM10
rm10<-Hydir[grepl("10A|10B",Hydir)]
rm10<-lapply(rm10,hyd.merge)
rm10.combi<-do.call(rbind,rm10)
rm10.combi<-rm10.combi[!duplicated(rm10.combi$day),]

Hydir.da<-Hydir[grepl("10A|10B",Hydir)==FALSE] #remove parts R10
hyd.da.list<-lapply(Hydir.da, hyd.merge)

##Replace rm10 with rm10.combi
hyd.da.list[[10]]<-rm10.combi

#Root zone 
hyd.da.list<-lapply(hyd.da.list, function(x) {x<-within(x,th_zone <-(th5*7.5+th10*7.5+th20*15 +th40*30)/60);x})

##To xts time series
#note that R uses a 0 based index for dates only
hyd.da.date<-lapply(hyd.da.list, function(x) as.Date(x$day-1,origin = "2016-01-01"))

#Plot with
hyd.da.xts<-mapply(function(x,y) xts(x[,2:6],y), hyd.da.list,hyd.da.date) 


#--------------------Combine or Plot together Root zone soil moisture from data, RF, hydus,--------------------
#Combine measurements, rf, and h1d values in one xts
data.zone<-lapply(all.list, function(x) xts(x[,"VWC_zone"],x[,1]))
data.zone<-lapply(data.zone, function(x) {colnames(x)<-"VWC_zone";x})

hyd.xts<-lapply(hyd.xts, function(x) x[,"th_zone"])
hyd.da.xts<-lapply(hyd.da.xts, function(x) x[,"th_zone"])

#Rename
hyd.da.xts<-lapply(hyd.da.xts, function(x) {colnames(x)<-"th_zone_DA";x})



all<-list()
for(i in seq_along(data.zone)){
  all[[i]]<-merge(data.zone[[i]],hyd.xts[[i]],hyd.da.xts[[i]],vl.xts[[i]][,44:47],all=TRUE)
  all[[i]]$th_zone<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone)
  all[[i]]$th_zone_DA<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone_DA)
  colnames(all[[i]])[4:7]<-c("RF_mean","vl.Q05","vl.Q50","vl.Q95")
  #all[[i]]<-all[[i]][complete.cases(all[[i]]$VWC_zone),]
}
all<-setNames(all,names(all.list))

#subset until 2018 only
all<-lapply(all, function(x) x[which(year(x) %in% c(2016,2017,2018)),])


#---------------------Accuracy metrics ------------------------------------------------------------
#load accu metric function
source("./0_Rscripts/0_fcn_accu.R")

#Accu RF--------------------
rm(accu)
all.comp<-lapply(all, function(x) x[complete.cases(x),])
accu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$RF_mean))

#Accu H1D-------------------
h1d.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x[,h1d.run]))## choose between th_zone or th_zone_DA at the start!




#-------------------------------------APPENDIX PLOT! ALL STATIONS------------------------------------------------
library(scales)
library(TeachingDemos)
min.max<-all
accu.min.max<-accu
##do this so assimilated data are also plotted.ONly for plotting
min.max<-lapply(min.max, function(x) {x$th_zone_DA<-ifelse(is.na(x$th_zone_DA)==TRUE,x$VWC_zone,x$th_zone_DA);x})


#inset plot function
ins.plot<-function(x,col,mod.var){
  mn<-round(min(x),1)
  mx<-round(max(x),1)
  plot(x,cex=0.55, col=alpha(col,0.31), bty = 'n', yaxt = 'n',xaxt= "n",cex.lab = 1.4,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = mod.var)
  box(lwd=0.15,col = "grey50")
  axis(side=2, at=seq(mn,mx,by=0.1), las=2,cex.axis=1.3, lwd=0.15)
  axis(side=1, at=seq(mn,mx,by=0.1),cex.axis=1.3, lwd=0.15,)
  abline(0,1, lty =1,col="grey38",lwd=0.5)
}
if(h1d=="da"){
  suff<-"_DA"
}else{
  suff<-"_inv"
}


#colors
co.rf<-"royalblue2"
co.hyd<-"tomato3"


pdf(paste0("./3_figs/append_",type,"_ts",suff,"6.pdf"), height=12,width=16.5)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2020-11-21", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")[1:4]


par(oma = c(4,5,2.5,0.5))
layout(matrix(c(1:15),ncol=3, byrow =TRUE))
for(i in seq_along(min.max)){
  x<-min.max[[i]]
  ##TIME SERIES
  par(mar=c(0.3,0,0,0.3), mgp = c(1.5,0.65,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone), ylim=c(0.05,0.46), type = "l",lwd=1.5, xlim=c(st,fi), col="black",xaxt= "n",yaxt="n", bty="n")
  #Hydrus
  lines(as.zoo(x[,h1d.run]),lty=1,cex=0.25,col =co.hyd,lwd= 1)
  
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("lightgrey",0.05),border = NA)

  st.nm<- "2016-01-30"
  text(x=as.Date(st.nm, format = "%Y-%m-%d"),y=0.015, names(min.max[i]), cex =1.28, font=2)
  if(i %in% c(13,14,15)){
    axis(side=1,at=yr.seq,labels = year(yr.seq),cex.axis=1.3, lwd=0.25)
  }
  if(i %in% c(1,4,7,10,13)){
    axis(side=2, at=seq(0.05,0.45,by=0.1), las=2,cex.axis=1.2, lwd=0.25)
  }
  
  
  # Generating the lower and upper uncertainty bounds
  par(new=TRUE)
  bands<-x[complete.cases(x$vl.Q05),c(5,7)]
  lband <- as.zoo(bands$vl.Q05)
  uband <-as.zoo(bands$vl.Q95)
  hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("cornflowerblue",0.2), add=TRUE)
  
  #PLOT OF RF results
  lines(as.zoo(x$RF_mean),type = "b", pch=3,col=co.rf,cex = 0.2, lwd=0.17) #RF valid/forecast
 
  
  ##Accuracy text 
  legend('bottomright', ncol = 3L, x.intersp = 0.15,xpd=NA, inset = c(-0.01,0),cex =1.25,y.intersp = 1,
         bg=alpha("white",0.5),box.col=alpha("white",0.3),xjust = 0.5,
         legend = c('', 'RMSE', 'Bias', 'Unb. RMSE', "Corr",
                    'RF', accu.min.max[[i]][[1]], accu.min.max[[i]][[3]], accu.min.max[[i]][[4]], round(accu.min.max[[i]][[2]],3),
                    'oBOFDA', h1d.acu[[i]][[1]],h1d.acu[[i]][[3]],h1d.acu[[i]][[4]],round(h1d.acu[[i]][[2]],3)))
  
  ##SUBPLOT INSET
  #in situ vs RF
  ins<-data.frame(x[complete.cases(x$vl.Q50),c("VWC_zone","vl.Q50")])
  par(mgp = c(2.3,0.53,0), tck = -0.057)
  subplot(ins.plot(ins,col=co.rf,"RF"),  x=as.Date("2019-07-27", format = "%Y-%m-%d"),y=0.38, size = c(0.6,0.6))
  
  #in situ vs H1D
  ins<-data.frame(x[complete.cases(x$vl.Q50),c("VWC_zone",h1d.run)])
  ins<-ins[complete.cases(ins),]
  subplot(ins.plot(ins,col=co.hyd,"oBOFDA"),  x=as.Date("2020-09-15", format = "%Y-%m-%d"),y=0.38, size = c(0.6,0.6))

}
leg<-as.Date("2016-03-10", format = "%Y-%m-%d")
legend(x=leg, y=-0.005, legend =c("RF", "oBOFDA","in situ"), horiz = TRUE, xpd =NA, text.width = c(150,150,200),
       xjust=0, x.intersp = 0.5,lty = c(NA,1,1),lwd=0.9,
       pch=c(3,NA,NA), col = c(co.rf,co.hyd,"black"), bty= "n",cex= 1.5, seg.len =2)



mtext(side=1, "Time", outer=TRUE, line=2.3,cex =1.25)
mtext(side=2, expression(paste("Root zone soil moisture  (",theta["rz"], ", ",m^3*m^-3,")")), outer=TRUE, line=2.75, cex =1.25)
if(type =="pred"){
  mnt<-"Interpolation"
}else if(type =="fcast"){
  mnt<-"Extrapolation"
}
mtext(side=3, mnt, outer=TRUE, line=0.25,cex =1.35, font= 3)


dev.off()

