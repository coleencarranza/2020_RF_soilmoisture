##---------------------------------Random forest-FORECASTING--r esults from caret/ranger--------------------------------------------
library(randomForest)
library(tsModel)
library(lubridate)
library(dplyr)
library(xts)
library(scales)
library(Metrics)
library(broman)
library(RColorBrewer)

##analysis type:::
type="pred"
mod.tol<-"inflec"

#For some plot labels
if(type =="pred"){
  labs<-"Prediction"
}else if (type == "fcast"){
  labs <- "Forecasting"
}

##SAMPLING:
samp<-20
samp<-sprintf("%02d",samp)

#0. read and prep files
PrepFiles<-function(x){
  x$BOFEK <-as.factor(x$BOFEK)
  x$Crop<-as.factor(x$Crop)
  x$date <- as.Date(x$date, format = "%Y-%m-%d")
  return(x)
}


##############-----------------------------IN SITU DATA------------------------------------------
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
if(mod.tol== "inflec"){
  rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25_inflec.RData")
}else{
  rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25.RData")
}

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
#plot(rf_mod05)
#dev.off()



#-------------------------------IMport imported DATA assimilated-----------------------------------
DA.situ<-list.files(path=paste0("./2_Hydrus/DA/sample_",samp),pattern=".csv",full.names = TRUE)
DA.situ<-DA.situ[grepl("10A|10B",DA.situ)==FALSE] #remove parts R10

DAsituPrep<-function(x){
  DA<-read.csv(x,header=TRUE)
  DA$date <- as.Date(DA$date, format = "%Y-%m-%d")
  DA$DA <- 1
  DA<-DA[,c("date","DA")]
  return(DA)
}

DA.situ<-lapply(DA.situ, DAsituPrep)

#-----------------------------------#Reading BOFEK------------------------------------
#-Read Soil moisture datasets----
Hydir<-list.dirs(path="./2_Hydrus/BOFEK")
hyd<-list.files(path=Hydir,pattern = "Obs_Node.out", full.names = TRUE, recursive = FALSE)

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
hyd.bof.xts<-lapply(hyd.df, function(x) xts(x, hyd.dates)) 



#-----------------------------------#Reading H1D OPTIMIZED ONLY------------------------------------
hyd.to.df<-function(x){
  z<-read.table(x,skip=10,nrows = length( readLines(x))-12,
                header=TRUE,stringsAsFactors = FALSE)
  z<-z[z$time %%1==0,c(1,3,6,9,12)] #integer for dailym,#Keep theta
  z<-z[!duplicated(z$time),]
  colnames(z)<-c("day","th5","th10","th20","th40")
  return(z)
}
#-Read Soil moisture datasets----
Hydir<-list.dirs(path="./2_Hydrus/H1D_opt")
hyd<-list.files(path=Hydir,pattern = "Obs_Node.out", full.names = TRUE, recursive = FALSE)

#Read rm10 parts first then save as RM10
rm10<-hyd[grepl("10A|10B",hyd)]
rm10<-lapply(rm10,hyd.to.df)
rm10.combi<-do.call(rbind,rm10)
rm10.combi<-rm10.combi[!duplicated(rm10.combi$day),]

##Read all stations without
hyd<-hyd[grepl("10A|10B",hyd)==FALSE] #remove parts R10
hyd.df<-lapply(hyd, hyd.to.df)

##Replace rm10 with rm10.combi
hyd.df[[10]]<-rm10.combi

#Root zone 
hyd.df<-lapply(hyd.df, function(x) {x<-within(x,th_zone <-(th5*7.5+th10*7.5+th20*15 +th40*30)/60);x})
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

DA.situ.xts<-lapply(DA.situ, function(x) xts(x$DA,x$date))
DA.situ.xts<-lapply(DA.situ.xts, function(x) {colnames(x)<-"DA";x})
hyd.xts<-lapply(hyd.xts, function(x) x[,"th_zone"])
hyd.da.xts<-lapply(hyd.da.xts, function(x) x[,"th_zone"])
hyd.bof.xts<-lapply(hyd.bof.xts, function(x) x[,"th_zone"])
#Rename
hyd.da.xts<-lapply(hyd.da.xts, function(x) {colnames(x)<-"th_zone_DA";x})
hyd.bof.xts<-lapply(hyd.bof.xts, function(x) {colnames(x)<-"th_zone_bof";x})


all<-list()
for(i in seq_along(data.zone)){
  all[[i]]<-merge(data.zone[[i]],hyd.xts[[i]],hyd.da.xts[[i]],hyd.bof.xts[[i]],DA.situ.xts[[i]],all=TRUE)
  all[[i]]$th_zone_bof<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone_bof)
  all[[i]]$th_zone<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone)
  all[[i]]$th_zone_DA<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone_DA)
  all[[i]]$DA<-all[[i]]$DA*all[[i]]$VWC_zone
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
all.comp<-lapply(all, function(x) x[complete.cases(x[,1:4]),])

#Accu H1D-------------------
h1d.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone))
h1dda.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone_DA))
h1dbof.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone_bof))







#-------------------------------------APPENDIX PLOT! ALL STATIONS------------------------------------------------
library(scales)
library(TeachingDemos)
all<-lapply(all, function(x) {x$th_zone_DA<-ifelse(is.na(x$th_zone_DA)==TRUE,x$VWC_zone,x$th_zone_DA);x})
min.max<-all


#########EXport
h1.bof<-"darkgoldenrod"
h1.col<-"mediumseagreen"
h1.da<-"darkred"

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

pdf(paste0("./3_figs/ts_append_H1D",samp,"6.pdf"), height=12,width=16.5)# height=7.5,width=15.5)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2020-11-21", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")[1:4]


par(oma = c(4,5,2.5,0.5))
layout(matrix(c(1:15),ncol=3, byrow =TRUE)) #layout(matrix(c(1:15),ncol=3, byrow =TRUE)) 
for(i in seq_along(min.max)){
  x<-min.max[[i]]
  ##TIME SERIES
  par(mar=c(0.3,0,0,0.3), mgp = c(1.5,0.65,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone), ylim=c(0.0,0.46), type = "l",lwd=1.2, xlim=c(st,fi), xaxt= "n",yaxt="n", bty="n",col="black")
  #Hydrus
  lines(as.zoo(x$th_zone_bof),cex=0.25,col=h1.bof,lwd= 0.6)
  lines(as.zoo(x$th_zone),cex=0.25,col =h1.col,lwd= 0.6)
  lines(as.zoo(x$th_zone_DA),cex=0.25,col=h1.da,lwd= 0.75)
  points(as.zoo(x$DA),cex=0.86,col="black",pch=20)
  
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("grey90",0.05),border = NA)
  
  st.nm<- "2016-01-30"
  text(x=as.Date(st.nm, format = "%Y-%m-%d"),y=0.015, names(min.max[i]), cex =1.28, font=2)
  if(i %in% c(13,14,15)){
    axis(side=1,at=yr.seq,labels = year(yr.seq),cex.axis=1.3, lwd=0.25)
  }
  if(i %in% c(1,4,7,10,13)){
    axis(side=2, at=seq(0.0,0.45,by=0.1), las=2,cex.axis=1.2, lwd=0.25)
  }
  
  ##Accuracy text 
  legend('bottomright', ncol = 3L, x.intersp = 0.15,xpd=NA, inset = c(-0.01,0),cex =1.25,y.intersp = 1,
         bg=alpha("white",0.5),box.col=alpha("white",0.3),xjust = 0.5,
         legend = c('', 'RMSE', 'Bias', 'Unb. RMSE', expression(R^2),
                    'oBOF', h1d.acu[[i]][[1]], h1d.acu[[i]][[3]], h1d.acu[[i]][[4]], round(h1d.acu[[i]][[2]],3),
                    'oBOFDA', h1dda.acu[[i]][[1]],h1dda.acu[[i]][[3]],h1dda.acu[[i]][[4]],round(h1dda.acu[[i]][[2]],3)))
  
  ##SUBPLOT INSET
  #in situ vs RF
  ins<-data.frame(all.comp[[i]])
  par(mgp = c(2.3,0.53,0), tck = -0.057)
  subplot(ins.plot(ins[,c(1,2)],col=h1.col,"oBOF"),  x=as.Date("2019-07-27", format = "%Y-%m-%d"),y=0.4, size = c(0.6,0.6))
  
  #in situ vs H1D
  subplot(ins.plot(ins[,c(1,3)],col=h1.da,"oBOFDA"),  x=as.Date("2020-09-20", format = "%Y-%m-%d"),y=0.4, size = c(0.6,0.6))
  
}

leg<-as.Date("2016-08-10", format = "%Y-%m-%d")
legend(x=leg, y=-0.075, legend =c("BOF","oBOF","oBOFDA,","in situ"), ncol=1, xpd =NA, 
       pch=c(NA,NA,21,NA),pt.bg =c(NA,NA,"black",NA),lty=1,horiz = TRUE,
       xjust=0, x.intersp = 0.35, y.intersp = 0.85,col = c(h1.bof,h1.col,h1.da,"black"), bty= "n",cex= 1.21, seg.len = 2.5)



mtext(side=1, "Time", outer=TRUE, line=2.3,cex =1.25)
mtext(side=2, expression(paste("Root zone soil moisture (",theta["rz"], ", ",m^3*m^-3,")")), outer=TRUE, line=2.75, cex =1.25)
mtext(side=3, paste0("Hydrus-1D simulations with DA, n=",samp), outer=TRUE, line=0.25,cex =1.35, font= 3)


dev.off()


###############PLOTS comparing Hydrus-1D data assimilation and inverse modleing reuslts#
#Select stations min and max accuracy
accu.rmse<-unlist(lapply(h1dbof.acu, function(x) x[[1]]))
raam.min.rmse<-all[which.min(accu.rmse)]
raam.max.rmse<-all[which.max(accu.rmse)]
min.max<-c(raam.min.rmse,raam.max.rmse)

#STations with min.max from RF results
h1dbof.min.max<-h1dbof.acu [c(which.min(accu.rmse),which.max(accu.rmse))]
h1d.min.max<-h1d.acu[c(which.min(accu.rmse),which.max(accu.rmse))]
h1dda.min.max<-h1dda.acu[c(which.min(accu.rmse),which.max(accu.rmse))]



########------------------------MIN MAXP PLOTS - all three-------------------------
#inset plot function
ins.plot<-function(x,col,mod.var){
  mn<-round(min(x),2)
  mx<-round(max(x),2)
  plot(x,cex=0.2, col=alpha(col,0.31), bty = 'n', yaxt = 'n',xaxt= "n",cex.lab = 1,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = mod.var)
  box(lwd=0.15,col = "grey50")
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.8, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.8, lwd=0.15,)
  abline(0,1, lty =4)
}

pdf(paste0("./3_figs/ts_minmax_bof_inv_DA",samp,"3.pdf"), height=6,width=12)# height=7.5,width=15.5)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2020-06-01", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")[1:4]

h1.da<-"chartreuse4"
h1.col<-"indianred"

h1.bof<-"darkgoldenrod"
h1.col<-"mediumseagreen"
h1.da<-"red4"

par(oma = c(3,4.25,1,0.5))
layout(matrix(c(1:2),ncol=1, byrow =TRUE)) 
for(i in seq_along(min.max)){
  x<-min.max[[i]]
  y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.01
  y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.01
  ##TIME SERIES
  par(mar=c(0.3,0,0,0.3), mgp = c(1.5,0.5,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone), ylim=c(0.05,0.5), type = "l",lwd=1.3,lty=1,xlim=c(st,fi), xaxt= "n",yaxt="n", bty="n",col=alpha("black",1))
  #Hydrus
  #lines(as.zoo(x$th_zone_bof),cex=0.25,col="grey60",lwd= 0.7)
  lines(as.zoo(x$th_zone_bof),cex=0.25,col=h1.bof,lwd= 1.3)
  lines(as.zoo(x$th_zone),cex=0.25,col=h1.col,lwd= 1.3)
  lines(as.zoo(x$th_zone_DA),cex=0.25,col =h1.da,lwd= 1.3)
  points(as.zoo(x$DA),cex=0.76,bg="black",pch=21,col=h1.da,lwd=0.01)
  
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("grey99",0.1),border = NA)
  
  st.nm<- "2016-01-01"
  text(x=as.Date(st.nm, format = "%Y-%m-%d"),y=0.085, names(min.max[i]), cex =1.28, font=2)
  if(i %in% 2){
    axis(side=1,at=yr.seq,labels = year(yr.seq),cex.axis=1, lwd=0.25)
  }
  ax.lab<-round(seq(0.05,0.5,length.out = 5),2)
  axis(side=2, at=ax.lab, labels = c(0.0,"",ax.lab[3],"",ax.lab[5]), las=2,cex.axis=1, lwd=0.25)

  
  ##Accuracy text 
  legend('bottomright', ncol = 4, x.intersp = 0.15,xpd=NA, inset = c(0.1,0.01),cex =0.9,y.intersp = 0.95,
         bg=alpha("white",0.15),box.col=alpha("white",0.15),xjust = 0.5,text.width = c(rep(110,5),rep(97,5),rep(60,5),rep(-10,5)),
         legend = c('', 'RMSE', 'Bias', 'Unb. RMSE', expression(R^2),
                    'BOF', h1dbof.min.max[[i]][[1]], h1dbof.min.max[[i]][[3]], h1dbof.min.max[[i]][[4]], round(h1dbof.min.max[[i]][[2]],3),
                    'oBOF', h1d.min.max[[i]][[1]], h1d.min.max[[i]][[3]], h1d.min.max[[i]][[4]], round(h1d.min.max[[i]][[2]],3),
                    'oBOFDA', h1dda.min.max[[i]][[1]],h1dda.min.max[[i]][[3]],h1dda.min.max[[i]][[4]],round(h1dda.min.max[[i]][[2]],3)))
  
  ##SUBPLOT INSET
  ins<-data.frame(min.max[[i]])
  ins<-ins[complete.cases(ins[1:4]),]
  yl<-sum(0.5,0.05)/2 + (0.5-0.05)*0.29
  par(mgp = c(1.3,0.2,0), tck = -0.025)
  
  #in situ vs BOF
  subplot(ins.plot(ins[,c(1,4)],col=h1.bof,"BOF"),  x=as.Date("2019-04-01", format = "%Y-%m-%d"),y=yl, size = c(0.8,0.8))
  #in situ vs inv.
  subplot(ins.plot(ins[,c(1,2)],col=h1.col,"oBOF"),  x=as.Date("2019-11-01", format = "%Y-%m-%d"),y=yl, size = c(0.8,0.8))
  #in situ vs DA
  subplot(ins.plot(ins[,c(1,3)],col=h1.da,"oBOFDA"),  x=as.Date("2020-06-5", format = "%Y-%m-%d"),y=yl, size = c(0.8,0.8))
  
}

leg<-as.Date("2018-09-01", format = "%Y-%m-%d")
legend(x=leg, y=-0.03, legend =c("BOF","oBOF","oBOFDA","in situ"),
       horiz = TRUE, xpd =NA, lty=1,text.width = c(35,65,90,105),
       pch=c(NA,NA,21,NA,NA),pt.bg = c(NA,NA,"black",NA,NA),lwd=c(rep(0.5,3),1),
       xjust=0, x.intersp = 0.35, col = c(h1.bof,h1.col,h1.da,"black"), bty= "n",cex= 1, seg.len = 2)



mtext(side=1, "Time", outer=TRUE, line=1.8,cex =1.25,adj=0.37)
mtext(side=2, expression(paste("Root zone soil moisture (",theta["rz"], ", ",m^3*m^-3,")")), outer=TRUE, line=2.3, cex =1.25)
#mtext(side=3, paste0("Hydrus-1D"), outer=TRUE, line=0.25,cex =1.35, font= 3)

#dev.off()





