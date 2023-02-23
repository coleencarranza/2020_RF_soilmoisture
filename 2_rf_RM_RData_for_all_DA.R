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
type="fcast"
mod.tol<-"inflec"
        
#For some plot labels
if(type =="pred"){
  labs<-"Interpolation"
}else if (type == "fcast"){
  labs <- "Extrapolation"
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
        
        
#=============================1. read IN SITU DATA===========================================
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
#read R.data run from SLM processing PC----------
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
#if(type=="pred"){
rf_mod05<-mods_tune[[1]]
#}else{
# rf_mod05<-mods_tune[[2]]
#}
#pdf(paste("./figs/rf.mod.tol.fcast.pdf"), height=6,width=7)
#plot(rf_mod05)
#dev.off()
#=================================2.PLOT RF hyperparemeters and runtime results-============================================
#read runtime results
rf.time<-paste0("./1_RF/RF_",type,".runtime_rftune_mod05.RData")
load(rf.time)
colpts<-palette(colorRampPalette(brewer.pal(9,"Paired"))(10))[c(2,3,6,10)]
#setEPS()
#postscript(paste0("./3_figs/RFtune_",type,"4.eps"), height=7,width=8)# height=5,width=14
#pdf(paste0("./3_figs/RFtune_",type,"2.pdf"), height=5,width=11)
##PLOT - only for training =50%
#layout(matrix(c(1,2,3,
#                1,2,4),ncol=3,byrow=TRUE),widths = c(3,3,1.5))
par(oma=c(2,5,3,3))
layout(matrix(c(1,2,3,4),ncol=2,byrow=TRUE),heights = c(2,3))
#layout.show((4))
#1. PLot mtry vs RMSE for all node size
#par(mar=c(3,4,1,3), mgp=c(2.1,0.65,0), tck=-0.01xpd=NA)
par(mar=c(1,2,0,2), mgp=c(1.9,0.5,0), tck=-0.015,xpd=NA)
plot(rf_mod05$results$mtry,rf_mod05$results$RMSE, col=colpts[as.factor(rf_mod05$results$min.node.size)],bty="n",xlim = c(0,25),
     ylab=expression(paste("RMSE"," (",m^3*m^-3,")")),xlab="mtry",cex=1.5,pch=20,cex.lab=1.4,cex.axis=1.35,
     yaxt="n")
title(main=labs, line =1.5,outer=TRUE,cex.main=1.73)
y1<-round(range(rf_mod05$results$RMSE),3)[[1]]
y2<-round(range(rf_mod05$results$RMSE),3)[[2]]
axis(side=2, at=seq(y1,y2,length.out = 3),cex.axis=1.13)
#Highlight inflection point from mod.tol
x<-mod.tol$mtry
y<-rf_mod05$results[rf_mod05$results$mtry==x & rf_mod05$results$min.node.size==5,"RMSE"]
#plot run times for tradeoff and best
points(x,y,pch=19,col="red3",cex=1.5)
points(best_mod[[1]]$mtry,best_mod[[1]]$RMSE ,pch=19,col="blue3",cex=1.5)
legend("topright", legend=c(5,10,20,30),col = colpts,pch=20,horiz = TRUE,bty = "n",x.intersp = 0.35,text.width = 2,cex=1.25,
       title="min. node size")
#2. PLOT runtimes 
plot(rftune.runtime.list[[1]],xlim = c(0,25),col="grey50",pch=20,bty="n",cex.lab=1.4,cex.axis=1.35,
     cex=1.5,xlab="mtry",ylab="Runtime (sec.)",yaxt="n")
yr<-range(rftune.runtime.list[[1]])
y.at<-round(seq(yr[[1]],yr[[2]],length.out = 5),0)
axis(2, at=y.at,cex.axis=1.5)
#plot run times for tradeoff and best
be<-best_mod[[1]]$mtry
be.run<-as.numeric(rftune.runtime.list[[1]])[be]
y.rmse<-rftune.runtime.list[[1]][x]
points(x,y.rmse,col="red3", pch=19,cex=2)
points(be,be.run,pch=19,col="blue3",cex=2)

#3.plot best vs tradeoff model
par(mar=c(2,0,4,0),mgp=c(2.5,0.5,0),tck=-0.01,xpd=FALSE)
source("./0_Rscripts/4_accu_rf_best_tol.R")
#mtext(side=3, outer=TRUE, text=labs, cex =1.7, line=1.5,adj=0.5)
#dev.off()
#====================================3. importance var for mod.tol!=============================================
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

#=-==================================4.IMport in situ that used for DA with H1D=========================================
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
#=========================================5.Reading BOFEK===================================================
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
    
#========================================6.Reading H1D OPTIMIZED ONLY============================================
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
#==================================7.Reading H1D +DATA ASSIMILATION - Direct insertion========================================
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


#==============================8.Combine or Plot together Root zone soil moisture from data, RF, hydus===========================
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
  all[[i]]<-merge(data.zone[[i]],hyd.xts[[i]],vl.xts[[i]][,44:47],hyd.da.xts[[i]],hyd.bof.xts[[i]],DA.situ.xts[[i]],all=TRUE)
  colnames(all[[i]])[3:6]<-c("RF_mean","vl.Q025","vl.Q50","vl.Q975")
  all[[i]]$th_zone_bof<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone_bof)
  all[[i]]$th_zone<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone)
  all[[i]]$th_zone_DA<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone_DA)
  all[[i]]$DA<-all[[i]]$DA*all[[i]]$VWC_zone
  #all[[i]]<-all[[i]][complete.cases(all[[i]]$VWC_zone),]
}
all<-setNames(all,names(all.list))

#subset until 2018 only
all<-lapply(all, function(x) x[which(year(x) %in% c(2016,2017,2018)),])



#==========================9.Accuracy metrics per station==============================================
#load accu metric function
source("./0_Rscripts/0_fcn_accu.R")

##get stations with min-max RF accuracy
all.comp<-lapply(all, function(x) x[complete.cases(x[,c(1,2,3:7)]),c(1,2,3:7)])
h1dda.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone_DA))
h1d.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone))
rf.acu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$RF_mean))

##PRINT range accuracy for table
accu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$th_zone_DA))
accu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$RF_mean))
accu<-lapply(accu, unlist)
accu<-do.call(rbind,accu)
#Range RMSE
range(accu[,1])
#Range Biac
range(accu[,3])
#Range UnbiasedRMSE
range(accu[,4])
#Range R2
range(accu[,2])
rm(accu)


#Select stations min and max accuracy
accu<-lapply(all.comp, function(x) accu.met(o=x$VWC_zone,m=x$RF_mean))
accu.rmse<-unlist(lapply(accu, function(x) x[[1]]))
raam.min.rmse<-all[which.min(accu.rmse)]
raam.max.rmse<-all[which.max(accu.rmse)]
min.max<-c(raam.min.rmse,raam.max.rmse)

#STations with min.max from RF results
rf.min.max<-rf.acu[c(which.min(accu.rmse),which.max(accu.rmse))]
h1dda.min.max<-h1dda.acu[c(which.min(accu.rmse),which.max(accu.rmse))]


#=====================================10.PLOT accuracy per train prop and run times===============================
#get RMSEs of best hyperpar for eac htrprop
best_mod<-lapply(mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)[c(1,3,4,5)] #select cols needed
best_mod.df
best_mod.df$prop<-seq(0.5,0.8,by=0.1)

#read runtimes results for each trprop--------
run.trprop.out<-as.numeric(read.csv(paste0("./1_RF/RF_",type,".runtime_trprop.csv"))[-1])

#combine and convert to mintues, sround 2 decimals
best_mod.df$runtime.mins<-round(run.trprop.out/60,2)
#add samples
best_mod.df$tr.set<-sapply(mods_tune, function(x) nrow(x$trainingData))

#plot
ylim.range<-function(x,seq.len,round.deci){
  y.rm<-range(x)
  y.rm1<-floor(y.rm[[1]]*10000)/10000
  y.rm2<-ceiling(y.rm[[2]]*10000)/10000
  se<-seq(y.rm1,y.rm2,length.out=seq.len)
  rnd<-round(se,round.deci)
  return(rnd)
}

#dev.off()
colP<-colorRampPalette(brewer.pal(8, "Dark2"))(8)[c(1,3,4,6)]
plt.cols<-c("RMSE","Rsquared","runtime.mins","tr.set")
#pdf(paste("./3_figs/qrf_",type,"trprop_accu.pdf"), height=3,width=6)
par(mar=c(3.15,7.5,1.5,5),mgp=c(1.3,0.35,0),tck=-0.01,pch=19,cex.axis=0.85,xpd=NA)

plot(best_mod.df$prop,best_mod.df[,plt.cols[1]],type="o",col=colP[1],bty="n",yaxt="n",ylab="",xaxt="n",
     xlab="Training set proportion",main=labs)
box(col = "grey38", lwd = 0.5)
axis(side=1,at=best_mod.df$prop)
a<-ylim.range(best_mod.df[,plt.cols[1]],seq.len = 4,round.deci = 4)
axis(side = 2, las=2,at=a,labels=a)
mtext(side=2,expression(paste("RMSE (",m^3*m^-3,")")),line=2.57,cex=0.9)

#legend
legend("bottomright",legend=c("RMSE",expression(paste(R^2)),"Run times","Tr. set size"),bty="n",pch=19,
       y.intersp =0.81,
       col=colP,ncol=2,inset=c(-0.28,-0.32),cex=0.75)

par(new=TRUE,xaxt="n",bty="n")
plot(best_mod.df$prop,best_mod.df[,plt.cols[2]],type="o",col=colP[2],yaxt="n",ylab="",xlab="")
a<-ylim.range(best_mod.df[,plt.cols[2]],seq.len = 4,round.deci = 4)
axis(side = 2, las=2,at=a,labels=a,line=4.15)
mtext(side=2,expression(paste(R^2)),line=6.25,cex=0.9)

par(new=TRUE)
plot(best_mod.df$prop,best_mod.df[,plt.cols[3]],type="o",col=colP[3],yaxt="n",ylab="",xlab="")
a<-ylim.range(best_mod.df[,plt.cols[3]],seq.len = 4,round.deci =0)
axis(side = 4,at=a,labels=a, las=2)
mtext(side=4,"Run times (mins.)",line=1.15,cex=0.9)

par(new=TRUE)
plot(best_mod.df$prop,best_mod.df[,plt.cols[4]],type="o",col=colP[4],yaxt="n",ylab="",xlab="")

if(type=="pred"){
  rm.sub<-signif(best_mod.df[,plt.cols[4]],1)
}else{
  rm.sub<-best_mod.df[,plt.cols[4]]
}
a<-ylim.range(rm.sub,seq.len = 4,round.deci =0)
axis(side = 4, at=a,labels=round(a,-3)/1000,las=2,line=2.8)
mtext(side=4,"Training set size (x1000)",line=3.8,cex=0.9)

#dev.off()


#===========================================11.Box plots meteo var with RF RMSE===================================
tr.list<-lapply(samps[[1]], function(x) x[[1]])
box.list<-as.list(c("rd","FG","Q","SQ","TN","TX","UG","EV24","LAI"))
rf.rmse<-unlist(lapply(rf.acu, function(x) x[[1]]))

#pdf(paste("./3_figs/boxplots_meteovars.pdf"), height=4,width=4.5)
par(mfrow=c(9,1),mar=c(0,2,0,2),oma=c(2,0.5,1,0.5),mgp=c(2,0.35,0),tck=-0.043,cex.axis=0.75,cex=0.5)
  for(i in seq_along(box.list)){
    varn<-box.list[[i]]
    x<-tr.list
    y<-lapply(x, function(z) z[,varn])
    if(varn=="rd"){
      y<-lapply(y, function(z) {z[z==0]<-NA;z})
    }
    cb<-do.call(qpcR:::cbind.na,y)
    if(i==1){
      ymx<-quantile(cb,probs=0.975,na.rm = TRUE)
      ymn<-min(cb,na.rm = TRUE)
    }else if(i ==9){
      ymx<-quantile(cb,probs=0.997,na.rm = TRUE)
      ymn<-min(cb,na.rm = TRUE)
    }else{
      ymx<-max(cb,na.rm = TRUE)*1.25
      ymn<-min(cb,na.rm = TRUE)
    }
    boxplot(cb,xaxt="n",ylim=c(ymn,ymx),cex=0.5,pch=19,frame.plot=FALSE,boxwex=0.65,yaxt="n",medcol="dodgerblue4",xlim=c(1,15),boxcol="grey30",
            col="lightgoldenrodyellow",outcex=0.15,
            medlwd=1.2,boxlwd=0.3,whisklwd=0.3,staplelwd=0.3)
    box(lwd=0.2,col="grey80")
    ax.seq<-round(seq(ymn,ymx, length.out = 3),0)
    if(i%%2 ==1){#even
      axis(side=4,las=2,at=ax.seq,lwd=0.3)
      mtext(side=4,box.list[[i]], line=1.25,cex=0.5)
    }else{
      axis(side=2,las=2,at=ax.seq,lwd=0.3)
      mtext(side=2,box.list[[i]],  line=1.25,cex=0.5)
    }
    
  }
axis(side = 1,at=1:15,labels=colnames(cb),lwd=0.3)
#plot(rf.rmse, type="o", pch=20,col="blue",bty="n")
#box(lwd=0.25)
#dev.off()



#=================================12.MIN MAX PLOTS===================================================
min.max<-lapply(min.max, function(x) {x$th_zone_DA<-ifelse(is.na(x$th_zone_DA)==TRUE,x$VWC_zone,x$th_zone_DA);x})

#inset plot function
ins.plot<-function(x,col,mod.var){
  mn<-round(min(x),2)
  mx<-round(max(x),2)
  plot(x,cex=0.2, col=alpha(col,0.31), bty = 'n', yaxt = 'n',xaxt= "n",cex.lab = 0.7,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = mod.var)
  box(lwd=0.15,col = "grey50")
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.58, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.58, lwd=0.15,)
  abline(0,1, lty =4)
}
library(scales)
library(TeachingDemos)
#colors
co.rf<-"royalblue1"
co.hyd<-"tomato3"
dev.off()

pdf(paste("./3_figs/qrf_",type,"_ts_min_max.tol_DA2.pdf"), height=5,width=9)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2019-11-21", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")
par(oma = c(3,4,2,0.5))
layout(matrix(c(1:2),ncol=1, byrow =TRUE)) 

#layout.show(6)
for(i in seq_along(min.max)){
  x<-min.max[[i]]
  y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.05
  y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.05
  ##TIME SERIES
  par(mar=c(0.3,0,0,0.3), mgp = c(1.5,0.5,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone),ylim=c(y.lo,y.hi), type = "l",lwd=0.01, xlim=c(st,fi), xaxt= "n",yaxt="n", bty="n")
  box(col="grey68", lwd=0.15)
  # Now set the plot region to grey
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("wheat3",0.15))
  #box(col = "grey38", lwd=0.25)
  text(x=st,y=y.lo+0.01, names(min.max[i]), cex =1.28)
  axis(side=2, at=seq(y.lo,y.hi,by=0.15), las=2,cex.axis=1, lwd=0.15)
  if(i %in% 2)
    axis(side=1,at=yr.seq[-5],labels = year(yr.seq[-5]),cex.axis=1, lwd=0.15)
  
  # Generating the lower and upper uncertainty bounds
  par(new=TRUE)
  bands<-x[complete.cases(x$vl.Q025),c(4,6)]
  lband <- as.zoo(bands$vl.Q025)
  uband <-as.zoo(bands$vl.Q975)
  hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("slategray1",0.3), add=TRUE)
  wd<-c("wheat2","firebrick3")
  lines(as.zoo(x$RF_mean),type = "b", pch=3,col=co.rf,cex =0.75, lwd=0.45) #RF valid/forecast
  #plot also training  points#
  #points(as.zoo(x$tr.Q50),pch=3,col=alpha("lightblue",0.35), cex = 0.15) #RF train
  
  #Situ and Hydrus
  lines(as.zoo(x$VWC_zone),lwd=1.5,col = "grey20")
  lines(as.zoo(x$th_zone_DA),lty=5,cex=0.25,col =co.hyd,lwd= 0.75)
  
  ##Accuracy text 
  legend('bottomright', ncol = 3L, x.intersp = 0.15,xpd=NA, inset = c(0.031,0.005),cex =0.75,y.intersp = 0.9,
         bg=alpha("white",0.5),box.col=alpha("white",0.3),xjust = 0.5,text.width = c(rep(120,5),rep(110,5),rep(90,5)),
         legend = c('', 'RMSE', 'Bias', 'Unb. RMSE', expression(R^2),
                    'RF', rf.min.max[[i]][[1]], rf.min.max[[i]][[3]], rf.min.max[[i]][[4]], round(rf.min.max[[i]][[2]],3),
                    'oBOFDA', h1dda.min.max[[i]][[1]],h1dda.min.max[[i]][[3]],h1dda.min.max[[i]][[4]],round(h1dda.min.max[[i]][[2]],3)))
  #in situ vs RF
  ins<-data.frame(x[complete.cases(x[,c("VWC_zone","RF_mean")]),c(1,3)])
  yl<-sum(y.hi,y.lo)/2 + (y.hi-y.lo)*0.3
  par(mgp = c(0.7,0.1,0), tck = -0.025)
  subplot(ins.plot(ins[,c(1,2)],col=co.rf,"RF."),  x=as.Date("2019-04-20", format = "%Y-%m-%d"),y=yl, size = c(0.7,0.7))
  
  #in situ vs RF
  ins<-data.frame(x[complete.cases(x[,c("VWC_zone","RF_mean")]),c(1,7)])
  subplot(ins.plot(ins[,c(1,2)],col=co.hyd,"oBOFDA"),  x=as.Date("2019-11-01", format = "%Y-%m-%d"),y=yl, size = c(0.7,0.7))
  
}

leg<-as.Date("2018-6-10", format = "%Y-%m-%d")
legend("bottomright", legend =c("Training RF","Validation RF", "oBFDA","in situ"),ncol =2, 
       inset=c(-0.05,-0.35),xpd=NA,lty = c(NA,NA,2,1),
       pch=c(19,3,NA,NA), col = c(alpha("azure4",0.75),co.rf,co.hyd,"black"), 
       bty= "n",cex= 0.7, seg.len = 2,text.width = 200,
       x.intersp = 0.31)

mtext(side=3, labs, outer=TRUE, line=0,cex =1.15)
mtext(side=1, "Time", outer=TRUE, line=1.7,cex =1.15)
mtext(side=2, expression("Root zone soil moisture ("~theta["rz"]~")"), outer=TRUE, line =2.5, cex =1)
dev.off()

        
        
    
#========================================13.SCATTER PLOT AND RESID all stations====================================================
all.df<-lapply(all.comp, function(x)  x[complete.cases(x),c("VWC_zone","RF_mean","th_zone_DA")]) #vl.Q50 th_zone
all.df<-data.frame(do.call("rbind",all.df))
        
#Resid - form 1:1 lines (x-y)
#function residual
obs<-all.df$VWC_zone
mod<-all.df[,2]
dif<-mod-obs
Out = boxplot(dif)$out
out_dif<-dif[which(dif %in% Out)]
out_obs<-obs[which(dif %in% Out)]
##Accuracy metrics
accu.all<-accu.met(obs,mod)


#==========================14.Residuals BOXplots for <=0.10, and >=0.35-- 2.5th and 97.5th percentile========================================
all.zone<-lapply(all.list, function(x) x$VWC_zone)
all.zone<-unlist(all.zone)
all.zone<-all.zone[complete.cases(all.zone)]
qs<-quantile(all.df$VWC_zone,c(0.025,0.975))
q5<-round(qs[1],2)
q95<-round(qs[2],2)


box.extr<-function(x){
  if(x=="dry"){
    vwc<-all.df[all.df$VWC_zone<=q5,1]
    sims<-all.df[all.df$VWC_zone<=q5,2:3]
    sym<-"\u2264"
    lab<-bquote(theta["rz"]~.(sym)~.(q5))
  }else if(x=="wet"){
    vwc<-all.df[all.df$VWC_zone>=q95,1]
    sims<-all.df[all.df$VWC_zone>=q95,2:3]
    sym<-"\u2265"
    lab<-bquote(theta["rz"]~.(sym)~.(q95))
  }else{
    vwc<-all.df[,1]
    sims<-all.df[,2:3]
    lab<-"All points"
  }
  #resid
  difs<-sims-vwc
  boxplot(difs, pch=20,frame=FALSE,cex = 1.3,outcex=0.21,
          col = c("springgreen4","steelblue"),
          yaxt="n",ylim=c(-0.2,0.2),
          names = c("RF","H1D"),boxwex=0.7)
  #number of points
  text(x=0.8,y=0.185,paste("n = ",nrow(difs),sep=""),cex=1.54)
  title(sub=lab, cex.sub=1.83,line=3)
  axis(side=2, las=2)
  box(col="grey60", lwd=0.15)
}

        
#setEPS()
#postscript(paste("./3_figs/scat_resid_",type,"_DA.tol_sm2.eps"), height=5,width=17)###sm=small
#png(paste0("./3_figs/scat_resid_",type,"_DA.tol_sm.png"), height=6,width=8,res=600,units ="in")###sm=small
#Resid - form 1:1 lines (x-y)
##new plot resid:
par(oma = c(3,3,2,0))
layout(matrix(c(1,2,0,5,6,7,
             3,4,0,5,6,7), ncol=6,nrow=2,byrow = TRUE),widths = c(1,1,0.05,1,1,1))
par(mar = c(2,4,0,4),  mgp = c(2,0.7,0),tck = -0.017,xpd=FALSE,
      cex.axis=1.5)
#function residual
#Figure labesl letters
if(type== "pred"){
  lets<- letters[c(1,3,5,7,9)]
}else{
  lets<- letters[c(2,4,6,8,10)]
}

obs<-all.df$VWC_zone
dif.range<-list()
for(i in 2:3){
  mod<-all.df[,i]
  dif<-mod-obs
  dif.range[[i-1]]<-range(dif)
  #Out = boxplot(dif)$out
  out_dif<-dif[which(dif %in% Out)]
  out_obs<-obs[which(dif %in% Out)]
  ##Accuracy metrics
  accu.all<-accu.met(obs,mod)
  cex.txt=1.3
  if(i==2){
    col="springgreen4"
  }else{
    col="steelblue"
  }
#scatterplot
plot(obs,mod, xaxt="n", yaxt="n", ylab="",pch=19, cex=0.04, col= col,bty="n",cex.lab=1.2,
      xlab="",xlim =c(0,0.45),ylim=c(0,0.45))
box(col = "grey50", lwd= 0.1)
# Now set the plot region to grey
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.07),border = NA)
axis(side=2,at=seq(0,0.45, by=0.15),las=2,lwd=0.15)
if(i==3){
  axis(side=1,at=seq(0,0.45, by=0.15),lwd=0.15)
}
abline(0,1, lty= 2, lwd= 1)
##Accuracy text 
y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.05
y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.05
txt.acc = 1.3
y.txt<-(y.hi-y.lo)/7
x.txt<-0.325
text(x = x.txt, y= y.lo +y.txt*4, bquote(RMSE == .(accu.all[[1]])),cex =txt.acc) #-17 or-20
text(x = x.txt, y= y.lo +y.txt*3, bquote(Bias == .(accu.all[[3]])),cex = txt.acc)
text(x = x.txt, y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.all[[4]])),cex=txt.acc-0.1)
text(x = x.txt, y=y.lo + y.txt, bquote(R^2 == .(round(accu.all[[2]],3))),cex =txt.acc)#resid

plot(obs,dif, ylim=c(-0.2,0.15),yaxt="n",bty="n",ylab="",xlab="",pch=19, cex=0.04, col=col,
     xlim=c(0,0.45), xaxt="n")
box(col = "grey50", lwd= 0.1)
# Now set the plot region to grey
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.02),border = NA)
if(i==3){
  axis(side=1,at=seq(0,0.45, by=0.15),lwd= 0.15)
}
axis(side=2,at=seq(-0.2,0.15, by=0.15),las=2, lwd= 0.15)
abline(h=0, lty= 2, lwd= 1)
}
#mtext(side=1, outer=TRUE, expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")), cex = 1.2, line=1.7, adj = 0.85)
txt.cex<-1.3
mtext(side=1, outer=TRUE, expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")), cex = txt.cex, line=1.7, adj = 0.19)
mtext(side=2, outer=TRUE, expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")), cex = txt.cex, line=0, adj = 0.85)
mtext(side=2, outer=TRUE, expression(paste("H1D ", theta["rz"], " (",m^3*m^-3,")")), cex = txt.cex, line=0, adj=0.15)
mtext(side=2, outer=TRUE, expression(paste("Residuals", " (",m^3*m^-3,")")), cex = txt.cex, line=-25)#-28.7
#mtext(side =3, labs, outer=TRUE, line =0.8,cex = txt.acc)#Hydrus-1D
#dev.off()

#P=BOXPLOTS
#cairo_pdf(file=paste("./3_figs/box_resid_extr_",type,"_3cols_DA_95.pdf"), height=4,width=10)
par(mar = c(1.7,2.5,0,1),mgp = c(2,0.7,0), tck = -0.02,cex.axis=1.5,xpd=NA)
box.extr("all")
box.extr("dry")
box.extr("wet")
mtext(side=2, expression(paste("Residuals"," (",m^3*m^-3,")")), cex = 1.3, line=-50,outer=TRUE)
#dev.off()
  



#================15.PLOTS comparing Hydrus-1D data assimilation and inverse modleing reuslts============#
all<-lapply(all, function(x) {x$th_zone_DA<-ifelse(is.na(x$th_zone_DA)==TRUE,x$VWC_zone,x$th_zone_DA);x})
#Select stations min and max accuracy
accu.rmse<-unlist(lapply(h1d.acu, function(x) x[[1]]))
raam.min.rmse<-all[which.min(accu.rmse)]
raam.max.rmse<-all[which.max(accu.rmse)]
min.max<-c(raam.min.rmse,raam.max.rmse)

#STations with min.max from RF results
h1d.min.max<-h1d.acu[c(which.min(accu.rmse),which.max(accu.rmse))]
h1dda.min.max<-h1dda.acu[c(which.min(accu.rmse),which.max(accu.rmse))]


#------------------------15.1 MIN MAXP PLOTS-------------------------
#inset plot function
ins.plot<-function(x,col,mod.var){
  mn<-round(min(x),2)
  mx<-round(max(x),2)
  plot(x,cex=0.2, col=alpha(col,0.31), bty = 'n', yaxt = 'n',xaxt= "n",cex.lab = 0.7,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = mod.var)
  box(lwd=0.15,col = "grey50")
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.58, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.58, lwd=0.15,)
  abline(0,1, lty =4)
}

#pdf(paste0("./3_figs/ts_minmax_H1D_DA",samp,".pdf"), height=5,width=9)# height=7.5,width=15.5)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2020-02-21", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")[1:4]

h1.da<-"chartreuse4"
h1.col<-"indianred"

h1.col<-"mediumseagreen"
h1.da<-"tomato3"

par(oma = c(3,4.25,1,0.5))
layout(matrix(c(1:2),ncol=1, byrow =TRUE)) 
for(i in seq_along(min.max)){
  x<-min.max[[i]]
  y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.01
  y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.01
  ##TIME SERIES
  par(mar=c(0.3,0,0,0.3), mgp = c(1.5,0.5,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone), ylim=c(y.lo,y.hi), type = "l",lwd=0.5,lty=1,xlim=c(st,fi), xaxt= "n",yaxt="n", bty="n",col=alpha("black",1))
  #Hydrus
  #lines(as.zoo(x$th_zone_bof),cex=0.25,col="grey60",lwd= 0.7)
  lines(as.zoo(x$th_zone),cex=0.25,col=h1.col,lwd= 0.5)
  lines(as.zoo(x$th_zone_DA),cex=0.25,col =h1.da,lwd= 0.75)
  points(as.zoo(x$DA),cex=0.6,bg="black",pch=21,col=h1.da,lwd=0.01)
  
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("lightgrey",0.05),border = NA)
  
  st.nm<- "2016-01-25"
  text(x=as.Date(st.nm, format = "%Y-%m-%d"),y=y.lo+0.01, names(min.max[i]), cex =1.28, font=2)
  if(i %in% 2){
    axis(side=1,at=yr.seq,labels = year(yr.seq),cex.axis=1, lwd=0.25)
  }
  ax.lab<-round(seq(y.lo,y.hi,length.out = 5),2)
  axis(side=2, at=ax.lab, labels = c(0.0,"",ax.lab[3],"",ax.lab[5]), las=2,cex.axis=1, lwd=0.25)
  
  
  ##Accuracy text 
  legend('bottomright', ncol = 3L, x.intersp = 0.15,xpd=NA, inset = c(0.031,0.008),cex =0.79,y.intersp = 0.85,
         bg=alpha("white",0.1),box.col=alpha("white",0.1),xjust = 0.5,text.width = c(rep(140,5),rep(130,5),rep(80,5)),
         legend = c('', 'RMSE', 'Bias', 'Unb. RMSE', expression(R^2),
                    'H1D', h1d.min.max[[i]][[1]], h1d.min.max[[i]][[3]], h1d.min.max[[i]][[4]], round(h1d.min.max[[i]][[2]],3),
                    'H1D+DA', h1dda.min.max[[i]][[1]],h1dda.min.max[[i]][[3]],h1dda.min.max[[i]][[4]],round(h1dda.min.max[[i]][[2]],3)))
  
  ##SUBPLOT INSET
  #in situ vs RF
  ins<-data.frame(all.comp[[i]])
  yl<-sum(y.hi,y.lo)/2 + (y.hi-y.lo)*0.27
  par(mgp = c(0.88,0.12,0), tck = -0.025)
  subplot(ins.plot(ins[,c(1,2)],col=h1.col,"H1D"),  x=as.Date("2019-05-30", format = "%Y-%m-%d"),y=yl, size = c(0.6,0.6))
  
  #in situ vs H1D
  subplot(ins.plot(ins[,c(1,3)],col=h1.da,"H1D+DA"),  x=as.Date("2020-01-20", format = "%Y-%m-%d"),y=yl, size = c(0.6,0.6))
  
}

leg<-as.Date("2018-11-01", format = "%Y-%m-%d")
legend(x=leg, y=-0.05, legend =c("H1D","H1D + DA","in situ"), horiz = TRUE, xpd =NA, lty=1,text.width = c(50,80,110),
       pch=c(NA,21,NA,NA),pt.bg = c(NA,"black",NA,NA),
       xjust=0, x.intersp = 0.35, col = c(h1.col,h1.da,"black"), bty= "n",cex= 0.8, seg.len = 2.5)



mtext(side=1, "Time", outer=TRUE, line=1.8,cex =1.25)
mtext(side=2, expression(paste("Root zone soil moisture (",theta["rz"], ", ",m^3*m^-3,")")), outer=TRUE, line=2.3, cex =1.25)
#mtext(side=3, paste0("Hydrus-1D"), outer=TRUE, line=0.25,cex =1.35, font= 3)

#dev.off()




