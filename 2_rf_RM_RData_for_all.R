##---------------------------------PREDICTION AND FORECAST RESULTS------------------------------------
library(randomForest)
library(tsModel)
library(lubridate)
library(dplyr)
library(xts)
library(scales)
library(Metrics)
library(corrplot)
library(RColorBrewer)

##analysis type:
type="fcast"
mod.tol<-"tol"

#For some plot labels
if(type =="pred"){
  labs<-"Prediction"
}else if (type == "fcast"){
  labs <- "Forecasting"
}

#######################  0. read and prep files  ########################################
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

#read RData from 4_tune_caret_for_all.R
if(mod.tol== "infl"){
  rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25_inflec.RData")
}else{
  rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25.RData")
}

##fc= pf2, pwp=pf4.2 bofek average over sites
fc<-0.388
pwp<-0.0

###Plotting distribution of root zone soil moisture and SSM-----#1
#RZSM
RZSM<-sapply(all.list, function(x) x$VWC_zone)

#pdf(paste("./3_figs/box_rzsm.pdf"), height=4,width=12)
par(mar=c(3,3,1,1),mgp=c(2,0.47,0),tck=-0.016)
boxplot(RZSM,boxwex=0.7, ylim=c(0,0.5),pch=19,cex=0.5,frame=FALSE,col=alpha("dodgerblue2",0.3),yaxt="n")
box(lwd=0.15)
axis(side=2,las=2)

#vertical lines fc and pwp
abline(h=fc, lty=2,col="grey80")
abline(h=pwp, lty=2,col="grey80")

#text
text(0.13,0.405, bquote(bar(theta)[s]),cex=1,col="grey30")
text(0.13,0.015, bquote(bar(theta)[r]),cex=1,col="grey30")
mtext(side=2, expression(paste("Root zone soil moisture", " (",theta["rz"],",",~m^3*m^-3,")")),cex=1, line=1.6)
mtext(side=1, "Station",cex=1, line=1.8)

# dev.off()

###-------------Plotting distribution of root zone soil moisture and SSM-----------------------#2
#RZSM
RZSM<-sapply(all.list, function(x) x$VWC_zone)
RZSM<-as.vector(RZSM)
RZSM<-RZSM[!is.na(RZSM)]

#SSM
SSM<-sapply(all.list, function(x) x$VWC5)
SSM<-as.vector(SSM)
SSM<-SSM[!is.na(SSM)]


#
SSM_RZSM<-data.frame(SSM,RZSM)
SSM_RZSM<-SSM_RZSM[SSM_RZSM>=0,]
#SSM_RZSM<-SSM_RZSM[complete.cases(SSM_RZSM),]
SSM_RZSM<-round(SSM_RZSM,2)


##-------------- density plots
#pdf(paste("./3_figs/theta_dist.pdf"), height=6,width=6)
par(mar=c(3,3,2,1),oma=c(1.5,1,0,0),cex.axis=1.5)
## calculate the density - don't plot yet
densSSM <- density(SSM,bw=0.01)
densRZSM <- density(RZSM,bw=0.01)
## calculate the range of the graph
xlim <- c(0,max(densRZSM$x,densSSM$x))
ylim <- range(-0.5,densRZSM$y, densSSM$y)
#pick the colours
SSMCol <- alpha("dodgerblue2",0.25) #rgb(1,0,0,0.2)
RZSMCol <- alpha("royalblue4",0.75) #rgb(0,0,1,0.2)
## plot the SSMs and set up most of the plot parameters
plot(densSSM, xlim = xlim, ylim = ylim, xlab = 'Lengths',
     main = 'Distribution of SSM and RZSM', yaxt="n",
     panel.first = grid())
# Add the data-poins with noise in the X-axis
rug(jitter(SSM),col=alpha("dodgerblue2",0.25),lwd=0.1,ticksize = 0.03)
rug(jitter(RZSM), line=-0.9,col=RZSMCol, ticksize = 0.03)
axis(side=2,las=2)
#put our density plots in
polygon(densRZSM, density = -1, col = RZSMCol)
polygon(densSSM, density = -1, col = SSMCol)
#vertical lines fc and pwp
abline(v=fc, lty=2)
abline(v=pwp, lty=2)

#text
text(0.41,5.5, bquote(theta[s]))
text(0.015,5.5, bquote(theta[r]))

## add a legend in the corner
legend('topright',c('SSM','RZSM'),
       fill = c(SSMCol, RZSMCol), bty = 'n',
       border =NA)
mtext(side=2, "Density",cex=1.5, line=2.5)
mtext(side=1, expression(paste("Soil moisture", " (",m^3*m^-3,")")),cex=1.5, line=2.75)


#dev.off()


ggplot(iris, aes(x = Sepal.Length)) +
  geom_density(aes(color = Species))


##oad rf .Rdata################
load(rf_out)

#GEt RMSEs for each run
fin_mods<-lapply(mods_tune,function(x) x$results)
best_mod<-lapply(mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)
best_mod.df#show RMSEs per training sample proportion


#use model using 0.5 training tr.all[[1]]
rf_mod05<-mods_tune[[1]]
#pdf(paste("./figs/rf.mod.tol.fcast.pdf"), height=6,width=7)
plot(rf_mod05)
#dev.off()

###############################  1.RF VARIABLE IMPORTANCE   #######################################
#PLot variables see collinearity - use results from train sample 50%
tr.all<-lapply(samps[[1]], function(x) x[[1]][,-c(1,3)])#remove date and VWC_zopne cols
tr.all<-do.call("rbind",tr.all)
tr.all<-tr.all[complete.cases(tr.all),]

#COrrelation
corr.tr<-cor(tr.all)
corr.tr[is.na(corr.tr)] <- 0
#rearrange  columns
orde<-colnames(corr.tr)[corrMatOrder(corr.tr,order = "FPC")]

#col func
nam.cols<-function(x){
  if(grepl("UG|TN|TX|rd|FG|Q|SQ|EV",x)==TRUE){
    col<-"skyblue3"
  }else if(grepl("Crop|LAI",x)==TRUE){
    col <- "darkolivegreen4"
  }else if(grepl("BOF",x)==TRUE){
    col<- "chocolate3"
  }else if(grepl("VWC|DOY",x)==TRUE){
    col <- "midnightblue"
  }
  return(col)
}

#colors
cols.orde<-sapply(orde,nam.cols)

#CORRPLOT!
#pdf(paste("./figs/corrplot_",type,".pdf"), height=7,width=8)
par(oma = c(1,1,1,1))
corrplot(corr.tr, type="upper",order = "FPC",tl.col = cols.orde,tl.cex = 0.8)
#dev.off()


#Varimp ranger
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

  


###############################  2. Reading H1D output --read B for longer simulation  ######################
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
hyd.dates<-files
hyd.to.dates<-function(x){
  x<-read.csv(x, header=TRUE, stringsAsFactors = FALSE)[,1]
  x<-as.Date(x, format = "%Y-%m-%d")
  return(x)
}
hyd.dates<-lapply(hyd.dates, hyd.to.dates)[[1]]
hyd.dates<-hyd.dates[1:nrow(hyd.df[[1]])]
hyd.xts<-lapply(hyd.df, function(x) xts(x, hyd.dates)) 




########################  3. Combine measurements, rf, and h1d values in one xts  ###########################
#
data.zone<-lapply(all.list, function(x) xts(x[,"VWC_zone"],x[,1]))
data.zone<-lapply(data.zone, function(x) {colnames(x)<-"VWC_zone";x})
hyd<-lapply(hyd.xts, function(x) x[,"th_zone"])
all<-list()
for(i in seq_along(data.zone)){
  all[[i]]<-merge(hyd[[i]],data.zone[[i]],vl.xts[[i]][,44:47],all=TRUE)
  all[[i]]$th_zone<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone)
  colnames(all[[i]])[3:6]<-c("RF_mean","vl.Q05","vl.Q50","vl.Q95")
  #all[[i]]<-all[[i]][complete.cases(all[[i]]$VWC_zone),]
}
all<-setNames(all,names(all.list))

#subset until 2018 only
all<-lapply(all, function(x) x[which(year(x) %in% c(2016,2017,2018)),])


######################   4. Accuracy metrics  #############################################
#load accu metric function
source("./0_Rscripts/0_fcn_accu.R")


##Hydrus-1D--------------
accu<-lapply(all, function(x) x[complete.cases(x),])
accu<-lapply(accu, function(x) accu.met(o=x$VWC_zone,m=x$th_zone))
accu.rmse<-unlist(lapply(accu, function(x) x[[1]]))
#print
round(range(unlist(lapply(accu, function(x) x[[1]]))),3)
round(range(unlist(lapply(accu, function(x) x[[3]]))),3)
round(range(unlist(lapply(accu, function(x) x[[4]]))),3)
round(range(or<-unlist(lapply(accu, function(x) x[[2]]))),3)



#RF---
rm(accu)
accu<-lapply(all, function(x) x[complete.cases(x),])
accu<-lapply(accu, function(x) accu.met(o=x$VWC_zone,m=x$RF_mean))
accu.rmse<-unlist(lapply(accu, function(x) x[[1]]))
#print
round(range(unlist(lapply(accu, function(x) x[[1]]))),3)
round(range(unlist(lapply(accu, function(x) x[[3]]))),3)
round(range(unlist(lapply(accu, function(x) x[[4]]))),3)
round(range(or<-unlist(lapply(accu, function(x) x[[2]]))),3)


#Select stations min and max accuracy
raam.min.rmse<-all[which.min(accu.rmse)]
raam.max.rmse<-all[which.max(accu.rmse)]
min.max<-c(raam.min.rmse,raam.max.rmse)
accu.min.max<-accu[c(which.min(accu.rmse),which.max(accu.rmse))]


##########--------------------------------PLOT min max stations-----------------------------
#pdf(paste("./3_figs/qrf_",type,"_ts_min_max.tol2.pdf"), height=4.5,width=9)
st<-as.Date("2016-01-01", format = "%Y-%m-%d")
fi<-as.Date("2019-11-21", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")
library(scales)
par(oma = c(4,5,2,0.5))
layout(matrix(c(1,1,1,2,3,
                1,1,1,1,1,
                4,4,4,5,6,
                4,4,4,4,4),ncol=5, byrow =TRUE), widths = c(2,2,2,1,0.85))
#layout.show(6)

for(i in seq_along(min.max)){
  x<-min.max[[i]]
  y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.05
  y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.05
  ##TIME SERIES
  par(mar=c(0,0,0,0), mgp = c(1.5,0.5,0), tck = -0.025)
  plot(as.zoo(x$VWC_zone),ylim=c(y.lo,y.hi), type = "l",lwd=0.01, xlim=c(st,fi), xaxt= "n",yaxt="n", bty="n")
  box(col="grey68", lwd=0.15)
  # Now set the plot region to grey
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("wheat3",0.15))
  #box(col = "grey38", lwd=0.25)
  text(x=st,y=y.lo+0.01, names(min.max[i]), cex =1.28)
  axis(side=2, at=seq(y.lo,y.hi,by=0.15), las=2,cex.axis=1.2, lwd=0.15)
  if(i %in% 2)
    axis(side=1,at=yr.seq[-5],labels = year(yr.seq[-5]),cex.axis=1.3, lwd=0.15)
  
  # Generating the lower and upper uncertainty bounds
  par(new=TRUE)
  bands<-x[complete.cases(x$vl.Q05),c(4,6)]
  lband <- as.zoo(bands$vl.Q05)
  uband <-as.zoo(bands$vl.Q95)
  hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("slategray1",0.2), add=TRUE)
  wd<-c("wheat2","firebrick3")
  lines(as.zoo(x$RF_mean),type = "b", pch=3,col="royalblue",cex = 0.8, lwd=0.37) #RF valid/forecast
  #plot also training  points#
  #points(as.zoo(x$tr.Q50),pch=3,col=alpha("lightblue",0.35), cex = 0.15) #RF train
  
  #Hydrus
  lines(as.zoo(x$VWC_zone),lwd=0.5,col = "grey20")
  lines(as.zoo(x$th_zone),lty=4,cex=0.25,col ="firebrick4",lwd= 1.2)
  
  if(i ==2){
    leg<-as.Date("2018-6-10", format = "%Y-%m-%d")
    legend("bottomright", legend =c("Training RF","Validation RF", "Hydrus 1D","in situ"),ncol =2, 
           inset=c(-0.05,-0.31),xpd=NA,lty = c(NA,NA,2,1),
           pch=c(19,3,NA,NA), col = c(alpha("azure4",0.75),"blue2","firebrick4","black"), 
           bty= "n",cex= 1.15, seg.len = 1.5,text.width = 200,
           x.intersp = 0.31)
  }
  
  ##Accuracy text
  dt<-"2019-04-22"
  txt.cex = 1
  y.txt<-(y.hi-y.lo)/10
  #RF
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*4, bquote(RMSE == .(accu.min.max[[i]][[1]])),cex =txt.cex) #-17 or-20
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*3, bquote(Bias == .(accu.min.max[[i]][[3]])),cex = txt.cex)
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.min.max[[i]][[4]])),cex=txt.cex-0.1)
  text(x =as.Date(dt, format = "%Y-%m-%d"), y=y.lo + y.txt, bquote(R^2 == .(round(accu.min.max[[i]][[2]],3))),cex =txt.cex)
  #H1D
  y<-x[complete.cases(x$RF_mean),]
  h1d.acu<-accu.met(y$VWC_zone,y$th_zone)
  dt2<-"2019-10-22"
  text(x =as.Date(dt2, format = "%Y-%m-%d"), y= y.lo +y.txt*4, bquote(RMSE == .(h1d.acu[[1]])),cex =txt.cex) #-17 or-20
  text(x =as.Date(dt2,  format = "%Y-%m-%d"), y= y.lo +y.txt*3, bquote(Bias == .(h1d.acu[[3]])),cex = txt.cex)
  text(x =as.Date(dt2, format = "%Y-%m-%d"), y= y.lo +y.txt*2, bquote(Unb.RMSE == .(h1d.acu[[4]])),cex=txt.cex-0.1)
  text(x =as.Date(dt2, format = "%Y-%m-%d"), y=y.lo + y.txt, bquote(R^2 == .(round(h1d.acu[[2]],3))),cex =txt.cex)
  
  ##INSET1 -RF
  par(mar = c(2,2.3,0.5,1.25),mgp = c(1.23,0.35,0), tck = -0.065)
  ins<-data.frame(x[complete.cases(x$vl.Q50),c("VWC_zone","vl.Q50")])
  mn<-round(min(ins),2)
  mx<-round(max(ins),2)
  plot(ins$VWC_zone,ins$vl.Q50, cex=0.25,lwd=0.3, col= alpha("royalblue",0.5), bty = 'n', yaxt = 'n',xaxt= "n",
       cex.lab = 1,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = "RF")
  box(lwd=0.15, col ="grey70")
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.8, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.8, lwd=0.15,)
  abline(0,1, lty =2)
  
  
  ##INSET2 - H1D
  par(mar = c(2,2.05,0.5,0.45),mgp = c(1.23,0.35,0), tck = -0.065)
  ins<-data.frame(x[complete.cases(x$RF_mean),c("VWC_zone","th_zone")])
  mn<-round(min(ins),2)
  mx<-round(max(ins),2)
  plot(ins$VWC_zone,ins$th_zone, cex=0.25,lwd=0.3, col= alpha("firebrick4",0.35), bty = 'n', yaxt = 'n',xaxt= "n",
       cex.lab = 1,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = "H1D")
  box(lwd=0.15,col="grey70")
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.8, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.8, lwd=0.15,)
  abline(0,1, lty =2)
  
}
mtext(side=1, "Time", outer=TRUE, line=2.3,cex =1.15)
mtext(side=2, expression("Root zone soil moisture ("~theta["rz"]~")"), outer=TRUE, line =3, cex =1)
#dev.off()




###########################  5. SCATTER PLOT AND RESID all stations   ############################################
all.df<-lapply(all, function(x)  x[complete.cases(x),c("VWC_zone","RF_mean","th_zone")]) #vl.Q50 th_zone
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


#setEPS()
#postscript(paste("./3_figs/scat_resid_",type,".tol2.eps"), height=7,width=10)

#Resid - form 1:1 lines (x-y)
##new plot resid:
par(mfrow = c(2,2), mar = c(0,3,0,3), oma = c(4,2,3,0), mgp = c(2,0.7,0),tck = -0.015)


#function residual
obs<-all.df$VWC_zone

for(i in 2:3){
  mod<-all.df[,i]
  dif<-mod-obs
  #Out = boxplot(dif)$out
  out_dif<-dif[which(dif %in% Out)]
  out_obs<-obs[which(dif %in% Out)]
  ##Accuracy metrics
  accu.all<-accu.met(obs,mod)
  cex.txt=1
  #scatterplot
  plot(obs,mod, xaxt="n", yaxt="n", ylab="",xlab="",pch=19, cex=0.08, col= "grey50",bty="n",
       xlim =c(0,0.45),ylim=c(0,0.45))
  box(col = "grey50", lwd= 0.1)
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.07),border = NA)
  axis(side=2,at=seq(0,0.45, by=0.15),las=2, cex.axis=cex.txt, lwd=0.15)
  if(i==3){
    axis(side=1,at=seq(0,0.45, by=0.15), cex.axis=cex.txt, lwd=0.15)
  }
  abline(0,1, lty= 2, lwd= 1)
  
  ##Accuracy text 
  y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.05
  y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.05
  txt.acc = 1.15
  y.txt<-(y.hi-y.lo)/10
  text(x =0.37, y= y.lo +y.txt*4, bquote(RMSE == .(accu.all[[1]])),cex =txt.acc) #-17 or-20
  text(x =0.37, y= y.lo +y.txt*3, bquote(Bias == .(accu.all[[3]])),cex = txt.acc)
  text(x =0.37, y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.all[[4]])),cex=txt.acc-0.1)
  text(x =0.37, y=y.lo + y.txt, bquote(R^2 == .(round(accu.all[[2]],3))),cex =txt.acc)
  
  #resid
  plot(obs,dif, ylim=c(-0.2,0.15),yaxt="n",bty="n",ylab="",xlab="",pch=19, cex=0.08, col= "grey50",
       xlim=c(0,0.45), xaxt="n")
  box(col = "grey50", lwd= 0.1)
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.12),border = NA)
  if(i==3){
    axis(side=1,at=seq(0,0.45, by=0.15),lwd= 0.15,cex.axis=cex.txt)
  }
  axis(side=2,at=seq(-0.2,0.15, by=0.15),las=2, lwd= 0.15,cex.axis=cex.txt)
  abline(h=0, lty= 2, lwd= 1)
  
}


mtext(side=1, outer=TRUE, expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")), cex = 1.2, line=2.75)
mtext(side=2, outer=TRUE, expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")), cex = 1.2, line=0, adj = 0.85)
mtext(side=2, outer=TRUE, expression(paste("H1D ", theta["rz"], " (",m^3*m^-3,")")), cex = 1.2, line=0, adj=0.15)
mtext(side=2, outer=TRUE, "Residuals", cex = 1.2, line=-28.7)

mtext(side =3, labs, outer=TRUE, line =0.8,cex = txt.acc,font=3, col ="blue4")#Hydrus-1D
#dev.off()


#-Residuals BOXplots for <=0.10, and >=0.35-- 5th and 95th percentile----------
all.zone<-lapply(all.list, function(x) x$VWC_zone)
all.zone<-unlist(all.zone)
all.zone<-all.zone[complete.cases(all.zone)]
qs<-quantile(all.df$VWC_zone,c(0.05,0.95))
q5<-round(qs[1],2)
q95<-round(qs[2],2)


#cairo_pdf(file=paste("./3_figs/box_resid_extr_",type,".pdf"), height=3,width=5)
box.extr<-function(x){
  if(x=="dry"){
    vwc<-all.df[all.df$VWC_zone<=q5,1]
    sims<-all.df[all.df$VWC_zone<=q5,2:3]
    #resid
    difs<-sims-vwc
    sym<-"\u2264"
    lab<-bquote(theta["rz"]~.(sym)~.(q5))
  }else if(x=="wet"){
    vwc<-all.df[all.df$VWC_zone>=q95,1]
    sims<-all.df[all.df$VWC_zone>=q95,2:3]
    #resid
    difs<-sims-vwc
    sym<-"\u2265"
    lab<-bquote(theta["rz"]~.(sym)~.(q95))
  }
  
  boxplot(difs, pch=20,frame=FALSE,cex = 0.8,
          col = c(alpha("palegreen3",0.2),alpha("cornflowerblue",0.2)),
          yaxt="n",ylim=c(-0.2,0.2),
          names = c("RF","H1D"),boxwex=0.7)
  title(main=lab, cex.main=1.2)
  axis(side=2, las=2)
  box(col="grey60", lwd=0.15)
  
}

#P
par(mfrow = c(1,2),mar = c(2,2.5,2,1), oma = c(0,2,0,0),mgp = c(2,0.7,0), tck = -0.025)
box.extr("dry")
box.extr("wet")
mtext(side=2, expression(paste("Residuals"," (",m^3*m^-3,")")), cex = 1.2, line=0,outer=TRUE)
#dev.off()


############################   6. Prediction interval scatterplot   ##################################
all.PI<-lapply(all, function(x)  x[,c("VWC_zone","vl.Q05","vl.Q95")]) #vl.Q50
all.PI<-data.frame(do.call("rbind",all.PI))
all.PI<-all.PI[complete.cases(all.PI),]


plot(all.PI$VWC_zone,all.PI$vl.Q05, type = 'n', xlim = c(0.05,0.45), ylim = c(0.05,0.45))
arrows(x0 = all.PI$VWC_zone, y0 = all.PI$vl.Q05, y1 = all.PI$vl.Q95, col = 'red4', lwd = 0.12, code=0,lend= 2)

