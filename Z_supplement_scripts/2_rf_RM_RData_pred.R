##---------------------------------Random forest-Prediction-results from caret/ranger-----------------------------------------
library(randomForest)
library(tsModel)
library(lubridate)
library(dplyr)
library(xts)
library(scales)
library(Metrics)

files<-list.files(path = "./data/all_raam/all_list", full.names = TRUE)
all.list<-lapply(files, read.csv, header=TRUE, stringsAsFactors = FALSE)
all.list<-lapply(all.list, function(x) {x$BOFEK <-as.factor(x$BOFEK);x})
all.list<-lapply(all.list, function(x) {x$Crop<-as.factor(x$Crop);x})
all.list<-lapply(all.list, function(x) {x$date <- as.Date(x$date, format = "%Y-%m-%d");x})
all.list<-lapply(all.list, function(x) x[complete.cases(x),])

#Remove other soil depths
all.list<-lapply(all.list, function(x) x[,-c(3:6,8)]) #VWC 40ave


#Rename to "zone
all.list<-lapply(all.list, function(x) {colnames(x)[3]<-"VWC_zone";x}) #VWC 40ave
#all.list<-all.list[1:4]

#Give names to all.list
nms<-substr(files, 26,29)
all.list<-setNames(all.list,nms)

##-------------------READ R.data run from SLM processing PC----------
load("./data/all_raam/RF_pred.ranger_mtry1_25.RData")

##GEt RMSEs for each run
fin_mods<-lapply(pred_mods_tune,function(x) x$results)
best_mod<-lapply(pred_mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)
best_mod.df
#plot RMSEs per training sample proportion




#All similar RMSE
#use model using 0.5 training tr.all[[1]]
rf_mod05<-pred_mods_tune[[1]]
pdf(paste("./figs/rf.mod.tol.pred.pdf"), height=6,width=7)
plot(rf_mod05)
dev.off()

###-----------------Other RF outputs - importance var-------------------------------- 
#pdf(paste("./figs/varimp_pred_new.pdf"), height=8,width=4)
par(oma= c(0,2,2,1),mar = c(4,0,0,0),family="serif", mgp = c(1.95,0.4,0), tck = -0.02)
plot(varImp(rf_mod05),sort=TRUE,type= 1,cex= 1, pt.cex =1.3, pch=22, bg = "lightblue",frame.plot = FALSE, lcolor="grey40", main = "",cex.lab = 1.25,lwd=2)
mtext(side =3, "Prediction", cex = 1.5, line =0, outer=TRUE, font = 3)
#dev.off()



#-----------------------------------#Reading H1D output --read B for longer simulation-------------------------------------
#-Read Soil moisture datasets----
Hydir<-list.dirs(path="./Hydrus/raam_all/H1D_opt")
    
#Hydir<-Hydir[grepl("*b",Hydir)]

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
hyd.dates<-list.files(path= "./Hydrus/raam_all",pattern="*.csv", full.names = TRUE, recursive = TRUE)
hyd.to.dates<-function(x){
    x<-read.csv(x, header=TRUE, stringsAsFactors = FALSE)[,1]
    x<-as.Date(x, format = "%Y-%m-%d")
  return(x)
}
hyd.dates<-lapply(hyd.dates, hyd.to.dates)[[1]]
hyd.xts<-lapply(hyd.df, function(x) xts(x, hyd.dates)) 
    



#--------------------Combine or Plot together Root zone soil moisture from data, RF, hydus,--------------------
#Combine measurements, rf, and h1d values in one xts
data.zone<-lapply(all.list, function(x) xts(x[,"VWC_zone"],x[,1]))
data.zone<-lapply(data.zone, function(x) {colnames(x)<-"VWC_zone";x})
hyd<-lapply(hyd.xts, function(x) x[,"th_zone"])
all<-list()
for(i in seq_along(data.zone)){
  all[[i]]<-merge(hyd[[i]],data.zone[[i]],pred_vl.xts[[i]][,44:47],all=TRUE)
  all[[i]]$th_zone<-ifelse(is.na(all[[i]]$VWC_zone)==TRUE, NA,all[[i]]$th_zone)
  colnames(all[[i]])[4:6]<-c("vl.Q05","vl.Q50","vl.Q95")
  #all[[i]]<-all[[i]][complete.cases(all[[i]]),]
}
all<-setNames(all,names(all.list))


#---------------------Accuracy metrics ------------------------------------------------------------
accu.met<- function(o,m){
  rmse<-round(Metrics::rmse(o,m),4)
  bias<-round(mean(m) - mean(o),3)
  cort<-cor.test(o, m)
  unb<-round(m-bias,4)
  unb.rmse<-round(Metrics::rmse(o,unb),4)
  
  combi<-list(rmse,cort,bias,unb.rmse)
  return(combi)
}


##Accu Hydrus-1D--------------
accu<-lapply(all, function(x) x[complete.cases(x),])
accu<-lapply(accu, function(x) accu.met(o=x$VWC_zone,m=x$th_zone))
accu.rmse<-unlist(lapply(accu, function(x) x[[1]]))

round(range(unlist(lapply(accu, function(x) x[[1]]))),3)
round(range(unlist(lapply(accu, function(x) x[[3]]))),3)
round(range(unlist(lapply(accu, function(x) x[[4]]))),3)
round(range(or<-unlist(lapply(accu, function(x) x[[2]]$estimate))),3)



#Accu RF--------------------
rm(accu)
accu<-lapply(all, function(x) x[complete.cases(x),])
accu<-lapply(accu, function(x) accu.met(o=x$VWC_zone,m=x$vl.Q50))
accu.rmse<-unlist(lapply(accu, function(x) x[[1]]))

round(range(unlist(lapply(accu, function(x) x[[1]]))),3)
round(range(unlist(lapply(accu, function(x) x[[3]]))),3)
round(range(unlist(lapply(accu, function(x) x[[4]]))),3)
round(range(or<-unlist(lapply(accu, function(x) x[[2]]$estimate))),3)



##--------------------Select stations where predictions are min and max------------------------------
raam.min.rmse<-all[which.min(accu.rmse)]
raam.max.rmse<-all[which.max(accu.rmse)]
min.max<-c(raam.min.rmse,raam.max.rmse)
accu.min.max<-accu[c(which.min(accu.rmse),which.max(accu.rmse))]



#-------------------------------------PLOT----------------------------
pdf(paste("./figs/qrf_pred_ts_min_max.tol.pdf"), height=4,width=7)
st<-as.Date("2016-04-21", format = "%Y-%m-%d")
fi<-as.Date("2019-08-01", format = "%Y-%m-%d")
yr.seq<-seq.Date(st,fi, by= "year")
library(scales)
par(oma = c(4,5,2,0.5))
layout(matrix(c(1,1,1,2,
                1,1,1,1,
                3,3,3,4,
                3,3,3,3),ncol=4, byrow =TRUE))
#layout.show(4)

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
    axis(side=1,at=yr.seq,labels = year(yr.seq),cex.axis=1.3, lwd=0.15)
  
  # Generating the lower and upper uncertainty bounds
  par(new=TRUE)
  bands<-x[complete.cases(x$vl.Q05),c(4,6)]
  lband <- as.zoo(bands$vl.Q05)
  uband <-as.zoo(bands$vl.Q95)
  hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("slategray1",0.35), add=TRUE)
  wd<-c("wheat2","firebrick3")
  lines(as.zoo(x$vl.Q50),type = "b", pch=3,col="royalblue",cex = 1, lwd=0.7) #RF valid/forecast
  #plot also training points#
  #points(as.zoo(x$tr.Q50),pch=3,col=alpha("lightblue",0.35), cex = 0.15) #RF train
  
  #Hydrus
  lines(as.zoo(x$VWC_zone),lwd=0.5)
  lines(as.zoo(x$th_zone),lty=4,cex=0.25,col ="firebrick4",lwd= 1)
  
  if(i ==2){
    leg<-as.Date("2018-6-10", format = "%Y-%m-%d")
    legend("bottomright", legend =c("Training RF","Validation RF", "Hydrus 1D","in situ"),ncol =2, inset=c(-0.05,-0.35),xpd=NA,lty = c(NA,NA,2,1),
           pch=c(19,3,NA,NA), col = c(alpha("azure4",0.75),"blue2","firebrick4","black"), bty= "n",cex= 1, seg.len = 1.5,text.width = 200,
           x.intersp = 0.31)
  }
  
  ##Accuracy text
  dt<-"2019-05-20"
  txt.cex = 1.15
  y.txt<-(y.hi-y.lo)/10
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*4, bquote(RMSE == .(accu.min.max[[i]][[1]])),cex =txt.cex) #-17 or-20
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*3, bquote(Bias == .(accu.min.max[[i]][[3]])),cex = txt.cex)
  text(x =as.Date(dt, format = "%Y-%m-%d"), y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.min.max[[i]][[4]])),cex=txt.cex-0.1)
  text(x =as.Date(dt, format = "%Y-%m-%d"), y=y.lo + y.txt, bquote(rho == .(round(accu.min.max[[i]][[2]]$estimate,3))),cex =txt.cex)
  
  
  ##INSET
  par(mar = c(2,6,0.5,1.75),mgp = c(1.23,0.35,0), tck = -0.065)
  ins<-data.frame(x[complete.cases(x$vl.Q50),c("VWC_zone","vl.Q50")])
  mn<-round(min(ins),2)
  mx<-round(max(ins),2)
  plot(ins$VWC_zone,ins$vl.Q50, cex=0.5,lwd=0.17, col= alpha("royalblue",0.5), bty = 'n', yaxt = 'n',xaxt= "n",cex.lab = 1,ylim=c(mn,mx), xlim=c(mn,mx), pch=3,
       xlab = "in situ", ylab = "RF")
  box(lwd=0.15)
  axis(side=2, at=seq(0.1,0.4,by=0.1), las=2,cex.axis=0.8, lwd=0.15)
  axis(side=1, at=seq(0.1,0.4,by=0.1),cex.axis=0.8, lwd=0.15,)
  abline(0,1, lty =2)
}
mtext(side=1, "Time", outer=TRUE, line=2,cex =1.15)
mtext(side=2, expression(theta["rz"]), outer=TRUE, line =3, cex =1.25)
dev.off()



##---------------------------SCAT AND REsid--------------------------------------------------
all.df<-lapply(all, function(x)  x[complete.cases(x),c("VWC_zone","vl.Q50")]) #vl.Q50 th_zone
all.df<-do.call("rbind",all.df)

#Resid - form 1:1 lines (x-y)
#function residual
obs<-as.numeric(all.df$VWC_zone)
mod<-as.numeric(all.df[,2])
dif<-mod-obs
Out = boxplot(dif)$out
out_dif<-dif[which(dif %in% Out)]
out_obs<-obs[which(dif %in% Out)]
##Accuracy metrics
accu.all<-accu.met(obs,mod)


setEPS()
postscript(paste("./figs/scat_resid_pred.tol.eps"), height=3,width=10.5)
#pdf(paste("./figs/scat_resid_pred2.pdf"), height=2,width=7)
cex.txt =1
#scatter
par(oma = c(2,2,0,0))
layout(matrix(c(1:3), ncol = 3), widths = c(4,7,2))
par(mar = c(1.5,1.95,1.5,2), mgp = c(1.25,0.35,0), tck = -0.015)
plot(obs,mod, xaxt="n", yaxt="n", ylab="",xlab="",pch=19, cex=0.08, col= "grey30",bty = "n",
     xlim =c(0,0.45),ylim=c(0,0.45))
# Now set the plot region to grey
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.12),border = NA)
axis(side=2,at=seq(0,0.45, by=0.15),las=2, cex.axis=cex.txt, lwd=0.15)
axis(side=1,at=seq(0,0.45, by=0.15), cex.axis=cex.txt, lwd=0.15)
abline(0,1, lty= 2, lwd= 1)

##Accuracy text 
y.lo<-round(min(x$VWC_zone, na.rm = TRUE),2)-0.05
y.hi<-round(max(x$VWC_zone,na.rm=TRUE),2)+0.05
txt.acc = 1.15
y.txt<-(y.hi-y.lo)/10
text(x =0.37, y= y.lo +y.txt*4, bquote(RMSE == .(accu.all[[1]])),cex =txt.acc) #-17 or-20
text(x =0.37, y= y.lo +y.txt*3, bquote(Bias == .(accu.all[[3]])),cex = txt.acc)
text(x =0.37, y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.all[[4]])),cex=txt.acc-0.1)
text(x =0.37, y=y.lo + y.txt, bquote(rho == .(round(accu.all[[2]]$estimate,3))),cex =txt.acc)

#resid
par(mar = c(1.5,2.75,1.5,0), mgp = c(1.25,0.35,0), tck = -0.015)
plot(obs,dif, ylim=c(-0.2,0.15),yaxt="n",bty="n",ylab="",xlab="",pch=19, cex=0.08, col= "grey30",
     xlim=c(0,0.45), xaxt="n")
# Now set the plot region to grey
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = alpha("palegoldenrod",0.12),border = NA)
axis(side=1,at=seq(0,0.45, by=0.15),lwd= 0.15,cex.axis=cex.txt)
axis(side=2,at=seq(-0.2,0.15, by=0.15),las=2, lwd= 0.15,cex.axis=cex.txt)
abline(h=0, lty= 2, lwd= 1)


#Boxplot
par(mar = c(1.5,0,1.5,0.5))
gr.cols<-"grey30"
lwds=0.25
medwd=1.5
bxcl<-brocolors("crayons")["Periwinkle"]
wkcl<-brocolors("crayons")["Black"]
boxplot(dif, ylim = c(-0.2,0.15), frame=FALSE,pch=3,width =2, yaxt= "n", col=bxcl, 
        medlwd = medwd,medcol = "black",
        boxcol=gr.cols,boxlwd=lwds, 
        outcol=wkcl,outcex =0.05,outwd = 0.01,
        whisklty=1,whisklwd = lwds,whiskcol = gr.cols,
        staplelwd=lwds, staplecol=gr.cols)
#points(out_obs,out_dif, col = "red2",pch=19, cex=0.15, ylim = c(-0.3,0.3))
#legend(x= 0.01, y = -0.15, pch = 19, col = "red2", "outliers", bty= "n", cex = 0.5)


mtext(side=1, outer=TRUE, expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")), cex = 0.8, line=0.75)
mtext(side=2, outer=TRUE, expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")), cex = 0.8, line=0.35)
mtext(side=2, outer=TRUE, "Residuals", cex = 0.8, line=-24)
mtext(side =3, "Prediction", outer=TRUE, line =-1.75,cex = txt.acc,font=3, col ="blue4")#Hydrus-1D


dev.off()

  
  
  
##Prediction interval scatterplot
all.PI<-lapply(all, function(x)  x[,c("VWC_zone","vl.Q05","vl.Q95")]) #vl.Q50
all.PI<-data.frame(do.call("rbind",all.PI))
all.PI<-all.PI[complete.cases(all.PI),]
  
  
plot(all.PI$VWC_zone,all.PI$vl.Q05, type = 'n', xlim = c(0.05,0.45), ylim = c(0.05,0.45))
arrows(x0 = all.PI$VWC_zone, y0 = all.PI$vl.Q05, y1 = all.PI$vl.Q95, col = 'red4', lwd = 0.12, code=0,lend= 2)
  