###_------------------------------------_TUNING RF hyperparameters--------------------------
library(caret)
library(ranger)
library(randomForest)
library(tsModel)
library(lubridate)
    
files<-list.files(path = "./data/all_raam/all_list", full.names = TRUE)
names<-lapply(files, function(x)  substr(x,nchar(x)-7,nchar(x)-4))
    
all.list<-lapply(files, read.csv, header=TRUE, stringsAsFactors = TRUE)
all.list<-lapply(all.list, function(x) {x$BOFEK <-as.factor(x$BOFEK);x})
all.list<-lapply(all.list, function(x) {x$Crop<-as.factor(x$Crop);x})
all.list<-lapply(all.list, function(x) {x$date <- as.Date(x$date, format = "%Y-%m-%d");x})
    
    
for(i in seq_along(all.list)){
  all.list[[i]]$names<-names[[i]]
}
#Remove other soil depths
all.list<-lapply(all.list, function(x) x[,-c(3:6,8)]) #VWC 40ave
    
#Rename to "zone
all.list<-lapply(all.list, function(x) {colnames(x)[3]<-"VWC_zone";x}) #VWC 40ave
all.list<-lapply(all.list, function(x) x[complete.cases(x$VWC_zone),])
    
#all.list<-all.list[1:4]
#all.list<-all.list[c(1,6,7)]
    
    
##FUNCTIONS for sampling
set.seed(78594)
#RF function - select train and valid, then rf
tr.vl<-function(df,samp){
  x<-data.frame(df)
  cut<- round(nrow(x)*samp,0)# whole number training set
  tr<-x[1:cut,]
  vl<-x[-c(1:cut),]
  samp.all<-list(tr,vl)
  return(samp.all)
}
    
    
#Converting every categorical variable to numerical using dummy variables
#fac<-c("Crop","BOFEK")
all<-do.call(rbind,all.list)
dmy<-function(x){
  dm<-dummyVars(" ~ .", data = x)
  dmy.df<-data.frame(predict(dm, newdata = x))
return(dmy.df)
}
dmy <- dmy(all[,-30])#not includign names
dmy.list<-split(dmy, f=as.factor(all$names))
    
    
##TRaining prop:
train.prop<-seq(0.5,0.8,by=0.1)
#1.Traning prop - use dummy 
samps<-list()
for(i in seq_along(train.prop)){
      samps[[i]]<-lapply(dmy.list, function(x) tr.vl(df=x,samp=train.prop[i]))
    }
    
    tr<-lapply(samps,function(x)  lapply(x, function(y) y[[1]]))
    
    
#data RF model:
tr.all<-lapply(tr, function(x) do.call("rbind",x))
tr.all<-lapply(tr.all, function(x) x[complete.cases(x),-1])
#PREPROCESS
  #nzv <- nearZeroVar(Train.all, saveMetrics= TRUE)
 #nzv$nzv
    
    
    
##parameter combinations
Rfgrid <- expand.grid(.mtry=c(1:25),
                      .splitrule = "variance",
                      .min.node.size =c(5,10,20,30)) #originally 1:17


#FUNCTION TUNING! - use ranger for fast RF
tune_rf<-function(trainData){
  #20% of total
  trees<-round(nrow(trainData)*0.10,-2)
  
  #2.Caret tuning
  model_rf<-train(VWC_zone ~ ., 
                  data = trainData, 
                  trControl = caret::trainControl(
                    returnData = TRUE,
                    method = "cv",
                    number = 10),
                  method = "ranger",
                  tuneGrid = Rfgrid,
                  num.trees = trees,
                  #quanteg=TRUE,
                  importance="permutation")
  return(model_rf)
}

#For all train set with varying proportions
mods_tune<-lapply(tr.all, tune_rf)

mods_tune<-fcast_mods_tune
#RESULTS!
##GEt RMSEs for each run
fin_mods<-lapply(mods_tune,function(x) x$results)
best_mod<-lapply(mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)
best_mod.df


#All similar RMSE
#use model using 0.5 training tr.all[[1]]
rf_mod05<-mods_tune[[1]]
whichTenPct <- tolerance(rf_mod05$results, metric = "RMSE", 
                         tol=10, maximize = FALSE)  
cat("best model within102 pct of best:\n")
best.mod.tol<-rf_mod05$results[whichTenPct,]
best.mod.tol


##Run again using best.mod.tol
mod.tol<-  ranger(VWC_zone ~ .,
                        data = tr.all[[1]],
                        mtry=best.mod.tol$mtry,
                        num.trees = round(nrow(tr.all[[1]])*0.10,-2),
                        min.node.size = best.mod.tol$min.node.size,
                        splitrule = "variance",
                        importance = "permutation",
                        keep.inbag = TRUE,
                        oob.error = TRUE)


#Predict with validation set:
vl<-lapply(samps,function(x) lapply(x, function(y) y[[2]]))[[1]]
vl<-lapply(vl, function(x) x[complete.cases(x),])
vl<-lapply(vl,function(x) {x$rf_mod.tol<-predict(mod.tol,x, type = "response")[[1]];x})


######----------------------------Quantreg with Ranger------------------------
qrf.mod <- ranger(VWC_zone ~ .,
                  data = tr.all[[1]],
                  mtry=best.mod.tol$mtry,
                  num.trees = round(nrow(tr.all[[1]])*0.10,-2),
                  min.node.size = best.mod.tol$min.node.size,
                  splitrule = "variance",
                  quantreg = TRUE,
                  keep.inbag = TRUE,
                  oob.error = TRUE)


##Validation set - xts
vl2<-lapply(samps,function(x) lapply(x, function(y) y[[2]]))[[1]]
vl2<-lapply(vl2, function(x) x[complete.cases(x),])


qrf.fcast.vl2 <- lapply(vl2, function(x) predict(qrf.mod,x, type = "quantiles", quantiles = c(0.05, 0.5, 0.95)))
qrf.fcast.vl2<-lapply(qrf.fcast.vl2, function(x) x$predictions)



#-----------------MErge vl and qrf------------------------------------
vl1<-list()
for(i in seq_along(vl)){
  vl1[[i]]<-cbind(vl[[i]],data.frame(qrf.fcast.vl2[[i]]))
}



##To xts
library(xts)
vl1<-lapply(vl1, function(x) {x$date<-as.Date(x$date,origin ="1970-01-01");x})
vl.xts<-lapply(vl1, function(x) xts(x[,-1],x[,1]))

##test plot
par(mfrow =c(4,2), mar = c(0,0,0,0))
lapply(fcast_vl.xts, function(x) {plot(as.zoo(x$VWC_zone), type = "b",pch=19,cex=0.7, ylim =c(0.05,0.45))
                            lband <- as.zoo(x$quantile..0.05)
                            uband <-as.zoo(x$quantile..0.95)
                            hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("slategray1",0.35), add=TRUE, ylim =c(0.05,0.45))
                            points(as.zoo(x$rf_fcast), col = "red3", cex=0.31, ylim =c(0.05,0.45))
                            points(as.zoo(x$quantile..0.5), col = "blue", cex=0.31, ylim =c(0.05,0.45))})



##Save RF_model to file
save(mods_tune,
    mod.tol,
    vl.xts,
    samps,
    file = "./data/all_raam/RF_fcast.ranger_mtry1_25.RData")



