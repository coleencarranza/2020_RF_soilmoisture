###_------------------------------------_TUNING RF hyperparameters--------------------------
library(caret)
library(ranger)
library(randomForest)
library(tsModel)
library(lubridate)

##Type of analysis
type="fcast"
############################# 0. Pred and read file   s######################################
files<-list.files(path = "./0_Data/all_list", full.names = TRUE)
files<-files[grepl("10A|10B",files)==FALSE] #remove parts R10

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
    
    
############################## 1. FUNCTIONS for sampling ##########################
set.seed(78594)
#RF function - select train and valid, then rf
tr.vl<-function(df,samp,type){
  if(type =="fcast"){
    x<-data.frame(df)
    cut<- round(nrow(x)*samp,0)# whole number training set
    tr<-x[1:cut,]
    vl<-x[-c(1:cut),]
    samp.all<-list(tr,vl)
    
  }else if(type=="pred"){
    x<-data.frame(df)
   # x<-x[complete.cases(x),]
    tr<- round(sample(nrow(x), samp*nrow(x), replace = FALSE),0)# whole number training set
    tr.sub<-x[tr,]
    vl.sub<-x[-tr,]
    tr.sub<-tr.sub[complete.cases(tr.sub),]
    samp.all<-list(tr.sub,vl.sub)
  }

  return(samp.all)
}
    
################2. Converting every categorical variable to numerical using dummy variables  ################
#fac<-c("Crop","BOFEK")
all<-do.call(rbind,all.list)
dmy<-function(x){
  dm<-dummyVars(" ~ .", data = x)
  dmy.df<-data.frame(predict(dm, newdata = x))
return(dmy.df)
}
dmy <- dmy(all[,-30])#not includign names
dmy.list<-split(dmy, f=as.factor(all$names))

#################  2. TRaining prop:  #####################################
train.prop<-seq(0.5,0.8,by=0.1)
#1.Traning prop - use dummy 
samps<-list()
for(i in seq_along(train.prop)){
      samps[[i]]<-lapply(dmy.list, function(x) tr.vl(df=x,samp=train.prop[i],type = type))
    }
    
    tr<-lapply(samps,function(x)  lapply(x, function(y) y[[1]]))
    
    
#data RF model:
tr.all<-lapply(tr, function(x) do.call("rbind",x))
tr.all<-lapply(tr.all, function(x) x[complete.cases(x),-1])
lapply(tr.all, nrow)
lapply(tr.all, function(x) round(nrow(x)*0.10,-2))

################### 3. parameter combinations  ###############################
Rfgrid <- expand.grid(.mtry=c(1:25),
                      .splitrule = "variance",
                      .min.node.size =c(5,10,20,30)) #originally 1:17



################  4. FUNCTION TUNING - use ranger for fast RF  ######################
tune_rf<-function(trainData,extr){
  #case weights for extreme conditions
  exd<-extr[[1]]
  exw<-extr[[2]]
  cw<-ifelse(trainData$VWC_zone>exd & trainData$VWC_zone<exw,1,10)
  #10% of total
  trees<-round(nrow(trainData)*0.10,-2)
  
  #2.Caret tuning
  model_rf<-train(VWC_zone ~ ., 
                  data = trainData, 
                  trControl = caret::trainControl(
                    returnData = TRUE,
                    method = "cv",
                    number = 10),
                  method = "ranger",
                  weights = cw,
                  tuneGrid = Rfgrid,
                  num.trees = trees,
                  keep.inbag = TRUE,
                  #quanteg=TRUE,
                  importance="permutation")
  return(model_rf)
}
#for case weights,extreme values - based on all the data
all.zone<-do.call(rbind,tr.all)[,"VWC_zone"]
qs<-round(quantile(all.zone,c(0.025,0.975)),2)

#For all train set with varying proportions
#RUNNING THIS TAKES A LONG TIME!
try<-tune_rf(tr.all[[1]],qs)


#mods_tune<-lapply(tr.all, function(x) tune_rf(x,qs))
mods_tune<-try

#read RData from 4_tune_caret_for_all.R
#rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25.RData")
#load(rf_out)
#mods_tune<-rf_out


###--------------------------------------RESULTS!-------------------------
#========================== 5. GEt RMSEs for each run==============================
fin_mods<-lapply(mods_tune,function(x) x$results)
best_mod<-lapply(mods_tune,function(x) x$results[x$results[1] == x$bestTune[[1]] & x$results[3] ==x$bestTune[[3]],])
best_mod.df<-do.call(rbind,best_mod)
best_mod.df

#All similar RMSE!, therefore use minimal samples to train: 0.5 training tr.all[[1]]
rf_mod05<-mods_tune[[1]]

#OPTION 1:10% RMSE tolerance model
whichTenPct <- tolerance(rf_mod05$results, metric = "RMSE", 
                           tol=10, maximize = FALSE)  
cat("best model within102 pct of best:\n")
best.mod.tol<-rf_mod05$results[whichTenPct,]
best.mod.tol


#OPTION2:#change in slope <1%
library(inflection)
library(ecp)
rf_mod05.accu<-rf_mod05$results
rf_mod05.accu.list<-split(rf_mod05.accu,f=as.factor(rf_mod05.accu$min.node.size))

t<-rf_mod05.accu.list[[1]] #min.node.size=5
rmse.diff<-c(abs(diff(t$RMSE,1)),0) #slope
diff.slp.perc<-(rmse.diff/t$RMSE)*100
nodiff.slp<-min(which(diff.slp.perc <1))

#plot
plot(t$mtry,t$RMSE)
points(t$mtry[nodiff.slp],t$RMSE[nodiff.slp],col="red",pch=19)

best.mod.tol<-rf_mod05$results[rf_mod05$results$min.node.size== 5 & rf_mod05$results$mtry ==nodiff.slp,]
best.mod.tol

#########################  6. Run again using best.mod.tol  ############################
exd<-qs[[1]]
exw<-qs[[2]]
cw<-ifelse(tr.all[[1]]$VWC_zone>exd & tr.all[[1]]$VWC_zone<exw,1,5)

mod.tol<-  ranger(VWC_zone ~ .,
                        data = tr.all[[1]],
                        mtry=best.mod.tol$mtry,
                        num.trees = round(nrow(tr.all[[1]])*0.10,-2),
                        min.node.size = best.mod.tol$min.node.size,
                        splitrule = "variance",
                        case.weights=cw,
                        importance = "permutation",
                        keep.inbag = TRUE,
                        oob.error = TRUE)

#Predict with validation set:
vl<-lapply(samps,function(x) lapply(x, function(y) y[[2]]))[[1]]
vl<-lapply(vl, function(x) x[complete.cases(x),])
vl<-lapply(vl,function(x) {x$rf_mod.tol<-predict(mod.tol,x, type = "response")[[1]];x})

################################  7. Quantreg with Ranger - PRed interval  #########################
qrf.mod <- ranger(VWC_zone ~ .,
                  data = tr.all[[1]],
                  mtry=best.mod.tol$mtry,
                  num.trees = round(nrow(tr.all[[1]])*0.10,-2),
                  min.node.size = best.mod.tol$min.node.size,
                  splitrule = "variance",
                  case.weights = cw,
                  quantreg = TRUE,
                  keep.inbag = TRUE,
                  oob.error = FALSE)


##Validation set - xts
vl2<-lapply(samps,function(x) lapply(x, function(y) y[[2]]))[[1]]
vl2<-lapply(vl2, function(x) x[complete.cases(x),])

qrf.fcast.vl2 <- lapply(vl2, function(x) predict(qrf.mod,x, type = "quantiles", quantiles = c(0.025, 0.5, 0.975)))
qrf.fcast.vl2<-lapply(qrf.fcast.vl2, function(x) x$predictions)

#####################################  8. MErge vl and qrf   ####################################
vl1<-list()
for(i in seq_along(vl)){
  vl1[[i]]<-cbind(vl[[i]],data.frame(qrf.fcast.vl2[[i]]))
}

#To xts
library(xts)
vl1<-lapply(vl1, function(x) {x$date<-as.Date(x$date,origin ="1970-01-01");x})
vl.xts<-lapply(vl1, function(x) xts(x[,-1],x[,1]))

#test plot
par(mfrow =c(4,2), mar = c(0,0,0,0))
lapply(vl.xts, function(x) {plot(as.zoo(x$VWC_zone), type = "b",pch=19,cex=0.7, ylim =c(0.05,0.45))
                            lband <- as.zoo(x$quantile..0.025)
                            uband <-as.zoo(x$quantile..0.975)
                            hydroGOF:::plotbandsonly(lband, uband,bands.col=alpha("slategray3",0.35), add=TRUE, ylim =c(0.05,0.45))
                            points(as.zoo(x$rf_mod.tol), col = "red3", cex=0.31, ylim =c(0.05,0.45))
                            points(as.zoo(x$quantile..0.5), col = "blue", cex=0.31, ylim =c(0.05,0.45))})



###########################   9. Save RF_model to file   ##########################
save(mods_tune,
    mod.tol,
    vl.xts,
    samps,
    file = paste0("./1_RF/RF_",type,".ranger_mtry1_25_inflec_cw.RData"))



