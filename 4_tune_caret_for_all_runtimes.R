###_------------------------------------_TUNING RF hyperparameters--------------------------
library(caret)
library(ranger)
library(randomForest)
library(tsModel)
library(lubridate)

##Type of analysis
type="pred"
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


################### 3. parameter combinations  ###############################
Rfgrid <- expand.grid(.mtry=c(1:25),
                      .splitrule = "variance",
                      .min.node.size =c(5,10,20,30)) #originally 1:17


################  4. FUNCTION TUNING - use ranger for fast RF  ######################
#For all train set with varying proportions
#RUNNING THIS TAKES A LONG TIME!
#mods_tune<-lapply(tr.all, tune_rf)

#read RData from 4_tune_caret_for_all.R
rf_out<-paste0("./1_RF/RF_",type,".ranger_mtry1_25.RData")
load(rf_out)
#mods_tune<-rf_out

##############===============1.RUNTIME for running RF model already. not tuning with 10-fold CV using CARET================#####################
#function - ranger
#RF.runtimes<-function(trainData,m,node.size){

ranger.runtime<-numeric()
set.seed(78594)
for(i in 1:nrow(Rfgrid)){  
  #10% of total
  trainData<-tr.all[[1]]## 50% training samples
  m<-Rfgrid[i,1]
  node.size<-Rfgrid[i,3]
  trees<-round(nrow(trainData)*0.10,-2)
  #2.Caret tuning
 t1 <- Sys.time() # Start timer
  mod.times<- ranger(VWC_zone ~ .,
                      data = trainData,#
                      mtry=m,
                      num.trees = trees,
                      min.node.size = node.size,
                      splitrule = "variance",
                      importance = "permutation",
                      keep.inbag = TRUE,
                      oob.error = TRUE)
    t2 <- Sys.time()   # End timer
    ranger.runtime[[i]]<-difftime(t2, t1, units = "secs")    # Display time
}

#if function
#set.seed(78594)
#trDat<-tr.all[[1]]
#ranger_runtime<-apply(Rfgrid,1, function(x) RF.runtimes(trainData = trDat,m=x[1],node.size = x[3]))


ranger.runtime.list<-split(ranger.runtime,f=as.factor(Rfgrid$.min.node.size))
par(mfrow=c(2,2))
lapply(ranger.runtime.list,plot)

#save  
save(ranger.runtime.list,
     file = paste0("./1_RF/RF_",type,".runtime_rfranger_mod05.RData"))

  
#######==================2.RUNTIME  tuning per parameter combination===================###################
tune_rf_runtime<-function(trainData,mtry,node.size){
#10% of total
trees<-round(nrow(trainData)*0.10,-2)
grid <- expand.grid(.mtry=mtry,
                      .splitrule = "variance",
                      .min.node.size =node.size) #originally 1:17
#trainData<-tr.all[[1]]
#2.Caret tuning
t1 <- Sys.time() # Start timer
model_rf<-train(VWC_zone ~ ., 
                data = trainData, 
                trControl = caret::trainControl(
                returnData = TRUE,
                method = "cv",
                number = 10),
                method = "ranger",
                tuneGrid = grid,
                num.trees = trees,
                #quanteg=TRUE,
                importance="permutation")
t2 <- Sys.time()   # End timer
runtime<-difftime(t2, t1, units = "secs")    # Display time
return(runtime)
}
  
#RF_grid as list, apply function row-wise, 50% training
trDat<-tr.all[[1]]
set.seed(78594)
grid.list<- split(Rfgrid, seq(nrow(Rfgrid)))
rftune.runtime<-mapply(function(x) tune_rf_runtime(trainData=trDat,mtry=x[[1]],node.size=x[[3]]),grid.list)
    
rftune.runtime.list<-split(rftune.runtime,f=as.factor(Rfgrid$.min.node.size))
#testplot
par(mfrow=c(2,2))
lapply(rftune.runtime.list,plot)

#save  
save(rftune.runtime.list,
     file = paste0("./1_RF/RF_",type,".runtime_rftune_mod05.RData"))


##PLOT
layout(matrix(c(1,2,3,3),ncol=2,byrow=TRUE),heights = c(1,1,2))


#######==================3.RUNTIME tuning per training set proportion- takes a long time!===================###################
tune_trprop_runtime<-function(trainData){
  #10% of total
  trees<-round(nrow(trainData)*0.10,-2)
  
  #2.Caret tuning
  t1 <- Sys.time() # Start timer
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
  t2 <- Sys.time()   # End timer
  runtime<-difftime(t2, t1, units = "secs")    # Display time
  return(runtime)
}

#For all train set with varying proportions
#RUNNING THIS TAKES A LONG TIME!
trprop_runtime<-sapply(tr.all,tune_trprop_runtime)

plot(trprop_runtime)

#save  
save(trprop_runtime,
     file = paste0("./1_RF/RF_",type,".runtime_trprop.RData"))


