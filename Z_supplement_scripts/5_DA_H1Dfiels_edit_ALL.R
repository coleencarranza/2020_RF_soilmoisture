#########==============Run DA for all stations in the RAAM######################
#Define interval for DA
inter<-30


## runs a source .R file for every raam station
nums<-seq(1:15)
rm.stn<-sprintf("RM%02d", nums)

#Raam station
for(i in rm.stn){
  stn<-i
  source("Z:/1_OWASIS/2_Statistical_analysis/rf_controls/data/all_raam/R_scripts/5_DA_H1Dfiels_edit.R")
  
}


for(r in 1:nrow(situ)){
  situ[n
}
if(seq.)
  
  if(stn=="RM11" && inter==30){
    remove <- 1031
    seq.sub<-seq.sub[!seq.sub %in% remove]
  }else if(stn=="RM11" && inter==15){
    remove <- c(986,1016,1031)
    seq.sub<-seq.sub[!seq.sub %in% remove]
    
  }
