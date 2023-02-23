#######_---------------------------DA direct replacement of initial conditions every n days------------------------################
###########################ADjust existing H1D runs using the full time period ALREADY simulated.#######################################
#Define interval for DA
inter<-30

#FUNCTION convert to df numeric to replace column
char2df<-function(x){
  a<-unlist(strsplit(x," "))
  a<-a[a!=""]
  a<-as.numeric(a)  
  return(a)
}

## runs a source .R file for every raam station
#run separatele for rm10parts
nums<-1
#nums<-c(10,10)

rm.stn<-sprintf("RM%02d", nums)
#rm.stn<-paste(rm.stn, c("A","B"),sep="")

#Raam station
for(q in rm.stn){

stn<-q

#Define directory paths
hydrus.path<-"C:/Program Files/PC-Progress/Hydrus-1D 4.xx"

#Project path
project.path<-paste("Z:/1_OWASIS/2_Statistical_analysis/rf_controls/0_RAAM/2_Hydrus/DA/H1Dopt_temp")

#Directory files
DA.folder<-paste("Z:/1_OWASIS/2_Statistical_analysis/rf_controls/0_RAAM/2_Hydrus/DA/")
#New directory where each run will be saved
setwd(project.path)
new_dir<-paste(DA.folder,"/",stn,"_DAout",sep="")
unlink(new_dir, recursive = TRUE)

dir.create(new_dir)


#Copy H1Dopt runs for station into H1Dopt temp
old.dir<-paste("Z:/1_OWASIS/2_Statistical_analysis/rf_controls/0_RAAM/2_Hydrus/H1D_opt/",stn,"opt",sep="")
new.dir<-project.path
fils<-list.files(old.dir,full.names=TRUE)
file.copy(from=fils, to=new.dir,overwrite = TRUE, recursive = FALSE)


#Read fulldata Hydrus1D runs
#function
H1Dfull.files<-function(file,path){
  fil<-list.files(path=path, pattern=file,full.names = TRUE)
  full<-readLines(fil)
  return(full)
}


#to be changed every run - origina bofek path
meteo.full<-H1Dfull.files(file="METEO",path=old.dir)
atm.full<-H1Dfull.files(file="ATMOSPH",path=old.dir)
select<-H1Dfull.files(file="SELECTOR",path=old.dir)

#IN situ data
situ<-read.csv(paste("Z:/1_OWASIS/2_Statistical_analysis/rf_controls/data/all_raam/rm_ts_all_daily/",stn,"_day.csv",sep=""),header=TRUE)
situ<-situ[,1:6]
situ<-situ[complete.cases(situ[,2:6]),]
situ$date<-strptime(situ$date, format = "%Y-%m-%d %H:%M:%S")
situ<-situ[situ$date<=as.POSIXlt("2018-12-31", format="%Y-%m-%d"),]
#REMOVE situ  element if theta less than theta_r in selector file
nmat<-char2df(select[grep("NMat",select)+1])[1]
shp.lin<-grep("thr     ths    Alfa      n         Ks       l",select)+1
mats<-shp.lin+nmat-1
thr<-select[shp.lin:mats]
thr<-lapply(thr, function(x) char2df(x)[1])
thr<-unlist(thr)

#seq.sub<-seq(DOYs[1],length(DOYs),inter)
mins<-apply(situ[,2:6],1,min)
situ<-situ[mins >= thr[1],]


#get DOY for first data
DOYs<-as.numeric(strftime(situ$date, format = "%j"))
yr<-as.numeric(format(as.Date(situ$date, format="%Y-%m-%d"),"%Y"))

for(y in seq_along(yr)){
  if(yr[y]==2017){
    DOYs[y]<-DOYs[y]+366
  }else if(yr[y]==2018){
    DOYs[y]<-DOYs[y]+366+365
  }
}
situ$DOYs<-DOYs
seq.sub<- DOYs[seq(1, length(DOYs), inter)]
seq.sub<-seq.sub[seq.sub<=1065]


situ.exp<-situ[which(situ$DOYs %in% seq.sub),1:6]
write.csv(situ.exp,paste(DA.folder,"/",q,"_DApts.csv",sep=""))


#############loop starts here#################################################
setwd(project.path)
for(i in seq_along(seq.sub)){

##########----------------------change the H1D files-----------------#############
start.sim<-seq.sub[i] ## changes day when initial condition is changed based on in situ values
last<-length(seq.sub)
if(i!=last){
  end.sim<-seq.sub[i+1]-1
}else {
  end.sim=1096
}

#Interval in days for changing initial conditions
DA.int<-end.sim-start.sim#+1

###---------------Change 1) tInit,2)lEnter, 3)InitCond in SELECTOR.IN----------------------
#stab.init=0
tInit<-grep("tInit",select)+1 #read next line
times<-c(start.sim, end.sim) #Set tInit to earlier time than data to "stabilize??
times<-paste(times, collapse=" ")
select[tInit]<-times


lEnter<-grep("lEnter",select)+1
select[lEnter]<-"f 1 1 f" ##last element should be f


IC<-grep("InitCond",select)+1
select[IC]<-" t t -1 t" ##last element should be t

dt<-grep("dtMin",select)+1
select[dt]<-" 1      0.0001           5     1.3     0.7     3     7     1" 

#write
write(x = select, file = paste(project.path,"/SELECTOR.IN",sep=""), append = F)


######----------------modify atm----------------- 
#find "MaxAL" line,next will be the value
maxAl<-grep("MaxAL",atm.full)
atm.full[maxAl+1]<-DA.int

#Subset contents
init<-grep("tAtm",atm.full) #line before data, 0 line
start.time<-char2df(atm.full[init+1])[1]

start.sub<-init+start.sim-start.time+1+1
end.sub<-start.sub + DA.int
content<-atm.full[start.sub:end.sub]

#replace contents
head<-atm.full[1:init]
end.line<-atm.full[length(atm.full)]

#new atm.fil
atm.fil<-c(head,content,end.line)
#write
write(x = atm.fil, file = paste(project.path,"/ATMOSPH.IN",sep=""), append = F)


######----------------modify meteo----------------- 
#find "MaxAL" line,next will be the value
mRec<-grep("MeteoRecords",meteo.full)
repl<-paste(c(DA.int,1,"f"),collapse= " ")
meteo.full[mRec+1]<-repl


#Subset contents
init<-grep("TMax",meteo.full)+1 #line before data, 0 line
start.time<-char2df(meteo.full[init+1])[1]
start.sub<-init+start.sim-start.time
end.sub<-start.sub + DA.int
content<-meteo.full[start.sub:end.sub]

#replace contents
head<-meteo.full[1:init]
end.line<-meteo.full[length(meteo.full)]
meteo.fil<-c(head,content,end.line)
#Write
write(x = meteo.fil, file = paste(project.path,"/METEO.IN",sep=""), append = F)


######---------------CHANGE intitial condition based on in situ data----------------
###Replace column for profile.dat
prof.file<-list.files(path=project.path, pattern="PROFILE",full.names = TRUE)
prof<-readLines(prof.file)

#select contents whole depth 100 or 101
init<-grep("Axz",prof)+1

  end<-init+100


content<-prof[init:end]

#to df
df<-lapply(content,function(x) char2df(x))
df<-do.call(rbind,df) 

#Replace column 3 with situ.sub
#situ.sub to change!!!!!!
if(i!=1){
  situ.sub<-situ[which(situ$DOYs==seq.sub[i])-stab.init,2:6]
}else{
  situ.sub<-situ[which(situ$DOYs==seq.sub[i]),2:6]
}


#check if length are same
situ.sub.vec<-unlist(c(rep(situ.sub[1],7),rep(situ.sub[2],8),rep(situ.sub[3],15),rep(situ.sub[4],30),rep(situ.sub[5],41)))


#change initial condition in Hydrus1d.dat
h1d.dat.f<-list.files(path=project.path, pattern="HYDRUS1D.DAT",full.names = TRUE)
h1d.dat<-readLines(h1d.dat.f)
inicond<-grep("InitialCondition",h1d.dat)

shp<-"theta"
if(shp=="matric"){
h1d.dat[inicond]<-"InitialCondition=0"
#Change situ.sub to pressure head- data from SELECTOR.in
#number NMat
nmat.lin<-grep("NMat",select)+1
nmat<-char2df(select[nmat.lin])[1]
nseq<-seq(1,nmat,1)

#function to matric potential
theta2mat<-function(thr,ths,alfa,n,theta){
  m=1+(1/n)
  mexp<-1/m
  nexp<-1/n
  th<-(ths-thr)/(theta-thr)
  h<- (((th^mexp) -1)^nexp)/alfa
  return(h)
}

#Line shp
shp.lin<-grep("thr     ths    Alfa      n         Ks       l",select)
#read succeeding shp's based on nmat
for(s in seq_along(nseq)){
  shp<-char2df(select[shp.lin+s])
for(r in 1:nrow(df)){
    if(df[r,4]==s){
      #replace 3rd column
      df[r,3]<-round(theta2mat(thr=shp[1],ths=shp[2],alfa = shp[3], n = shp[4],situ.sub.vec[r])*-1,3)
      
    }
  }
  
}
}else{
  df[,3]<-round(situ.sub.vec,3)
  h1d.dat[inicond]<-"InitialCondition=1"
}

#write h1d.dat
write(x = h1d.dat, file = paste(project.path,"/HYDRUS1D.DAT",sep=""), append = F)


#return backto profile.dat format
df[,3][is.nan(df[,3])]<-0
char<-apply(df,1, function(x) paste(x,collapse=" "))
prof[init:end]<-char

#write to file
write(x = prof, file = paste(project.path,"/PROFILE.DAT",sep=""), append = F)




#####################RUN HYDRUS1D.exe##########################################

#create level01 file where H1D is located
file_level01 = file.path(hydrus.path, "LEVEL_01.DIR")
Sys.chmod(file_level01, "0777")


#write to file
write(x = noquote(project.path), file = file_level01, append = F)


#RUN  
setwd(hydrus.path)
hyd.exe<-"H1D_CALC.EXE"
system(hyd.exe, show.output.on.console = TRUE,
minimized = FALSE, invisible = FALSE)



############COpy output obs.node.out to another folder and rename with interval doc###############
setwd(project.path)
# copy the files to the new folder
file.copy("Obs_Node.out", new_dir)

#rename onb_node for segment
setwd(new_dir)
suff<-sprintf("%04d", start.sim)
new.nam<-paste("Obs_Node",suff,".out",sep="")
file.rename("Obs_Node.out",new.nam)


#Rund script again
}
}