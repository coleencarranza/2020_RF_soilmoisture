##---------------------Extract meteo data into Raam points-------------------
library(RColorBrewer)
library(scales)
#read 0_meteo_knmi.R first
#read raster stacks of meteo files -- all are in WGS
library(raster)
library(rgdal)
library(plyr)

#read raam points and catchment bdy
##Extract values to Raam stations - already with BOFEK/PWAN codes for each station
RM<-readOGR(dsn="./0_Data/shp/rm_pts_wgs_bofek.shp", layer = "rm_pts_wgs_bofek", pointDropZ = TRUE )
bdy<-readOGR(dsn="./0_Data/shp/raam_catch_bdy.shp", layer = "raam_catch_bdy", pointDropZ = TRUE )


#METEO
grds<-list.files(path = "./3_figs/meteo_maps",pattern = ".grd",full.names = TRUE)
meteo<-lapply(grds, stack)
met.nam<-substr(grds,28,nchar(grds)-4)

##for sample maps
jul<-lapply(meteo, function(x) x[[185]])

#remove RH
jul<-jul[-c(5,7)]
met.nam<-met.nam[-c(5,7)]

#resample rd so it has same resolution as others
jul[[4]]<- resample(jul[[4]], jul[[1]], method = 'bilinear') # resample output



#LAI MODIS
library(MODIS)
library(gdalUtils)
path<-"./0_Data/meteo_rmpts_ts/"
sds <- get_subdatasets(paste0(path,'MOD15A2H.A2016185.h18v03.006.2016194194852.hdf'))
# Any sds can then be read directly using the raster function
lai <- raster(sds[2])
#reprojection
lai<-projectRaster(lai, crs=crs(jul[[1]]))


#BOFEK
bof<- readOGR(dsn = "./0_Data/shp/raam_crop.shp", 
                      layer = "raam_crop")
#PBOFEK
e2 <- extent(bdy)# coerce to a SpatialPolygons object
e2[1]<-e2[1]-0.03
e2[2]<-e2[2]+0.03
e2[3]<-e2[3]-0.03
e2[4]<-e2[4]+0.03
#crop to bdy
bof.crop<-crop(bof,e2)
col.rmp2<-colorRampPalette(brewer.pal(12,"Set3"))(length(levels(bof.crop$BOFEK2012)))
col.lev<-levels(bof.crop$BOFEK2012)
## intersect polygons with points, keeping the information from both
cols_o<-col.rmp2[which(col.lev %in% as.character(RM$BOFEK2012))]


#PLOTS
plts<-function(bdy,ras,pts,units, names,col,...){
  e <- extent(bdy)
  e2<-e
  # coerce to a SpatialPolygons object
  e2[1]<-e2[1]-0.05
  e2[2]<-e2[2]+0.05
  e2[3]<-e2[3]-0.03
  e2[4]<-e2[4]+0.03
  p <- as(e2, 'SpatialPolygons')
  r<-range(getValues(mask(ras,p)),na.rm = TRUE)
#plot
  plot(ras, asp=2,col=col,xlim = c(e2[1],e2[2]), ylim = c(e2[3],e2[4]),
        legend=FALSE,axes=FALSE, box=FALSE,...)
  title(main=names,cex.main=1.85)
  box(col="grey30",lwd=0.1)
  ys<-round(seq(e2[3],e2[4],length.out = 4),1)
  axis(side=1,col.axis="grey45",col="grey45",cex.axis=1.5)
  axis(side=2, las=2,at=ys,col.axis="grey45",col="grey45",cex.axis=1.5)
  #legend only
  plot(bdy,add=TRUE,  border = "black", lwd=1.5, lty=4)
  points(pts, pch=16, cex =1.5)
  # Plot legend
  s<-seq(r[1], r[2], length.out = 4)
  plot(ras, legend.only=TRUE,
       xlim = c(e2[1],e2[2]), ylim = c(e2[3],e2[4]),
       col=col,legend.width=4, legend.shrink=0.5,lwd=0.01, border="grey50",legend.line=0.01,
       axis.args=list(at=s,labels=round(s,2),  cex.axis=1.4),
       legend.args=list(text=units, adj=0.3, side=3, font=2, line=0.5, cex=1.2))
}

#COls!
Or<-colorRampPalette(brewer.pal(8,"OrRd"))(10)
Bu<-colorRampPalette(brewer.pal(8,"GnBu"))(10)
Gn<-colorRampPalette(brewer.pal(8,"GnBu"))(10)
Pi<-colorRampPalette(brewer.pal(8,"PiYG"))(10)
BrB<-colorRampPalette(brewer.pal(8,"BrBG"))(10)
Gr<-colorRampPalette(brewer.pal(8,"Greys"))(10)
col.list<-list(Or,Bu,Pi,Bu,Gr,Or,Or,Bu)

#PLOTS
#png(file=paste("./3_figs/samp_mapsb.png"), height=10,width=16,
 #   units="in", res=500)
par(oma = c(1,1,1,1),mar = c(3,4,2,6), mgp = c(2,0.4,0), tcl = -0.31, xpd=NA)
layout(matrix(c( 0,1,4,7,
                 0,0,5,8,
                 0,2,6,3), nrow=3, byrow = TRUE))
#layout.show(10)

nams.list<-as.list(c(expression(ET[pot]),"Ave. Wind Speed","Global Radiation","Precipitation","Ave. Sun Hours","Min. Temperature","Max. Temperature",
                                "Relative Humidity"))
unis<-as.list(c("mm","km/day","MJ/m2","cm","hr","°C","°C","%"))
mapply(function(x,y,z,c) plts(bdy,x,RM,units=y,names=z, col=alpha(c,0.65)), jul,unis,nams.list,col.list )

#plot LAI
plts(bdy,lai,RM,"-","LAI", col=alpha(BrB,0.6))

#BOFEK
plot(bof.crop["BOFEK2012"], xlim =c(e2[1],e2[2]), ylim = c(e2[3],e2[4]),col=alpha(col.rmp2,0.7),  
     main= "BOFEK 2012",border=NA,asp=2)
legend("topright", legend = unique(RM$BOFEK2012),col = cols_o, pch=15, xpd=NA,cex =1.65, ncol = 1, x.intersp = 0.75,y.intersp = 1.2,
       pt.cex=2.25,inset = c(-0.5,0),bty="n")
plot(bdy,add=TRUE,  border = "black",  lwd=1.5, lty=4)
points(RM, pch=16, cex=1.5)

#dev.off()
