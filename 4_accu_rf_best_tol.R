##------------------PLotting best and tolerance parameters----------------------

#Results from best model:
x_data<-rf_mod05$trainingData$.outcome
y_best<-rf_mod05$finalModel$predictions

#results tolerancew model:
y_tol<-mod.tol$predictions

#load accu metric function
source("./0_Rscripts/0_fcn_accu.R")

accu.best<-accu.met(o=x_data,m=y_best)
accu.tol<-accu.met(o=x_data,m=y_tol)

mtry.tol<-mod.tol$mtry
mtry.best<-rf_mod05$finalModel$mtry


#setEPS()
#postscript(paste0("./figs/accu.",type,".best.tol.eps"), height=4,width=7)
##PLOTTING
#par(mfrow=c(1,2), mar =c(2,0,2,0), oma = c(2,4,0,1), mgp = c(1,0.5,0), tck = -0.01)
##Accuracy text 
y.lo<-0
y.hi<-0.5
txt.acc = 1.3
y.txt<-(y.hi-y.lo)/12
x<-0.3
#plot 1
plot(x_data,y_best, pch =19, cex=0.01, col="grey70", xlim = c(0,0.47), ylim = c(0,0.47), 
     #ylab=expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")),
     #xlab=expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")),
     xlab="",
     cex.axis=1.4,cex.lab=1.5,
     bty="n")
title(main = "'best'",line=0.2, col.main="blue3",cex.main=1.7)
box(col = "grey38", lwd = 0.5)
text(x =x, y= y.lo +y.txt*4, bquote(RMSE == .(accu.best[[1]])),cex =txt.acc) #-17 or-20
text(x =x, y= y.lo +y.txt*3, bquote(Bias == .(accu.best[[3]])),cex = txt.acc)
text(x =x, y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.best[[4]])),cex=txt.acc-0.1)
text(x =x, y=y.lo + y.txt, bquote(R^2 == .(round(accu.best[[2]],3))),cex =txt.acc)
text(x =0.11, y=0.45,bquote(mtry == .(mtry.best)),cex =txt.acc+0.2)
text(x =0.12, y=0.41,bquote(min.~node~size == 5),cex =txt.acc+0.2)
abline(0,1, lwd=0.5)
mtext(side=2,  expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")), cex = 1.4, line=2.5)

#plot2
plot(x_data,y_tol, pch =19, cex=0.01, col="grey70", xlim = c(0,0.47), ylim = c(0,0.47),
     #ylab=expression(paste("RF ", theta["rz"], " (",m^3*m^-3,")")),
     #xlab=expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")),
     yaxt="n",xlab="",
     cex.axis=1.4,cex.lab=1.5,
     bty="n")
title(main = "tradeoff'",line=0.2, col.main="red3",cex.main=1.7)
box(col = "grey38", lwd = 0.5)
text(x =x, y= y.lo +y.txt*4, bquote(RMSE == .(accu.tol[[1]])),cex =txt.acc) #-17 or-20
text(x =x, y= y.lo +y.txt*3, bquote(Bias == .(accu.tol[[3]])),cex = txt.acc)
text(x =x, y= y.lo +y.txt*2, bquote(Unb.RMSE == .(accu.tol[[4]])),cex=txt.acc-0.1)
text(x =x, y=y.lo + y.txt, bquote(R^2 == .(round(accu.tol[[2]],3))),cex =txt.acc)
text(x =0.11, y=0.45,bquote(mtry == .(mtry.tol)),cex =txt.acc+0.2)
text(x =0.12, y=0.41,bquote(min.~node~size == 5),cex =txt.acc+0.2)
abline(0,1,lwd=0.5)

mtext(side=1, outer=TRUE, expression(paste("in situ ", theta["rz"], " (",m^3*m^-3,")")), cex =1.4, line=0.8,adj=0.5)
#dev.off()