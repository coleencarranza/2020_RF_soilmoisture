accu.met<- function(o,m){
  rmse<-round(Metrics::rmse(o,m),4)
  bias<-round(mean(m) - mean(o),4)
  cort<-cor.test(as.vector(o), as.vector(m))
  unb<-round(m-bias,4)
  unb.rmse<-round(Metrics::rmse(o,unb),4)
  mod.lm <- lm(m ~ o)
  R2 <- summary(mod.lm)$r.squared
  combi<-list(rmse,R2,bias,unb.rmse)
  return(combi)
}
