xfactor <- function(obsx=1000,obsy=1000,obsz=1000,mux=c(2,2,2),rhox=0.9,sdx1=2,savecsv=TRUE,savepdf=TRUE,seed=1234){
  
  library(mvtnorm)
  library(scatterplot3d)
  
  set.seed(seed)

  covTerm <- rhox * sdx1^2
  cvc <- matrix(c(sdx1^2,covTerm,covTerm,covTerm,sdx1^2,covTerm,covTerm,covTerm,sdx1^2),ncol = 3)
  x3d <- rmvnorm(obsx,mean = mux,sigma = cvc)

  cvc <- matrix(c(sdx1^2,-covTerm,-covTerm,-covTerm,sdx1^2,covTerm,-covTerm,covTerm,sdx1^2),ncol = 3)
  y3d <- rmvnorm(obsy,mean = mux,sigma = cvc)

  cvc <- matrix(c(sdx1^2,covTerm,-covTerm,covTerm,sdx1^2,-covTerm,-covTerm,-covTerm,sdx1^2),ncol = 3)
  z3d <- rmvnorm(obsz,mean = mux,sigma = cvc)
  
  lab3d <- c(rep(1,obsx),rep(2,obsy),rep(3,obsz))
  lab3da <- c(rep("a",obsx),rep("b",obsy),rep("c",obsz))
  
  dat3d <- data.frame(rbind(x3d,y3d,z3d))
  dat3d <- data.frame(dat3d,lab3d)
  dat3d <- data.frame(dat3d,lab3da)
  
  
  if(savecsv==TRUE){
    write.csv(dat3d, file = "dataset.csv", row.names=FALSE)
  }
  
  if(savepdf==TRUE){
    pdf("dataPlot.pdf")
    s3d <- with(dat3d, scatterplot3d(X1, X2, X3, color = as.numeric(lab3d),
                                     col.axis = "blue",col.grid = "lightblue"))
    legend(s3d$xyz.convert(11, 0.7, 0.5), pch = 19, yjust=0,
           legend = levels(dat3d$lab3da), col = seq_along(levels(dat3d$lab3da)))
    dev.off() 
  }
  
  return(dat3d)
}
