f <- function(obs=100,savecsv=TRUE,savepdf=TRUE){
  library(mvtnorm)
  library(ggplot2)
  
  if(savecsv=TRUE){
    write.csv(dataaa, file = "dataset.csv", row.names=FALSE)
  }
  
  if(savepdf=TRUE){
    pdf("dataPlot.pdf")
    plot()
    dev.off() #return output to terminal
  }
  
  return(list())
}
