adaBoost.predict <- function(object,test,ytest){
    
  y <- ifelse(ytest==1,1,-1)
  
  error <- rep(NA,length(object$dtmat))
  alpha <- object$alpha
  predictions <- matrix(NA,nrow = nrow(test), ncol = length(object$dtmat))
  
  for(i in 1:length(object$dtmat)){
    predictions[,i] <- predict(object$dtmat[[i]],test, type="class")
    predictions[,i] <- ifelse(predictions[,i]==2,1,-1)
    pred <- sign(rowSums(t(alpha[1:i]*t(predictions[,1:i]))))
    error[i] <- 1 - sum(pred==y)/length(y)
  }
  
  return(list(testerror=error))
}

#using caret package to make balanced folds
library(caret)
set.seed(1234)
trainIndex <- createDataPartition(spambase$V58, p=0.80, list=FALSE)
data_train <- spambase[ trainIndex,]
data_test <- spambase[-trainIndex,]


formula <- V58~.
pr1 <- adaBoost(formula=formula,data = data_train,depth = 20, noTrees = 80)


tsterr <- adaBoost.predict(pr1,data_test[,-58],data_test[,58])


library(ada)

ada <- ada(x=data_train[,-58],y=data_train[,58],test.x = data_test[,-58],
           test.y = data_test[,58],loss = "ada",iter = 80)

#add <- addtest(ada,test.x = data_test[,-58],test.y = data_test[,58])

adatr <- ada[[1]]$errs[,1]
adats <- ada[[1]]$errs[,3]

pdf("adaErrors.pdf")
plot(pr1$trainerror,type = "l", col="blue",main = "adaBoost train and test error",
     ylim = c(0.00,0.12))
lines(tsterr$testerror, col="red")
lines(adatr,col="blue",type = "p",pch=16)
lines(adats,col="red",type = "p",pch=16)
legend(50,0.12, 
      legend = c("train error","test error","ada::ada train","ada::ada test"),
      pt.cex = 1,
      cex = 1,
      lty=c(1,1,NA,NA), 
      pch=c(NA,NA,16,16),
      lwd=c(2.5,2.5,2.5,2.5),col=c("blue","red","blue","red")) 
dev.off()


