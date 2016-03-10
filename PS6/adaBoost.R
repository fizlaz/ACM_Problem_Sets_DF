adaBoost <- function(formula,data,depth,noTrees){
  library(rpart)

  y <- data[,all.vars(formula)[1]] #extract target variable vector
  
  y <- ifelse(y==1,1,-1) #transform levels to -1 and 1

  dtmat <- vector(mode = "list",length = noTrees) #for storing tree output
  w <- rep(1/nrow(data),nrow(data)) #initial weight vector
  alpha <- rep(NA, noTrees) #for storing alphas
  predictions <-matrix(NA,nrow=nrow(data),ncol=noTrees)#for storing predictions
  err <- rep(NA, noTrees)#for storing individual tree errors
  error <- rep(NA,noTrees)#for storing global error
  environment(formula) <- environment()#passsing formula to current environment
  for(i in 1:noTrees){
    dt <- rpart(formula=formula, data = data, weights = w, method = "class",
                control = list(maxdepth = depth))
    dtmat[[i]] <- dt
    predictions[,i] <- predict(dt,newdata = data,type="class")
    predictions[,i] <- ifelse(predictions[,i]==2,1,-1)#rpart outputs 1 and 2
    err[i] <- sum(w*(predictions[,i]!=y)) / sum(w)
    alpha[i] <- log((1-err[i])/err[i])
    w <- w * exp(alpha[i] * (predictions[,i]!=y))
    #final classifier formula below. Weighing all individual tree results
    pred <- sign(rowSums(t(alpha[1:i]*t(predictions[,1:i]))))
    error[i] <- 1 - sum(pred==y)/length(y)
    print(c(i,err[i],sum(predictions[,i]!=y)/length(y),error[i]))
  }
  
  predLabels <- sign(rowSums(t(alpha*t(predictions))))
  
  return(list(predLabels=predLabels,dtmat=dtmat,trainerror=error,alpha=alpha))
}
