cTree <- function(formula,data,depth,minPoints=10,costFnc="Entropy"){

  MissError <- function(prob){
    MissError <- 1-apply(prob,1,max)
    return(MissError)
  }

  Gini <- function(prob){
    Gini <- rowSums(prob*(1-prob))
    return(Gini)
  }

  CrossEntropy <- function(prob){
    CrossEntropy <- -rowSums(prob*log(prob))
    return(CrossEntropy)
  }

  if(all.vars(formula)[2] != "."){
      features <- which(colnames(data) %in% all.vars(formula)[-1])#get indices
    } else {
      features <- which(colnames(data) != all.vars(formula)[1])
    }

  x <- as.matrix(data[,features])
  y <- data[,all.vars(formula)[1]]
  maxdepth <- depth
  costFnc <- costFnc

  #multiclass
  findThresholdmulti <- function(x,y,costFnc){
    noPoints<-length(x)
    errors <- rep(NA,noPoints-1)
    thresholds <- rep(NA,noPoints-1)
    splitLabels <- matrix(NA,ncol=2,nrow=(noPoints-1))
    splitProbs <- matrix(NA,ncol=2,nrow=(noPoints-1))

    # we go sequentially over each point and cut between that point and the
    # closest neighbor
    for (idx in 1:(noPoints-1)){
      # locate a potential threshold, a split between two points
      potThres <- mean(x[idx:(idx+1)])

      # check the classification error, when both sides,
      # are classified with mean label
      predictedClasses <- rep(NA,noPoints)
      predictedClasses[x<potThres] <- names(sort(table(y[x<potThres]),
                               decreasing=TRUE)[1])
      predictedClasses[x>=potThres] <- names(sort(table(y[x>=potThres]),
                               decreasing=TRUE)[1])

      probleft <- sort(table(y[x<potThres]), decreasing=TRUE)[1] /
                  length(y[x<potThres])
      probright <- sort(table(y[x>=potThres]), decreasing=TRUE)[1] /
                   length(y[x>=potThres])

      prob <- cbind(probleft,probright)

      if(costFnc=="Entropy"){

        misError <- CrossEntropy(prob)

      } else if(costFnc=="ME"){

        misError <- MissError(prob)

      } else if(costFnc=="Gini"){

        misError <- Gini(prob)

      } else{stop("error in cost function argument")}

      # recording the accuracy, thresholds and labels of the splitted interval
      errors[idx] <- misError
      thresholds[idx] <- potThres
      splitLabels[idx,] <- c(predictedClasses[x<potThres][1],
                             predictedClasses[x>potThres][1])
      splitProbs[idx,] <- prob
    }#end of for idx
    # print(cbind(errors, thresholds, splitLabels))

    # next we find the minimum and the best threshold
    minError <- min(errors)
    bestThreshold <- thresholds[which(errors==minError)]
    # if more than 1 threshold has the same accuracy we choose one randomly
    index <- sample(which(sample(bestThreshold,1)==bestThreshold),1)
    bestThreshold <- thresholds[index]
    #bestThreshold <- sample(bestThreshold, 1)
    labels <- splitLabels[index,]
    # print(cbind(minError, bestThreshold, labels))
    #return probabilities
    prob <- splitProbs[index,]
    labvec <- c(rep(labels[1],length(which(x<bestThreshold))),
                rep(labels[2],length(which(x>=bestThreshold))))
    probvec <- c(rep(prob[1],length(which(x<bestThreshold))),
                 rep(prob[2],length(which(x>=bestThreshold))))

    return(list(thres = bestThreshold, err = minError,labels = labels,prob=prob,
                labvec=labvec,probvec=probvec))
  }#end of findThresholdmulti

  #multidimensional
  findfeature <- function(X,Y,costFnc){
    X <- as.matrix(X)
    costFnc <- costFnc
    errors <- rep(NA,ncol(X))
    labels <- matrix(NA,nrow=ncol(X),ncol=2)
    prob <- matrix(NA,nrow=ncol(X),ncol=2)
    potthresholds <- rep(NA,ncol(X))
    labvec <- matrix(NA,nrow=length(Y),ncol=ncol(X))
    probvec <- matrix(NA,nrow=length(Y),ncol=ncol(X))
    for(i in 1:ncol(X)){
      errors[i] <- findThresholdmulti(X[,i],Y,costFnc)$err
      labels[i,] <- findThresholdmulti(X[,i],Y,costFnc)$labels
      prob[i,] <- findThresholdmulti(X[,i],Y,costFnc)$prob
      potthresholds[i] <- findThresholdmulti(X[,i],Y,costFnc)$thres
      labvec[,i] <- findThresholdmulti(X[,i],Y,costFnc)$labvec
      probvec[,i] <- findThresholdmulti(X[,i],Y,costFnc)$probvec
    }
    minError <- min(errors)
    featindex <- sample(which(errors==minError),1)
    bestlabels <- labels[featindex,]
    bestprob <- prob[featindex,]
    bestthres <- potthresholds[featindex]
    bestlabvec <- labvec[,featindex]
    bestprobvec <- probvec[,featindex]

    return(list(labels=bestlabels,prob=bestprob,thres=bestthres,
                labvec=bestlabvec,featindex=featindex,probvec=bestprobvec))
  }

  #predLabels <- rep(NA,length(Y))

  recursivetree <- function(K,X,Y,depth=1,minPoints=minPoints,costFnc){
    depth <- depth
    k <- K
    mp <- minPoints
    costFnc <- costFnc
    X <- as.matrix(X)
    Y <- Y
    if(depth<=k){
      if(length(Y)>=minPoints){
        if(length(unique(Y))==1){
          labels <-rep(Y[1],length(Y))
          prob <- rep(1,length(Y))
          return(list(labels=labels,prob=prob))
        } else{
          threshold <- findfeature(X,Y,costFnc)$thres
          labels <- findfeature(X,Y,costFnc)$labvec
          prob <- findfeature(X,Y,costFnc)$probvec
          featindex <- findfeature(X,Y,costFnc)$featindex
          print(featindex)
          left <- which(X[,featindex]<threshold)
          leftres <- recursivetree(K=k,X=X[left,],Y=Y[left], depth=(depth+1),
                                   minPoints=mp,costFnc)
          if(!is.null(leftres)){
            labels[left] <- leftres$labels
            prob[left]<- leftres$prob
          }

          right <- which(X[,featindex]>=threshold)
          rightres <- recursivetree(K=k,X=X[right,],Y=Y[right], depth=(depth+1),
                                    minPoints=mp,costFnc)
          if(!is.null(rightres)){
            labels[right]<- rightres$labels
            prob[right]<- rightres$prob
          }

          return(list(labels=labels,prob=prob))
        }
      }
    }
  }

  result <- recursivetree(K=maxdepth,X=x,Y=y,depth=1,minPoints=minPoints,
                          costFnc)
  predLabels <- result$labels
  prob <- result$prob
  return(list(predLabels=predLabels,prob=prob))
}#cTree end
