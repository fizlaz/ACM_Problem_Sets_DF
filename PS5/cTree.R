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

  X <- matrix(data[,features])
  Y <- data[,all.vars(formula)[1]]
  K <- depth

  #multiclass
  findThresholdmulti <- function(x,y){
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
    bestThreshold <- sample(bestThreshold, 1)
    labels <- splitLabels[which(thresholds==bestThreshold),]
    # print(cbind(minError, bestThreshold, labels))
    #return probabilities
    prob <- splitProbs[which(thresholds==bestThreshold),]
    labvec <- c(rep(labels[1],length(which(x<bestThreshold))),
                rep(labels[2],length(which(x>=bestThreshold))))
    probvec <- c(rep(prob[1],length(which(x<bestThreshold))),
                 rep(prob[2],length(which(x>=bestThreshold))))

    return(list(thres = bestThreshold, err = minError,labels = labels,prob=prob,
                labvec=labvec,probvec=probvec))
  }#end of findThresholdmulti

  #multidimensional
  findfeature <- function(X,Y){
    errors <- rep(NA,ncol(X))
    labels <- matrix(NA,nrow=ncol(X),ncol=2)
    prob <- matrix(NA,nrow=ncol(X),ncol=2)
    potthresholds <- rep(NA,ncol(X))
    labvec <- matrix(NA,nrow=length(Y),ncol=ncol(X))
    probvec <- matrix(NA,nrow=length(Y),ncol=ncol(X))
    for(i in 1:ncol(X)){
      errors[i] <- findThresholdmulti$err
      labels[i,] <- findThresholdmulti$labels
      prob[i,] <- findThresholdmulti$prob
      potthresholds[i] <- findThresholdmulti$thres
      labvec[,i] <- findThresholdmulti$labvec
      probvec[,i] <- findThresholdmulti$probvec
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

  recursivetree <- function(K,X,Y,depth=1,minPoints=minPoints){
    depth <- depth
    if(depth<=K){
      if(length(Y)>=minPoints){
        if(length(unique(Y))==1){
          labels <-rep(Y[1],length(Y))
          prob <- rep(1,length(Y))
          return(list(labels=labels,prob=prob))
        } else{
          threshold <- findfeature(X,Y)$thres
          labels <- findfeature(X,Y)$labvec
          prob <- findfeature(X,Y)$probvec
          featindex <- findfeature(X,Y)$featindex
          left <- which(X[,featindex]<threshold)
          if(!is.null(recursivetree(X=X[left,],Y=Y[left],depth=(depth+1)))){
            labels[left] <-recursivetree(X=X[left,],Y=Y[left],
                                        depth=(depth+1))$labels
            prob[left]<-recursivetree(X=X[left,],Y=Y[left],depth=(depth+1))$prob
          }

          right <- which(X[,featindex]>=threshold)
          if(!is.null(recursivetree(X=X[right,],Y=Y[right],depth=(depth+1)))){
            labels[right]<-recursivetree(X=X[right,],Y=Y[right],
                                        depth=(depth+1))$labels
            prob[right]<-recursivetree(X=X[right,],Y=Y[right],
                                          depth=(depth+1))$prob
          }

          return(list(labels=labels,prob=prob))
        }
      }
    }
  }

  result <- recursivetree(K=K,X=X,Y=Y,depth=1,minPoints=minPoints)
  predLabels <- result$labels
  return(list(predLabels=predLabels,prob=prob))
}#cTree end

