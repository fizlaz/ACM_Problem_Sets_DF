library(dplyr)
kNN <- function(features,labels,k=3,p=2,test=NA){
	features <- as.matrix(features)
	n <- nrow(features)

	if(length(test)==0){
		distMatrix <- matrix(NA,n,n)

		for (obs in 1:n){
			probe <- features[obs,]
			probeExpanded <- matrix(probe,nrow=n,ncol=ncol(features),byrow=TRUE)

			if(p %in% c(1,2)){
				distMatrix[obs,] <- (rowSums( (abs (features-probeExpanded) )^p))^(1/p)
			}
			else if (p==Inf){
				distMatrix[obs,] <- apply(abs(features-probeExpanded),1,max)
			}
		}

		neighbors <- apply(distMatrix,2,order) %>% t()
	
		#different method
		neighborslabels <- matrix(labels[neighbors],nrow(neighbors))
		prob <- rep(NA,n)
		predictedClasses <- rep(NA,n)
		for (obs in 1:n){
	  	predictedClasses[obs] <- names(sort(table( neighborslabels[obs,1:k] ),
	  													 decreasing=TRUE)[1])
	  	prob[obs] <- sort(table(neighborslabels[obs,1:k]), decreasing=TRUE)[1] /k
		}

		return(list(predLabels=predictedClasses,prob=prob))
		
	}
	
	####with test set
	else{
	  test <- as.matrix(test)
	  distmat <- matrix(NA,nrow = nrow(test), ncol = n)
	  
	  for (obs in 1:nrow(test)){
	    probe <- test[obs,]
	    probeExpanded <- matrix(probe,nrow=n,ncol=ncol(features),byrow=TRUE)
	    
	    if(p %in% c(1,2)){
	      distmat[obs,] <- (rowSums( (abs (features-probeExpanded) )^p))^(1/p)
	    }
	    else if (p==Inf){
	      distmat[obs,] <- apply(abs(features-probeExpanded),1,max)
	    }
	  }
	  
	  neighbors <- t(apply(distmat,1,order))
	  
	  neighborslabels <- matrix(labels[neighbors],nrow(neighbors))
	  prob <- rep(NA,n)
	  predictedClasses <- rep(NA,n)
	  for (obs in 1:n){
	    predictedClasses[obs] <- names(sort(table( neighborslabels[obs,1:k] ),
	     decreasing=TRUE)[1])
	    prob[obs] <- sort(table(neighborslabels[obs,1:k]), decreasing=TRUE)[1] /k
	  }
	  
	  return(list(predLabels=predictedClasses,prob=prob))
	  
	}
	
	####multiclass test
# 	ftable <- function(a){
# 	  b <- table(a)
# 	  return(b/sum(b))
# 	}
# 	nclass <- length(unique(labels))
# 	probmatrix <- matrix(0,nrow = n,ncol = nclass)
# 	for (obs in 1:n){
# 	  predictedClasses[obs] <- names(sort(table( neighborslabels[obs,1:k] ), decreasing=TRUE)[1])
# 	  probmatrix[obs,] <- ftable(factor( neighborslabels[obs,1:k], levels =  unique(labels) ))
# 	}
	####
	
}


