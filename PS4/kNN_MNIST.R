source("kNN.R")

dmodel <- kNN(features = MNIST_training[,2:256],labels = MNIST_training[,1], k=1, p=2)


library(class)

kmodel <- knn(train = MNIST_training[,2:257],test = MNIST_test,cl=MNIST_training[,1],k=3)



###training
library(caret)
set.seed(1234)
trainIndex <- createDataPartition(MNIST_training[,1], p=0.80, list=FALSE)
train <- MNIST_training[ trainIndex,]
test <- MNIST_training[-trainIndex,]


k <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 )
p <- 2
errorTest <- rep(NA, length(k))


for (iter in 1:length(k)) {
  # get the training error
#   errorTrain[iter] <- 1 - knn(train = train[,2:257],
#                               test = test[,2:257],
#                               cl=MNIST_training[,1],
#                               k = k[iter], p = p)$accuracy
  # get the test error
  predictedClasses <- knn( train = train[,2:257],
                           cl = train[,1],
                           test = test[2:257],
                           k = k[iter], p = p)
  errorTest[iter] <- mean(predictedClasses!=test[,1])
}

library(dplyr)
plottingData <-
  data.frame(k, Test=errorTest) %>%
  reshape2::melt(id="k",
                 variable.name = "Sample",
                 value.name = "Error")

ggplot(data = plottingData,
       aes(x = factor(k), y = Error, colour=Sample, group=Sample)) +
  geom_line() +
  geom_point() +
  xlab("k -Number of nearest neighbors") +
  ylab("Misclassification error") +
  theme_bw(base_size = 14, base_family = "Helvetica") +
  scale_color_manual("Sample",
                     values = c("Train" = "blue", "Test" = "red"))


kmodel <- knn(train = MNIST_training[,2:257],test = MNIST_test,cl=MNIST_training[,1],k=1)
preds <- as.vector(kmodel)
preds1 <- data.frame(kmodel)
write.csv(preds1, file = "MNIST_predictions.csv", row.names=FALSE)
