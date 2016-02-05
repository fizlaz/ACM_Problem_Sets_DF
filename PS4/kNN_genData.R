source("ZsuzsaGenData.R")
source("kNN.R")

data <- genUrban(N=1000,saveData=FALSE,savePlot=FALSE)

px1 <- seq(from = -0, to = 1, length.out = 40 )
px2 <- seq(from = -0, to = 1, length.out = 25 )
xnew <- expand.grid(x1 = px1, x2 = px2)


model <- kNN(data[,1:2],data[,3],k=15,p=2,test = xnew)
prob <- model$prob
prob <- ifelse(model$predLabels=="1", prob, 1-prob)
prob15 <- matrix(prob, nrow = length(px1), ncol = length(px2))


par(mar = rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(data[,1:2], col=ifelse(data[,3]==1, "coral", "cornflowerblue"))
points(xnew, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

##saving plot
cairo_pdf("plot.pdf")

par(mar = rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(data[,1:2], col=ifelse(data[,3]==1, "coral", "cornflowerblue"))
points(xnew, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

dev.off()


##model without test set
sum(model$predLabels==data[,3])
model1 <- kNN(features=data[,1:2],labels=data[,3],k=15,p=2)
sum(model1$predLabels==data[,3])

preds <- cbind(data,model1)
write.csv(preds, file = "predictions.csv", row.names=FALSE)


##
model2 <- kNN(features=data[,1:2],labels=data[,4],k=15,p=2)
sum(model2$predLabels==data[,4])
