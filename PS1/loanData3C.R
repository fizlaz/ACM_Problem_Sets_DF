library(mvtnorm)
library(ggplot2)
library(extrafont)
loadfonts()

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

loanDf <- loanData(noApproved=50, noDenied=50, c(4, 150), c(10, 100), 
                   c(1,20), c(2,30), -0.1, 0.6, 1221)

ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

ggplot(data = loanDf, 
       aes(x = PIratio, y = target)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("PIratio", limit = c(1, 16), 
                     breaks = seq(1, 16, 1), expand = c(0, 0)) +
  scale_y_continuous("deny", limit = c(-0.4, 1.8), 
                     breaks = seq(-0.4, 1.8, 0.2), 
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, size = 0.3, linetype = 2) +
  annotate("text", x = 13, y = 0.1, label = "Approved", family = "Arial",
           size = 3.15) +
  geom_hline(yintercept = 1, size = 0.3, linetype = 2) +
  annotate("text", x = 3, y = 1.1, label = "Denied", family = "Arial",
           size = 3.15) +
  geom_hline(yintercept = 0.5, size = 0.3, linetype = 2) +
  annotate("text", x = 13, y = 0.6, label = "Threshold", family = "Arial",
           size = 3.15) +
  theme_bw() +
  theme(text = element_text(family = "Arial"))


###########my code
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved, 
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target1 = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
  target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
  target3 = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
  loanDf <- data.frame(loanDf, deny, target1, target2, target3)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target1", "target2", "target3")
  return(loanDf)
}

loanDf3 <- loanData(noApproved=50, noDenied=50, noUndecided = 50, c(4, 150), c(10, 100), c(10,200), 
                   c(1,20), c(2,30), c(1,15), -0.1, 0.6, 0.6, 1221)

ggplot(data = loanDf3, 
       aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

datafit1 <- lm(target1 ~ solvency + PIratio, data=loanDf3)

datafit2 <- lm(target2 ~ solvency + PIratio, data=loanDf3)

datafit3 <- lm(target3 ~ solvency + PIratio, data=loanDf3)

weights1 <- coef(datafit1)[c("solvency", "PIratio")]
bias1 <- coef(datafit1)[1]

weights2 <- coef(datafit2)[c("solvency", "PIratio")]
bias2 <- coef(datafit2)[1]

weights3 <- coef(datafit3)[c("solvency", "PIratio")]
bias3 <- coef(datafit3)[1]

#########plot with line below

intercept1 <- (-bias1 + 0.5)/weights1["PIratio"]
slope1 <- -(weights1["solvency"]/weights1["PIratio"])

intercept2 <- (-bias2 + 0.5)/weights2["PIratio"]
slope2 <- -(weights2["solvency"]/weights2["PIratio"])

intercept3 <- (-bias3 + 0.5)/weights3["PIratio"]
slope3 <- -(weights3["solvency"]/weights3["PIratio"])

# illustrating the data, now with the boundary, we use geom_abline(), 
# this will work only for lines
ggplot(data = loanDf3, aes(x = solvency, y = PIratio, 
                          colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial")) +
  geom_abline(intercept = intercept1, slope = slope1) +
  geom_abline(intercept = intercept2, slope = slope2) +
  geom_abline(intercept = intercept3, slope = slope3)

#######

X <- as.matrix(cbind(ind=rep(1, nrow(loanDf3)), 
                     loanDf3[,c("PIratio", "solvency")]))

Y <- as.matrix(loanDf3[,4:6])

weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

predictions <- X %*% weightsOptim
head(predictions)

####classifying
predictedLabels <- rep(NA,150)

label1 <- (predictions==apply(predictions, 1, max))[,1]
predictedLabels <- ifelse(label1, "Approved",predictedLabels)

label2 <- (predictions==apply(predictions, 1, max))[,2]
predictedLabels <- ifelse(label2, "Denied",predictedLabels)

label3 <- (predictions==apply(predictions, 1, max))[,3]
predictedLabels <- ifelse(label3, "Undecided",predictedLabels)

######

confMatrixFreq <- table(loanDf3$deny, predictedLabels)
confMatrixFreq

confMatrixProp <- prop.table(confMatrixFreq, 1)
confMatrixProp

