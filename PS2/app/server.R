# loading libraries
library(ggplot2)
library(scales)
library(mvtnorm)


shinyServer(function(input,output){

    #######################################
    ###my code bellow
    ######
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

    loanData <- function(noApproved=50, noDenied=50, muApproved, muDenied, sdApproved,
                     sdDenied, rhoApproved=-0.1, rhoDenied=0.6, seed=1111) {
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

    genData <- reactive({

        muApproved <- c()
        muApproved[1] <- input$muxa
        muApproved[2] <- input$muya
        
        muDenied <- c()
        muDenied[1] <- input$muxd
        muDenied[2] <- input$muyd

        sdApproved <- c()
        sdApproved[1] <- input$sdxa
        sdApproved[2] <- input$sdya

        sdDenied <- c()
        sdDenied[1] <- input$sdxd
        sdDenied[2] <- input$sdyd

        gen <- loanData(muApproved=muApproved,muDenied=muDenied,
            sdApproved=sdApproved,sdDenied=sdDenied)

        return(gen)        
    })

    generatePlot <- function(){
        loanDf <- data.frame(genData())

        datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
        weights <- coef(datafit)[c("solvency", "PIratio")]
        bias <- coef(datafit)[1]
        intercept <- (-bias + 0.5)/weights["PIratio"]
        slope <- -(weights["solvency"]/weights["PIratio"])

        plot <- ggplot(data = loanDf, aes(x = solvency, y = PIratio,
                colour=deny, fill=deny)) +
                geom_point() +
                xlab("solvency") +
                ylab("Weight") +
                theme_bw() +
                geom_abline(intercept = intercept, slope = slope)

        return(plot)
    }

    output$plot1 <- renderPlot({
        generatePlot()}
    )

    generateMatrix <- function(){
        loanDf <- data.frame(genData())

        loanDf <- cbind(loanDf,
        target1 = c(rep(0, 50), rep(1, 50)),
        target2 = c(rep(1, 50), rep(0, 50))
        )

        X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)),
                    loanDf[,c("PIratio", "solvency")]))
        Y <- cbind(target1 = c(rep(0, 50), rep(1, 50)),
                    target2 = c(rep(1, 50), rep(0, 50))
                    )
        weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

        predictions <- X %*% weightsOptim

        denied <- (predictions==apply(predictions, 1, max))[,1]

        predictedLabels <- ifelse(denied, "Denied", "Approved")

        confMatrixFreq <- table(loanDf$deny, predictedLabels)

        return(confMatrixFreq)
    }

    output$conf <- renderPrint({
        generateMatrix()
        })

    ###my code above
    #######################################

})

