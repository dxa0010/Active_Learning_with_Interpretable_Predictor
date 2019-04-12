library(data.table)
library(foreach)
library(dplyr)
library(class)
library(Metrics)
library(randomForest)

trainSet <- c(trainSet,gainSet)
xrfeer <- NULL
eerError <- NULL

pb <- txtProgressBar(min = 1, max = ir, style = 3)
foreach(j = 1:ir) %do% {
    setTxtProgressBar(pb, j)
    rloss <- foreach(q = 1:length(eerPool), .combine = "c") %do% {
        xinp <- eerPool[q]

        data <- usingDataset[c(trainSet,xrfeer),]
        rmodel <- randomForest(Y~.,data= data)
        yt <- predict(rmodel,usingDataset[valSet,fs:target])
        tError <- mse(usingDataset[valSet,Y], yt)

        bootstrapModel <- randomForest(Y~.,data= data, 2000)
        #calc x in pool
        yp <- predict(bootstrapModel,usingDataset[xinp, fs:target])
        addata <- cbind(usingDataset[xinp, fs:target],Y=yp)
        data <- rbind(data,addata)
        rmodel <- randomForest(Y~.,data= data)
        ytp <- predict(rmodel,usingDataset[valSet, fs:target])
        tpError <- mse(usingDataset[valSet,Y], ytp)

        loss<-(tError-tpError)        
    }
    
    prime <- which.min(rloss)
    xrfeer <- c(xrfeer, eerPool[prime])
    eerPool <- eerPool[-prime]
    rmodel <- randomForest(Y ~ ., usingDataset[c(trainSet, xrfeer), ])
    predeer <- predict(rmodel, usingDataset[valSet, fs:target])
    ErrorC <- rmse(predeer, usingDataset[valSet, Y])
    eerError <- c(eerError, ErrorC)
    
}
