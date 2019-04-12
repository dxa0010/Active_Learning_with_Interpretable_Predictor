# qbc impre 
library(data.table)
library(foreach)
library(dplyr)
library(class)
library(Metrics)
library(randomForest)

xqbc <- NULL
qbbError <- NULL

B <- 50
rfmodels <- list()
usingDataset <- data.matrix(usingDataset) 
trainSet <- c(trainSet, gainSet)
param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread=1,   objective = "reg:linear")


pb <- txtProgressBar(min = 0, max = ir, style = 3)
foreach(j = 1:ir) %do% {
    setTxtProgressBar(pb, j)

    data <- usingDataset[c(xqbc,trainSet),]
    samplesize <- dim(data)[1]
    
    rftrees <- foreach(t = 1:B) %do% {      
            bootstrap <- sample(1:samplesize, samplesize, replace = T)  
            bootstrap <- data[bootstrap, ]
            # rtree <-randomForest(Y ~ ., data = bootstrap)
            # rtree <- gbm(Y ~ ., data = bootstrap, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
            # best.iter_rtree <- gbm.perf(modelE,method="cv") 
            rtree <- xgboost(param = param, data = xgb.DMatrix((bootstrap[, fs:target]), label = (bootstrap[,"Y" , drop=FALSE])), nrounds = 1000, verbose = 0)
    }

    Vars <- foreach(q = 1:length(qbcPool), .combine="rbind") %do% {

        htx <- foreach(t = 1:B, .combine = "c") %do% {      
            # bootstrap <- sample(1:samplesize, samplesize, replace = T)  
            # bootstrap <- data[bootstrap, ]
            # rtree <-randomForest(Y ~ ., data = bootstrap)
            yt <- predict(rftrees[[t]], (usingDataset[qbcPool[q], fs:target, drop=FALSE]))
        }
        meanhtx <- mean(htx)
        Var <- mean((htx - meanhtx)^2)
        c(Var, meanhtx)
    }
    
    prime <- which.max(Vars[, 1])
    
    xqbc <- c(xqbc, qbcPool[prime])
    qbcPool <- qbcPool[-prime]
    data <- usingDataset[c(xqbc,trainSet),]
    # modelq <- randomForest(Y ~ ., data = data)
    # modelq <- gbm(Y ~ ., data = data, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter_q <- gbm.perf(modelE,method="cv") 
    modelq <- xgboost(param = param, data = xgb.DMatrix((data[,fs:target]), label = (data[, "Y", drop=FALSE])), nrounds = 1000, verbose = 0)
    qbbpr <- predict(modelq, (usingDataset[valSet, fs:target]))
    ErrorC <- rmse(qbbpr, usingDataset[valSet, "Y"])
    qbbError <- c(qbbError, ErrorC)
    
}