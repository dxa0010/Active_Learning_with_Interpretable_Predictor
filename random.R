# maybe new My idea: predict error reduction imprement

library(data.table)
library(foreach)
library(dplyr)
library(class)
library(Metrics)
library(randomForest)
library(xgboost)

xprime = c(NULL)

randError  <- NULL
models <- list()

param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread = 1, objective = "reg:linear")

# setting for god
# godTrain <- trainSet
# godPool <- poolSet
# godPrime <- godErrors <- NULL
# goddif <- godErrorslist <- list()

pb <- txtProgressBar(min = 1, max = ir , style = 3)
usingDataset <- data.matrix(usingDataset)
foreach(j = 1:ir) %do% {
    setTxtProgressBar(pb, j)

    # compair Random
    randPool <- sample(randPool, length(randPool), replace = FALSE)
    dataR <- usingDataset[c(trainSet, gainSet, randPool[1:j]), ]
    # modelr <- randomForest(Y ~ ., data = dataR,ntree = 1000)
    # modelr <- gbm(Y ~ ., data = dataR, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter_r <- gbm.perf(modelE,method="cv") 
    modelr <- xgboost(param = param, data = xgb.DMatrix(dataR[,fs:target], label = dataR[,"Y"]), nrounds = 1000, verbose = 0)
    pred <- predict(modelr, data.matrix(usingDataset[valSet, fs:target]))
    randError <- c(randError, rmse(pred, usingDataset[valSet, "Y"]))
    
}
