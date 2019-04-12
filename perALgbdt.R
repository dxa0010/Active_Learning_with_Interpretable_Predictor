# maybe new My idea: predict error reduction imprement

library(data.table)
library(foreach)
library(dplyr)
library(class)
library(Metrics)
library(randomForest)
library(xgboost)


xprime = c(NULL)

usingDataset <- data.matrix(usingDataset) 
perError <- gainError <- gainC <- NULL
models <-modelf <- list()
Df_s<- list()
importance_gain <- importance_f <- predEs <-NULL

param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread=1 , objective = "reg:linear")

# setting for god
# godTrain <- trainSet
# godPool <- poolSet
# godPrime <- godErrors <- NULL
# goddif <- godErrorslist <- list()

init_Dgt.size <- length(Dgt)
Dgv.size <- length(Dgv)
trainSet.size <- length(trainSet)
pb <- txtProgressBar(min = 0, max = ir*init_Dgt.size, style = 3)

foreach(j = 1:ir) %do% {
    # setTxtProgressBar(pb, j)
    
    Dgt.size <- length(Dgt)
    # Dgt.size <- init_Dgt.size + j -1  
    
    Df.size <- trainSet.size + j -1    

    # \shuffle Dgt Dgv and Trainset
    shuffled <- sample(c(trainSet, gainSet, xprime), length(c(trainSet, gainSet, xprime)), replace = F)
    Df <- shuffled[1:Df.size]
    Df_s <-c(Df_s, list(Df)) 
    Dgt <- shuffled[(Df.size+1):(Df.size+Dgt.size)]
    Dgv <- setdiff(shuffled,c(Df,Dgt))

    
    data <- usingDataset[c(Df, xprime), ]
    # modely <- randomForest(Y ~ ., data = data,ntree = 1000)
    # modely <- gbm(Y ~ ., data = data, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter_y <- gbm.perf(modelE,method="cv")
    modely <- xgboost(param = param, data = xgb.DMatrix(data[,fs:target], label = data[,"Y",drop = FALSE]), nrounds = 1000, verbose = 0)   
    # modelf <- c(modelf, list(modely))
    predO <- predict(modely, usingDataset[Dgv, fs:target])
    ErrorO <- mse(predO, usingDataset[Dgv, "Y"])
    
    # compute gain
    gain <- foreach(i = 1:length(Dgt), .combine = "c") %do% {
        setTxtProgressBar(pb, (j - 1) * length(Dgt) + i)
        data <- usingDataset[c(Df, Dgt[i]), ]
        # modeli <- randomForest(Y ~ ., data = data,ntree = 1000)
        # modeli <- gbm(Y ~ ., data = data, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
        # best.iter_i <- gbm.perf(modelE,method="cv")
        modeli <- xgboost(param = param, data = xgb.DMatrix(data[,fs:target], label = data[,"Y",drop = FALSE]), nrounds = 1000, verbose = 0)   
        predPlus <- predict(modeli, (usingDataset[Dgv, fs:target]))
        ErrorPlus <- mse(predPlus, usingDataset[Dgv, "Y"])
        Error <- ErrorO - ErrorPlus
    }
    
    # model g : predict gain
    trainError <- cbind(usingDataset[Dgt, fs:target], gain)
    # modelE <- gbm(trainError$gain ~ ., data = trainError, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter <- gbm.perf(modelE,method="cv")
    # predE <- predict(modelE, usingDataset[poolSet, fs:target], best.iter)
    # predEs <- cbind(predEs,predE)

    modelE <- randomForest(gain ~ ., data = trainError, n.tree = 1000)
    # modelE <- gbm(gain ~ ., data = trainError, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter_E <- gbm.perf(modelE,method="cv")  
    # modelE <- xgboost(param = param, data = xgb.DMatrix(data.matrix(trainError %>% select(-contains("gain"))), label = data.matrix(trainError$gain)), nrounds = 1000, verbose = 0)    
    # modelg <- c(modelg, list(modelE))
    # importance_gain <- cbind(importance_gain, importance(modelE))
    predE <- predict(modelE, usingDataset[poolSet, fs:target])
    # predEs <- cbind(predEs,predE)

    sortedGain <- sort(predE, index = T, decreasing = T)
    
    prime <- sortedGain$ix[1]
    gainC <- sortedGain$x[1]
    
    xprime <- c(xprime, poolSet[prime])
    poolSet <- poolSet[-prime]
    dataT <- usingDataset[c(trainSet, xprime, gainSet), ]
    # modelt <- randomForest(Y ~ ., data = dataT,ntree = 1000)
    # modelt <- gbm(Y ~ ., data = dataT, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    # best.iter_E <- gbm.perf(modelE,method="cv")

    modelt <- xgboost(param = param, data = xgb.DMatrix(dataT[,fs:target], label = dataT[,"Y",drop = FALSE]), nrounds = 1000, verbose = 0)    
    # importance_f <- cbind(importance_f, importance(modelt))
    #models <- c(models, modelt)
    pred <- predict(modelt, usingDataset[valSet, fs:target])
    ErrorC <- rmse(pred, usingDataset[valSet, "Y"])
    perError <- c(perError, ErrorC)

}
