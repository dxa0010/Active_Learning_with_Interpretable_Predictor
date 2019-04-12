# learning active learning

xprime = c(NULL)

param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread=1,  objective = "reg:linear")


lalError <- gainError <- gainC <- NULL
gains <- read.table("./env/ForGain_imp_gbdt.csv", sep=",", header=T)
gains <- data.frame(gains)
# colnames(gains) <- c("rsq", "treesize", "mse", "minidist", "maxidist", "Var", "Error")
# modelE <- gbm(Error ~ ., data = gains, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
# best.iter <- gbm.perf(modelE,method="cv")
modelE <- xgboost(param = param, data = xgb.DMatrix(data.matrix(gains %>% select(-contains("Error"))), label = data.matrix(gains$Error)), nrounds = 1000, verbose = 0)

usingDataset <- data.matrix(usingDataset) 
pb <- txtProgressBar(min = 0, max = ir, style = 3)

foreach(j = 1:ir) %do% {
    
    setTxtProgressBar(pb, j)
    trainSet <- c(trainSet, gainSet)
    # model g : predict gain
    labeled <- usingDataset[c(trainSet,xprime), ]
    # taumodel <- randomForest(Y ~ ., data = labeled ,ntree = 1000)
    taumodel <- xgb.cv(nfold = 5, param = param, data = xgb.DMatrix(labeled[,fs:target], label = labeled[,"Y", drop=FALSE]), nrounds = 1000, verbose = 0,callbacks = list(cb.evaluation.log()))

    # rsq <- mean(taumodel$rsq)
    # treesize <- mean(treesize(taumodel))
    # mse <- mean(taumodel$mse)

    train_minrmse <- min(scale(taumodel$evaluation_log$train_rmse_mean))
    # train_meanrmse <- mean(taumodel$evaluation_log$train_rmse_mean)
    train_maxrmse <- max(scale(taumodel$evaluation_log$train_rmse_mean))
    train_argminrmse <- which.min(taumodel$evaluation_log$train_rmse_mean)
    train_minstd <- min(scale(taumodel$evaluation_log$train_rmse_std))
    # train_meanstd <- mean(taumodel$evaluation_log$train_rmse_std)
    train_maxstd <- max(scale(taumodel$evaluation_log$train_rmse_std))
    train_argminstd <- which.min(taumodel$evaluation_log$train_rmse_std)
    
    test_minrmse <- min(scale(taumodel$evaluation_log$test_rmse_mean))
    # test_meanrmse <- mean(taumodel$evaluation_log$test_rmse_mean)
    test_maxrmse <- max(scale(taumodel$evaluation_log$test_rmse_mean))
    test_argminrmse <- which.min(taumodel$evaluation_log$test_rmse_mean)
    test_minstd <- min(scale(taumodel$evaluation_log$test_rmse_std))
    # test_meanstd <- mean(taumodel$evaluation_log$test_rmse_std)
    test_maxstd <- max(scale(taumodel$evaluation_log$test_rmse_std))
    test_argminstd <- which.min(taumodel$evaluation_log$test_rmse_std)


    distf <- dist(usingDataset[c(poolSet,trainSet,xprime), fs:target])
    distf <- as.matrix(distf)
    mindistf <- foreach(dmn = 1:length(poolSet), .combine="c")%do%{
        min(scale(distf[dmn,-dmn]))
    }
    maxdistf <- foreach(dmx = 1:length(poolSet), .combine="c")%do%{
        max(scale(distf[dmx,-dmx]))
    }

    B = 5
    rftrees <- foreach(t = 1:B) %do% {    

        lenlabel <- dim(labeled)[1]
        bootstrap <- sample(1:lenlabel, lenlabel, replace = T)  
        bootstrap <- labeled[bootstrap, ]
        # rtree <- gbm(Y ~ ., data = bootstrap, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
        # best.iter_rtree <- gbm.perf(rtree, method="cv")
        # rtree <-randomForest(Y ~ ., data = bootstrap)
        rtree <- xgboost(param = param, data = xgb.DMatrix(bootstrap[,fs:target], label = bootstrap[,"Y", drop=FALSE]), nrounds = 1000, verbose = 0)
    }
    Vars <- foreach(q = 1:length(poolSet), .combine="rbind") %do% {
       
        htx <- foreach(t = 1:B, .combine = "c") %do% {      
            yt <- predict(rftrees[[t]], usingDataset[poolSet[q], fs:target, drop=FALSE])
        }
        meanhtx <- mean(htx)
        Var <- mean((htx - meanhtx)^2)
    }

    # futurePool <- data.frame(train_minrmse, train_meanrmse, train_maxrmse, train_argminrmse,train_minstd,  
    #     train_meanstd, train_maxstd, train_argminstd,
    #     test_minrmse, test_meanrmse, test_maxrmse, test_argminrmse,test_minstd, test_meanstd, test_maxstd, test_argminstd,
    #     minidist=scale(mindistf), maxidist=scale(maxdistf), Var=scale(Vars))
    
    futurePool <- data.frame(train_minrmse, train_maxrmse, train_argminrmse,
        train_minstd,  train_maxstd, train_argminstd,
        test_minrmse, test_maxrmse, test_argminrmse,test_minstd,  test_maxstd, test_argminstd,
        minidist=mindistf, maxidist=maxdistf, Var=scale(Vars))
    predE <- predict(modelE, data.matrix(futurePool))
    
    sortedGain <- sort(predE, index = T, decreasing = T)
    
    prime <- sortedGain$ix[1]
    gainC <- sortedGain$x[1]
    
    xprime <- c(xprime, poolSet[prime])
    poolSet <- poolSet[-prime]
    dataT <- usingDataset[c(trainSet, xprime, gainSet), ]
    # modelt <- randomForest(Y ~ ., data = dataT,ntree = 1000)
    # modelt <- gbm(Y ~ ., data = dataT, distribution = "gaussian", n.tree = 1000, shrinkage = 1e-1, n.minobsinnode = 0, cv.folds = 5)
    modelt <- xgboost(param = param, data = xgb.DMatrix(dataT[,fs:target], label = dataT[,"Y", drop=FALSE]), nrounds = 1000, verbose = 0)
    # best.iter_t <- gbm.perf(modelE,method="cv")   
    pred <- predict(modelt, usingDataset[valSet, fs:target])
    ErrorC <- rmse(pred, usingDataset[valSet, "Y"])
    lalError <- c(lalError, ErrorC)

}
