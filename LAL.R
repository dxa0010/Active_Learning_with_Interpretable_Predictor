# learning active learning
library(data.table)
library(foreach)
library(dplyr)
library(class)
library(Metrics)
library(randomForest)
library(xgboost)


tau <- 10
qq <- 40

# setting for god
# godTrain <- trainSet
# godPool <- poolSet
# godPrime <- godErrors <- NULL
# goddif <- godErrorslist <- list()



param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread = 1, objective = "reg:linear")
increase_oneterm <- 2
init_size <- 50
max_size <- 200
itr <- round((max_size-init_size)/increase_oneterm)

gainset <- NULL
# seeds <- c(462,  290,  690 ,1356 ,1780)
seeds <- c(462)


pb <- txtProgressBar(min = 1, max = itr, style = 3)

gains_set <- foreach(seed=seeds, .combine="rbind")%do%{
    print(paste0("seed = ", seed))
    set.seed(seed)
    source('loadFreid.R')
    dataT <- usingDataset[1:1000,]
    # test <- 1001:1200
    test <- 7000:9000

    gains <- foreach(d=1:itr, .combine="rbind") %do% {
        setTxtProgressBar(pb, d)
        if(d==1){    
            fullset <- 1:dim(dataT)[1]
            # fullset <- sample(fullset, length(fullset), replace = FALSE)
            fulllen <- length(fullset)
            # onest <- round(d*fulllen/(1+tau))
            labeled <- fullset[1:init_size]
            nonlabel <- fullset[(init_size+1):fulllen]
        }
        else{
            predE <- predict(modelE, gain[,position_feature])
            sortedGain <- sort(predE, index = T, decreasing = T)
            prime <- sortedGain$ix[1:increase_oneterm]
            labeled <- c(labeled, nonlabel[prime])
            nonlabel <- nonlabel[-prime]
        }
        data <- usingDataset[labeled, ]
            # modely <- randomForest(Y ~ ., data = data,ntree = 1000)
        modely <- xgb.cv(nfold=5, param = param, data = xgb.DMatrix(data.matrix(data %>% select(-contains("Y"))), label = data.matrix(data$Y)), nrounds = 1000, verbose = 0,  callbacks = list(cb.evaluation.log()))


        # rsq <- mean(modely$rsq)
        # treesize <- mean(treesize(modely))
        # mse <- mean(modely$mse)

        train_minrmse <- min(scale(modely$evaluation_log$train_rmse_mean))
        # train_meanrmse <- mean(modely$evaluation_log$train_rmse_mean)
        train_maxrmse <- max(scale(modely$evaluation_log$train_rmse_mean))
        train_argminrmse <- which.min(modely$evaluation_log$train_rmse_mean)
        train_minstd <- min(scale(modely$evaluation_log$train_rmse_std))
        # train_meanstd <- mean(modely$evaluation_log$train_rmse_std)
        train_maxstd <- max(scale(modely$evaluation_log$train_rmse_std))
        train_argminstd <- which.min(modely$evaluation_log$train_rmse_std)
        
        test_minrmse <- min(scale(modely$evaluation_log$test_rmse_mean))
        # test_meanrmse <- mean(modely$evaluation_log$test_rmse_mean)
        test_maxrmse <- max(scale(modely$evaluation_log$test_rmse_mean))
        test_argminrmse <- which.min(modely$evaluation_log$test_rmse_mean)
        test_minstd <- min(scale(modely$evaluation_log$test_rmse_std))
        # test_meanstd <- mean(modely$evaluation_log$test_rmse_std)
        test_maxstd <- max(scale(modely$evaluation_log$test_rmse_std))
        test_argminstd <- which.min(modely$evaluation_log$test_rmse_std)

        modely <- xgboost(param = param, data = xgb.DMatrix(data.matrix(data %>% select(-contains("Y"))), label = data.matrix(data$Y)), nrounds = 1000, verbose = 0)
        predO <- predict(modely, data.matrix(usingDataset[test, fs:target]))
        ErrorO <- mse(predO, usingDataset[test, Y])
        
        # compute gain
        gain <- foreach(i = 1:length(nonlabel), .combine = "rbind") %do% {
            # setTxtProgressBar(pb, (j - 1) * length(Dgt) + i)
            data <- usingDataset[c(labeled, nonlabel[i]), ]
            # modeli <- randomForest(Y ~ ., data = data,ntree = 1000)
            modeli <- xgboost(param = param, data = xgb.DMatrix(data.matrix(data %>% select(-contains("Y"))), label = data.matrix(data$Y)), nrounds = 1000, verbose = 0)
            predPlus <- predict(modeli, data.matrix(usingDataset[test, fs:target]))
            ErrorPlus <- mse(predPlus, usingDataset[test, Y])
            
            # future extraction
            distg <- dist(usingDataset[c(nonlabel[i],labeled), fs:target])
            distg <- as.matrix(distg)
            minidist <- min(scale(distg[1,-1]))
            maxidist <- max(scale(distg[1,-1]))
            # rsqi <- mean(modeli$rsq)
            B = 5
            samplesize <- dim(length(labeled))[1]

            rftrees <- foreach(t = 1:B) %do% {      
                    bootstrap <- sample(labeled, length(labeled), replace = T)  
                    bootstrap <- usingDataset[bootstrap, ]
                    # rtree <-randomForest(Y ~ ., data = bootstrap)
                    rtree <- xgboost(param = param, data = xgb.DMatrix(data.matrix(bootstrap %>% select(-contains("Y"))), label = data.matrix(bootstrap$Y)), nrounds = 1000, verbose = 0)
            }

            htx <- foreach(t = 1:B, .combine = "c") %do% {      
                # bootstrap <- sample(1:samplesize, samplesize, replace = T)  
                # bootstrap <- data[bootstrap, ]
                # rtree <-randomForest(Y ~ ., data = bootstrap)
                yt <- predict(rftrees[[t]], data.matrix(usingDataset[nonlabel, fs:target]))
            }
            meanhtx <- mean(htx)
            Var <- mean((htx - meanhtx)^2)
                
            # vari <- var(modeli$mse)
            # impi <- modeli$importance[length(c(trainSet, xprime))+1]
            Error <- ErrorO - ErrorPlus
            # cbind(train_minrmse, train_meanrmse, train_maxrmse, train_argminrmse, train_minstd, train_meanstd, train_maxstd, train_argminstd,
            # test_minrmse, test_meanrmse, test_maxrmse, test_argminrmse,test_minstd, test_meanstd, test_maxstd, test_argminstd,
            # minidist, maxidist, Var, Error)
            cbind(train_minrmse,  train_maxrmse, train_argminrmse, train_minstd,  train_maxstd, train_argminstd,
            test_minrmse,  test_maxrmse, test_argminrmse,test_minstd, test_maxstd, test_argminstd,
            minidist, maxidist, Var, Error)
        }
        gain[,15] <- c(scale(gain[,15]))
        gain[,16] <- c(scale(gain[,16]))
        gainset <- rbind(gainset, gain)
        name <- colnames(gainset)
        position_feature <- seq(1, (length(name)-1))
        modelE <- xgboost(param = param, data = xgb.DMatrix(gainset[,position_feature], label = gainset[,"Error"]), nrounds = 1000, verbose = 0)
        gain
    }
}
# model g : predict gain
gains_set <- data.frame(gains_set)
write.table(gains_set, "./env/ForGain_imp_gbdt.csv", sep=",", row.names=F, col.names=T)

    
