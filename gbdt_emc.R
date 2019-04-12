library(data.table)
library(foreach)
library(dplyr)
library(tcltk)
library(class)
library(Metrics)
library(xgboost)

dump2tree <- function(dumpfile , x, data, data.Y, index=1){
    feature_name <- as.character(dumpfile[index, "Feature"])
    boundary <- as.numeric(dumpfile[index, "Split"])

    if(feature_name == "Leaf") return(as.numeric(colMeans(data[, data.Y])))
    if(as.logical(x[,feature_name] < boundary)){
        res <- which(data[,feature_name] < boundary)
        index <- which(as.character(dumpfile[index, "Yes"])==dumpfile[,"ID"])
    }else{
        res <- which(data[,feature_name] >= boundary)
        index <-  which(as.character(dumpfile[index, "No"])==dumpfile[,"ID"])
    }
    dump2tree(dumpfile, x, data[res,], data.Y, index)
}

usingDataset <- data.matrix(usingDataset)
B <- 10

lenH <- dim(usingDataset)[1]


param <- list(max_depth = 6, eta = 0.01, silent = 1, nthread = 3, objective = "reg:linear")

trainSet <- c(trainSet,gainSet)

xemc <- NULL
emcError <- NULL

pb <- txtProgressBar(min = 1, max = length(poolSet) * ir, style = 3)
foreach(j = 1:ir) %do% {
    
    b_f <- foreach(k = 1:B) %do% {
            boottrain <- sample(trainSet, trainsize, replace = TRUE)
            BS <- usingDataset[boottrain, ]  #Bootstrapsample
            # struct model
            
            fk <- xgboost(param = param, data = BS[, fs:target], label = BS[, "Y", drop = FALSE], nrounds = 1000, verbose = 0)
    }
    m_nround <- 101
    param_m <- list(max_depth = 4, eta = 0.5, silent = 1, nthread=3,  objective = "reg:linear")
    f <- xgboost(param = param_m, data = xgb.DMatrix(usingDataset[trainSet, fs:target], label = usingDataset[trainSet, "Y", drop = FALSE]), nrounds = m_nround, verbose = 0)
    dump_fs <- foreach(m = 1:(m_nround-1))%do%{
        dump_f <- xgb.model.dt.tree(colnames(usingDataset)[fs:target], f, tree = m)
    }
    gradth <- foreach(q = 1:length(poolSet), .combine = "c") %do% {
        setTxtProgressBar(pb, q + length(poolSet) * (j - 1))
        # global f in step0
        
        fx <- predict(f, usingDataset[poolSet[q], fs:target, drop = FALSE])
        
        phix <- foreach(mm = 1:(m_nround-1), .combine = "c") %do% {
            hmx <- dump2tree(dump_fs[[mm]], data.frame(usingDataset[poolSet[q],,drop=FALSE]), data.frame(usingDataset[trainSet,]), "Y")           
        }
        gradthk <- foreach(k = 1:B, .combine = "c") %do% {
            ykx <- predict(b_f[[k]], usingDataset[poolSet[q], fs:target, drop = FALSE])
                
            # step3~7
            sqrt(sum(((fx - ykx) * phix)^2))
        }
        mean(gradthk)
        
    }
    
    prime <- which.max(gradth)
    xemc <- c(xemc, poolSet[prime])
    poolSet <- poolSet[-prime]
    
    emcmdl <- xgboost(param = param, data = usingDataset[c(trainSet, xemc), fs:target], label = usingDataset[c(trainSet, xemc), "Y", drop = FALSE], nrounds = 1000, verbose = 0)
    emcpr <- predict(emcmdl, usingDataset[valSet, fs:target])
    ErrorC <- rmse(emcpr, usingDataset[valSet, "Y"])
    emcError <- c(emcError, ErrorC)
    
    
}
