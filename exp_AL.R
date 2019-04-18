library(foreach)
# library(pforeach)
# library(doParallel)
library(dplyr)
library(data.table)
library(class)
library(Metrics)
library(randomForest)
library(xgboost)

StartTime = Sys.time()
print(StartTime)

#number of getting pool
ir <- 100

datasets <- c("loadCasp.R", "loadHousing.R", "loadredWine.R", "loadwhiteWine.R", "loadBike.R")
# seeds <- c( 231 , 145,  345,  678,  890,  462,  290,  690 ,1356 ,1780,  693,  435 ,1035 ,2034, 2670, 1848, 1160, 2760, 5424, 7120)
# seeds <- c( 231 , 145,  345,  678,  890,  462,  290,  690 ,1356 ,1780)
# seeds <- c( 231 , 145,  345,  678,  890)
## defalt uses
seeds <- c(462,  290,  690 ,1356 ,1780)

# interpretation seed = 462, args0 = 2 and 6


# seeds <- 231

args0 <- as.numeric(commandArgs(trailingOnly=TRUE)[1])
# args0 <- 1

print(args0)

# split data
# trainsize <- 30
# valsize <- 200
# gsize <- 60

# trainsize <- 50
# valsize <- 200
# gsize <- 40

trainsize <- 40
valsize <- 200
gsize <- 50

init_train_size = trainsize + gsize 

perErrors <- foreach(seed = seeds, .combine= "cbind")%do%{
    set.seed(seed)
    source(datasets[args0])
    lenH <- dim(usingDataset)[1]
    source("splitdata.R")
    source("perALgbdt.R")
    print("1 seed ended___PER")
    perError
}
write.table(perErrors, paste0("./env/IAL_exp_per_gbdt_dataset40_50_", args0, ".csv"), sep=",", col.names=F, row.names=F)

# # home 99, 133 -> 100, 134
# # bike 104, 150 -> gpf = 105, 151
# filepath <- "~/sdalab/activelearn/env/per_exp/"
# importance_f <- t(importance_f)
# importance_gain <- t(importance_gain)

# itr_timing = 44

# astime <- usingDataset[c(trainSet, gainSet, xprime[1:itr_timing]),]
# timeshift <- astime %>% group_by(hr) %>% summarise(max=max(Y) , min=min(Y) , mean=mean(Y))

# png(paste0(filepath, "timeshift_bike_", (itr_timing + init_train_size), ".png"), width = 800, height = 600)
# par(mar=c(5,5,2,1), mgp=c(3, 0.7, 0))
# barplot(timeshift[,4], names.arg=c(0:23),space=c(0.5, 2), xlab = "hour", ylab ="count",axis.lty=1, cex.main=3, cex.lab = 3, cex.axis =1.5, cex.names = 1.5)
# dev.off()

# png(paste0(filepath, "gpf_imp_bike", (itr_timing + init_train_size), ".png"), width = 800, height = 600)
# par(mar=c(7,7,2,1), mgp=c(4.4, 0.7, 0))
# barplot(importance_gain[(itr_timing+1),]/sum(importance_gain[(itr_timing+1),]), las = 2, ylab ="importance", cex.main=3, cex.lab = 2.5, cex.axis = 2, cex.names = 2, col = gray(0.3))
# dev.off()  

# png(paste0(filepath, "opf_imp_bike", (itr_timing + init_train_size), ".png"), width = 800, height = 600)
# par(mar=c(7,7,2,1), mgp=c(4.4
# , 0.7, 0))
# barplot(importance_f[(itr_timing),]/sum(importance_f[(itr_timing),]), las = 2, ylab ="importance", cex.main=3, cex.lab = 2.5, cex.axis = 2, cex.names = 2, col = gray(0.3))
# dev.off()  

lalErrors <- foreach(seed = seeds, .combine= "cbind")%do%{
    set.seed(seed)
    source(datasets[args0])
    lenH <- dim(usingDataset)[1]
    source("splitdata.R")
    source("LAL_exe.R")
    print("1 seed ended___LAL")
    lalError
}
write.table(lalErrors, paste0("../../env/IAL_exp_lal_gbdt_a_dataset_kai", args0, ".csv"), sep=",", col.names=F, row.names=F)

qbcErrors <- foreach(seed = seeds, .combine= "cbind")%do%{
    set.seed(seed)
    source(datasets[args0])
    lenH <- dim(usingDataset)[1]
    source("splitdata.R")
    source("qbc_impre.R")
    print("1 seed ended___QBC")
    qbbError
}
write.table(qbcErrors, paste0("./env/IAL_exp_qbb_gbdt_dataset_50", args0, ".csv"), sep=",", col.names=F, row.names=F)

emcErrors <- foreach(seed = seeds, .combine= "cbind")%do%{
    set.seed(seed)
    source(datasets[args0])
    lenH <- dim(usingDataset)[1]
    source("splitdata.R")
    source("gbdt_emc.R")
    print("1 seed ended___emc")
    emcError
}
write.table(emcErrors, paste0("./env/IAL_exp_emc_gbdt_101_dataset", args0, ".csv"), sep=",", col.names=F, row.names=F)


randErrors <- foreach(seed = seeds, .combine= "cbind")%do%{
    set.seed(seed)
    source(datasets[args0])
    lenH <- dim(usingDataset)[1]
    source("splitdata.R")
    source("random.R")
    print("1 seed ended___RANDOM")
    randError
}
write.table(randErrors, paste0("./env/IAL_exp_rand_gbdt_dataset", args0, ".csv"), sep=",", col.names=F, row.names=F)

# print(paste0(datasets[args0], "---- 1 dataset ended"))


# PER <- rowMeans(perErrors)
# LAL <- rowMeans(lalErrors)
# qbc <- rowMeans(qbbErrors)
# RND <- rowMeans(randErrors)

# png(paste0("exp_dataset",args0,".png"), width = 800, height = 500)
# matplot(cbind(PER, LAL, EER, RND), type="b", main = "compare methods", ylab = "mean square error", xlab = "number of data")
# dev.off()


EndTime <- Sys.time()
print(EndTime - StartTime)


# # sigma <- apply(usingDataset, 2,sd)
# # s <- apply(usingDataset[Df_s[[3]]], 2,sd)
# # order(abs (sigma - s ))

# Rscript exp_AL.R 6 >& /dev/null