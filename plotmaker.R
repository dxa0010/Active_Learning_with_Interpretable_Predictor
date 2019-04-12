# plot
library(foreach)
datasets <- c("Casp", "Housing", "Freid", "redWine", "whiteWine", "Bike")

Times <- foreach(args0 = c(1,2,4,5,6), rbind)%do%{

    filepath <- "./env/"
    per <-read.table(paste0(filepath,"IAL_exp_per_gbdt_dataset40_50_", args0, ".csv"), header = F,sep = ",")
    lal <-read.table(paste0(filepath, "IAL_exp_lal_gbdt_a_dataset", args0, ".csv"), header = F,sep = ",")
    qbb <-read.table(paste0(filepath, "IAL_exp_qbb_gbdt_dataset_kai", args0, ".csv"), header = F,sep = ",")
    # rand <-read.table(paste0(filepath, "IAL_exp_rand_dataset", args0, ".csv"), header = F,sep = ",")
    # per2 <-read.table(paste0(filepath,"per_exp/IAL_exp_per_dataset", args0, ".csv"), header = F,sep = ",")
    # lal2 <-read.table(paste0(filepath, "per_exp/IAL_exp_lal_dataset", args0, ".csv"), header = F,sep = ",")
    # qbb2 <-read.table(paste0(filepath, "per_exp/IAL_exp_qbb_dataset_kai", args0, ".csv"), header = F,sep = ",")
    # rand2 <-read.table(paste0(filepath, "per_exp/IAL_exp_rand_dataset", args0, ".csv"), header = F,sep = ",")
    
    
    perError <- rowMeans(cbind(per))
    lalError <-  rowMeans(cbind(lal))
    qbbError <-  rowMeans(cbind(qbb))
    # randError <-  rowMeans(cbind(rand))
    errorMtrx <- cbind(perError, lalError, qbbError)
    # , randError)

    png(paste0(filepath, datasets[args0], ".png"), width = 800, height = 600)
    # par(mar=c(6,7,7,13.5))
    par(mar=c(6,7,3,3))
    matplot(errorMtrx,type="b",lwd=2.0,lty=1:4,pch=2:6,  ylab = "Test Error", xlab="Number of acquired data",cex.lab = 3.2, cex.axis = 2.7,cex.main = 4.0, cex = 2.5)
    par(xpd=T)
    if(args0 == 2){legend("bottomleft", legend = c("PER", "LAL", "QBC")
    # , "RAND")
     ,col=1:4,lty=1:4,pch=2:6, cex = 2.5,  bty="n", lwd=2,bg = "transparent")}
    dev.off()  

    # errorMtrx[100,]
    # pertime <-read.table(paste0(filepath,"exp_time_per_dataset_a_", args0, ".csv"), header = F,sep = ",")
    # laltime <-read.table(paste0(filepath, "exp_time_lal_dataset", args0, ".csv"), header = F,sep = ",")
    # qbbtime <-read.table(paste0(filepath, "exp_time_qbb_dataset", args0, ".csv"), header = F,sep = ",")
    # randtime <-read.table(paste0(filepath, "exp_time_rand_dataset", args0, ".csv"), header = F,sep = ",")

    # Time <- c(as.numeric(colMeans(pertime)), as.numeric(colMeans(laltime)), as.numeric(qbbtime), as.numeric(colMeans(randtime)))
}

CASP <- read.table("./env/IAL_TIme/IAL_expAL_time_gbdt_dataset_a_1.csv", sep = ",", header = F)
Housing <- read.table("./env/IAL_TIme/IAL_expAL_time_gbdt_dataset_a_2.csv", sep = ",", header = F)
wineRed <- read.table("./env/IAL_TIme/IAL_expAL_time_gbdt_dataset_a_4.csv", sep = ",", header = F)
wineWhite <- read.table("./env/IAL_TIme/IAL_expAL_time_gbdt_dataset_a_5.csv", sep = ",", header = F)
Bike <- read.table("./env/IAL_TIme/IAL_expAL_time_gbdt_dataset_a_6.csv", sep = ",", header = F)

time <- data.frame(CASP=CASP, Housing=Housing, wineRed=wineRed, wineWhite=wineWhite, Bike=Bike)
rownames(time) <- c("PER", "LAL", "QBC", "EMC", "RAND")

# write.table(Times, paste0("~/sdalab/activelearn/env/per_exp/exp_time.csv"), sep=",", col.names=F, row.names=F)

# importance_gain <- cbind(  276897.9   ,115662.3  , 429195.6  ,2282363.5 , 1554628.4  , 520292.6  , 373506.3  , 921169.3   ,820459.0 ,658238.5  ,1072764.9 ,  728161.0)
# name <- colnames(usingDataset)
# colnames(importance_gain) <- name[1:12]
# png(paste0(filepath, "gpf_imp_155_bike", ".png"), width = 800, height = 600)
# par(mar=c(10,7,4,1), mgp=c(4.0, 0.7, 0))
# barplot(importance_gain/sum(importance_gain), las = 2, main = "GPF importance", ylab ="importance", cex.main=3, cex.lab = 2.5, cex.axis = 2, cex.names = 2)
# dev.off()  

# importance_f <- cbind(163176.17  ,118536.67 , 255976.04 ,2297266.73  , 42074.85 , 224809.23  , 56365.27  , 84579.44  ,952421.55 ,1011111.76  ,463008.41, 519464.39 )
# name <- colnames(usingDataset)
# colnames(importance_f) <- name[1:12]
# png(paste0(filepath, "opf_imp_155_bike", ".png"), width = 800, height = 600)
# par(mar=c(10,7,4,1), mgp=c(4.0, 0.7, 0))
# barplot(importance_f/sum(importance_f), las = 2, main = "OPF importance", ylab ="importance", cex.main=3, cex.lab = 2.5, cex.axis = 2, cex.names = 2)
# dev.off()  

# png(paste0(filepath, "exp_time_datagroup_3", ".png"), width = 800, height = 600)
# time <- read.table("~/sdalab/activelearn/env/per_exp/exp_time.csv", sep=",")
# colnames(time) <- c( "CASP", "Housing", "Fried", "winequality-red", "winquality-white", "Bike")
# rownames(time) <- c("PER", "LAL", "QBC", "RAND" )
# # time <- time[,c(2,6,1,4,5,3)]
# time <- time[,c(2,6,1,4,5)]
# time <- data.matrix(time)
# time <- time[,1:3]
# par(mar=c(13,6,2,1), mgp=c(3, 0.7, 0))
# # barplot(time,beside = TRUE, names.arg= colnames(time) , ylab ="Time", cex.lab = 2.5, cex.axis = 2, cex.names = 2, col = brewer.pal(4, "Set1"),las = 2,)
# barplot(time,beside = TRUE, names.arg= colnames(time) , ylab ="", cex.lab = 2.5, cex.axis = 2, cex.names = 2, col = 1:5,las = 2,)

# # legend("topleft", legend = rownames(time),fill = brewer.pal(4, "Set1") , cex = 2.5,  bty="n", lwd=2,bg = "transparent")
# legend("topleft", legend = rownames(time),fill = 1:5 , cex = 2.5,  bty="n", lwd=2,bg = "transparent")
# # dev.off()

png(paste0("exp_time_datagroup_l", ".png"), width = 800, height = 600)
par(mar=c(10,8,2,1), mgp=c(, 0.7, 0))
barplot(data.matrix(ltime),beside = TRUE, names.arg= colnames(time) , ylab ="sec", cex.lab = 2.5, cex.axis = 2, cex.names = 2, col = 1:5,las = 2)
legend("topright", legend = rownames(time),fill = 1:5 , cex = 1.5,  bty="n", lwd=2,bg = "transparent")
dev.off()