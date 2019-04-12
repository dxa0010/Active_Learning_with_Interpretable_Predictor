# wine white dataset 
# for predict quality y = 12 ,x = 1~11
# dataset size = 4898

usingDataset <- fread("./dataset/winequality-white.csv", header = T)
colnames(usingDataset) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "Y")
shake <- sample(1:dim(usingDataset)[1], 1290, replace = FALSE)
# shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)

usingDataset <- usingDataset[shake,]
fs <- 1
target <- 11