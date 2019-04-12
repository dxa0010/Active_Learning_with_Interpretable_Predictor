

usingDataset <- fread("./dataset/friedman1_1.csv", header = T)
usingDataset <- usingDataset[,2:11]
colnames(usingDataset) <- c( "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9","Y")
shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)
usingDataset <- usingDataset[shake,]
fs <- 1
target <- 9