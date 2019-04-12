

usingDataset <- fread("./dataset/CASP.csv", header = T)
colnames(usingDataset) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
shake <- sample(1:dim(usingDataset)[1], 1290, replace = FALSE)
# shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)

usingDataset <- usingDataset[shake,]
fs <- 2
target <- 10