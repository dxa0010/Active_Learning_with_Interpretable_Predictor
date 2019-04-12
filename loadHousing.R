# load housing data 
# x = 13 , y= 1 ,but data has logicaldata. rm this.
# dataset size is 506 


usingDataset <- fread("./dataset/housing.data", header = F)
# name = colnames(usingDataset)
# name[14] = "Y"
# colnames(usingDataset) <- name
# colnames(usingDataset) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "Y")

colnames(usingDataset) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "Y")
shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)
usingDataset <- usingDataset[shake,]

#rm logical data
usingDataset <- usingDataset[,-4] 
fs <- 1
target <- 12