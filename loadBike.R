# load bike_share data 
# x = 16 , y= 1 ,but dataset includes data_index. rm it.
# dataset size is 17379

usingDataset <- fread("./dataset/bike_shar/hour.csv", header = T)
usingDataset$instant <- NULL
usingDataset$dteday <- NULL
usingDataset$casual <- NULL
usingDataset$registered <- NULL
# usingDataset$dteday =as.Date(usingDataset$dteday)
name <- colnames(usingDataset)
name[13] = "Y"
colnames(usingDataset) <- name
# shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)
shake <- sample(1:dim(usingDataset)[1], 1290, replace = FALSE)
# shake <- sample(1:dim(usingDataset)[1], dim(usingDataset)[1], replace = FALSE)

usingDataset <- usingDataset[shake,]
fs <- 1
target <- 12