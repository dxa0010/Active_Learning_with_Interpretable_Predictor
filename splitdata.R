# split data
# pre trainsize, valsize, gsize, lenH

trainSet <- seq(1, length = trainsize)
valSet <- seq(length(trainSet) + 1, length = valsize)
gainSet <- seq(length(trainSet) + length(valSet) + 1, length = gsize)
poolSet <- setdiff((1:lenH), union(union(trainSet, valSet), gainSet))
Dgt <- gainSet[1:round((gsize)/2)]
Dgv <- gainSet[(round((gsize)/2) + 1):gsize]
randPool <- eerPool <- qbcPool <- poolSet
