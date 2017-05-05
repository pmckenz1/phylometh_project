produce_next_rows <- function(currentmatrix,sharedmatrix) {
whichrow <- nrow(currentmatrix) + 1
currentmatrix <- t(apply(currentmatrix,1,c,rep(0,sharedmatrix[whichrow,whichrow])))
#partition up the matrix... this will help later
i <- 2
partition <- 1
partitions <- list()
q <- 1
while (i <= ncol(currentmatrix)) {
  while (i <= ncol(currentmatrix) && isTRUE(all.equal(currentmatrix[,i],currentmatrix[,i-1]))) {
      partition <- c(partition,i)
      i <- i + 1
  }
  partitions[[q]] <-  partition
  partition <- i # set the new starting value for next partition
  i <- i+1 # set new i
  q <- q+1
}


rowlength <- ncol(currentmatrix)
alpha <- sharedmatrix[whichrow,whichrow]
comb.indices <- combn(rowlength,alpha)
row <- rep(0,rowlength)

rowcombs_df <- data.frame()
rows <- for (i in 1:ncol(comb.indices)) {
  row <- rep(0,rowlength)
  row[comb.indices[,i]] <- 1
  rowcombs_df<-rbind(rowcombs_df,row)
}

working<- rowcombs_df[(apply(rowcombs_df[,1:sharedmatrix[1,1]] == 1,1,sum) == sharedmatrix[1,whichrow]),]
for (i in 2:(whichrow-1)) {
working <- working[(apply((working[,currentmatrix[i,] == 1] == 1),1,sum) == sharedmatrix[i,whichrow]),]
}

for (i in 1:length(partitions)) {
  if (length(partitions[[i]]) > 1) {
  working[,partitions[[i]]] <- t(apply(working[,partitions[[i]]],1,sort,decreasing = TRUE))
  }
}

newrows <- unique(split(as.matrix(working),row(working)))
newmatrices <- list()
for (i in 1:length(newrows)) {
  temp <- rbind(currentmatrix,newrows[[i]])
  temp <- temp[,(colSums(temp) != 0)]
  newmatrices[[i]] <- temp
}
newmatrices
}




