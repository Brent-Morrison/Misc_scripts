act <- c('A','B','C','D','E','F','G','H',I)
pred <- c(NULL, NULL, 'A','A','C','D','B','EF','G')

start_node <- c(1,1,2,2,4,5,3,6,7)
end_node <- c(2,3,4,5,6,6,7,8,8)
dur <- c(5,2,6,12,10,9,5,9,1)
zs <- rep(0, length(start_node))

mtx <- matrix(c(start_node, end_node, dur, zs, zs), ncol = 5)
mtx

# Forward
for (i in 1:length(start_node)) {
  if (mtx[i, 1] == 1) {
    # No previous node, forward pass equals duration
    mtx[i, 4] <- mtx[i, 3]
  } else {
  mtx[i, 4] <- max(mtx[which(mtx[, 2] == mtx[i, 1]), 4]) + mtx[i, 3]
  mtx[i, 4] <- max(mtx[which(mtx[, 2] == mtx[i, 2]), 4])  # get maximum , first is max
  }
}
mtx


# Backward
for (i in length(start_node):1) {
  mtx[i, 4] <- max(mtx[which(mtx[, 2] == mtx[i, 2]), 4])  # get maximum , second is max
  if (mtx[i, 2] == max(end_node)) {
    # No previous node, forward pass equals duration
    mtx[i, 5] <- max(mtx[which(mtx[, 2] == max(end_node)), 4])   #mtx[i, 4]
  } else {
    mtx[i, 5] <- min(mtx[which(mtx[, 1] == mtx[i, 2]), 5] - mtx[which(mtx[, 1] == mtx[i, 2]), 3])
  }
}
mtx