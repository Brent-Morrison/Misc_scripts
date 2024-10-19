library(igraph)
library(ggplot2)

act <- c('A','B','C','D','E','F','G','H',I)
pred <- c(NULL, NULL, 'A','A','C','D','B','EF','G')

start_node <- c(1,1,2,2,4,5,3,6,7)
end_node <- c(2,3,4,5,6,6,7,8,8)
dur <- c(5,2,6,12,10,9,5,9,1)
zs <- rep(0, length(start_node))


mtx <- matrix(c(start_node, end_node, dur, rep(zs, 8)), ncol = 11)
colnames(mtx) <- c("snode","enode","drtn","fwrd","bkwd","est","eft","lst","lft","slhd","sltl")
mtx

# Forward
for (i in 1:length(start_node)) {

  if (mtx[i, "snode"] == 1) {
    # No previous node, forward pass equals duration
    mtx[i, "fwrd"] <- mtx[i, "drtn"]
  } else {
    mtx[i, "fwrd"] <- max(mtx[which(mtx[, "enode"] == mtx[i, "snode"]), "fwrd"]) + mtx[i, "drtn"]
    mtx[i, "fwrd"] <- max(mtx[which(mtx[, "enode"] == mtx[i, "enode"]), "fwrd"])  # get maximum , first is max
    
  }
}
mtx


# Backward
for (i in length(start_node):1) {
  
  mtx[i, "fwrd"] <- max(mtx[which(mtx[, 2] == mtx[i, 2]), "fwrd"])  # get maximum , second is max
  if (mtx[i, "enode"] == max(end_node)) {
    # No previous node, forward pass equals duration
    mtx[i, "bkwd"] <- max(mtx[which(mtx[, "enode"] == max(end_node)), "fwrd"])   #mtx[i, 4]
  } else {
    mtx[i, "bkwd"] <- min(mtx[which(mtx[, "snode"] == mtx[i, "enode"]), "bkwd"] -
                            mtx[which(mtx[, "snode"] == mtx[i, "enode"]), "drtn"])
    
  }
mtx[i, "est"] <- max(mtx[which(mtx[, "enode"] == mtx[i, "snode"]), "fwrd"])
}

mtx[!is.finite(mtx)] <- 0
mtx[, "eft"] <- mtx[, "est"] + mtx[, "drtn"]
mtx[, "lst"] <- mtx[, "bkwd"] - mtx[, "drtn"]
mtx[, "lft"] <- mtx[, "bkwd"]
mtx[, "slhd"] <- mtx[, "bkwd"] - mtx[, "fwrd"]

# Forward
for (i in 1:length(start_node)) {

  if (mtx[i, "snode"] == 1) {
    # No previous node, forward pass equals duration
    mtx[i, "sltl"] <- 0
  } else {
    mtx[i, "sltl"] <- max(mtx[which(mtx[, "enode"] == mtx[i, "snode"]), "slhd"])
  }

}
mtx


# Critical Path
mtx[, c("snode", "enode")]
mtx[which(mtx[, "est"] == mtx[, "lst"]), c("snode", "enode", "drtn")]

# Plot gantt
d <- data.frame(mtx[, c("est", "eft")])
d$name <- as.factor(1:9)
d$critical_path <- mtx[, "est"] == mtx[, "lst"]

d1 <- d[d$critical_path == TRUE, ]
d1$name <- as.numeric(as.character(d1$name))
d1$end_name <- c(d1$name[2:nrow(d1)], 0)
d1 <- d1[1:(nrow(d1)-1), ]

adj <- 0.31
ggplot(d, aes(x=est, xend=eft, y=name, yend=name, color=critical_path)) +
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=16) + #increase line width of segments in the chart
  geom_segment(aes(x = eft, y = name-adj, xend = eft, yend = end_name+adj), data = d1, size = 1.25) + #, size = 0.1
  #scale_y_discrete(limits=rev) +
  labs(title='Project Schedule', x='Time', y='Activity')


# igraph
g <- graph_from_edgelist(mtx[, c("snode", "enode")])
E(g)$weight <- mtx[, "drtn"]
E(g)$critical_path <- mtx[, "est"] == mtx[, "lst"]

plot(g, 
  layout = layout_with_fr,
  edge.width = ifelse(E(g)$critical_path, 4, 1),
  main = "Force-directed layout"
  )

