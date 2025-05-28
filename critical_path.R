library(igraph)
library(ggplot2)

start_node <- c(1,1,2,2,4,5,3,6,7)
end_node <- c(2,3,4,5,6,6,7,8,8)
drtn <- c(5,2,6,12,10,9,5,9,1)
#drtn <- c(5,2,6,12,10,9,5,9,38)
cost <- c(10,6,8,7,3,3,2,9,9)

# Monte carlo data
sapply(cost, rnorm, n = 5, sd = 0.2) 
lapply(cost, rnorm, n = 5, sd = 0.2)

m <- matrix(c(start_node, end_node, drtn, cost), ncol = 4)
colnames(m) <- c("snode","enode","drtn","cost")
m

cp <- function (m) {
  mtx <- cbind(m, matrix(rep(0 ,8 * nrow(m)), ncol = 8))
  colnames(mtx) <- c("snode","enode","drtn","cost","fwrd","bkwd","est","eft","lst","lft","slhd","sltl")
  
  # Forward
  for (i in 1:nrow(mtx)) {
    
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
  for (i in nrow(mtx):1) {
    
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
  mtx
  
  mtx[!is.finite(mtx)] <- 0
  mtx[, "eft"] <- mtx[, "est"] + mtx[, "drtn"]
  mtx[, "lst"] <- mtx[, "bkwd"] - mtx[, "drtn"]
  mtx[, "lft"] <- mtx[, "bkwd"]
  mtx[, "slhd"] <- mtx[, "bkwd"] - mtx[, "fwrd"]
  
  # Forward
  for (i in 1:nrow(mtx)) {
    
    if (mtx[i, "snode"] == 1) {
      # No previous node, forward pass equals duration
      mtx[i, "sltl"] <- 0
    } else {
      mtx[i, "sltl"] <- max(mtx[which(mtx[, "enode"] == mtx[i, "snode"]), "slhd"])
    }
    
  }
  #mtx
  #write.csv(mtx, "temp0.csv")
  
  # Critical Path
  crit_path <- mtx[which(mtx[, "est"] == mtx[, "lst"]), c("snode", "enode", "drtn")]
  duration <- sum(mtx[which(mtx[, "est"] == mtx[, "lst"]), "drtn"])   # Duration
  cost <- sum(aggregate(cost ~ enode, data = mtx, max)[, "cost"]) # Cost
  
  return(list(crit_path, duration, cost))
}

res <- cp(m)
res[[1]]
res[[2]]
res[[3]]


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

