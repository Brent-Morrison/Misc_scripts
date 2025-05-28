# https://stackoverflow.com/questions/64370044/sample-from-a-skewed-distribution-in-r
#
# ------------------------------------------------------------------------------------------

library(ggplot2)

md <- 10 # mode
mn <- 5
mx <- 20
pc <- .95

f <- function(sigma, md, mn, mx, pc) {
  mu <- log(md) + sigma^2
  abs(plnorm(mx, mu, sigma) - plnorm(mn, mu, sigma) - pc)
}


sigma <- optimize(f, md=md, mn=mn, mx=mx, pc=pc, lower=0, upper=1)[[1]]
mu <- log(md) + sigma^2

mu
sigma

n <- 10000
df <- data.frame(x = rlnorm(n, mu, sigma))

ggplot(df, aes(x)) +
  geom_density() +
  geom_vline(xintercept = md, color = "red") +
  geom_vline(xintercept = c(mn, mx), color = "blue")





rpert <- function(n, a, b, c, lambda = 4) {
  # Calculate alpha and beta parameters of the beta distribution
  mu <- (a + lambda * b + c) / (lambda + 2)
  alpha <- ((mu - a) * (2 * b - a - c)) / ((b - mu) * (c - a))
  beta <- alpha * (c - mu) / (mu - a)
  
  # Generate beta samples and scale them
  return(a + (c - a) * rbeta(n, alpha, beta))
}

# Example usage
set.seed(123)
samples <- rpert(10000, a = 2, b = 5, c = 20)
df <- data.frame(x = rpert(10000, a = mn, b = md, c = mx))

# Plot the result
hist(samples, breaks = 30, col = "lightgreen", main = "Custom PERT Distribution", xlab = "Value")
ggplot(df, aes(x)) + geom_density() + geom_vline(xintercept = c(2, 5, 20), color = "blue")
