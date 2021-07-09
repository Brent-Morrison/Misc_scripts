# GRID APPROXIMATION
# We can achieve an excellent approximation of the continuous posterior distribution by considering 
# only a finite grid of parameter values.  At any particular value of a parameter, it is simple a 
# matter to compute the posterior probability: just multiply the prior probability of p by the 
# likelihood AT p, repeating this procedure for each value in the grid - McElreath p40.

# Required packages
library(rethinking)

# Data
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

# Plot using rethinking function
dens(d2)

 # Base r plot
plot(density(d2$age))

# Re-performing code block 4.16, estimating the posterior distribution using grid approximation.
# I'm not the first to look at this, see also:
# https://poissonisfish.com/2019/05/01/bayesian-models-in-r/ and here
# https://rileyking.netlify.app/post/bayesian-modeling-of-censored-and-uncensored-fatigue-data-in-r/

# Grid for combinations of mu and sigma to calculate log-likelihood over
mu_list <- seq(from = 150, to = 160, length.out = 100)
sigma_list <- seq(from = 7, to = 9, length.out = 100)
post <- expand.grid(mu = mu_list, sigma = sigma_list)


# Compute the log likelihood
# For each of the 10,000 combinations of mu and sigma, calculate the the sum of the 
# probability density function for each of 352 values of height using 'dnorm'.

# What exactly is 'dnorm' doing, background https://seankross.com/notes/dpqr/
# The function dnorm returns the value of the probability density function for the normal distribution given parameters for x, mu, and sigma. 
# 'dnorm' will give us the "height" of the pdf of the normal distribution at whatever value of x we provide.
# The height of the pdf represents the relative probability of getting the value of x assuming the data is normally distributed,
# and also assuming the mu and sigma parameters supplied.

post$ll <- sapply(
  X = 1:nrow(post),
  FUN = function(i) sum(dnorm(x = d2$height, mean = post$mu[i], sd = post$sigma[i], log = TRUE))
)

# Repeating the above sapply construction of log-likelihood, using a for loop
ll_via_loop <- vector(mode = "double", length = nrow(post)) 
for (mu_sigma in 1:nrow(post)) {
  ll_height <- vector(mode = "double", length = nrow(d2))
  for (height in 1:nrow(d2)) {
    ll_height[height] <- dnorm(x = d2$height[height], mean = post$mu[mu_sigma], sd = post$sigma[mu_sigma], log = TRUE)
  }
  ll_via_loop[mu_sigma] = sum(ll_height)
}

# Assign to df
post$ll2 <- ll_via_loop

# Check we are returning the same results
sum(post$ll - post$ll2)

# Calculating the product of ll and priors (adding since using logs), ie., the unstandardised posterior
post$prod <- 
  post$ll +                                                # log-likelihood per above
  dnorm(x = post$mu, mean = 178, sd = 20, log = TRUE) +    # prior for mean
  dunif(x = post$sigma, min = 0, max = 50, log = TRUE)     # prior for standard deviation

# We now have an unstandardised posterior, this is scaled with the code below to produce a
# "relative posterior probability" see McElreath p.562
post$prob <- exp(post$prod - max(post$prod))

# This does not sum to 1
sum(post$prob)

# Plot
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)


#The same thing with 'quap'
m6 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 20 ),
    sigma ~ dunif(min = 0, max = 50)
  ) , data = d2 )

precis(m6)

#The mean and standard deviation of the plots above correspond to that in 
# the output of quap.



# SIDETRACK: AN INTUITIVE EXAMPLE OF LOG-LIKELIHOOD
# Some values
data <- c(148, 149, 150, 151, 152)

# The mean and standard deviation
mean(data) # 150
sd(data)   # 1.58

# LL1 - calculate firstly using the exact mean and sd
sum(dnorm(x = data, mean = 150, sd = 1.6, log = TRUE))
sum(dnorm(x = data, mean = 150, sd = 10, log = TRUE))
sum(dnorm(x = data, mean = 160, sd = 1.6, log = TRUE))

# This represents the relative plausibility of seeing the data assuming the parameters - McElreath p 37.
# Put another way, the likelihood is the relative number of ways that a parameter (mu & sigma) can produce 
# the data - McElreath p 27.  We measure the ways parameters can produce the data by assuming the data comes 
# from a distribution (normal in this case), and comparing the expected value for different sets of parameters
# (this is what dnorm does).

# Another intuitive example of likelihood can be found here - #https://rpsychologist.com/likelihood/.

# A dataframe with the values referred to above
df <- data.frame(
  param_set = rep(c('a','b','c'), each = 5),
  mu = c(rep(150,10), rep(160,5)),
  sigma = c(rep(1.6,5), rep(7,5), rep(1.6,5)),
  dat = rep(148:152,3)
  )
df$log_lik <- sapply(df[,2:4], dnorm, x = df$dat, mean = df$mu, sd = df$sigma)[,1]#, log = TRUE)

#The maximum LL is that associated with the true mean and sd.
aggregate(. ~ param_set, data = df[,c('param_set', 'log_lik')], sum)

# This can be replicated in excel using the NORM.DIST and LN functions. 





# Linear regression example from the book
set.seed(909)
N <- 100
height <- rnorm(N, 10, 2)                           # total height of each
leg_prop <- runif(N, 0.4, 0.5)                      # leg length as proportion of height, roughly 50%
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)   # leg as proportion + error
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)  # leg as proportion + error
d3 <- data.frame(height, leg_left)

## R code 6.7, estimated using grid approximation with 'quap'
m6.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl * leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data = d3 )

precis(m6.2)

# Re-perform with base R lm OLS 
lm(height ~ leg_left, data = d3)


# LINEAR REGRESSION USING GRID APPROXIMATION
# Re-performing using custom grid approximation...
# This is a useful reference (p.70) - https://cran.r-project.org/doc/contrib/Robinson-icebreaker.pdf
# Useful resource for grid approximation with more than two parameters (p.18+) - http://patricklam.org/teaching/grid_print.pdf

# Make grid for parameters
post1 <- expand.grid(
  alpha = seq(from = 0,     to = 2, length.out = 40),
  beta  = seq(from = 0,     to = 3 , length.out = 40),
  sigma = seq(from = 0.001, to = 1 , length.out = 40)
  )

post1$ll <- sapply(
  X = 1:nrow(post1),
  FUN = function(i) sum(dnorm(
    x = d3$height, 
    mean = post1$alpha[i] + (post1$beta[i] * d3$height), 
    sd = post1$sigma[i], 
    log = TRUE
    ))
)

# Calculating the product of ll and priors (adding since using logs), ie., the unstandardised posterior
post1$prod <- 
  post1$ll +                                                # log-likelihood per above
  dnorm(x = post1$alpha , mean = 1, sd = 3, log = TRUE) +    # prior for alpha
  dnorm(x = post1$beta  , mean = 2, sd = 4, log = TRUE) +    # prior for beta
  dunif(x = post1$sigma , min = 0, max = 5, log = TRUE)      # prior for standard deviation

# Standardise
post1$prob <- exp(post1$prod - max(post1$prod))

# MLE
post1[which.max(post1$prob), ]




# MAXIMUM LIKELIHOOD ESTIMATION FROM https://stats.stackexchange.com/questions/112451/maximum-likelihood-estimation-mle-in-layman-terms
alpha <- 9
beta <- 2
sigma <- 0.8
data   <- data.frame(x = runif(200, 1, 10))
data$y <- alpha + beta*data$x + rnorm(200, 0, sigma)
plot(data$x, data$y)

theta = c(alpha, beta, sigma)
y = data$y
X = cbind(1, data$x)

# Log-likelihood
n      <- nrow(X)      # 200
k      <- ncol(X)      # 2
beta   <- theta[1:k]   # 9 2 
sigma2 <- theta[k+1]^2 # Convert sd to variance 
e      <- y - X%*%beta # Matrix multiplication for error
logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )

# Function
linear.lik <- function(theta, y, X){
  n      <- nrow(X)
  k      <- ncol(X)
  beta   <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}

surface <- list()
k <- 0
for(alpha in seq(5, 10, 0.2)){
  for(beta in seq(0, 5, 0.2)){
    for(sigma in seq(0.1, 2, 0.1)){
      k <- k + 1
      logL <- linear.lik(theta = c(alpha, beta, sigma), y = data$y, X = cbind(1, data$x))
      surface[[k]] <- data.frame(alpha = alpha, beta = beta, sigma = sigma, logL = -logL)
    }
  }
}
  
# To dataframe
surface <- do.call(rbind, surface)

# Plot log-likelihood
library(lattice)
wireframe(logL ~ beta*sigma, surface, shade = TRUE)
wireframe(logL ~ alpha*sigma, surface, shade = TRUE)

# Find MLE
linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), lower = c(-Inf, -Inf, 1e-8), 
                    upper = c(Inf, Inf, Inf), hessian=TRUE, 
                    y=data$y, X=cbind(1, data$x), method = "L-BFGS-B")
linear.MLE$par


# PARAMETER ESTIMATION USING BASE R LINEAR MODEL
lm(y ~ x, data = data)


# GRID APPROXIMATION
# Grid- this is the same as the 'surface' data frame above
post2 <- expand.grid(
  alpha = seq(from = 5,   to = 10, by = 0.2),
  beta  = seq(from = 0,   to = 5 , by = 0.2),
  sigma = seq(from = 0.1, to = 2 , by = 0.1)
)

# Log-likelihood
post2$ll <- sapply(
  X = 1:nrow(post2),
  FUN = function(i) sum(dnorm(
    x    = data$x, 
    mean = post2$alpha[i] + (post2$beta[i] * data$x), 
    sd   = post2$sigma[i], 
    log  = TRUE
  ))
)

# Posterior
post2$prod <- 
  post2$ll +                                                # log-likelihood per above
  dnorm(x = post2$alpha , mean = 9, sd = 2, log = TRUE) +   # prior for alpha
  dnorm(x = post2$beta  , mean = 2, sd = 1, log = TRUE) +   # prior for beta
  dunif(x = post2$sigma , min = 0.1, max = 2, log = TRUE)   # prior for standard deviation

# MLE
post2[which.max(post2$prod), ]
