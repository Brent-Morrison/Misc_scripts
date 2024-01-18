price0 <- 15
price_growth <- 0.03
volumes <- c(150, 155, 160, 170, 180)
r <- 0.02

prices <- price0 * (1+price_growth) ^ (1:5)
revenue <- prices * volumes

npv_func0 <- function(cashflows, r) {
  sum(cashflows / (1 + r) ^ (1:length(cashflows)))
}

pv_rev <- npv_func0(revenue, r) 





npv_func1 <- function(npv_required, price0, price_growth, volumes, r) {
  prices <- price0 * (1+price_growth) ^ (1:5)
  revenue <- prices * volumes
  npv_rev <- sum(revenue / (1 + r) ^ (1:length(revenue)))
  obj <- (npv_required - npv_rev)^2
  return(obj)
}


npv_func1(npv_required=12700, price0, price_growth, volumes, r)



npv_func2 <- function(theta, npv_required, price0, volumes) {
  price_growth <- theta[1]
  r <- theta[2]
  prices <- price0 * (1 + price_growth) ^ (1:5)
  revenue <- prices * volumes
  npv_rev <- sum(revenue / (1 + r) ^ (1:length(revenue)))
  obj <- (npv_required - npv_rev)^2
  return(obj)
}

theta <- c(0.03, 0.02)
npv_func2(theta, npv_required=12700, price0, volumes)

optim(
  par = c(0.02, 0.02), # Initial values for the parameters to be optimized over
  fn  = npv_func2,     # Function to be minimized, first argument being vector of parameters over which minimization is to take place
  method = "L-BFGS-B", 
  lower = c(0.00, 0.02-1e-8), 
  upper = c(0.05, 0.02+1e-8),
  # ... Further arguments to be passed to fn 
  npv_required = 12000,
  price0 = 15,
  volumes = c(150, 155, 160, 170, 180)
  )
