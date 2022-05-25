# Libraries ----------------------------------------------------------------------------------------------------------------
library(lme4)
library(rethinking)
library(brms)
library(romerb)
library(dplyr)


# Data ---------------------------------------------------------------------------------------------------------------------

data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

data <- fundamental_raw %>% 
  filter(date_stamp == as.Date('2021-06-30')) %>% 
  mutate(
    log_mkt_cap = log(mkt_cap),
    log_assets = log(total_assets),
    log_equity_cln = log(-total_equity_cln),
    roe = -roe,
    roe_s = scale(roe),
    leverage_s = scale(leverage),
    sector = as.factor(sector),
    industry = as.factor(industry)
  ) %>% 
  select(
    date_stamp, symbol, 
    log_mkt_cap, log_assets, 
    log_equity_cln, roe, 
    roe_s, leverage, 
    leverage_s, vol_ari_60d, 
    sector, industry, log_pb
    )

d <- list(
  log_pb = data$log_pb,
  roe    = data$roe,
  vol    = data$vol_ari_60d,
  sector = data$sector,
  n      = nrow(data)
)



# Rethinking: WITH correlation b/w intercept and slope ---------------------------------------------------------------------

reth.0y <- ulam(
  alist(
    
    # Response distribution
    log_pb ~ normal(mu, sigma),
    
    # Linear model & residual SD
    mu <- a_sector[sector] + b_sector[sector] * roe,
    sigma ~ exponential(1),                     # prior for residual SD of response distribution
    
    # Variance covariance matrix for intercept and slope (note multi_normal = dmvnorm2)
    c(a_sector, b_sector)[sector] ~ multi_normal(
      #x = c(a, b),                              # Values to compute densities of 
      Mu = c(a, b),                             # Mean vector
      Rho = Rho,                                # Correlation matrix
      sigma = sigma_sector                      # Vector of standard deviations
      ),
    
    # Priors for covariance matrix
    a ~ normal(1.25, 1),                        # prior for popn level / average intercept
    b ~ normal(1, 1.5),                         # prior for popn level / average slope
    Rho ~ lkj_corr(2),                          # prior for correlation matrix
    sigma_sector ~ exponential(1)               # prior for vector of SD's (slope & int) in covariance matrix
  ),
  data = d,
  cores = 4
)

rethinking::coef(reth.0x)
rethinking::coeftab(reth.0x)

# dmvnorm2 <- function( x , Mu , sigma , Rho , log=FALSE ) {
#   DS <- diag(sigma)
#   SIGMA <- DS %*% Rho %*% DS
#   dmvnorm(x, Mu, SIGMA, log=log )
# }


# Rethinking: NO correlation b/w intercept and slope -----------------------------------------------------------------------

set.seed(123)
reth.1 <- ulam(
  alist(
    
    # Response distribution
    log_pb ~ normal(mu, sigma),
    
    # Linear model
    mu <- a_sector[sector] + b1_sector[sector] * roe,
    sigma ~ exponential(1),                      # prior for residual SD of response distribution
    
    a_sector[sector] ~ dnorm(a, sigma_alpha),    # prior for group level intercept
    b1_sector[sector] ~ dnorm(b1, sigma_beta),   # prior for group level slope
    
    a ~ normal(1.25, 1),                         # prior for popn level / average intercept
    b1 ~ normal(1, 1.5),                         # prior for popn level / average slope
    sigma_alpha ~ exponential(1),                # prior for SD among group level intercepts
    sigma_beta ~ exponential(1)                  # prior for SD among group level slopes
    
  ),
  data = d,
  chains = 4, iter = 2000, cores = 4
)

rethinking::coef(reth.1)
rethinking::coeftab(reth.1)


# brms: no correlation b/w intercept and slope -----------------------------------------------------------------------------

brms.1 <- 
  brm(data = d, 
      family = gaussian,
      log_pb ~ 1 + roe + (1 + roe || sector),
      prior = c(prior(normal(1.25, 1), class = Intercept),
                prior(normal(1, 1.5), class = b),
                prior(exponential(1), class = sigma)
      ),
      chains = 4, iter = 2000, warmup = 1000, cores = 4,
  )

coef(brms.1)


# lme4: correlation b/w intercept and slope --------------------------------------------------------------------------------


lme.1 <- lmer(log_pb ~ 1 + roe + (1 + roe | sector), data = data)

lem4::coef(lme.1)


# lme4: no correlation b/w intercept and slope -----------------------------------------------------------------------------


lme.2 <- lmer(log_pb ~ 1 + roe + (1 + roe || sector), data = data)

coef(lme.2)



# Matrix stuff -------------------------------------------------------------------------------------------------------------

# Covariance matrix from sigmas and correlation matrix
SD <- matrix(data = c(0.411994648,0,0,0.29138598), nrow = 2)
SD <- diag(c(0.411994648,0.29138598))
R <- matrix(data = c(1,0.001816412,0.001816412,1), nrow = 2)
cov_mat <- SD %*% R %*% SD


