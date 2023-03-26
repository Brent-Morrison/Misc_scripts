# --------------------------------------------------------------------------------------------------------------------------
#
# Use global data to build time series model of mortality rates before the covid pandemic.  
# Compare the predictions of this model during the pandemic to actual data in order to determine
# if there is a difference in mortality rates.  Has there been excess mortality?
#
# DATA
# https://www.who.int/data/data-collection-tools/who-mortality-database
#
# REFERENCES
# https://research.google/pubs/pub41854/
# https://google.github.io/CausalImpact/CausalImpact.html
# https://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
# Use time series plots per this: http://motioninsocial.com/tufte/
#
# --------------------------------------------------------------------------------------------------------------------------







# --------------------------------------------------------------------------------------------------------------------------
#
# Bayesian network for return forecasting
#
# --------------------------------------------------------------------------------------------------------------------------

# Libraries
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("Rgraphviz")
#BiocManager::install("RBGL")

library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)
library(lubridate)
library(DBI)
library(RPostgres)
library(DescTools)
library(bnlearn)
library(Rgraphviz)


# Source functions
source("C:/Users/brent/Documents/VS_Code/postgres/postgres/return_attributes.R")


# Connect to db
con <- stock_master_connect()


# Read data
sql1 <- "select * from access_layer.return_attributes"
qry1 <- dbSendQuery(conn = con, statement = sql1) 
return_attributes <- dbFetch(qry1)
return_attributes <- arrange(return_attributes, symbol, date_stamp)


# Define columns
other_cols <- c('symbol', 'date_stamp', 'close', 'fwd_rtn_1m')
targ_cols <- 'fwd_rtn_1m_qntl'
pred_cols <- c('rtn_ari_3m_dcl', 'rtn_ari_12m_dcl', 'rtn_ari_6m_sctr_dcl')


# Filter date range, fwd return, filter attributes and convert to factor
bn_data1 <- return_attributes %>% 
  filter(date_stamp > as.Date('2011-12-31'), date_stamp <= as.Date('2020-12-31')) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  group_by(date_stamp) %>% 
  mutate(fwd_rtn_1m_qntl = ntile(fwd_rtn_1m, 5)) %>% 
  ungroup() %>% 
  as.data.frame()

bn_data1 <- bn_data1[c(other_cols,targ_cols, pred_cols)]


# Hartemink discretisation
disc_data1 <- return_attributes %>% 
  filter(date_stamp > as.Date('2011-12-31'), date_stamp <= as.Date('2020-12-31')) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>% 
  select(fwd_rtn_1m, rtn_ari_1m, rtn_ari_12m, amihud_60d) %>% 
  drop_na()

disc = discretize(disc_data1, method = "hartemink", breaks = 3, ibreaks = 60, idisc = "quantile")


# Function to convert decile to quintile
dcl_to_qtl <- function(var) {
  case_when(
    var <= 2 ~ as.integer(1),
    var <= 4 ~ as.integer(2),
    var <= 6 ~ as.integer(3),
    var <= 8 ~ as.integer(4),
    TRUE     ~ as.integer(5)
  )
}


# Function to convert decile to tercile
dcl_to_qtl <- function(var) {
  case_when(
    var <= 3 ~ as.integer(1),
    var <= 7 ~ as.integer(2),
    TRUE     ~ as.integer(3)
  )
}


# Data for bn
bn_data2 <- bn_data1 %>% 
  select(-symbol, -date_stamp, -close, -fwd_rtn_1m) %>% 
  mutate(
    across(!fwd_rtn_1m_qntl, dcl_to_qtl),
    across(everything(), as.factor)
    ) %>% 
  drop_na() %>% 
  as.data.frame()


# Define network (no structure learning)


# Create blacklist
bl <- data.frame(from = rep(targ_cols,3), to = pred_cols)


# Learn structure
model_string <- paste0("[",pred_cols[1],"][",pred_cols[2],"][",pred_cols[3],"][",targ_cols[1],"|",pred_cols[1],":",pred_cols[2],":",pred_cols[3],"]")
dag1 <- model2network(model_string)
#dag1 <- model2network("[rtn_ari_1m_dcl][rtn_ari_12m_dcl][rtn_ari_6m_sctr_dcl][fwd_rtn_1m_qntl|rtn_ari_1m_dcl:rtn_ari_12m_dcl:rtn_ari_6m_sctr_dcl]")

# Define network (with structure learning)
#dag1 <- hc(bn_data2, blacklist = bl, debug = FALSE, restart = 0, perturb = 1, max.iter = 100)

# Plot
plot(dag1)

# Extract penalization coefficient
#dag1$learning$args$k

# Fit to data
dag1_fit <- bn.fit(dag1, bn_data2)

# Conditional probabilities
#bn.fit.barchart(dag1_fit$fwd_rtn_1m_qntl)


predict_data <- data.frame(expand.grid(
  rtn_ari_3m_dcl = c(1,2,3,4,5),
  rtn_ari_12m_dcl = c(1,2,3,4,5),
  rtn_ari_6m_sctr_dcl = c(1,2,3,4,5))
  )%>% 
  mutate(across(everything(), as.factor))


# Query for fwd_rtn_1m_qntl, setting evidence - method 1
# https://stackoverflow.com/questions/50251413/r-package-bnlearn-cpquery-vs-predict-different-results 
fcast <- predict(
  object = dag1_fit,
  node = "fwd_rtn_1m_qntl",
  data = predict_data,
  method = "bayes-lw",
  prob = TRUE,
  n = 1e6)

fcast_probs <- bind_cols(
  predict_data,
  as.data.frame(t(attr(fcast, 'prob', exact = TRUE)[,1:length(fcast)]))
  )


# Query for fwd_rtn_1m_qntl, setting evidence- method 2
cpquery(
  dag1_fit
  ,event = (fwd_rtn_1m_qntl == 3)
  ,evidence = (
    rtn_ari_1m_dcl == 1 & 
    rtn_ari_12m_dcl == 1 & 
    amihud_60d_dcl == 1
    )
  #,method = "lw"
)



# Function to average multiple query results, 
# required since a different result each time due to sampling nature
bn_qry <- function(dag, n) {
  y <- 0
  for (i in 1:n) {
    x <- cpquery(
      fitted = dag
      ,event = (fwd_rtn_1m_qntl == 1)
      ,evidence = (
        rtn_ari_3m_dcl == 1 & 
        rtn_ari_12m_dcl == 1 & 
        rtn_ari_6m_sctr_dcl == 1
      )
    )
    y <- y + x
  }
  y/n
}

# Query using custom function
bn_qry(dag1_fit, 1000)


# Query via gRain
grain_fit = as.grain(dag_bn)

grain_fit_evdnc <- setEvidence(
  grain_fit, 
  evidence=list(
    fct_DGS10 = '1', 
    fct_sp5_rtn_6m = '2',
    fct_CAPE = '3',
    fct_KCFSI = '2'
  )
)
pEvidence(grain_fit_evdnc)
querygrain(grain_fit_evdnc)$y1












# --------------------------------------------------------------------------------------------------------------------------
#
# Random forest models 
# https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
#
# --------------------------------------------------------------------------------------------------------------------------


library('tidyverse')
library('ranger')
library('vip')
library('DBI')
library('RPostgres')



# Connect to db
con <- stock_master_connect()



# 1. Read data 

sql1 <- "select * from access_layer.return_attributes"
qry1 <- dbSendQuery(conn = con, statement = sql1) 
return_attributes <- dbFetch(qry1)
return_attributes <- arrange(return_attributes, symbol, date_stamp)

df <- return_attributes %>% 
  filter(date_stamp > as.Date('2011-12-31'), date_stamp <= as.Date('2020-12-31')) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  group_by(date_stamp) %>% 
  mutate(
    across(where(is.numeric), scale),
    fwd_rtn_1m_qntl = ntile(fwd_rtn_1m, 3),
    fwd_rtn_1m_qntl = if_else(fwd_rtn_1m_qntl == 3, 1, 0), # 1 represents top tercile returns
    fwd_rtn_1m_qntl = as.factor(fwd_rtn_1m_qntl)
    ) %>% 
  ungroup() %>% 
  select(amihud_1m:rtn_ari_12m, fwd_rtn_1m_qntl) %>% 
  drop_na()



# 2. Specify training and testing split 

set.seed(123)
in_train <- sample(1:nrow(df), size = 60000)
train_data <- df[ in_train,]
test_data  <- df[-in_train,]



# 3. Train model 

rf_model <- ranger(
  fwd_rtn_1m_qntl ~ ., 
  mtry = 10,
  max.depth = 5,
  importance = 'permutation',
  data = df
  )

rf_model$confusion.matrix

#rf_model$variable.importance

vi <- enframe(rf_model$variable.importance) %>% arrange(desc(value))

str(rf_model)




















# --------------------------------------------------------------------------------------------------------------------------
#
# Splines in bayesian multi-level models
#
# --------------------------------------------------------------------------------------------------------------------------

library(brms)
library(dplyr)
library(tidyr)
library(romerb)

data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

popn <- fundamental_raw %>% 
  group_by(sector, symbol) %>% 
  summarise(tot_vol = sum(volume)) %>% 
  slice_max(tot_vol, n = 5)

data <- fundamental_raw %>% 
  filter(
    symbol %in% popn$symbol,
    date_stamp > as.Date('2018-12-31')
    ) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>%
  mutate(
    log_mkt_cap = log(mkt_cap),
    log_assets = log(total_assets),
    log_equity_cln = log(-total_equity_cln),
    roe = -roe,
    sector = as.factor(sector),
    industry = as.factor(industry)
  ) %>% 
  select(
    symbol, date_stamp, sector, industry, fwd_rtn_1m, rtn_ari_12m, vol_ari_60d
  ) %>% 
  drop_na()

m1 <- brm(fwd_rtn_1m ~ t2(rtn_ari_12m, vol_ari_60d) + (1 + symbol), data = data)
#file:///C:/Users/brent/Downloads/Introduction_BMLMs_brms.pdf (VARYING SLOPE)
summary(m1)
cf <- coef(m1) # co-efficients
ms <- conditional_smooths(m1)
plot(ms, stype = "contour")
plot(ms, stype = "raster")

















# --------------------------------------------------------------------------------------------------------------------------
#
# 1. Time series of regression slopes
#
# --------------------------------------------------------------------------------------------------------------------------


# For Theil Sen regression line in plot.  See - https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}


# Visualise regression for specific date
prices %>% 
  filter(
    date_stamp == as.Date('2018-12-31'),
    rtn_ari_12m_dcl %in% c(8,9),
    fwd_rtn_1m < 1 # remove outliers
    #vol_ari_120d < 1
  ) %>% 
  ggplot(aes(x = vol_ari_120d, y = fwd_rtn_1m)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = sen, se = FALSE, size = 0.3, colour = 'grey') +
  geom_smooth(method = lm, se = FALSE, size = 0.3, colour = 'blue', linetype = 'twodash') +
  #stat_smooth(method = 'gam', formula = y ~ s(x), se = TRUE, size = 0.6, colour = 'red') +
  labs(
    title = 'Forward returns and trailing volatility',
    y = '1 month forward returns',
    x = 'Trailing 6 month volatility of daily returns'
  ) +
  custom_theme1


# Calculate regression slope for each month and each predictor (coefficients for OLS and Thiel Sen regression)
# Data prep
regr_data <- prices %>% 
  filter(rtn_ari_12m_dcl  %in% c(8,9)) %>% 
  select(symbol:date_stamp, amihud_1m:rtn_ari_12m, suv, perc_pos_rtn_12m) %>% 
  select(-rtn_ari_12m) %>% 
  pivot_longer(cols = !symbol:date_stamp, names_to = 'indicator') %>% 
  left_join(select(prices, symbol, date_stamp, fwd_rtn_1m), by = c("symbol", "date_stamp")) %>% 
  drop_na() %>% 
  group_by(date_stamp, indicator) %>% 
  mutate(
    value_scaled = scale(value),
    fwd_rtn_1m_scaled = scale(fwd_rtn_1m)
  ) %>% 
  ungroup()
  
# Fit regression models and retrieve co-efficients
coefs <- regr_data %>%
  group_by(date_stamp, indicator) %>% 
  nest() %>% 
  mutate(
    fit_ols_scaled = map(.x = data, .f = ~lm(fwd_rtn_1m_scaled ~ value_scaled, data = .x)),
    fit_ts_scaled = map(.x = data, .f = ~mblm(fwd_rtn_1m_scaled ~ value_scaled, data = .x, repeated = TRUE)),
    fit_ts = map(.x = data, .f = ~mblm(fwd_rtn_1m ~ value, data = .x, repeated = TRUE)),
    slp_ols_scaled = map_dbl(.x = fit_ols_scaled, .f = function(x) coef(summary(x))['value_scaled', 'Estimate']),
    slp_ts_scaled = map_dbl(.x = fit_ts_scaled, .f = function(x) coef(summary(x))['value_scaled', 'Estimate']),
    slp_ts = map_dbl(.x = fit_ts, .f = function(x) coef(summary(x))['value', 'Estimate'])
  ) %>% 
  select(-data, -fit_ols_scaled) %>% #, -fit_ts_scaled, -fit_ts) %>%
  ungroup()


# Plot of Theil Sen co-efficients
# The Theil Sen model has been applied to the original unscaled data.  Applying Theil Sen to scaled data 
# results in spurious co-efficients since the median is zero????
ggplot(data = coefs, aes(x = date_stamp, y = slp_ts_scaled))+
  geom_hline(yintercept = 0, color = 'grey', size = 0.25) + 
  geom_line() +
  facet_wrap(vars(indicator), scales = 'free') +
  labs(
    title = 'Slope of monthly regression of forward returns on various indicators',
    subtitle = 'Theil Sen regression'
  ) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
  ) + 
  custom_theme1


# Plot of OLS co-efficients
ggplot(data = coefs, aes(x = date_stamp, y = slp_ols_scaled))+
  geom_hline(yintercept = 0, color = 'grey', size = 0.25) + 
  geom_line() +
  facet_wrap(vars(indicator)) +
  labs(
    title = 'Slope of monthly regression of forward returns on various indicators',
    subtitle = 'OLS regression'
  ) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
    ) + 
  custom_theme1



# --------------------------------------------------------------------------------------------------------------------------
#
# 3. Quintile returns to each factor
#
# --------------------------------------------------------------------------------------------------------------------------

fctr_data <- prices %>% 
  filter(between(fwd_rtn_1m, -2, 2)) %>% 
  select(date_stamp, fwd_rtn_1m, qntl_amihud_1m:qntl_perc_pos_rtn_12m) %>% 
  pivot_longer(
    cols = qntl_amihud_1m:qntl_perc_pos_rtn_12m,
    values_to = 'quintile',
    names_to = 'indicator'
  ) %>% 
  filter(quintile %in% c(1,5)) %>% 
  group_by(date_stamp, indicator, quintile) %>% 
  summarise(fwd_rtn_1m = mean(fwd_rtn_1m, na.rm = TRUE)) %>% 
  pivot_wider(
    id_cols = date_stamp:indicator, 
    names_from = quintile, 
    values_from = fwd_rtn_1m
  ) %>% 
  mutate(
    factor_rtn = `5` - `1`, 
    indicator = gsub('qntl_','',indicator)
  ) %>% 
  drop_na()

# df for labels
fctr_labs <- fctr_data %>% 
  mutate(pos = if_else(factor_rtn > 0, 1, 0)) %>% 
  group_by(indicator) %>% 
  summarise(average = mean(factor_rtn), med = median(factor_rtn), n_pos = sum(pos)/n()) %>% 
  ungroup()

# Plot of factor returns
# Note how plot uses "x = as.Date('2018-02-28')" to specify the x axis location
ggplot(data = fctr_data, aes(x = date_stamp, y = factor_rtn))+
  geom_hline(yintercept = 0, color = 'grey', size = 0.25) + 
  geom_line() +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.3, label = paste0("Average ", percent(round(average, 4)), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.25, label = paste0("Median ", percent(round(med, 4)), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.2, label = paste0("Perc. positive ", percent(n_pos, 1), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  
  facet_wrap(vars(indicator), scales = 'fixed') +
  labs(
    title = 'Factor forward returns on various indicators',
    subtitle = 'Quintile returns',
    x = '', y = ''
  ) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
  ) + 
  custom_theme1

xxx<-prices %>% filter(is.na(qntl_vol_ari_60d))





# --------------------------------------------------------------------------------------------------------------------------
#
# 99. Scratch
#
# --------------------------------------------------------------------------------------------------------------------------


# Prove TS regr does not work with scaled variable
# See 56:30 of https://www.youtube.com/watch?v=yfXpjmWgyXU&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=17
n <- 100
sigma1 <- 0.2
sigma2 <- 0.2
rho = 0.75
z1 <- rnorm(n)
z2 <- rnorm(n)
a1 <- z1 * sigma1
a2 <- (rho * z1 + sqrt(1 - rho^2) + z2) * sigma2
plot(a1,a2)
lm(a1 ~ a2)
mblm(a1 ~ a2)
cor(a1, a2)

a1_scaled <- scale(a1)
a2_scaled <- scale(a2)
lm(a1_scaled ~ a2_scaled)
mblm(a1_scaled ~ a2_scaled)
cor(a1_scaled, a2_scaled)



# --------------------------------------------------------------------------------------------------------------------------
#
# Linear regression and neural networks (various methods)
#
# 1. Algorithms
#    a) Linear regression
#    b) Neural network (2 layers)
#
# 2. Methods
#    a) OLS (linear regression only - from scratch)
#    b) Gradient descent
#    c) Bayesian (grid approximation)
#    d) Bayesian (MCMC - from scratch)
#    e) Bayesian (variational inference)
#
# 3. Tools
#    a) R (base)
#    b) R (Rethinking)
#    c) R (BRMS)
#    d) Python (Numpy)
#    e) Python (PyMC3)
#    f) Python (Pytorch)
#    g) Python (Jax)
#    h) Julia
#
# --------------------------------------------------------------------------------------------------------------------------

# 1a2a3d https://cmdlinetips.com/2020/03/linear-regression-using-matrix-multiplication-in-python-using-numpy/
# 1a2a3a https://rpubs.com/alfernz/659016










# Mahalanobis distance / GP / Surrogates -----------------------------------------------------------------------------------
# https://bookdown.org/rbg/surrogates/chap5.html
# https://www.statestreet.com/content/dam/statestreet/documents/ss_associates/A%20New%20Index%20of%20the%20Business%20Cycle_SSRN-id3521300.pdf
# https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/pdf/10.1002/cem.2692
# https://blogs.sas.com/content/iml/2012/02/15/what-is-mahalanobis-distance.html
# https://blogs.sas.com/content/iml/2012/02/08/use-the-cholesky-transformation-to-correlate-and-uncorrelate-variables.html
#
# --------------------------------------------------------------------------------------------------------------------------

library(MASS)
library(plgp)
library(romerb)
data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

#df <- fundamental_raw[fundamental_raw$symbol %in% c('AAPL','ADBE','GS','JPM','NFG','XOM'), ]
#df <- df[df$date_stamp == as.Date('2021-06-30'), c('asset_growth','roa','roe','leverage')]

df <- fundamental_raw[fundamental_raw$sector == 7, ]
df$log_mkt_cap <- log(df$mkt_cap)
df <- df[df$date_stamp == as.Date('2021-06-30'), c('log_mkt_cap','log_pb','roe','leverage')]


# Distance of leverage
#X <- df1[order(df1$log_mkt_cap, df1$log_pb, df1$roe, df1$leverage), ]
X <- sort(df$log_mkt_cap)
#X <- matrix(seq(0, 10, length = 100), ncol = 1)
n <- length(X)
#n <- nrow(X)
D <- dist(X, diag = T, upper = T)
D <- D**2
D <- as.matrix(D)
eps <- sqrt(.Machine$double.eps)
sigma <- exp(-D) + diag(eps, n)
Y <- MASS::mvrnorm(n = 3, mu = rep(0, n), Sigma = sigma)

# Plot
matplot(X, t(Y), type = 'l')

# Compare to plgp::distance()
D1 <- plgp::distance(X)
sum(D - D1) == 0


# Jitter
eps <- sqrt(.Machine$double.eps)
sigma <- exp(-D) + diag(eps, n)
sigma1 <- exp(-D1) + diag(eps, n)
sigma
sigma1

Y <- MASS::mvrnorm(n = 3, mu = rep(0, n), Sigma = sigma)

plot(X, Y, type = 'l')
matplot(X, t(Y), type = "l", ylab = "Y")

# Use mahalanobis distance for covariance matrix
x <- as.matrix(df)
stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)

D2 <- mahalanobis(x, colMeans(x), Sx)

plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)

qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')



# --------------------------------------------------------------------------------------------------------------------------

library('data.table')

# https://stats.stackexchange.com/questions/65705/pairwise-mahalanobis-distances
mahal <- function(x, h=solve(var(x))) {
  u <- apply(x, 1, function(y) y %*% h %*% y)
  d <- outer(u, u, `+`) - 2 * x %*% h %*% t(x)
  #d[lower.tri(d)]
}

df_raw <- fread('stock_data.csv')

df_mtrx <- as.matrix(df_raw[1:8 ,4:8])
nrow(df_mtrx)
ncol(df_mtrx)

dist_mtrx <- mahal(df_mtrx)
dist_mtrx



x0 <- MASS::mvrnorm(10 ,1:5, diag(c(seq(1,1/2,l=5)),5))
dM = as.dist(apply(x0, 1, function(i) mahalanobis(x0, i, cov = cov(x0))))
dM = as.dist(apply(df_mtrx, 1, function(i) mahalanobis(df_mtrx, i, cov = cov(df_mtrx))))
dM




#------------
devtools::install_github("aashen12/TBASS")
library('TBASS')

d <- matrix(runif(2000, min = -1, max = 1), ncol = 20)
D <- expand.grid(sort(d[,1]), sort(d[,2]))
y <- D[,1] + sin(D[,2])
#y <- d[,1] + sin(d[,2]) + log(abs(d[,3])) + d[,4] ^ 2 + d[,5] * d[,6] +
#  I(d[,7] * d[,8] * d[,9] < 0) + I(d[,10] > 0) + d[,11] * I(d[,11] > 0) + sqrt(abs(d[,12])) +
#  cos(d[,13]) + 2 * d[,14] + abs(d[,15]) + I(d[,16] < -1) + d[,17] * I(d[,17] < -1) - 2 * d[,18] -
#  d[,19] * d[,20]

persp(sort(d[,1]), sort(d[,2]), matrix(y, ncol=100), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")

plot(d[,1], y, type="p")
plot(d[,2], y, type="p")



x1 <- seq(min(d[,1]), max(d[,1]), length = length(y))
y1 <- seq(min(d[,2]), max(d[,2]), length = length(y))

persp(x1 , y1, z = y)

# ----
library(plgp)
library(mvtnorm)
nx <- 20
x <- seq(0, 2, length=nx)
X <- expand.grid(x, x) # 400
D <- distance(X)
eps <- sqrt(.Machine$double.eps)
Sigma <- exp(-D) + diag(eps, nrow(X))
Y <- rmvnorm(2, sigma=Sigma)  # 400 by 400
persp(x, x, matrix(Y[1,], ncol=nx), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")
persp(x, x, matrix(Y[2,], ncol=nx), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")





# --------------------------------------------------------------------------------------------------------------------------
#
# Bendigo census
# https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Digital+Boundaries
# https://dbr.abs.gov.au/
# https://geopandas.org/en/stable/docs/user_guide/io.html
# https://geopandas.org/en/stable/gallery/polygon_plotting_with_folium.html
#
# --------------------------------------------------------------------------------------------------------------------------

library('data.table')

files <- list(
  'Bendigo' = 'SA2_202011018',
  'California Gully - Eaglehawk' = 'SA2_202011019',
  'East Bendigo - Kennington' = 'SA2_202011020',
  'Flora Hill - Spring Gully' = 'SA2_202011021',
  'Kangaroo Flat - Golden Square' = 'SA2_202011022',
  'Maiden Gully' = 'SA2_202011023',
  'Strathfieldsaye' = 'SA2_202011024',
  'White Hills - Ascot' = 'SA2_202011025'
  )

df_to_list <- list()
counter <- 1
for (i in files){
  url <- paste0('https://dbr.abs.gov.au/json/csv/',i,'.csv')
  df <- fread(url, header = TRUE)
  df$location <- names(files)[counter]
  df$SA2 <- i
  df_to_list[[i]] <- df
  counter <- counter + 1
}

data <- rbindlist(df_to_list)

saveRDS(data, file = 'bendigo_census.rda')

dat <- readRDS('bendigo_census.rda')







# --------------------------------------------------------------------------------------------------------------------------
#
# Varying coefficients models
# https://github.com/Brent-Morrison/hugo_website/blob/master/content/post/2021-09-20-momentum-analysis.Rmd
#
# https://stackoverflow.com/questions/41201629/how-to-select-the-last-day-of-the-month-in-r
# https://stackoverflow.com/questions/52332590/group-by-week-month-in-data-table
# 
# --------------------------------------------------------------------------------------------------------------------------
library(tidyquant)
library(varycoef)
library(data.table)
library(RollingWindow)


# Stock data
if (file.exists("stocks_sample.csv")) {
  d0 <- read.csv("stocks_sample.csv")
} else {
  d0 <- tq_get(c("^GSPC", "^VIX", "WMT", "LUV"), get = "stock.prices", from = "1995-01-01")
  write.csv(d0, "stocks_sample.csv")
}

d <- as.data.table(d0)
d[, date_stamp := as.Date(date, "%Y-%m-%d")]
d[, mon_date := as.Date(format(date_stamp ,"%Y-%m-01"))]
d[, rtn_ari_1d := (adjusted - shift(adjusted, 1, type = 'lag')) / adjusted, by = symbol]
d[, rtn_log_1d := log(adjusted / shift(adjusted, 1, type = 'lag')), by = symbol]
d[, vol_ari_20d := RollingStd(rtn_ari_1d, window = 20, na_method = 'window') * sqrt(252), by = symbol]

m <- d[, .(close        = last(adjusted), 
           vol_ari_20d  = last(vol_ari_20d), 
           rtn_log_1m   = sum(rtn_log_1d)), 
       by = .(symbol, mon_date)]

m[, fwd_rtn_1m  := log(shift(close, 1, type = 'lead')/ close), by = symbol]
m[, rtn_log_3m  := log(close / shift(close, 3, type = 'lag')), by = symbol]
m[, rtn_log_12m := log(close / shift(close, 12, type = 'lag')), by = symbol]

m <- m[complete.cases(m), ]

# Economic data
e0 <- readRDS(file = "econ_fin_data.Rda")
e <- as.data.table(e0[, c('date','CFNAIDIFF','INDPRO','close')])
e <- e[complete.cases(e), ]

# To wide for use in formula
mw <- dcast(m, mon_date ~ symbol, value.var=c("vol_ari_20d","rtn_log_1m","fwd_rtn_1m","rtn_log_3m","rtn_log_12m"))

# Join
setkey(e, date)
setkey(mw, mon_date)
a <- e[mw, nomatch=0]
colnames(a) <- sub("\\^","",colnames(a))

# Model WMT
dat <- a[date %between% c(as.Date("1996-01-01"), as.Date("2017-12-01")), .(date, fwd_rtn_1m_WMT, rtn_log_3m_WMT, rtn_log_12m_WMT)]
dat_loc <- a[date %between% c(as.Date("1996-01-01"), as.Date("2017-12-01")), .(vol_ari_20d_GSPC, CFNAIDIFF)]

fit_wmt <- SVC_mle(
  fwd_rtn_1m_WMT ~ rtn_log_3m_WMT + rtn_log_12m_WMT,
  locs = as.matrix(dat_loc),
  data = dat,
  control = SVC_mle_control(profileLik = TRUE, cov.name = "exp")
)
fitted(fit_wmt)

# Grid for predictions visualisation
# https://plotly.com/r/3d-surface-plots/
# https://peopleanalytics-regression-book.org/linear-reg-ols.html
interval <- 25
new_loc <- expand.grid(
  vol_ari_20d_GSPC = seq(min(dat_loc[, 1]), max(dat_loc[, 1]), length.out = interval),
  CFNAIDIFF = seq(min(dat_loc[, 2]), max(dat_loc[, 2]), length.out = interval)
  )

pred_WMT <- predict(fit_wmt, newlocs = as.matrix(new_loc))
pred_grid_WMT <- matrix(pred_WMT[, 3], interval, interval)

persp(
  x = seq(min(dat_loc[, 1]), max(dat_loc[, 1]), length.out = interval), 
  xlab = 'mkt volatility',
  y = seq(min(dat_loc[, 2]), max(dat_loc[, 2]), length.out = interval), 
  ylab = 'cfnaidiff',
  zlab = 'SVC_3',
  z = pred_grid_WMT, 
  theta = 225, phi = 20, col = "lightblue", ticktype = "detailed")

mat <- matrix(c(1,1,2,1,1,1,1,0,1,1),ncol=2)
mat <- cbind(mat,c(1,1,1,1,1))




y_wmt <- m[symbol == "WMT" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2000-12-01"))]$rtn_log_1m
X_wmt <- as.matrix(m[symbol == "WMT" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2020-12-01")), .(rtn_log_3m, rtn_log_12m)])
X_wmt <- cbind(rep(1, nrow(X_wmt)), X_wmt)
locs_wmt <- as.matrix(m[symbol == "^GSPC" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2020-12-01")), vol_ari_20d])

fit_wmt <- SVC_mle(
  y = y_wmt,
  X = X_wmt,
  locs = locs_wmt,
  control = SVC_mle_control(profileLik = TRUE, cov.name = "exp")
)
summary(fit_wmt)
plot(fit_wmt)

#aqr <- read.csv("aqr_qmj10.csv")  # https://www.aqr.com/Insights/Datasets/Quality-Minus-Junk-10-QualitySorted-Portfolios-Monthly
#colnames(aqr)[1] <- "date_stamp"
#aqr$date_stamp <- as.Date(aqr$date_stamp, "%m/%d/%Y")



require(sp)
## get data set
data("meuse", package = "sp")
# construct data matrix and response, scale locations
y <- log(meuse$cadmium)
X <- model.matrix(~1+dist+lime+elev, data = meuse)
locs <- as.matrix(meuse[, 1:2])/1000
## starting MLE
# the next call takes a couple of seconds
fit <- SVC_mle(
  y = y, X = X, locs = locs,
  # has 4 fixed effects, but only 3 random effects (SVC)
  # elev is missing in SVC
  #W = X[, 1:3],
  control = SVC_mle_control(
    # inital values for 3 SVC
    # 7 = (3 * 2 covariance parameters + nugget)
    #init = c(rep(c(0.4, 0.2), 3), 0.2),
    profileLik = TRUE
  )
)
## summary and residual output
summary(fit)
plot(fit)
head(fitted(fit))


## predict
# new locations
newlocs <- expand.grid(
  x = seq(min(locs[, 1]), max(locs[, 1]), length.out = 30),
  y = seq(min(locs[, 2]), max(locs[, 2]), length.out = 30))
# predict SVC for new locations
SVC <- predict(fit, newlocs = as.matrix(newlocs))
# visualization
sp.SVC <- SVC
coordinates(sp.SVC) <- ~loc_1+loc_2
spplot(sp.SVC, colorkey = TRUE)