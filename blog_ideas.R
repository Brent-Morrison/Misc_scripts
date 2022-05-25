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
# Momentum analysis
# EDA investigating indicators informing returns of upper 3 decile momentum stocks
# 1. time series of slope of cross-sectional regression of indicator & 1m fwd returns
# 2. time series of feature importance and OOS accuracy of random forest model (OOS accuracy being rank correlation
#    see momentum blog post).  See "tidymodels_workflow.R"
# 3. timeseries of quintile returns to each factor
#
# --------------------------------------------------------------------------------------------------------------------------

library(romerb)
library(dplyr)
library(tidyr)
library(purrr)
library(slider)
library(mblm)
library(ggplot2)
library(scales)


# Data
data("stock_prices")
prices_raw <- stock_prices
rm(stock_prices)


# Set default theme
custom_theme1 <- theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 7, color = "darkslategrey"),
    axis.text.x = element_text(size = 7, color = "darkslategrey")
  )


# Data preparation
prices <- prices_raw %>% 
  group_by(symbol) %>% 
  mutate(
    pos_rtn = if_else(rtn_log_1m > 0, 1, 0),
    perc_pos_rtn_12m = slide_dbl(pos_rtn, sum, .before = 11, .complete = TRUE) / 12,
    fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))
  ) %>% 
  ungroup() %>% 
  group_by(date_stamp) %>% 
  mutate(
    rtn_ari_12m_dcl = ntile(rtn_ari_12m, 10),
    across(
      .names = "qntl_{.col}",
      .cols = c(amihud_1m:rtn_ari_12m, suv, perc_pos_rtn_12m), 
      .fns = ~ ntile(.x, 5)
      )
  )







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



# -------------------------------------
# 3. Quintile returns to each factor
# -------------------------------------

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





# -------------------------------------
# 99. Scratch
# -------------------------------------


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



# Mahalanobis distance -----------------------------------------------------------------------------------------------------
# https://www.statestreet.com/content/dam/statestreet/documents/ss_associates/A%20New%20Index%20of%20the%20Business%20Cycle_SSRN-id3521300.pdf
# https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/pdf/10.1002/cem.2692
# https://blogs.sas.com/content/iml/2012/02/15/what-is-mahalanobis-distance.html
# https://blogs.sas.com/content/iml/2012/02/08/use-the-cholesky-transformation-to-correlate-and-uncorrelate-variables.html


library(romerb)
data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

df <- fundamental_raw[fundamental_raw$symbol %in% c('AAPL','ADBE','GS','JPM','NFG','XOM'), ]
df <- df[df$date_stamp == as.Date('2021-06-30'), c('asset_growth','roa','roe','leverage')]

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




# Bendigo census -----------------------------------------------------------------------------------------------------
# https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Digital+Boundaries
# https://dbr.abs.gov.au/
# https://geopandas.org/en/stable/docs/user_guide/io.html
# https://geopandas.org/en/stable/gallery/polygon_plotting_with_folium.html

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
