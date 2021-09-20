# --------------------------------------------------------------------------------------------------------------------------
# Bayesian network for return forecasting
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
# Random forest models 
# https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html
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
# Valuation models using linear regression and multi-level models (frequentist and bayesian)
# --------------------------------------------------------------------------------------------------------------------------

# Libraries
library(tidyverse)
library(DBI)
library(RPostgres)
library(DescTools)
library(lubridate)
library(mblm)
library(romerb)
#library(broom)

# Source functions
#source("C:/Users/brent/Documents/VS_Code/postgres/postgres/return_attributes.R")

# Connect to db
#con <- stock_master_connect()

# Read data
#sql1 <- "
#  select 
#  date_stamp
#  ,sector
#  ,ticker 
#  ,log_pb
#  ,mkt_cap
#  ,roe
#  ,total_assets 
#  ,total_equity 
#  ,total_equity_cln
#  ,leverage
#  from access_layer.fundamental_attributes
#  order by 1,2,3,4,5
#  "
#qry1 <- dbSendQuery(conn = con, statement = sql1) 
#data_raw <- dbFetch(qry1)

data("stock_fundamentals")
data_raw <- stock_fundamentals
rm(stock_fundamentals)

data("stock_prices")
stock_prices_raw <- stock_prices
rm(stock_prices)

# Visualise raw data
#hist(data_raw$log_pb)
#hist(log(data_raw$mkt_cap))
data_raw %>% 
  mutate(
    log_mkt_cap = log(mkt_cap),
    log_assets = log(total_assets),
    log_equity_cln = log(-total_equity_cln)
  ) %>% 
  pivot_longer(
    cols = c(log_pb, log_mkt_cap, roe, log_assets, log_equity_cln, leverage),
    names_to = 'attribute', 
    values_to = 'value'
    ) %>% 
  ggplot(aes(value)) + 
  geom_histogram(bins = 45) + 
  facet_wrap(vars(attribute), scales = 'free') +
  theme_bw()



# Plot log_pb against roe - all industries
data_raw %>% 
  filter(date_stamp == as.Date('2017-06-30')) %>% 
  ggplot(aes(x = -roe, y = log_pb)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, se = FALSE, size = 0.3) +
  labs(
    title = 'Log book / price ratio versus return on equity',
    x = 'Return on equity',
    y = 'Log price / book ratio'
  ) +
  theme(
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey", hjust = 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  theme_bw()



# Model coefficients for OLS and Thiel Sen regression
model_coefs <- data_raw %>% 
  mutate(roe = -roe) %>% 
  group_by(date_stamp, sector) %>% 
  filter(n() > 1) %>% 
  nest() %>% 
  mutate(
    fit_ols = map(.x = data, .f = ~lm(log_pb ~ roe, data = .x)),
    fit_ts = map(.x = data, .f = ~mblm(log_pb ~ roe, data = .x, repeated = TRUE)),
    int_ols = map_dbl(.x = fit_ols, .f = function(x) coef(summary(x))['(Intercept)', 'Estimate']),
    slp_ols = map_dbl(.x = fit_ols, .f = function(x) coef(summary(x))['roe', 'Estimate']),
    int_ts = map_dbl(.x = fit_ts, .f = function(x) coef(summary(x))['(Intercept)', 'Estimate']),
    slp_ts = map_dbl(.x = fit_ts, .f = function(x) coef(summary(x))['roe', 'Estimate']),
    x_min = map_dbl(.x = data, .f = ~min(.x$roe)),
    x_max = map_dbl(.x = data, .f = ~max(.x$roe)),
    y_min = int_ts + slp_ts * x_min,
    y_max = int_ts + slp_ts * x_max
  ) %>% 
  select(-fit_ols, -fit_ts, -data) %>%
  ungroup()




# Plot log_pb against roe - by Industry
# Includes with-in group (industry) linear regression and Theil Sen robust regression fitted values
p1 <- data_raw %>% 
  filter(date_stamp == as.Date('2017-06-30')) %>% 
  ggplot(aes(x = -roe, y = log_pb)) +
  facet_wrap(~reorder(sector, as.numeric(sector)), ncol = 4, scales = 'free') + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, se = FALSE, size = 0.4) + #, linetype = 'dotted'
  #geom_abline(  
  #  aes(intercept = int_ts, slope = slp_ts),
  #  data = filter(model_coefs, date_stamp == as.Date('2017-06-30'))
  #) +
  geom_segment(
    aes(x = x_min, xend = x_max, y = y_min, yend = y_max),
    alpha = 0.3,
    data = filter(model_coefs, date_stamp == as.Date('2017-06-30'))
  ) +
  labs(
    title = 'Log book / price ratio versus return on equity by industry',
    subtitle = 'Estimated with independent OLS (blue) and independent Theil-Sen (grey)',
    x = 'Return on equity',
    y = 'Log price / book ratio'
  ) +
  theme(
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey", hjust = 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  theme_bw()

p1




# Plot intercepts and slopes for individual linear models by sector
plot(
  model_coefs[model_coefs$date_stamp == as.Date('2017-06-30'), ]$slp_ols, 
  model_coefs[model_coefs$date_stamp == as.Date('2017-06-30'), ]$int_ols,
  xlab = 'intercept',
  ylab = 'slope',
  main = 'Intercepts and slopes from individually fit linear regression models'
) +
theme_bw()




# Least squares dummy variables (LSDV) by industry
# LSDV allows for different intercept for each industry
library(recipes)
data <- data_raw %>% 
  mutate(
    log_mkt_cap = log(mkt_cap),
    log_assets = log(total_assets),
    log_equity_cln = log(-total_equity_cln),
    roe = -roe,
    sector = as.factor(sector)
  ) %>% 
  filter(date_stamp == as.Date('2017-06-30')) %>% 
  select(date_stamp, sector, log_pb, log_mkt_cap, roe, log_assets, log_equity_cln, leverage)

rec <- recipe(log_pb ~ roe + sector, data = data)

lsdv_data <- rec %>% step_dummy(sector, keep_original_cols = TRUE) %>% prep(training = data) %>% bake(new_data = NULL)

lsdv_mdl <- lm(log_pb ~ roe + sector_X2 + sector_X3 + sector_X4 + 
               sector_X5 + sector_X6 + sector_X7 + sector_X8 + 
               sector_X9 + sector_X10 + sector_X11 + sector_X13, 
               data = lsdv_data)

lsdv_data$pred <- predict(lsdv_mdl)


# Plot
p2 <- p1 + 
  geom_line(aes(x = roe, y = pred), data = lsdv_data, size = 0.3) +
  labs(subtitle = 'Estimated with independent OLS (blue), independent Theil-Sen (grey) and LSDV (black)') +
  theme_bw()

p2



# Plot intercepts and slopes from LSDV model and compare to individual LM 
# Data prep
lsdv_mdl_coef <- as.data.frame(coef(lsdv_mdl)) %>% 
  rownames_to_column(var = 'intercept') %>% 
  mutate(slope = unname(coef(lsdv_mdl)['roe']))
lsdv_mdl_coef[1, 1] <- 1
lsdv_mdl_coef <- lsdv_mdl_coef[lsdv_mdl_coef$intercept != 'roe', ]
lsdv_mdl_coef$intercept <- as.factor(gsub("[^0-9.]", "",  lsdv_mdl_coef$intercept))
lsdv_mdl_coef$source <- 'LSDV'
names(lsdv_mdl_coef)[1:2] <- c('sector','intercept')

lm_mdl_coefs <- model_coefs %>% 
  filter(date_stamp == as.Date('2017-06-30')) %>% 
  select(sector, int_ols, slp_ols) %>% 
  mutate(sector = as.factor(sector), source = 'OLS') %>% 
  rename(intercept = int_ols, slope = slp_ols)

model_coefs_all <- bind_rows(lsdv_mdl_coef, lm_mdl_coefs)  

# Plot
c2 <- model_coefs_all %>% 
  ggplot(aes(x = intercept, y = slope, colour = source)) +
  geom_point(alpha = 0.7) + ylim(-5, 5) +
  #geom_text(aes(label = sector), nudge_x = 0.05, nudge_y = 0.05) +
  labs(
    title = 'Regression intercept and slope',
    subtitle = 'Estimated with independent OLS and LSDV',
    x = 'Intercept',
    y = 'Slope'
  ) +
  theme(
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey", hjust = 0),
    legend.position = 'bottom', legend.justification = 'centre'
  )

c2



# Mixed effects models using lme4
# lme4 manual:- https://lme4.r-forge.r-project.org/book/
# lme4 manual:- https://www.chalmers.se/sv/institutioner/math/forskning/forskarutbildning/forskarutbildning-matematisk-statistik/forskarutbildningskurser-matematisk-statistik/Documents/bates_manual.pdf
# vignette:- https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
# DougBats pres:- http://pages.stat.wisc.edu/~bates/UseR2008/WorkshopD.pdf
# Example to find lme4 predict coefficients:- https://stats.stackexchange.com/questions/174203/predict-function-for-lmer-mixed-effects-models/174227
# Various syntax notes:- https://yury-zablotski.netlify.app/post/mixed-effects-models-2/
# Syntax cs:- https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet
# Sweep function:- https://stackoverflow.com/questions/3444889/how-to-use-the-sweep-function
# Plot reference:-https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/
# Model diagnostics:- https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

library(lme4)

lme_1_data <- data_raw %>% 
  filter(date_stamp == as.Date('2017-06-30')) %>% 
  select(log_pb, roe, sector) %>% 
  mutate(
    roe = -roe, 
    sector = as.factor(sector)
  )

# Mixed effects model with fixed and random intercept 
# Random effect defined with industry sector as the grouping factor
lme_1 <- lmer(log_pb ~ roe + (1 + roe | sector), data = lme_1_data)
summary(lme_1)
coef(lme_1)
fixef(lme_1)
ranef(lme_1)


# Add predicted values for line in scatter plot
lme_1_data$pred <- predict(lme_1)

p3 <- p2 + 
  geom_line(aes(x = roe, y = pred), data = lme_1_data, size = 0.4, color = 'black', linetype = 'longdash') + #darkorchid3
  labs(subtitle = 'Estimated with independent OLS (blue), independent Theil-Sen (grey), LSDV (black) and mixed effects (dashed)') +
  facet_wrap(~reorder(sector, as.numeric(sector)), ncol = 4, scales = 'free')

p3


# Plot coefficients - add coefficients to existing plot
# CHANGE SOURCE TO MODEL
c3 <- c2 +
  geom_point(
    aes(
      x = intercept, 
      y = slope,
      colour = source
    ), 
    data = data.frame(sector = rownames(ranef(lme_1)$sector),
                      intercept = unname(coef(lme_1)$sector['(Intercept)']),  # works only for single grouping variable
                      slope = unname(coef(lme_1)$sector['roe']),  # works only for single grouping variable
                      source = rep('Mixed effects1',12)
                      ),
    alpha = 0.7
  ) +
  #geom_path(aes(group = sector, color = NULL), arrow = arrow(length = unit(.02, "npc"))) + 
  labs(subtitle = 'Estimated with independent OLS, LSDV and Mixed effects')
c3
# Observation - not a lot of pooling / pulling of parameters to the mean

#library(lattice)
#library(cowplot)
#c4 <- qqmath(ranef(lme_1, condVar = TRUE))
#grid.arrange(c4[[1]], c4[[2]])
#plot_grid(c4[[1]], c4[[2]], ncol = 1, align = 'v')


# There is essentially no difference between the mixed model and individual OLS 
# While this is probably due to the small number of groups (12) and the large amount of
# data points within each group (EXPLAIN WHY THIS OCCURS - LESS VARIATION B/W GROUPS ETC
# MAY BE PRUDENT TO GO NEXT LEVEL DOWN - INDUSTRY), 
# What if the mixed model is estimated with more data, adding all dates pre June.  
# Specify the model as crossed (as oppose to nested) since each stock is included in each 
# month and vice versa 
lme_2_data <- data_raw %>% 
  filter(date_stamp <= as.Date('2017-06-30')) %>% 
  select(log_pb, roe, sector, date_stamp) %>% 
  mutate(
    roe = -roe, 
    sector = as.factor(sector),
    month = as.factor(month(date_stamp))
  )

# Can the model below be changed so that only a random intercept is included for month, 
# i.e., exclude the random slope for roe??
lme_2 <- lmer(log_pb ~ roe + (1 + roe | sector) + (1 + roe | month), data = lme_2_data) # crossed syntax
summary(lme_2)
coef(lme_2)
fixef(lme_2)
ranef(lme_2)

# Predictions for slope in plot
lme_2_data$pred <- predict(lme_2)



p4 <- p3 +
  geom_point( 
    aes(x = roe, y = pred), 
    data = filter(lme_2_data, date_stamp == as.Date('2017-06-30')), 
    size = 0.4, color = 'red', shape = 3 #linetype = 'dashed'
  ) + 
  labs(subtitle = 'Estimated with independent OLS (blue), independent Theil-Sen (grey), LSDV (black) and mixed effects (dashed)') +
  facet_wrap(~reorder(sector, as.numeric(sector)), ncol = 4, scales = 'fixed')

p4



# Plot coefficients  - add coefficients to existing plot
# note derivation of coefficients in the plot below - see proof of this derivations further down
c4 <- c3 +
  geom_point(
    aes(
      x = intercept, 
      y = slope,
      colour = source
    ), 
    data = data.frame(sector = rownames(ranef(lme_1)$sector),
                      intercept = fixef(lme_2)['(Intercept)'][[1]] + 
                        ranef(lme_2)$sector['(Intercept)'][[1]] +
                        ranef(lme_2)$month['(Intercept)'][6, ],
                      slope = fixef(lme_2)['roe'][[1]] + 
                        ranef(lme_2)$sector['roe'][[1]] +
                        ranef(lme_2)$month['roe'][6, ],
                      source = rep('Mixed effects2',12)
    ),
    alpha = 0.7
  ) +
  labs(subtitle = 'Estimated with independent OLS, LSDV and Mixed effects')
c4

# FYI - extract data from ggplot object
layer_data(c4, 4)

# Prove the method for extraction of prediction coefficients, that used in the plot above
# Derive month 6 coefficients as sum of each grouping factor based on 'coef' function
# Method 1 - from "coef" function
coef_sector <- as.matrix(unname(coef(lme_2)$sector)) # sector coefficients
rownames(coef_sector)<-NULL  # sector coefficients, remove rownames from matrix
coef_month6 <- matrix(unlist(unname(coef(lme_2)$month[6, ])), nrow = 1, ncol = 2) # month 6 only coefficients
coef1 <- sweep(coef_sector, 2, coef_month6, '+') # add month 6 coefs to all sector coefs, see link for sweep function

# Method 2 - from "fixef" and "ranef" functions
fixef <- matrix(unlist(unname(fixef(lme_2))), nrow = 1, ncol = 2) # sector coefficients
ranef_sector <- as.matrix(unname(ranef(lme_2)$sector)) # sector coefficients
rownames(ranef_sector)<-NULL  # remove rownames from matrix
ranef_month <- matrix(unlist(unname(ranef(lme_2)$month[6, ])), nrow = 1, ncol = 2) # month 6 coefficients
coef2 <- sweep(ranef_sector, 2, fixef, '+') # add fixed effects and random effects for sector
coef2 <- sweep(coef2, 2, ranef_month, '+') # take above and add random effect for month

# Check against coefficients implied from predicted data
# A linear model applied to the predicted data will return the intercept and slope used in the model
# deriving that predicted data.
coef3 <- unlist(unname(coef(lm(pred ~ roe, data = lme_2_data[lme_2_data$date_stamp == as.Date('2017-06-30') & lme_2_data$sector == 1, ]))))

library(DT)
datatable(
  data.frame(rbind(coef1[1, ], coef2[1, ], coef3), 
             row.names = c("'coef' function","'fixef' plus 'ranef' functions",'lm result')),
  colnames = c("Intercept", "Slope"),
  options = list(dom = 't')
) %>% formatRound(c("X1", "X2"), 5)
  




# Plot coefficients
coef(lme_2) # Sum of the random and fixed effects for each level
fixef(lme_2) # Estimates of the fixed-effects coefficients
ranef(lme_2) # Conditional modes of the random effects

# Diagnostics - https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
plot(lme_2)
qqnorm(residuals(lme_2))
