
# --------------------------------------------------------------------------------------------------------------------------
#
# Tidymodels work flow
# 
# Information based - random forest / xgboost
# Similarity based  - knn / kernal regression
# Probability based - naive bayes / bayesian network
# Error based - neural network / logistic regression
# WHAT CATEGORY IS SVM?
# https://www.kirenz.com/post/2021-02-17-r-classification-tidymodels/
#
# --------------------------------------------------------------------------------------------------------------------------

library('tidyverse')
library('lubridate')
library('mondate')
library('DescTools')
library('tidymodels')
library('vip')
library('reticulate')
library('romerb')
library('xgboost')
#library('ranger')
#library('randomForest')
#library('neuralnet')
#library('nnet')
#library('earth')

# Custom theme
custom_theme1 <- theme_minimal() +
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

# 1. Data ------------------------------------------------------------------------------------------------------------------

# Raw data
data("stock_data")
df_raw <- stock_data
rm(stock_data)

# Data for model input & OOS scoring
df_features <- df_raw %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>%
  group_by(sector) %>%
  mutate(
    rtn_ari_1m_sct = mean(rtn_ari_1m),
    rtn_ari_12m_sct = mean(rtn_ari_12m),
    vol_ari_60d_sct = mean(vol_ari_60d)
  ) %>% 
  ungroup() %>% 
  group_by(industry) %>%
  mutate(
    rtn_ari_1m_ind = mean(rtn_ari_1m),
    rtn_ari_12m_ind = mean(rtn_ari_12m),
    vol_ari_60d_ind = mean(vol_ari_60d)
  ) %>% 
  # Date to character - required for stratified sampling, "vfold_cv" does not accept date
  mutate(date_char = as.character(date_stamp)) %>%  
  select(
    date_stamp, date_char, symbol, fwd_rtn_1m,
    rtn_ari_1m, rtn_ari_1m_sct, rtn_ari_1m_ind,
    rtn_ari_12m, rtn_ari_12m_sct, rtn_ari_12m_ind,
    vol_ari_60d, vol_ari_60d_sct, vol_ari_60d_ind
  ) %>% 
  ungroup()

# Sample
#set.seed(234)
#s <- sample(unique(df_features$symbol), 40)
#s <- c("GPC","SHLD","PXD","ANF","EPC","CLX","MGM","DXCM","BEN","ED","TOL","DO","FLR","RT","ACCO","BFAM","HOG","MTD","PEGI","DORM")
#df_features <- filter(df_features, symbol %in% s)

# Model training / testing
df_model_in <- df_features %>% 
  select(-industry) %>% 
  filter(
    date_stamp < max(df_features$date_stamp),
    between(fwd_rtn_1m, -0.5, 0.5),
    date_stamp >= as.Date('2017-06-01')                                                                  #### PARAMETER ####
  ) %>% drop_na() 

# OOS scoring data (current month)
df_oos_score <- df_features %>% 
  filter(date_stamp == max(df_features$date_stamp)) %>% 
  select(-fwd_rtn_1m)


# Remove dupes
df_model_in <- df_model_in[!duplicated(df_model_in[, c('symbol', 'date_stamp')]), ]

# Check dupes
df_model_in %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)


# Train / test parameters
train_months <- 36                                                                                       #### PARAMETER ####
test_months <- 6                                                # out of sample test_months              #### PARAMETER ####
months <- sort(unique(df_model_in$date_stamp))
total_months <- length(months)                                  # this is also the stride
sample_months <- train_months + test_months                     # length of resultant df
loops <- floor((total_months - train_months - 1) / test_months) # minus 1 since last month is "live" and does not contain labels / forward returns
start_month_idx <- total_months - (test_months * loops) - train_months + 1


# Empty list for loop output
preds_list <- list()
var_imp_list <- list()
tune_metrics_list <- list()


# Set model 
xgb_model <- boost_tree(
  mtry = tune(),
  min_n = 50,
  trees = 250,
  tree_depth = tune()
  ) %>% 
  set_engine("xgboost", importance = TRUE) %>% 
  set_mode("regression")


# Set tuning grid
xgb_grid <- grid_regular(
  mtry(range = c(5, 9)), 
  #min_n(range = c(6, 8)),
  tree_depth(range = c(3, 7)),
  levels = 2
)


# Loop to slide over time series -------------------------------------------------------------------------------------------

for (i in seq(from = start_month_idx, by = test_months, length.out = loops)) {
  
  start <- months[i]
  end <- months[i + sample_months - 1]
  print(c(start, end))
  df <- df_model_in %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end)))


  # 2. Specify training and testing split ---------------------------------------------------------
  
  test_ref_date <- as.Date(mondate::mondate(max(df$date_stamp)) - test_months + 1)
  
  # Sort so that index for test split is appropriate
  df <- arrange(df, date_stamp, symbol)
  
  # Index ref for first date
  first_date_idx <- which(df$date_stamp == test_ref_date)[1] - 1
  
  # Determine proportion of records to select
  prop <- 1 - (nrow(df) - first_date_idx) / nrow(df)
  split <- initial_time_split(df, prop = prop)
  
  # Training and test data
  train <- training(split)
  test <- testing(split)
  
  
  
  
  # 3. Create resamples of the training data ------------------------------------------------------
  
  set.seed(123)
  resamples <- group_vfold_cv(train, v = 3, group = date_char)                                    #### PARAMETER FOR v #####
  
  
  
  
  # 4. Preprocessing ------------------------------------------------------------------------------
  # https://stats.stackexchange.com/questions/258307/raw-or-orthogonal-polynomial-regression
  
  recipe <- recipe(fwd_rtn_1m ~ ., data = train) %>% 
    update_role(date_stamp, new_role = 'date_stamp') %>% 
    update_role(date_char, new_role = 'date_char') %>% 
    update_role(symbol, new_role = 'symbol') #%>% 
    #step_normalize(all_predictors()) 
    #step_corr(all_predictors(), threshold = .5)
  
  
  
  
  # 5. Specify models(s) --------------------------------------------------------------------------
  
  #mars_model <- mars(
  #  num_terms = tune(),            # nprune - maximum number of terms (including intercept) in the pruned model
  #  prod_degree = tune(),          # degree -the highest possible degree of interaction between features
  #  prune_method = "exhaustive"    # pmethod
  #  ) %>% 
  #  set_engine("earth") %>%
  #  set_mode("regression")
  
  #lm_model <- linear_reg() %>% 
  #  set_engine("lm") %>% 
  #  set_mode("regression")
  
  #nn_model <- mlp(                # https://community.rstudio.com/t/extending-parsnip/99290
  #  hidden_units = tune()
  #  ) %>% 
  #  set_engine("nnet") %>% 
  #  set_mode("regression")
  
  #rf_model <- rand_forest(
  #  mtry = tune(),                # An integer for the number of predictors that will be randomly sampled at each split when creating the tree models
  #  min_n = 100,                  # An integer for the minimum number of data points in a node
  #  trees = 250                   # An integer for the number of trees contained in the ensemble
  #  ) %>% 
  #  set_engine("randomForest", importance = TRUE) %>% 
  #  set_mode("regression")
  
  
  
  
  # 6. Create parameter grid ------------------------------------------------------------------------
  
  #mars_grid <- grid_regular(
  #  num_terms(range = c(5, 8)),
  #  prod_degree(range = c(1, 2)),
  #  levels = 5
  #  )
  
  #nn_grid <- grid_regular(
  #  hidden_units(range = c(3, 5))
  #  )
  
  #rf_grid <- grid_regular(
  #  mtry(range = c(5, 10)), 
  #  #min_n(range = c(6, 8)),
  #  #trees(range = c(1000, 1500))
  #  levels = 2
  #  )
  
  #xgb_grid <- grid_regular(
  #  mtry(range = c(5, 9)), 
  #  #min_n(range = c(6, 8)),
  #  tree_depth(range = c(3, 7)),
  #  levels = 3
  #)
  
  
  
  # 6. Create workflow ------------------------------------------------------------------------------
  
  workflow <- workflow() %>% 
    add_model(xgb_model) %>%                                                            # set model here #### PARAMETER ####
    add_recipe(recipe)
    
  
  
  
  # 7. Fit re-samples -------------------------------------------------------------------------------
  
  tune_resamples <- tune::tune_grid(
    object = workflow, 
    resamples = resamples,
    grid = xgb_grid,                                                                     # set grid here #### PARAMETER ####
    # unsure metric corresponds to regression / classification
    metrics = metric_set(mae),                                                                           #### PARAMETER ####
    control = control_grid(save_pred = TRUE)
    )
  
  
  
  
  # 7.1 Assess stability of model -----------------------------------------------------------------
  # Export - determine if the different hyperparameter specifications lead to different loss
  tune_metrics <- tune::collect_metrics(tune_resamples, summarize = FALSE)
  
  
  
  # 8. Select best parameters ---------------------------------------------------------------------
  best_param <- tune::select_best(tune_resamples, metric = "mae")
  
  
  
  # 9. Finalise workflow --------------------------------------------------------------------------
  final_workflow <- tune::finalize_workflow(workflow, best_param)
  
  
  
  # 10. Final fit ---------------------------------------------------------------------------------
  # Fit the final best model to the training set and evaluate the test set
  
  set.seed(456)
  final_fit <- tune::last_fit(final_workflow, split) 
  
  preds <- tune::collect_predictions(final_fit)
  
  
  # Join labels to predictions  #### RETURN ####
  preds <- bind_cols(preds, select(test, symbol, date_stamp))
  
  
  # Extract VI into dataframe  #### RETURN ####
  var_imp <- extract_fit_engine(final_fit) %>% vip::vi()

  # Label start & end date
  preds$start <- start
  preds$end <- end
  var_imp$start <- start
  var_imp$end <- end
  tune_metrics$start <- start
  tune_metrics$end <- end

  # Add data frame to list
  preds_list[[i]] <- preds
  var_imp_list[[i]] <- var_imp
  tune_metrics_list[[i]] <- tune_metrics
  
}

# End loop -----------------------------------------------------------------------------------------------------------------


# Data frames in list to single data frame
preds_all <- dplyr::bind_rows(preds_list)
var_imp_all <- dplyr::bind_rows(var_imp_list)
tune_metrics_all <- dplyr::bind_rows(tune_metrics_list)


# Remove dupes
preds_all <- preds_all[!duplicated(preds_all[, c('symbol', 'date_stamp')]), ]
preds_all %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)




# MC_TEST ------------------------------------------------------------------------------------------------------------------


# Preds to wide for numpy -------------------------------------------------------------------------

positions <- preds_all %>% 
  select(
    date_stamp, 
    symbol, 
    .pred
  ) %>% 
  pivot_wider(
    names_from = symbol, 
    values_from = .pred
  )   


# Ensure columns are in alphabetical order
sort_cols <- sort(colnames(positions))
sort_cols <- c('date_stamp', sort_cols[sort_cols != 'date_stamp'])
positions <- positions[, sort_cols]


# To matrix
positions_mtrx <- data.matrix(positions[, 2:ncol(positions)], rownames.force = FALSE)

# Replace na's with zero
positions_mtrx[is.na(positions_mtrx)] <- 0

# Assign long indicator (1) if forecast return > x ('fixed') OR if in top quantile ('relative')  
position_entry_method <- 'fixed'                                                                         #### PARAMETER ####
long_filter <- .02                                                                                       #### PARAMETER ####
qtle_thres <- 0.8                                                                                        #### PARAMETER ####

mtrx_long_filter <- function(x, qntl){
  x[x > quantile(x, probs = thres, na.rm = TRUE)] <- 1
  x[x != 1] <- 0
  return(x)
}

if (position_entry_method == 'fixed') {
  positions_mtrx[positions_mtrx > long_filter] <- 1
  positions_mtrx[positions_mtrx <= long_filter] <- 0
} else {
  positions_mtrx <- t(apply(X = positions_mtrx, MARGIN = 1, FUN = mtrx_long_filter, qntl = qtle_thres))
  #positions_mtrx[is.na(positions_mtrx)] <- 0
}




# Prices to wide for numpy ------------------------------------------------------------------------

# - remove dupes
df_raw <- df_raw[!duplicated(df_raw[, c('symbol', 'date_stamp')]), ]

# - wide format
prices <- df_raw %>% select(symbol, date_stamp, adjusted_close) %>% 
  filter(
    date_stamp %in% unique(preds_all$date_stamp),
    symbol %in% colnames(positions)
    ) %>% 
  pivot_wider(
    names_from = symbol, 
    values_from = adjusted_close
  ) %>% 
  arrange(date_stamp)

# Ensure columns are in alphabetical order 
prices <- prices[, sort_cols]


# Replace na's with prior price (else previous price)
prices <- prices %>% fill(everything(), .direction = 'downup')

# To matrix
prices_mtrx <- data.matrix(prices[, 2:ncol(prices)], rownames.force = FALSE)


# Invoke python -----------------------------------------------------------------------------------
# https://community.rstudio.com/t/reticulate-source-python-import-modules/8355/3

use_condaenv(condaenv = 'MC_TEST', required = TRUE)
import_from_path("mc_test", path = 'C:/Users/brent/Documents/VS_Code/MC_TEST/MC_TEST/')
source_python('C:/Users/brent/Documents/VS_Code/MC_TEST/MC_TEST/mc_test.py')

mc_backtest1 = monte_carlo_backtest1(
  prices = prices_mtrx, 
  positions = positions_mtrx, 
  seed_capital = as.integer(10000), 
  max_positions = as.integer(15),
  iter = as.integer(1000)
  )

mc_backtest2 = monte_carlo_backtest1(
  prices = prices_mtrx, 
  positions = positions_mtrx, 
  seed_capital = as.integer(10000), 
  max_positions = as.integer(15),
  iter = as.integer(1000),
  rndm = TRUE
)


# Join prediction based and random backtest results
mc_backtest1$src <- rep('pred',nrow(mc_backtest1)) 
mc_backtest2$src <- rep('rand',nrow(mc_backtest2))
mc_backtest1$sim_no <- rep(1:nrow(mc_backtest1)) 
mc_backtest2$sim_no <- rep(1:nrow(mc_backtest2))
mc_backtest <- dplyr::bind_rows(mc_backtest1[,c(1:3,7)],mc_backtest2[,c(1:3,7)])

mcb_plot_data <- mc_backtest %>% group_by(src) %>% 
  summarise(
    median_cagr = median(cagr),
    median_dd = median(max_drawdown),
    median_vol = median(volatility)
    )


# Time series of portfolio valuation --------------------------------------------------------------
portfolio_valuation <- dplyr::bind_rows(
  tidyr::unnest(mc_backtest1, portfolio_valuation_ts)[,c(4,7:8)],
  tidyr::unnest(mc_backtest2, portfolio_valuation_ts)[,c(4,7:8)]
  )

date_seq <- unique(preds_all$date_stamp)

portfolio_valuation$date_stamp <- rep(date_seq, nrow(portfolio_valuation) / length(date_seq))


# Time series of portfolio valuation (positions for each date for each simulation) ----------------
positions_df <- unnest(data = mc_backtest1[ ,5:8], col = c(pstn_idx, pstn_qty))
positions_df$date_stamp <- rep(date_seq, max(positions_df$sim_no))
positions_df <- unnest(data = positions_df, col = c(pstn_idx, pstn_qty))

# Data frame of symbols to join to (note Python zero index configuration)
symbols_df <- data.frame(idx = 0:(ncol(positions_mtrx)), symbol = c(colnames(positions_mtrx),'cash'))

# Join positions index to symbol
positions_df <- dplyr::left_join(positions_df, symbols_df, by = c('pstn_idx' = 'idx'))

# Data price data for joining to positions
prices <- rbind(
  # Cash
  data.frame(
    symbol=rep('cash',length(date_seq)), 
    date_stamp = date_seq,
    close = rep(1,length(date_seq)),
    adjusted_close = rep(1,length(date_seq))
  ), 
  # Stock prices
  df_raw[ ,1:4]
)

positions_df <- dplyr::left_join(positions_df, prices, by = c('symbol' = 'symbol', 'date_stamp' = 'date_stamp'))
positions_df$vltn <- positions_df$adjusted_close * positions_df$pstn_qty
positions_df <- positions_df %>% group_by(date_stamp, sim_no) %>% mutate(date_vltn = sum(vltn, na.rm = TRUE))
portfolio_valuation_check <- positions_df %>% group_by(date_stamp, sim_no) %>% summarise(date_vltn = sum(vltn, na.rm = TRUE))
# This should agree to portfolio_valuation


# Line plot
# TO DO - ADD AVERAGE PORTFOLIO VALUATION & S&P500 RETURNS TO THIS PLOT
portfolio_agg_rtn <- diff(prices_mtrx) / prices_mtrx[-nrow(prices_mtrx),]
portfolio_agg_rtn[portfolio_agg_rtn == 0] <- NA 
portfolio_agg_rtn <- rowMeans(portfolio_agg_rtn, na.rm = TRUE)
portfolio_agg_val <- cumprod(1 + c(9999, portfolio_agg_rtn))
portfolio_agg_val <- data.frame(
  date_stamp = rep(date_seq, 2), 
  agg_val = rep(portfolio_agg_val, 2), 
  src = c(rep('pred', length(date_seq)),rep('rand', length(date_seq))),
  sim_no = c(rep('pred', length(date_seq)),rep('rand', length(date_seq)))
  )

ggplot(
  data = portfolio_valuation,
  aes(x = date_stamp, y = portfolio_valuation_ts, group = sim_no)
  ) + 
  geom_line(color = "grey", size = .0001) +
  ylim(min(portfolio_valuation$portfolio_valuation_ts) * 0.9, max(portfolio_agg_val$agg_val) * 1.1) +
  geom_abline(intercept = 10000, slope = 0, linetype = "dashed", size = 0.5) +
  geom_line(aes(x = date_stamp, y = agg_val), data = portfolio_agg_val, size = 0.6, color = "red") +
  facet_grid(cols = vars(src), scales = 'fixed') +
  labs(
    title = 'Monte carlo portfolio formation - valuation',
    x = '',
    y = 'Portfolio value'
  ) +
  custom_theme1





mc_backtest %>% 
  ggplot(aes(x = cagr, fill = src, color = src)) +
  geom_density(alpha = 0.3) +
  xlim(min(mc_backtest$cagr), 1) +
  geom_vline(data = mcb_plot_data, aes(xintercept = median_cagr, colour = src), linetype = "dashed", size = 0.5) + 
  labs(
    title = 'Monte carlo portfolio formation - CAGR',
    x = 'Compound annual growth rate',
    y = ''
  ) +
  custom_theme1

mc_backtest %>% 
  ggplot(aes(x = max_drawdown, fill = src, color = src)) +
  geom_density(alpha = 0.3) +
  xlim(min(mc_backtest$max_drawdown), 0.25) +
  geom_vline(data = mcb_plot_data, aes(xintercept = median_dd, colour = src), linetype = "dashed", size = 0.5) + 
  labs(
    title = 'Monte carlo portfolio formation - drawdown',
    x = 'Maximum drawdown',
    y = ''
  ) +
  custom_theme1

mc_backtest %>% 
  ggplot(aes(x = volatility, fill = src, color = src)) +
  geom_density(alpha = 0.3) +
  xlim(min(mc_backtest$volatility), 0.3) +
  geom_vline(data = mcb_plot_data, aes(xintercept = median_vol, colour = src), linetype = "dashed", size = 0.5) + 
  labs(
    title = 'Monte carlo portfolio formation - volatility',
    x = 'Volatility',
    y = ''
  ) +
  custom_theme1



# 11. Scatter plot of actual vs predicted ---------------------------------------------------------

# For Theil Sen line https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

preds_all %>% 
  ggplot(aes(x = .pred, y = fwd_rtn_1m)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = 'blue', linetype = 'twodash') +
  #geom_smooth(method = lm, se = FALSE, size = 0.3, colour = 'blue', linetype = 'twodash') +
  #geom_smooth(method = sen, se = FALSE, size = 0.3, colour = 'grey') +
  facet_wrap(vars(date_stamp), scales = 'fixed') +
  labs(
    title = 'Actual vs predicted',
    x = 'Predicted',
    y = 'Actual'
  ) +
  custom_theme1




# 12. Variable importance --------------------------------------------------------------------------------------------------
var_imp_summary <- var_imp_all %>% 
  group_by(Variable) %>% summarise(mean_vi = mean(Importance)) %>% arrange(mean_vi)

var_imp_all %>% 
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() + 
  scale_y_discrete(limits = var_imp_summary$Variable) +
  facet_wrap(vars(end), scales = "fixed") +
  labs(
    title = 'Variable importance',
    x = 'Variable',
    y = 'Importance'
  ) +
  custom_theme1
  
# TO DO explain interactions
# https://cran.r-project.org/web/packages/EIX/vignettes/EIX.html
# https://cran.r-project.org/web/packages/flashlight/vignettes/flashlight.html
# https://ema.drwhy.ai/
m <- xgb.model.dt.tree(model = extract_fit_engine(final_fit))


# 13. Variability of hyper-parameters ---------------------------------------------------------------------------------------
tune_metrics_all %>% 
  mutate(mtry_depth = paste0(mtry, "_", tree_depth)) %>% 
  group_by(mtry_depth, end) %>% 
  summarise(mae = mean(.estimate)) %>% 
  ggplot(aes(x = mtry_depth, y = mae)) + 
  geom_col() +
  #ylim(0.05, max(tune_metrics_all$.estimate)) +
  facet_wrap(vars(as.factor(end)), scales = "fixed") +
  labs(
    title = 'Hyper-parameter variability',
    x = 'Hyper parameters (mtry_depth)',
    y = 'Mean absolute error'
  ) +
  custom_theme1



# 13. Save final model object ----------------------------------------------------------------------------------------------

# Final model
set.seed(456)
# Fit on most recent dataset (same training period as above)
final_model <- final_workflow %>% 
  fit(data = filter(df_model_in, between(date_stamp, as.Date(!!start), as.Date(!!end))))

saveRDS(final_model, file = "C:/Users/brent/Documents/R/R_import/final_model")

loaded_model <- readRDS("C:/Users/brent/Documents/R/R_import/final_model")

loaded_model_preds <- predict(loaded_model, new_data = df_oos_score)

oos_preds <- bind_cols(df_oos_score, loaded_model_preds)







# SCRATCH -----------------------------------------------------------------------------------------
# In months

months <- sort(unique(df_raw$date_stamp))
total_months <- length(months)
train_months <- 8
test_months <- 4                                   # this is also the stride
sample_months <- train_months + test_months        # length of resultant df


loops <- floor((total_months - train_months - 1) / test_months) # minus 1 since last month is "live"
start_month_idx <- total_months - (test_months * loops) - train_months 


# Loop
for (i in seq(from = start_month_idx, by = test_months, length.out = loops)) {
  start <- months[i]
  end <- months[i + sample_months - 1]
  print(c(start, end))
  #df_raw %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end)))
}

test <- df_raw %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end)))
unique(test$date_stamp)




# SCRATCH -----------------------------------------------------------------------------------------


datalist = list()

for (i in 1:5) {
  # ... make some data
  dat <- data.frame(x = rnorm(10), y = runif(10))
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}

#big_data = do.call(rbind, datalist)
big_data <- dplyr::bind_rows(datalist)





# Rsample test -----------------------------------------------------------------------------------------

library('rsample')

# Mock data
df <- data.frame(
  date = sort(rep(LETTERS[seq(from = 1, to = 10)], 8)),
  stock = rep(letters[seq(from = 1, to = 8)],10),
  rtn = sort(rep(1:10, 8)) + rep(seq(0.1, 0.8, length.out = 8),10)
)

# Example 1 v fold cv
vfold <- vfold_cv(df, v = 5)
analysis(vfold$splits[[1]]) %>% group_by(date) %>% summarise(n = n())
assessment(vfold$splits[[1]]) %>% group_by(date) %>% summarise(n = n())

# Example 2
vfold1 <- vfold_cv(df, v = 5, strata = rtn)
analysis(vfold1$splits[[1]]) %>% group_by(date) %>% summarise(n = n())
assessment(vfold1$splits[[1]]) %>% group_by(date) %>% summarise(n = n())

# Example 3
g_vfold <- group_vfold_cv(df, v = 5, group = date)
analysis(g_vfold$splits[[1]]) %>% group_by(date) %>% summarise(n = n())
assessment(g_vfold$splits[[1]]) %>% group_by(date) %>% summarise(n = n())
dirname(sys.frame(1)$ofile)



# Matrix test -----------------------------------------------------------------------------------------
m <- matrix(c(1,2,12,4,50,6,7,8,9,10,11,3), ncol = 4)
m
#m[apply(m, 1, function(x) x[x > quantile(x,0.8)] <- 1), ]
#m[m > quantile(m, 0.8)] <- 1
#m
cutoff <- function(x){
  x[x > quantile(x, 0.8)] <- 1
  return(x)
}
t(apply(X = m, MARGIN = 1, FUN = cutoff))
