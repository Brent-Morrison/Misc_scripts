
# --------------------------------------------------------------------------------------------------------------------------
#
# Tidymodels work flow
# 
# Information based - random forest / xgboost - classification / regression
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
library('ranger')
library('randomForest')
library('neuralnet')
library('nnet')
library('earth')
library('xgboost')
library('vip')
library('reticulate')
library('romerb')


# 1. Data ------------------------------------------------------------------------------------------------------------------

# Raw data
data("stock_data")
df_raw <- stock_data
rm(stock_data)

# Data for model input
df_filter <- df_raw %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>%
  mutate(date_char = as.character(date_stamp)) %>%  # Required for stratified sampling, vfold_cv does not accept date
  select(date_stamp, date_char, symbol, amihud_1m:rtn_ari_12m, suv, fwd_rtn_1m) %>% 
  drop_na() %>% 
  filter(between(fwd_rtn_1m, -0.5, 0.5), date_stamp >= as.Date('2020-06-01'))


# Check dupes
df_filter %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)


# Train / test parameters
train_months <- 6
test_months <- 2                                                # out of sample ##### PARAMETER FOR test_months ######
months <- sort(unique(df_filter$date_stamp))
total_months <- length(months)                                  # this is also the stride
sample_months <- train_months + test_months                     # length of resultant df
loops <- floor((total_months - train_months - 1) / test_months) # minus 1 since last month is "live" and does not contain labels / forward returns
start_month_idx <- total_months - (test_months * loops) - train_months 


# Empty list for loop output
preds_df1_list <- list()
var_imp_list <- list()


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
  levels = 3
)


# Loop to slide over time series
for (i in seq(from = start_month_idx, by = test_months, length.out = loops)) {
  start <- months[i]
  end <- months[i + sample_months - 1]
  #print(c(start, end))
  df <- df_filter %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end)))


  # 2. Specify training and testing split -----------------------------------------------------------
  
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
  
  
  
  
  # 3. Create resamples of the training data -------------------------------------------------------------------------------
  
  set.seed(123)
  resamples <- group_vfold_cv(train, v = 3, group = date_char) ##### PARAMETER FOR v ######
  
  
  
  
  # 4. Preprocessing -------------------------------------------------------------------------------------------------------
  # https://stats.stackexchange.com/questions/258307/raw-or-orthogonal-polynomial-regression
  
  recipe <- recipe(fwd_rtn_1m ~ ., data = train) %>% 
    update_role(date_stamp, new_role = 'date_stamp') %>% 
    update_role(date_char, new_role = 'date_char') %>% 
    update_role(symbol, new_role = 'symbol') %>% 
    step_normalize(all_predictors()) 
    #step_corr(all_predictors(), threshold = .5)
  
  
  
  
  # 5. Specify models(s) ----------------------------------------------------------------------------
  
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
    add_model(xgb_model) %>%           # set model here PARAMETER??????
    add_recipe(recipe)
    
  
  
  
  # 7. Fit re-samples -------------------------------------------------------------------------------
  
  tune_resamples <- tune_grid(
    object = workflow, 
    resamples = resamples,
    grid = xgb_grid,                 # set grid here PARAMETER??????
    metrics = metric_set(rmse),      # unsure metric corresponds to regression / classification
    control = control_grid(save_pred = TRUE)
    )
  
  
  
  
  # 7.1 Assess stability of model ------------------------------------------------------------------------------------------
  # Export - determine if the different hyperparameter specifications lead to different rmse
  tune_metrics <- collect_metrics(tune_resamples, summarize = FALSE)
  
  
  
  # 8. Select best parameters ----------------------------------------------------------------------------------------------
  # tune_metrics %>% group_by(mtry, tree_depth) %>% summarise(ave_rmse = mean(.estimate)) %>% arrange(ave_rmse)
  best_param <- select_best(tune_resamples, metric = "rmse")
  
  
  
  
  # 9. Finalise workflow ---------------------------------------------------------------------------------------------------
  final_workflow <- finalize_workflow(workflow, best_param)
  
  
  
  # 10. Final fit ----------------------------------------------------------------------------------------------------------
  # Fit the final best model to the training set and evaluate the test set
  
  set.seed(456)
  final_fit <- last_fit(final_workflow, split) 
  
  preds_df <- collect_predictions(final_fit)
  
  
  # Join labels to predictions  #### RETURN ####
  preds_df1 <- bind_cols(preds_df, select(test, symbol, date_stamp))
  
  
  # Extract VI into dataframe  #### RETURN ####
  var_imp <- final_fit %>% 
    pluck(".workflow", 1) %>%   
    pull_workflow_fit() %>%          # this has been deprecated in 0.2.3, use extract_fit_engine()
    vip::vi()

  preds_df1$i <- i                   # Iteration number
  var_imp$start <- start             # Label start & end date
  var_imp$end <- end
  preds_df1_list[[i]] <- preds_df1   # add data frame to list
  var_imp_list[[i]] <- var_imp
  
}


# Data frames in list to one data frame
preds_df1_all <- bind_rows(preds_df1_list)
var_imp_all <- dplyr::bind_rows(var_imp_list)


# Remove dupes
preds_df1_all <- preds_df1_all[!duplicated(preds_df1_all[, c('symbol', 'date_stamp')]), ]
preds_df1_all %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)


# MC_TEST ------------------------------------------------------------------------------------------------------------------

# Preds to wide for numpy
positions <- preds_df1_all %>% 
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
sort_cols<- c('date_stamp', sort_cols[sort_cols != 'date_stamp'])
positions <- positions[, sort_cols]
positions[is.na(positions)] <- 0


# To matrix
positions_mtrx <- data.matrix(positions[, 2:ncol(positions)], rownames.force = FALSE)
positions_mtrx[positions_mtrx > 0] <- 1
positions_mtrx[positions_mtrx <= 0] <- 0


# Prices to wide for numpy
# - remove dupes
df_raw <- df_raw[!duplicated(df_raw[, c('symbol', 'date_stamp')]), ]

# - wide format
prices <- df_raw %>% select(symbol, date_stamp, close) %>% 
  filter(
    date_stamp %in% unique(preds_df1_all$date_stamp),
    symbol %in% colnames(positions)
    ) %>% 
  pivot_wider(
    names_from = symbol, 
    values_from = close
  )

# Ensure columns are in alphabetical order 
prices <- prices[, sort_cols]

# Replace na's with column maximum
na_to_max <- function(x) replace(x, is.na(x), max(x, na.rm = TRUE))
prices[] <- lapply(prices, na_to_max)


# To matrix
prices_mtrx <- data.matrix(prices[, 2:ncol(prices)], rownames.force = FALSE)
prices_mtrx1 <- apply(prices_mtrx, 2, max)
which(prices_mtrx == 0) <- prices_mtrx1

# Write to csv for test
write.csv(positions_mtrx, 'positions_mtrx.csv', col.names = FALSE, row.names = FALSE)
write.csv(prices_mtrx, 'prices_mtrx.csv', col.names = FALSE, row.names = FALSE)


# Invoke python
# https://community.rstudio.com/t/reticulate-source-python-import-modules/8355/3
use_condaenv(condaenv = 'MC_TEST', required = TRUE)
import_from_path("mc_test", path = 'C:/Users/brent/Documents/VS_Code/MC_TEST/MC_TEST/')
source_python('C:/Users/brent/Documents/VS_Code/MC_TEST/MC_TEST/mc_test.py')


mc_backtest1 = monte_carlo_backtest1(
  prices = prices_mtrx, 
  positions = positions_mtrx, 
  seed_capital = as.integer(100), 
  max_positions = as.integer(10),
  iter = as.integer(10000)
  )

mc_backtest2 = monte_carlo_backtest1(
  prices = prices_mtrx, 
  positions = positions_mtrx, 
  seed_capital = as.integer(100), 
  max_positions = as.integer(10),
  iter = as.integer(10000),
  rndm = TRUE
)

hist(mc_backtest1$cagr, breaks = 25, main = 'CAGR', xlab = '')
hist(mc_backtest2$cagr, breaks = 25, main = 'Random CAGR', xlab = '')
hist(mc_backtest1$max_drawdown, breaks = 25, main = 'Drawdown', xlab = '')
hist(mc_backtest1$volatility, breaks = 25, main = 'Volatility', xlab = '')




# 11. Scatter plot of actual vs predicted ---------------------------------------------------------

# For Theil Sen line https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

bind_cols(preds_df, select(test, symbol, date_stamp)) %>% 
  ggplot(aes(x = fwd_rtn_1m, y = .pred)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, size = 0.3, colour = 'blue', linetype = 'twodash') +
  geom_smooth(method = sen, se = FALSE, size = 0.3, colour = 'grey') +
  facet_wrap(vars(date_stamp), scales = 'free')

# Discretise predictions and visualise
bind_cols(preds_df, select(test, symbol, date_stamp)) %>% 
  group_by(date_stamp) %>% 
  mutate(
    pred_tercile = as.factor(ntile(.pred, 3)),
    actual_tercile = as.factor(ntile(fwd_rtn_1m, 3)),
    pred_sign = as.factor(sign(.pred)),
    actual_sign = as.factor(sign(fwd_rtn_1m))
  ) %>% 
  ungroup() %>% 
  conf_mat(actual_tercile, pred_tercile) %>% 
  #conf_mat(actual_sign, pred_sign) %>% 
  autoplot(type = 'heatmap')

# Spearmon correlation of actual and predicted returns
bind_cols(preds_df, select(test, symbol, date_stamp)) %>% 
  group_by(date_stamp) %>% 
  summarise(spearman_cor = cor(.pred, fwd_rtn_1m, method = 'spearman')) 
  




# 12. Variable importance --------------------------------------------------------------------------------------------------
# plot
final_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 7)

# Extract VI into dataframe.  
var_imp <- final_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>%   # this has been deprecated in 0.2.3, use extract_fit_engine()
  vip::vi()



# 13. Save final model object ----------------------------------------------------------------------------------------------

# Final model
set.seed(456)
# Fit on most recent dataset (same training period as above)
final_model <- final_workflow %>% 
  fit(data = filter(df_filter, between(date_stamp, as.Date(!!start), as.Date(!!end))))

saveRDS(final_model, file = "C:/Users/brent/Documents/R/R_import/final_model")

loaded_model <- readRDS("C:/Users/brent/Documents/R/R_import/final_model")

loaded_model_preds <- predict(loaded_model, new_data = filter(df_filter,date_stamp == tail(months, n = 1)))









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