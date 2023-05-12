library(tidyverse)
library(lubridate)
library(mondate)
library(DescTools)
library(tidymodels)
library(slider)
library(readr)
#library(xgboost)
#library(earth)
library(fastshap)
library(jsonlite)


# External parameters
json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))

fwd_rtn_months <- json_args$fwd_rtn_months
train_months   <- json_args$train_months
test_months    <- json_args$test_months                                                   # out of sample test_months / stride
model_type     <- json_args$model_type
cross_val_v    <- json_args$cross_val_v

print(paste0("Train months : ", train_months))
print(paste0("Test months  : ", test_months))
print(paste0("Fcast months : ", fwd_rtn_months))
print(paste0("Model type   : ", model_type))

# Read data
df_train <- read_csv(paste0(getwd(),"/02-data_01-training.csv"))


# Loop parameters
months <- sort(unique(df_train$date_stamp))
n_months <- length(months)
sample_months <- ((train_months / fwd_rtn_months) + (test_months / fwd_rtn_months))                            # length of resultant df
loops <- floor((n_months - (train_months  / fwd_rtn_months)) / (test_months / fwd_rtn_months))
start_month_idx <- n_months - ((test_months / fwd_rtn_months) * loops) + 1 - (train_months / fwd_rtn_months)


# Empty list for loop results
preds_list <- list()
var_imp_list <- list()
tune_metrics_list <- list()
pred_shap_list <- list()


# Loop sliding over time series ============================================================================================

for (i in seq(from = start_month_idx, by = test_months / fwd_rtn_months, length.out = loops)) {
  
  start <- months[i]
  end <- months[i + sample_months - 1]
  df <- df_train %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end))) # inclusive
  
  
  # 2. Specify training and testing split ----------------------------------------------------------------------------------
  
  test_start_date <- as.Date(mondate::mondate(max(df$date_stamp)) - test_months + fwd_rtn_months)
  print(paste0("Sliding window : ", start," to ", end, " (test : ", test_start_date, " to ", end,")"))
  
  # Sort so that index for test split is appropriate
  df <- arrange(df, date_stamp, symbol)
  
  # Index ref for first date
  analysis <- as.integer(rownames(df[df$date_stamp <  test_start_date, ]))
  indices <- list(
    analysis   = analysis, 
    assessment = (max(analysis)+1):nrow(df)
  )
  
  # Determine proportion of records to select
  split <- make_splits(indices, df)
  
  # Training and test data
  train <- training(split)
  test <- testing(split)
  
  
  # 3. Create resamples of the training data -------------------------------------------------------------------------------
  
  set.seed(123)
  resamples <- group_vfold_cv(train, v = cross_val_v, group = date_char)
  
  
  # 4. Preprocessing -------------------------------------------------------------------------------------------------------
  # https://stats.stackexchange.com/questions/258307/raw-or-orthogonal-polynomial-regression
  
  recipe <- recipe(fwd_rtn ~ ., data = train) %>% 
    update_role(date_stamp, new_role = 'date_stamp') %>% 
    update_role(date_char, new_role = 'date_char') %>% 
    update_role(symbol, new_role = 'symbol') #%>% 
  #step_normalize(all_predictors()) 
  #step_corr(all_predictors(), threshold = .5)
  
  
  # 5. Specify models(s) ---------------------------------------------------------------------------------------------------
  
  xgb_model <- boost_tree(
    mtry = tune(),
    min_n = 50,
    trees = 250,
    tree_depth = tune()
  ) %>% 
    set_engine("xgboost", importance = TRUE) %>% 
    set_mode("regression")
  
  mars_model <- mars(
    num_terms = tune(),            # nprune - maximum number of terms (including intercept) in the pruned model
    prod_degree = tune(),          # degree -the highest possible degree of interaction between features
    prune_method = "exhaustive"    # pmethod
    ) %>% 
    set_engine("earth") %>%
    set_mode("regression")
  
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

  if (model_type == "xgb") {
    model <- xgb_model
  } else if (model_type == "mars") {
    model <- mars_model
  } else if (model_type == "rf") {
    model <- rf_model
  } else {
    model <- nn_model
  } 
  
  
  
  
  # 6. Create parameter grid -----------------------------------------------------------------------------------------------
  
  xgb_grid <- grid_regular(
    mtry(range = c(5, 9)), 
    #min_n(range = c(6, 8)),
    tree_depth(range = c(3, 7)),
    levels = 2
    )
  
  mars_grid <- grid_regular(       # http://www.milbo.org/doc/earth-notes.pdf
    num_terms(range = c(5, 8)),    # nprune - maximum number of terms (including intercept) in the pruned model
    prod_degree(range = c(1, 2)),  # degree - the highest possible degree of interaction between features
    levels = 5
    )
  
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
 
  if (model_type == "xgb") {
    grid <- xgb_grid
  } else if (model_type == "mars") {
    grid <- mars_grid
  } else if (model_type == "rf") {
    grid <- rf_grid
  } else {
    grid <- nn_grid
  } 
  
  
  # 6. Create workflow -----------------------------------------------------------------------------------------------------
  
  workflow <- workflow() %>% 
    add_model(model) %>%
    add_recipe(recipe)
  
  
  # 7. Fit re-samples ------------------------------------------------------------------------------------------------------
  
  tune_resamples <- tune::tune_grid(
    object = workflow, 
    resamples = resamples,
    grid = grid,
    # ensure metric corresponds to regression / classification
    metrics = metric_set(mae),                                                   #### PARAMETER ####
    control = control_grid(save_pred = TRUE)
  )
  
  
  # 7.1 Assess stability of model ------------------------------------------------------------------------------------------
  # Export - determine if the different hyperparameter specifications lead to different loss
  tune_metrics <- tune::collect_metrics(tune_resamples, summarize = FALSE)
  
  
  # 8. Select best parameters ----------------------------------------------------------------------------------------------
  best_param <- tune::select_best(tune_resamples, metric = "mae")
  
  
  # 9. Finalise workflow ---------------------------------------------------------------------------------------------------
  final_workflow <- tune::finalize_workflow(workflow, best_param)
  
  
  # 10. Final fit (fit best model to the training set) ---------------------------------------------------------------------
  
  set.seed(456)
  final_fit <- tune::last_fit(final_workflow, split) 
  
  # 10.1 Evaluate the test set
  preds <- tune::collect_predictions(final_fit)
  
  # Join labels to predictions
  preds <- bind_cols(preds, select(test, symbol, date_stamp))
  
  
  # Extract VI into dataframe  TO DO: document how variable importance is derived for different models
  var_imp <- extract_fit_engine(final_fit) %>% vip::vi()
  
  fit_model <- extract_fit_engine(final_fit)
  
  # Prediction wrapper
  # this function must return a numeric vector of predicted outcomes (not a matrix)
  pfun <- function(object, newdata) {
    if (model_type == "mars") {
      p <- predict(object, newdata = newdata)
      p <- as.vector(p)
    }
    return(p)
  }
  
  # Prepare test data
  new_test_data <- bake(prep(recipe), new_data = test, all_predictors())
  
  if (model_type == "xgb") {
    new_test_data <- xgb.DMatrix(data.matrix(new_test_data), missing = NA)
    preds_shap <- predict(fit_model, newdata = new_test_data, predcontrib = TRUE, approxcontrib = FALSE)
  }
  
  preds_shap <- fastshap::explain(fit_model, X = as.data.frame(new_test_data), pred_wrapper = pfun, nsim = 10)
  
  preds_shap <- as.data.frame(preds_shap)
  
  
  # Label start & end date
  preds$start <- start
  preds$end <- end
  var_imp$start <- start
  var_imp$end <- end
  preds_shap$start <- start
  preds_shap$end <- end
  tune_metrics$start <- start
  tune_metrics$end <- end
  
  # Add data frame to list
  preds_list[[i]] <- preds
  var_imp_list[[i]] <- var_imp
  pred_shap_list[[i]] <- preds_shap
  tune_metrics_list[[i]] <- tune_metrics
  
}

# End loop =================================================================================================================


# Data frames in list to single data frame
preds_all <- dplyr::bind_rows(preds_list)
var_imp_all <- dplyr::bind_rows(var_imp_list)
preds_shap_all <- dplyr::bind_rows(pred_shap_list)
tune_metrics_all <- dplyr::bind_rows(tune_metrics_list)


# Remove dupes
preds_all <- preds_all[!duplicated(preds_all[, c('symbol', 'date_stamp')]), ]
preds_all %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)


# Write to csv
# ^^^ same number of records
write_csv(preds_all,        paste0(getwd(),"/02-data_04-vldtn-preds.csv")) # ^^^ 
write_csv(preds_shap_all,   paste0(getwd(),"/02-data_05-shap-values.csv")) # ^^^
write_csv(tune_metrics_all, paste0(getwd(),"/02-data_06-hyper-parms.csv"))
write_csv(var_imp_all,      paste0(getwd(),"/02-data_07-var-imp.csv"))


# 13. Save final model object ----------------------------------------------------------------------------------------------

# Final model
set.seed(456)
# Fit on most recent dataset (same training period as above)
final_model <- final_workflow %>% 
  fit(data = filter(df_train, between(date_stamp, as.Date(!!start), as.Date(!!end))))

saveRDS(final_model, paste0(getwd(),"/03-model_01-oos-pred-model"))











# ==========================================================================================================================
# SCRATCH 
# ==========================================================================================================================

# Empty list for loop results
coefs_list <- list()
preds_list <- list()

# Formula
f <- as.formula(paste("fwd_rtn", paste(json_args$predictors, collapse=" + "), sep=" ~ "))

# Function to preprocess data
preprocess <- function(df) {
  df <- df %>% 
    group_by(date_stamp) %>% 
    mutate(
      fwd_rtn     = as.vector(scale(fwd_rtn)),
      rtn_ari_3m  = as.vector(scale(rtn_ari_3m)),
      rtn_ari_12m = as.vector(scale(rtn_ari_12m))
    ) %>% 
    ungroup() %>% 
    select(date_stamp, symbol, fwd_rtn, rtn_ari_3m, rtn_ari_12m)
  
  return(df)
}


# Back test loop
for (i in seq(from = start_month_idx, by = test_months / fwd_rtn_months, length.out = loops)) {
  
  start <- months[i]
  end <- months[i + sample_months - 1]
  df <- df_train %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end))) # inclusive
  
  
  # 2. Specify training and testing split ----------------------------------------------------------------------------------
  
  test_start_date <- as.Date(mondate::mondate(max(df$date_stamp)) - test_months + fwd_rtn_months)
  print(paste0("Sliding window : ", start," to ", end, " (test : ", test_start_date, " to ", end,")"))
  
  # Sort so that index for test split is appropriate
  df <- arrange(df, date_stamp, symbol)
  
  # Index ref for first date
  analysis <- as.integer(rownames(df[df$date_stamp <  test_start_date, ]))
  indices <- list(
    analysis   = analysis, 
    assessment = (max(analysis)+1):nrow(df)
  )
  
  # Determine proportion of records to select
  split <- make_splits(indices, df)
  
  # Training and test data
  train <- training(split)
  test <- testing(split)
  

  
  # 4. Pre-processing ------------------------------------------------------------------------------------------------------
  
  train <- preprocess(train)
  
  # Cross sectional scaling so no parameters inherited from test pre-processing
  test <- preprocess(test)
  
  
  
  # 10. Fit model to training data ------------------------------------------------------------------------------------------
  
  # Sliding window size for average betas
  n <- length(unique(train$date_stamp))
  
  mdl_fit <- train %>% 
    nest_by(date_stamp) %>% 
    mutate(model = list(lm(fwd_rtn ~ rtn_ari_3m + rtn_ari_12m, data = data))) %>% 
    summarise(tidy(model)) %>% 
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>% 
    ungroup() %>% 
    rename_with(~ gsub(pattern = "[()]", replacement = "", x = .x)) %>% 
    rename_with(~ tolower(.x)) %>% 
    mutate(
      across(starts_with("estimate"), 
             ~ slide_dbl(.x = .x, .f = mean, .before = 11, .complete = TRUE),
             .names = "{col}^{as.character(n)}MA")
    )
  # TO DO: retain the monthly coefficients for future analysis
  
  # Extract averaged coefficients
  coefs <- t(as.matrix(mdl_fit[mdl_fit$date_stamp == max(mdl_fit$date_stamp), grepl("\\^", names(mdl_fit))]))
  coefs_df <- as.data.frame(t(coefs))
  
  
  
  # # 10.1 Evaluate the test set -------------------------------------------------------------------------------------------
  
  model_mat <- as.matrix(cbind(intercept = rep(1,nrow(test)), test[, c('rtn_ari_3m', 'rtn_ari_12m')]))
  preds <- as.vector(model_mat %*% coefs)
  
  # Join labels to predictions
  preds <- bind_cols(preds, select(test, symbol, date_stamp))
  colnames(preds)[1] <- "preds"
  
  
  
  # 10. Collect predictions ------------------------------------------------------------------------------------------------

  # Label start & end date
  preds$start <- start
  preds$end <- end
  # Label start & end date
  coefs_df$start <- start
  coefs_df$end <- end
  
  # Add data frame to list
  preds_list[[i]] <- preds
  coefs_list[[i]] <- coefs_df
  
}

# End loop =================================================================================================================

# Data frames in list to single data frame
preds_all <- dplyr::bind_rows(preds_list)
coefs_all <- dplyr::bind_rows(coefs_list)


# 13. Save final model object ----------------------------------------------------------------------------------------------

# Fit to data comprising both the training and test set
# data prep
final_fit_data <- bind_rows(train, test)
train_dates <- unique(train$date_stamp)
excl_dates <- train_dates[1:(test_months / fwd_rtn_months)]
final_fit_data <- final_fit_data %>% filter(!date_stamp %in% excl_dates)
final_fit_data <- preprocess(final_fit_data)

# train model
final_model <- lm(fwd_rtn ~ rtn_ari_3m + rtn_ari_12m, data = final_fit_data)
saveRDS(final_model, paste0(getwd(),"/03-model_01-oos-pred-model"))


#xxx <- preprocess(df_train)
#group_by(xxx, date_stamp) %>% summarise(x = mean(fwd_rtn))
#sum(c("date_stamp","fwd_rtn","rtn_ari_3m","rtn_ari_12m") %in% colnames(test))
