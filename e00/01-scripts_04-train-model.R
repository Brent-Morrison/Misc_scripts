library(tidyverse)
library(lubridate)
library(mondate)
library(DescTools)
library(tidymodels)
library(slider)
library(readr)
library(glmnet)
library(fastshap)
library(jsonlite)
library(romerb)


# 'ggparcoord' for eda visualisation, https://uc-r.github.io/gda | https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/parcor.html | https://ggobi.github.io/ggally/reference/ggparcoord.html
# https://brunocarlin.github.io/tidy.outliers/index.html

# External parameters
json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))

fwd_rtn_months <- json_args$fwd_rtn_months
train_months   <- json_args$train_months
test_months    <- json_args$test_months                                                   # out of sample test_months / stride
model_type     <- json_args$model_type
cross_val_v    <- json_args$cross_val_v
predictors     <- json_args$predictors
x_sect_scale   <- as.logical(json_args$x_sect_scale)
ts_normalise   <- as.logical(json_args$ts_normalise)
hyper_params   <- as.logical(json_args$hyper_params)
train_on_qntls <- as.logical(json_args$train_on_qntls)

print("BACKTEST PARAMETERS ---------")
print(paste0("Train months        : ", train_months))
print(paste0("Test months         : ", test_months))
print(paste0("Fcast months        : ", fwd_rtn_months))
print(paste0("Model type          : ", model_type))
print(paste0("X-sect scaling      : ", x_sect_scale))
print(paste0("Hyper params        : ", hyper_params))
print(paste0("Train on quintiles  : ", train_on_qntls))


# Load appropriate library
if (model_type == "xgb") {
  library(xgboost)
} else if (model_type == "mars") {
  library(earth)
} else if (model_type == "rf") {
  library(randomForest)
} else if (model_type == "nnet") {
  library(nnet)
} else if (model_type == "glm") {
  library(glmnet)
}

# Read data
df_train <- read_csv(paste0(getwd(),"/02-data_01-training.csv")) %>% 
  mutate(across(date_char, ~ as.character(.)))


# Loop parameters
months <- sort(unique(df_train$date_stamp))
n_months <- length(months)
sample_months <- ((train_months / fwd_rtn_months) + (test_months / fwd_rtn_months))                            # length of resultant df
loops <- floor((n_months - (train_months  / fwd_rtn_months)) / (test_months / fwd_rtn_months))
start_month_idx <- n_months - ((test_months / fwd_rtn_months) * loops) + 1 - (train_months / fwd_rtn_months)


# Empty list for loop results
coefs_list <- list()
coefs_list1 <- list()
preds_list <- list()
var_imp_list <- list()
tune_metrics_list <- list()
pred_shap_list <- list()


# Models with no hyper-parameters, for use in conditional logic re variable importance and parameter grids
#model_no_hyper <- c("lm")


# Formula for lm function call
f <- as.formula(paste("fwd_rtn", paste(predictors, collapse=" + "), sep=" ~ "))


# Loop sliding over time series ============================================================================================

for (i in seq(from = start_month_idx, by = test_months / fwd_rtn_months, length.out = loops)) {
  
  start <- months[i]
  end <- months[i + sample_months - 1]
  df <- df_train %>% filter(between(date_stamp, as.Date(!!start), as.Date(!!end))) # inclusive
  
  
  # 2. Specify training and testing split ----------------------------------------------------------------------------------
  
  test_start_date <- as.Date(mondate::mondate(max(df$date_stamp)) - test_months + fwd_rtn_months)
  #print(paste0("Sliding window : ", start," to ", end, " (test : ", test_start_date, " to ", end,")"))
  
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
  
  print(paste0("Sliding window : Train ", min(train$date_stamp)," to ", max(train$date_stamp), " || Test : ", min(test$date_stamp), " to ", max(test$date_stamp)))
  
  
  # 3. Create resamples of the training data -------------------------------------------------------------------------------

  if (hyper_params) {
    set.seed(123)
    resamples <- group_vfold_cv(train, v = cross_val_v, group = date_char)
  }
  
  # 4. Preprocessing -------------------------------------------------------------------------------------------------------
  # https://stats.stackexchange.com/questions/258307/raw-or-orthogonal-polynomial-regression
  
  if (x_sect_scale) {
    train <- xsect_scale(train, predictors)
    test  <- xsect_scale(test, predictors)
  } 
  
  if (train_on_qntls) {
    train <- df_train %>% 
      group_by(date_stamp) %>% 
      mutate(qntl = ntile(fwd_rtn, 5)) %>% 
      filter(qntl %in% c(1,5)) %>% 
      ungroup() %>% 
      select(-qntl)
  }
  
  if (hyper_params) {
    recipe <- recipe(train) %>% 
      update_role(date_stamp, new_role = 'date_stamp') %>% 
      update_role(date_char, new_role = 'date_char') %>% 
      update_role(symbol, new_role = 'symbol') %>% 
      update_role(unlist(predictors), new_role = 'predictor') %>% 
      update_role(fwd_rtn, new_role = 'outcome') 
  } 

  if (ts_normalise) {
    recipe <- recipe %>% 
      step_normalize(all_predictors()) 
      #step_corr(all_predictors(), threshold = .5)
  } 
  
  
  # 5. Specify models(s) ---------------------------------------------------------------------------------------------------

  lm_model  <- as.formula(paste("fwd_rtn", paste(predictors, collapse=" + "), sep=" ~ "))
  glm_model <- as.formula(paste("fwd_rtn", paste(predictors, collapse=" + "), sep=" ~ "))

  if (model_type == "xgb") {
    
    model <- boost_tree(
      mtry = tune(),
      min_n = 50,
      trees = 250,
      tree_depth = tune()
      ) %>% 
      set_engine("xgboost", importance = TRUE) %>% 
      set_mode("regression")
    
  } else if (model_type == "mars") {
    
    model <- mars(
      num_terms = tune(),            # nprune - maximum number of terms (including intercept) in the pruned model
      prod_degree = tune(),          # degree -the highest possible degree of interaction between features
      prune_method = "exhaustive"    # pmethod
      ) %>% 
      set_engine("earth") %>%
      set_mode("regression")
    
  } else if (model_type == "rf") {
    
    model <- rand_forest(
      mtry = tune(),                # An integer for the number of predictors that will be randomly sampled at each split when creating the tree models
      min_n = 100,                  # An integer for the minimum number of data points in a node
      trees = 250                   # An integer for the number of trees contained in the ensemble
      ) %>% 
      set_engine("randomForest", importance = TRUE) %>% 
      set_mode("regression")
    
  } else if (model_type == "nnet") {
    
    model <- mlp(                # https://community.rstudio.com/t/extending-parsnip/99290
      hidden_units = tune(),
      penalty = tune()
      ) %>% 
      set_engine("nnet") %>% 
      set_mode("regression")
    
  } else {
    
    model <- as.formula(paste("fwd_rtn", paste(predictors, collapse=" + "), sep=" ~ "))
    
  } 
  
  
  
  
  # 6. Create parameter grid -----------------------------------------------------------------------------------------------
  # Steps 6 though 10 for models with hyper-parameters only
  
  if (hyper_params) {
   
    if (model_type == "xgb") {
      
      grid <- grid_regular(
        mtry(range = c(floor(length(predictors) / 2), length(predictors))), 
        #min_n(range = c(6, 8)),
        tree_depth(range = c(3, 7)),
        levels = 2
        )
      
    } else if (model_type == "mars") {
      
      grid <- grid_regular(            # http://www.milbo.org/doc/earth-notes.pdf
        num_terms(range = c(5, 8)),    # nprune - maximum number of terms (including intercept) in the pruned model
        prod_degree(range = c(1, 2)),  # degree - the highest possible degree of interaction between features
        levels = 5
        )
      
    } else if (model_type == "rf") {
      
      grid <- grid_regular(
        mtry(range = c(floor(length(predictors) / 2), length(predictors))),  # cannot exceed maximum number of columns
        #min_n(range = c(6, 8)),
        #trees(range = c(1000, 1500))
        levels = 2
        )
      
    } else if (model_type == "nnet") {
      
      grid <- grid_regular(
        hidden_units(range = c(3, 5)),
        penalty()
        )
      
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
      control = control_grid(
        save_pred = TRUE,
        save_workflow = TRUE
        )
    )
    
    
    # 7.1 Assess stability of model ----------------------------------------------------------------------------------------
    # Export - determine if the different hyper-parameter specifications lead to different loss
    tune_metrics <- tune::collect_metrics(tune_resamples, summarize = FALSE)
    
    
    # 8. Select best parameters --------------------------------------------------------------------------------------------
    best_param <- tune::select_best(tune_resamples, metric = "mae")
    cat("\nBest parameters ---------------")
    print(best_param)
    cat("\n")
    
    # 9. Finalise workflow (update the workflow with the best tuning parameters) -------------------------------------------
    final_workflow <- tune::finalize_workflow(workflow, best_param)
    
    
    # 10. Final fit (fit best model to the training set) -------------------------------------------------------------------
    
    # https://www.tidyverse.org/blog/2023/04/tuning-delights/
    # While fit_best() gives a fitted workflow, last_fit() gives you the performance results. 
    # If you want the fitted workflow, you can extract it from the result of last_fit() via extract_workflow()
    
    set.seed(456)
    final_fit <- tune::last_fit(final_workflow, split) 
    #final_fit <- fit(final_workflow, train)
    
  }	else if (model_type == "lm") {
    
    final_fit <- lm(f, data = train)
    
  } else if (model_type == "glm") {
    
    alpha <- seq(0, 1, len=11)^3
    nfolds <- 10
    foldid <- sample(rep(seq_len(nfolds), length=nrow(train)))
    glm_models <- lapply(
      alpha, 
      cv.glmnet, 
      x = as.matrix(train[, unlist(predictors)]), 
      y = as.matrix(train[, "fwd_rtn"]), 
      nfolds = nfolds, 
      foldid = foldid,
      family = "gaussian",
      weights = NULL,
      offset = NULL,
      lambda = NULL,
      type.measure = "mse",
      alignment = "lambda",
      grouped = TRUE,
      keep = FALSE,
      parallel = FALSE,
      gamma = c(0, 0.25, 0.5, 0.75, 1),
      relax = FALSE,
      trace.it = 0
    )
    
    # Find the model with the lowest error
    cvms <- sapply(glm_models, "[[", "cvm")
    min_model <- which.min(sapply(cvms, min))
    final_fit <- glm_models[[min_model]]
    
  }
  
  # 11. Evaluate model / predict on the test set ----------------------------------------------------------------------------
  
  if (hyper_params) {
    preds <- tune::collect_predictions(final_fit, summarize = FALSE)      # https://tune.tidymodels.org/reference/collect_predictions.html
    #preds <- predict(final_fit, test)
    preds <- bind_cols(preds, select(test, symbol, date_stamp, fwd_rtn_chk = fwd_rtn))  # join labels to predictions
  } else if (model_type == "lm") {
	  preds <- predict(final_fit, test[, paste(predictors)])
	  preds <- bind_cols(preds, select(test, symbol, date_stamp, fwd_rtn))  # join labels to predictions
	  colnames(preds)[1] = ".pred"
	  print(str(preds))
  } else if (model_type == "glm") {
    preds <- predict(final_fit, newx = as.matrix(test[ , unlist(predictors)]), s = "lambda.min")
    preds <- bind_cols(preds, select(test, symbol, date_stamp, fwd_rtn))  # join labels to predictions
  }
  
  # Join labels to predictions
  #preds <- bind_cols(.pred = preds, select(test, symbol, date_stamp, fwd_rtn), select(test, fwd_rtn) %>% rename(fwd_rtn_raw = fwd_rtn))
  #print(str(preds)) #------------------------------------------------------------------
  
  
  # 12. Extract model specific variable importance -------------------------------------------------------------------------
  
  # TO DO: document how variable importance is derived for different models
  # https://koalaverse.github.io/vip/articles/vip.html | https://koalaverse.github.io/vip/reference/vi_model.html
  
  if (hyper_params) {
	  fit_model <- extract_fit_engine(final_fit)
	} else {
	  fit_model <- final_fit
	}
	
  var_imp <- fit_model %>% vip::vi_model()
  
  
  # 13. Extract model agnostic (Shapley) variable importance ---------------------------------------------------------------
  
  # Prediction wrapper, this function must return a numeric vector of predicted outcomes ### NOT A MATRIX ####
  pfun <- function(object, newdata) {
    if (model_type == "glm") {
      p <- predict(object, newx = as.matrix(newdata), s = "lambda.min")
      p <- as.vector(p)
    } else {
      p <- predict(object, newdata = newdata)
      p <- as.vector(p)
    }
    return(p)
  }
  
  # Prepare test data
  if (hyper_params) {
    new_test_data <- bake(prep(recipe), new_data = test, all_predictors())
  } else if (x_sect_scale) {
    new_test_data <- xsect_scale(test, predictors)
  } else {
    new_test_data <- test
  }
  
  # Shapley values
  if (model_type == "xgb") {
    new_test_data <- xgb.DMatrix(data.matrix(new_test_data), missing = NA)
    preds_shap <- predict(fit_model, newdata = new_test_data, predcontrib = TRUE, approxcontrib = FALSE)
  } else if (model_type == "glm") {
    preds_shap <- fastshap::explain(fit_model, X = as.matrix(new_test_data[ , unlist(predictors)]), pred_wrapper = pfun, nsim = 10)
  } else {
    preds_shap <- fastshap::explain(fit_model, X = as.data.frame(new_test_data), pred_wrapper = pfun, nsim = 10)
  }
  
  preds_shap <- as.data.frame(preds_shap)
  
  
  # Label start & end date
  if (hyper_params) {
    tune_metrics$start <- start
	  tune_metrics$end   <- end
  } 
    
  preds$start        <- start
	preds$end          <- end
	var_imp$start      <- start
	var_imp$end        <- end
	preds_shap$start   <- start
	preds_shap$end     <- end

  
  
  # Add data frame to list
  if (hyper_params) {
    
    tune_metrics_list[[i]] <- tune_metrics
    
  } 
    
  preds_list[[i]]        <- preds
  var_imp_list[[i]]      <- var_imp
  pred_shap_list[[i]]    <- preds_shap
  
}

# End loop =================================================================================================================


# Data frames in list to single data frame
if (hyper_params) {
  tune_metrics_all <- dplyr::bind_rows(tune_metrics_list)
} 

preds_all        <- dplyr::bind_rows(preds_list)
var_imp_all      <- dplyr::bind_rows(var_imp_list)
preds_shap_all   <- dplyr::bind_rows(pred_shap_list)
  
# Remove dupes
preds_all <- preds_all[!duplicated(preds_all[, c('symbol', 'date_stamp')]), ]
preds_all %>% group_by(symbol, date_stamp) %>% summarise(n = n()) %>% filter(n > 1)



# Write to csv
# ^^^ same number of records
if (hyper_params) {
  write_csv(tune_metrics_all, paste0(getwd(),"/02-data_06-hyper-parms.csv"))
} 

write_csv(preds_all,        paste0(getwd(),"/02-data_04-vldtn-preds.csv")) # ^^^ 
write_csv(preds_shap_all,   paste0(getwd(),"/02-data_05-shap-values.csv")) # ^^^
write_csv(var_imp_all,      paste0(getwd(),"/02-data_07-var-imp.csv"))



# 13. Save final model object ----------------------------------------------------------------------------------------------

# Final model
set.seed(456)
# Fit on most recent data set (same training period as above)
if (hyper_params) {
  final_model <- final_workflow %>% 
	fit(data = filter(df_train, between(date_stamp, as.Date(!!start), as.Date(!!end))))
} else {
  final_model <- lm(f, data = filter(df_train, between(date_stamp, as.Date(!!start), as.Date(!!end))))
}


saveRDS(final_model, paste0(getwd(),"/03-model_01-oos-pred-model"))