library(tidyverse)
library(lubridate)
library(mondate)
library(DescTools)
library(tidymodels)
library(slider)
library(readr)
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
coefs_list <- list()
coefs_list1 <- list()
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
  train1 <- training(split)
  test1 <- testing(split)
  
  print(paste0("Sliding window : Train ", min(train1$date_stamp)," to ", max(train1$date_stamp), " || Test : ", min(test1$date_stamp), " to ", max(test1$date_stamp),")"))

  
  # 4. Pre-processing ------------------------------------------------------------------------------------------------------
  
  train <- preprocess(train1)
  
  # Cross sectional scaling so no parameters inherited from test pre-processing
  test <- preprocess(test1)
  
  
  
  # 10. Fit model to training data ------------------------------------------------------------------------------------------
  
  # Sliding window size for average betas
  n <- length(unique(train$date_stamp))
  
  mdl_fit <- train %>% 
    nest_by(date_stamp) %>% 
    #mutate(model = list(lm(fwd_rtn ~ rtn_ari_3m + rtn_ari_12m, data = data))) %>% 
    mutate(model = list(lm(f, data = data))) %>% 
    summarise(tidy(model)) %>% 
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>% 
    ungroup() %>% 
    rename_with(~ gsub(pattern = "[()]", replacement = "", x = .x)) %>% 
    rename_with(~ tolower(.x)) %>% 
    mutate(
      across(starts_with("estimate"), 
             ~ slide_dbl(.x = .x, .f = mean, .before = (n-1), .complete = TRUE),
             .names = "{col}^{as.character(n)}MA")
    )
  
  # Extract PIT coefficients TO DO: retain the monthly coefficients for future analysis
  
  # Extract averaged coefficients
  coefs <- t(as.matrix(mdl_fit[mdl_fit$date_stamp == max(mdl_fit$date_stamp), grepl("\\^", names(mdl_fit))]))
  coefs_df <- as.data.frame(t(coefs))
  
  
  
  # # 10.1 Evaluate the test set -------------------------------------------------------------------------------------------
  
  model_mat <- as.matrix(cbind(intercept = rep(1,nrow(test)), test[, c('rtn_ari_3m', 'rtn_ari_12m')]))
  preds <- as.vector(model_mat %*% coefs)
  
  # Join labels to predictions
  preds <- bind_cols(.pred = preds, select(test, symbol, date_stamp, fwd_rtn), select(test1, fwd_rtn) %>% rename(fwd_rtn_raw = fwd_rtn))
  
  
  
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
  coefs_list1[[i]]<- mdl_fit[, c('date_stamp','estimate_intercept','estimate_rtn_ari_3m','estimate_rtn_ari_12m')]
  
}

# End loop =================================================================================================================

# Data frames in list to single data frame
preds_all <- dplyr::bind_rows(preds_list)
coefs_all <- dplyr::bind_rows(coefs_list)
coefs_all1 <- dplyr::bind_rows(coefs_list1)
coefs_all1 <- unique.data.frame(coefs_all1)


# Save preds & coefficients 
write_csv(preds_all, paste0(getwd(),"/02-data_04-vldtn-preds.csv"))
write_csv(coefs_all1, paste0(getwd(),"/02-data_06-coefs.csv"))


# 13. Save final model object ----------------------------------------------------------------------------------------------

# Fit to data comprising both the training and test set
# data prep
final_fit_data <- bind_rows(train, test)
train_dates <- unique(train$date_stamp)
excl_dates <- train_dates[1:(test_months / fwd_rtn_months)]
final_fit_data <- final_fit_data %>% filter(!date_stamp %in% excl_dates)
final_fit_data <- preprocess(final_fit_data)

# train model
final_model <- lm(f, data = final_fit_data)
saveRDS(final_model, paste0(getwd(),"/03-model_01-oos-pred-model"))

