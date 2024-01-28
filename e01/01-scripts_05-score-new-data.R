library(xgboost)
library(workflows)
library(dplyr)
library(tidyr)
library(readr)

# Data
df <- read_csv(paste0(getwd(),"/02-data_02-scoring.csv"))

# Needs to be the same as the function in training file
preprocess <- function(df) {
  df <- df %>% 
    group_by(date_stamp) %>% 
    mutate(across(.cols = unlist(json_args$predictors), .fns = ~ as.vector(scale(.x)))) %>% 
    ungroup() %>% 
    select(date_stamp, symbol, unlist(json_args$predictors))
  
  return(df)
}

df <- preprocess(df)

# Load model and predict new data 
model <- readRDS(paste0(getwd(),"/03-model_01-oos-pred-model"))
preds <- predict(model, newdata = df)

#Join predictions to labelled data
oos_preds <- bind_cols(df, .pred = preds)

# Write to csv
write_csv(oos_preds, paste0(getwd(),"/02-data_07-oos-preds.csv"))


# ==========================================================================================================================
# SCRATCH 
# ==========================================================================================================================

# Load function from training script
#preprocess <- function() {}
#insertSource(paste0(getwd(),"/01-scripts_04-train-model.R"), functions="preprocess", force=T) 
#preprocess <- preprocess@.Data


