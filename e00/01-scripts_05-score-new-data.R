#library(xgboost)
library(workflows)
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(romerb)


# External parameters
json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))

model_type     <- json_args$model_type
x_sect_scale   <- as.logical(json_args$x_sect_scale)
hyper_params   <- as.logical(json_args$hyper_params)


# Data
df <- read_csv(paste0(getwd(),"/02-data_02-scoring.csv"))
print(head(df))

if (x_sect_scale) {
  df <- xsect_scale(df, json_args$predictors)
  }

print(head(df))

# Load model and predict new data 
model <- readRDS(paste0(getwd(),"/03-model_01-oos-pred-model"))
preds <- predict(model, newdata = df)

#Join predictions to labelled data
oos_preds <- bind_cols(df, .pred = preds)

# Write to csv
write_csv(oos_preds, paste0(getwd(),"/02-data_07-oos-preds.csv"))