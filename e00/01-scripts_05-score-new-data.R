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
predictors     <- json_args$predictors


# Data
df <- read_csv(paste0(getwd(),"/02-data_02-scoring.csv"))
print(head(df))

print("date_char to character")
df <- df %>% mutate(date_char = as.character(date_stamp))

if (x_sect_scale) {
  df <- xsect_scale(df, predictors)
  }

print(head(df))
#print(colnames(df))

# Load model and predict new data 
print("Load model")
model <- readRDS(paste0(getwd(),"/03-model_01-oos-pred-model"))
print("Print model")
model

print("Call predict")
preds <- predict(model, new_data = df)

#Join predictions to labelled data
print("Join preds to data")
oos_preds <- bind_cols(df, .pred = preds)

# Write to csv
print("Write csv")
write_csv(oos_preds, paste0(getwd(),"/02-data_07-oos-preds.csv"))