library(xgboost)
library(workflows)
library(dplyr)
library(tidyr)
library(readr)

df <- read_csv(paste0(getwd(),"/02-data_02-scoring.csv"))

model <- readRDS(paste0(getwd(),"/03-model_01-oos-pred-model"))

preds <- predict(model, new_data = df)

oos_preds <- bind_cols(df, preds)

# Write to csv
write_csv(oos_preds, paste0(getwd(),"/02-data_07-oos-preds.csv"))