# Libraries
library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(slider)
library(readr)


# External arguments / parameters
json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))

lookback_yrs <- json_args$lookback_yrs
fwd_rtn_months <- json_args$fwd_rtn_months


# Database connection
config <- jsonlite::read_json('C:/Users/brent/Documents/VS_Code/postgres/postgres/config.json')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host      = 'localhost',
  port      = '5432',
  dbname    = 'stock_master',
  user      = 'postgres',
  password  = config$pg_password
)


# Retrieve stock data
qry_text <- sprintf("select * from access_layer.return_attributes where date_stamp > current_date - interval '%s years' order by 1, 2", lookback_yrs)
qry_send <- DBI::dbSendQuery(conn = con, statement = qry_text) 
qry_rslt <- DBI::dbFetch(qry_send)


# Retrieve S&P500 data
sp5_sql <- "select * from access_layer.daily_sp500_ts_vw"
sp5_send <- dbSendQuery(conn = con, statement = sp5_sql) 
df_sp500 <- dbFetch(sp5_send)


# Functions
perc_range <- function(x){
  current <- tail(x, 1)
  min <- min(x)
  range <- max(x) - min
  res <- ( current - min ) / range
  return(res)
}

perc_pos<- function(x){
  x <- x[!is.na(x)]
  res <- sum(x > 0) / length(x)
  return(res)
}

rtn_from_high <- function(x){
  current <- tail(x, 1)
  max <- max(x)
  res <- log( current / max )
  return(res)
}


# Feature engineering

df <- qry_rslt %>% 
  group_by(symbol) %>% 
  mutate(
    fwd_rtn = lead((adjusted_close-lag(adjusted_close, n = fwd_rtn_months)) / lag(adjusted_close, n = fwd_rtn_months), n = fwd_rtn_months),
    perc_range_12m = slide_dbl(.x = adjusted_close, .f = perc_range, .before = 11, .complete = TRUE),
    perc_pos_12m = slide_dbl(.x = rtn_log_1m, .f = perc_pos, .before = 11, .complete = TRUE),
    rtn_from_high_12m = slide_dbl(.x = adjusted_close, .f = rtn_from_high, .before = 11, .complete = TRUE)
    ) %>% 
  ungroup() %>%
  group_by(sector) %>%
  mutate(
    rtn_ari_1m_sct = mean(rtn_ari_1m),
    rtn_ari_3m_sct = mean(rtn_ari_3m),
    rtn_ari_12m_sct = mean(rtn_ari_12m),
    vol_ari_60d_sct = mean(vol_ari_60d)
  ) %>% 
  ungroup() %>% 
  group_by(industry) %>%
  mutate(
    rtn_ari_1m_ind = mean(rtn_ari_1m),
    rtn_ari_3m_ind = mean(rtn_ari_3m),
    rtn_ari_12m_ind = mean(rtn_ari_12m),
    vol_ari_60d_ind = mean(vol_ari_60d)
  ) %>% 
  ungroup() %>% 
  # Date to character - required for stratified sampling, "vfold_cv" does not accept date 
  # TO DO - will this work after writing to csv? Do this in 03-train_model?
  mutate(date_char = as.character(date_stamp)) %>%  
  select(
    date_stamp, date_char, symbol, fwd_rtn,
    rtn_ari_1m, #rtn_ari_1m_sct, rtn_ari_1m_ind,
    rtn_ari_3m, #rtn_ari_3m_sct, rtn_ari_3m_ind,
    rtn_ari_12m, #rtn_ari_12m_sct, rtn_ari_12m_ind, 
    perc_range_12m, perc_pos_12m, rtn_from_high_12m,
    vol_ari_60d#, #vol_ari_60d_sct, vol_ari_60d_ind,
    #skew_ari_120d, kurt_ari_120d 
  ) 


# Remove dupes
df <- df[!duplicated(df[, c('symbol', 'date_stamp')]), ]


# Model training / testing
df_train <- df %>% 
  filter(
    date_stamp < max(df$date_stamp),
    between(fwd_rtn, -0.5, 0.5)
  ) %>% 
  drop_na() 

# Filter df_train for months corresponding to fwd_rtn_months setting and current oos prediction month
max_date <- max(df$date_stamp)
month_max_date <- month(max_date)

# Date filter
if (fwd_rtn_months == 2) {
  if (month_max_date %in% c(1,3,5,7,9,11)) {
    month_filter <- c(1,3,5,7,9,11)
  } else if (month_max_date %in% c(2,4,6,8,10,12)) {
    month_filter <- c(2,4,6,8,10,12)
  }
} else if (fwd_rtn_months == 3) {
  if (month_max_date %in% c(3,6,9,12)) {
    month_filter <- c(3,6,9,12)
  } else if (month_max_date %in% c(2,5,8,11)) {
    month_filter <- c(2,5,8,11)
  } else if (month_max_date %in% c(1,4,7,10)) {
    month_filter <- c(1,4,7,10)
  }
} else {
  month_filter <- c(1:12)
}

df_train <- df_train %>% filter(month(date_stamp) %in% month_filter)

# Check for non-contiguous data and dates with minimal data
df_check <- df_train %>% group_by(date_stamp) %>% summarise(n = n()) %>% 
  mutate(months_next = round((year(lead(date_stamp)) + month(lead(date_stamp)) / 12) - (year(date_stamp) + month(date_stamp) / 12),3)) 

first_non_contig <- which(df_check$months_next != round(fwd_rtn_months / 12, 3))
if(length(first_non_contig) == 0) first_non_contig <- 0

# Dates to include / exclude
date_to_include <- df_check$date_stamp[(first_non_contig+1):nrow(df_check)]
date_to_exclude <- as.list(df_check[df_check$n < 100, "date_stamp"])[[1]]

df_train <- df_train %>% filter(date_stamp %in% date_to_include)
df_train <- df_train %>% filter(!date_stamp %in% date_to_exclude)


# OOS scoring data (current month)
df_score <- df %>% 
  filter(date_stamp == max_date) %>% 
  select(-fwd_rtn)


# Write to csv
write_csv(df_train, paste0(getwd(),"/02-data_01-training.csv"))
write_csv(df_score, paste0(getwd(),"/02-data_02-scoring.csv"))
write_csv(df_sp500, paste0(getwd(),"/02-data_03-sp500.csv"))
