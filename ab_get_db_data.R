# Libraries
library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(slider)
library(readr)


# External parameters (aa_get_dp_data.bat)
args <- commandArgs(trailingOnly = TRUE)

lookback_yrs <- args[1]        
var2 <- args[2]  


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


# Retrieve data
#qry_tmpl <- "select * from access_layer.return_attributes where date_stamp > current_date - interval '?lookback_yrs years' order by 1, 2"
#qry_text <- DBI::sqlInterpolate(conn = con, sql = qry_tmpl, lookback_yrs = lookback_yrs)
qry_text <- sprintf("select * from access_layer.return_attributes where date_stamp > current_date - interval '%s years' order by 1, 2", lookback_yrs)
qry_send <- DBI::dbSendQuery(conn = con, statement = qry_text) 
qry_rslt <- DBI::dbFetch(qry_send)


# Feature engineering

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

df <- qry_rslt %>% 
  group_by(symbol) %>% 
  mutate(
    fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close)),
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
  # Date to character - required for stratified sampling, "vfold_cv" does not accept date
  mutate(date_char = as.character(date_stamp)) %>%  
  select(
    date_stamp, date_char, symbol, fwd_rtn_1m,
    rtn_ari_1m, rtn_ari_1m_sct, rtn_ari_1m_ind,
    rtn_ari_3m, rtn_ari_3m_sct, rtn_ari_3m_ind,
    rtn_ari_12m, rtn_ari_12m_sct, rtn_ari_12m_ind, 
    perc_range_12m, perc_pos_12m, rtn_from_high_12m,
    vol_ari_60d, vol_ari_60d_sct, vol_ari_60d_ind,
    skew_ari_120d, kurt_ari_120d 
  ) %>% 
  ungroup() %>% 
  mutate(name = var2)


# Create folder for data
if (!dir.exists(var2)) {dir.create(path = var2)}


# Write csv
write_csv(df, paste0(var2,"/",as.character(Sys.Date()),".csv"))

