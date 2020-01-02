#===============================================================================================
#==    REQIURED PACKAGES                                                                      ==
#===============================================================================================

library("tidyverse")
library("DescTools")
library("tidyquant")
library("timetk")
library("broom")
library("tibbletime")
library("RcppRoll")


#===============================================================================================
#==    DATE FILTER & PARAMETERS                                                               ==
#===============================================================================================

lb = 6                  #  Lookback period
pc = 0.2                #  Percent drawdown for binary market in/out indicator cutoff
fr = -0.025             #  Forward return for binary market in/out indicator cutoff
s.date = as.Date("1945-01-01")
e.date = as.Date("2019-12-01")
quandl_api_key("hpbPcsfGudN3viBgh8th")
qndlm = c("NAHB/NWFHMI.1",  #  NAHB / Wells Fargo National Housing Market Index
          "ISM/MAN_NEWORDERS.5")  #  ISM Manufacturing New Orders Index
fredd = c("DTB3", "DGS2", "DGS10")
fredw = c("IC4WSA")
fredm = c("AAA",       #	Moody's Seasoned Aaa Corporate Bond Yield
          "ACDGNO",    #	Value of Manufacturers' New Orders for Consumer Goods: Consumer
          "AHETPI",    #	Average Hourly Earnings of Production and Nonsupervisory Employees:
          "AWHMAN",    #	Average Weekly Hours of Production and Nonsupervisory Employees:
          "BAA",       #	Moody's Seasoned Baa Corporate Bond Yield
          "BOGMBASE",  #	Monetary Base; Total
          "CFNAIDIFF", #	Chicago Fed National Activity Index: Diffusion Index
          "CPIAUCSL",  #	Consumer Price Index for All Urban Consumers: All Items
          "CPILFESL",  #	Consumer Price Index for All Urban Consumers: All Items Less Food and
          "FEDFUNDS",  #	Effective Federal Funds Rate
          "GS10",      #	10-Year Treasury Constant Maturity Rate
          "GS2",       #	2-Year Treasury Constant Maturity Rate
          "INDPRO",    #	Industrial Production Index
          "ISRATIO",   #	Total Business: Inventories to Sales Ratio
          "KCFSI",     #	Kansas City Financial Stress Index
          "M2SL",      #	M2 Money Stock
          "NEWORDER",  #	Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft
          "PERMIT",    #	New Private Housing Units Authorized by Building Permits
          "TB3MS",     #	3-Month Treasury Bill: Secondary Market Rate
          "TWEXMMTH",  #  Trade Weighted U.S. Dollar Index: Major Currencies
          "UNRATE",    #	Civilian Unemployment Rate
          "LOANS")     #	Loans and Leases in Bank Credit, All Commercial Banks 


#===============================================================================================
#==    GET DATA                                                                               ==
#===============================================================================================

# Get stock data
sp_5 <- tq_get("^GSPC", get = "stock.prices", from = s.date)

# Get fred daily economic data
econ.d1 <- tq_get(fredd, get = "economic.data", from = s.date)

# Get fred monthly economic data
econ.m1 <- tq_get(fredm, get = "economic.data", from = s.date)

# Spread fred monthly data to column, add spreads
econ.m2 <- spread(econ.m1, symbol, price) %>% 
  fill(ACDGNO, CFNAIDIFF, ISRATIO)

# Get fred weekly economic data
econ.w1 <- tq_get(fredw, get = "economic.data", from = s.date)

# Convert weekly data to monthly frequency
econ.m3 <- econ.w1 %>% 
  rename("IC4WSA" = "price") %>%
  group_by(month = floor_date(date, "month")) %>% 
  summarize(IC4WSA = last(IC4WSA)) %>%
  rename("date" = "month") 

# Get quandl monthy data
econ.m4 <- tq_get(qndlm,get="quandl",from="1985-03-01") %>%
  mutate(price = if_else(is.na(value), index, value), 
         date = floor_date(if_else(is.na(date), month, date),"month")) %>%
  select(symbol, date, price) %>% spread(symbol, price) %>%
  rename(HMI = "NAHB/NWFHMI.1", NEWORD = "ISM/MAN_NEWORDERS.5") 

# Get Shiller download: http://www.econ.yale.edu/~shiller/data.htm
econ.m5 <- read.zoo(file = "Shiller.csv",FUN = as.Date, header = T, sep = ",", format= "%d/%m/%Y", index.column = 1)
econ.m5 <- tk_tbl(econ.m5, rename_index = "date") %>% 
  mutate(date = floor_date(date, "month"))

# Convert daily bond data to monthly frequency  
econ.m6 <- econ.d1 %>% 
  group_by(symbol) %>% fill(price) %>% 
  ungroup() %>% 
  spread(symbol, price) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarize(DTB3 = last(DTB3),
            DGS2 = last(DGS2),
            DGS10 = last(DGS10)) %>%
  rename("date" = "month") 

# Join all data (except stock data)
econ.m  <- full_join(econ.m2, econ.m3, by = "date")
econ.m  <- full_join(econ.m, econ.m4, by = "date")
econ.m  <- full_join(econ.m, econ.m5, by = "date") 
econ.m  <- full_join(econ.m, econ.m6, by = "date")%>% 
  filter(date >= s.date & date <= e.date)


#===============================================================================================
#==    MANIPULATE STOCK DATA                                                                  ==
#===============================================================================================

# Volatility calcs
sp_5vol <- sp_5 %>% mutate(
  rtn = log(close)-lag(log(close)),
  vol_1m = roll_sd(rtn, n = 20, na.rm = TRUE, align = "right", fill = NA) * sqrt(252),
  vol_3m = roll_sd(rtn, n = 60, na.rm = TRUE, align = "right", fill = NA) * sqrt(252)) %>% 
  
  # Roll up to monthly periodicity
  group_by(month = floor_date(date, "month")) %>% 
  summarise(
    sp5_dly_vol_1m = last(vol_1m),
    sp5_dly_vol_3m = last(vol_3m)) %>% 
  select(
    date = month, 
    sp5_dly_vol_1m, 
    sp5_dly_vol_3m) %>% 
  ungroup()

# To monthly
sp_5 <- sp_5 %>% 
  group_by(month = floor_date(date, "month")) %>%
  summarise(low = min(low), close = last(close), volume = sum(volume)) %>%
  rename("date" = "month") %>% 
  tq_mutate(select = close, mutate_fun = periodReturn, period = "monthly", type = "log", col_rename  = "sp5_rtn_1m") %>%
  mutate(sp5_fwd_rtn_1m = lead(sp5_rtn_1m,1)) %>% 
  tq_mutate(select = sp5_rtn_1m, mutate_fun = rollapply, width = lb, FUN = sum, col_rename  = "sp5_rtn_6m") %>%
  tq_mutate(select = low, mutate_fun = rollapply, width = lb, FUN = min, col_rename  = "sp5_min_6m") %>%
  mutate(
    dd_6m = -lag(log(close),n = lb) + log(sp5_min_6m),
    flag = ifelse(sp5_rtn_6m < fr | dd_6m < -pc , 1, 0),
    y1 = lead(flag,lb),
    diff_flag = c(NA, diff(y1))
    )


#===============================================================================================
#==    DATA FOR STOCK RETURN SHADING                                                          ==
#===============================================================================================

sp_5s    <-  sp_5 %>% filter(diff_flag==1) %>% select(date) %>% rename(start = date)
sp_5e    <-  sp_5 %>% filter(diff_flag==-1) %>% select(date) %>% rename(end = date)
short    <-  min(count(sp_5s), count(sp_5e))
sp_shade <-  data.frame(head(sp_5s,short), head(sp_5e,short))


#===============================================================================================
#==    JOIN STOCK & ECON. DATA / SAVE TO DISK                                                 ==
#===============================================================================================

econ_fin_data <- inner_join(econ.m, sp_5, by = "date")
econ_fin_data <- inner_join(econ_fin_data, sp_5vol, by = "date")

saveRDS(econ_fin_data, file = "econ_fin_data.Rda")
saveRDS(sp_shade, file = "sp_shade.Rda")

write_csv(econ_fin_data, path = "econ_fin_data.csv")
write_csv(sp_shade, path = "sp_shade.csv")
