#===============================================================================================
#==    REQIURED PACKAGES                                                                      ==
#===============================================================================================

library("tidyverse")
library("DescTools")
library("tidyquant")
library("timetk")
library("broom")
library("tibbletime")


#===============================================================================================
#==    DATE FILTER & PARAMETERS                                                               ==
#===============================================================================================

lb = 6                  #  Lookback period
pc = 0.2                #  Percent drawdown for binary market in/out indicator cutoff
fr = -0.025             #  Forward return for binary market in/out indicator cutoff
s.date = as.Date("1945-01-01")
e.date = as.Date("2019-11-01")
quandl_api_key("hpbPcsfGudN3viBgh8th")
qndlm = c("NAHB/NWFHMI.1",
          "ISM/MAN_NEWORDERS.5")
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

#get stock data
sp_5   <-  tq_get("^GSPC",get = "stock.prices",from = s.date)

#get fred monthly economic data
econ.m1 <- tq_get(fredm, get = "economic.data",from = s.date)

#spread fred monthly data to column, add spreads
econ.m2 <- spread(econ.m1, symbol, price) %>% 
  fill(ACDGNO, CFNAIDIFF, ISRATIO)

#get fred weekly economic data
econ.w1 <- tq_get(fredw, get = "economic.data", from = s.date)

#convert weekly data to monthly frequency
econ.m3 <- econ.w1 %>% 
  rename("IC4WSA" = "price") %>%
  group_by(month=floor_date(date, "month")) %>% 
  summarize(IC4WSA = last(IC4WSA)) %>%
  rename("date" = "month") 

#get quandl monthy data
econ.m4 <- tq_get(qndlm,get="quandl",from="1985-03-01") %>%
  mutate(price = if_else(is.na(value), index, value), 
         date = floor_date(if_else(is.na(date), month, date),"month")) %>%
  select(symbol, date, price) %>% spread(symbol, price) %>%
  rename(HMI = "NAHB/NWFHMI.1", NEWORD = "ISM/MAN_NEWORDERS.5") 

#get Shiller download: http://www.econ.yale.edu/~shiller/data.htm
econ.m5 <- read.zoo(file = "Shiller.csv",FUN = as.Date, header = T, sep = ",", format= "%d/%m/%Y", index.column = 1)
econ.m5 <- tk_tbl(econ.m5, rename_index = "date") %>% 
  mutate(date = floor_date(date, "month"))

#join all data (except stock data)
econ.m  <- full_join(econ.m2, econ.m3, by = "date")
econ.m  <- full_join(econ.m, econ.m4, by = "date")
econ.m  <- full_join(econ.m, econ.m5, by = "date") %>% 
  filter(date >= s.date & date <= e.date)


#===============================================================================================
#==    MANIPULATE STOCK DATA                                                                  ==
#===============================================================================================

sp_5 <- sp_5 %>% 
  group_by(month=floor_date(date, "month")) %>%
  summarize(low = min(low), close = last(close), volume = sum(volume)) %>%
  rename("date" = "month") %>% 
  tq_mutate(select = close, mutate_fun = periodReturn, period = "monthly", type = "log", col_rename  = "rtn_m") %>%
  mutate(fwd_rtn_m = lead(rtn_m,1)) %>% 
  tq_mutate(select = rtn_m, mutate_fun = rollapply, width = lb, FUN = sum, col_rename  = "rtn_6m") %>%
  tq_mutate(select = low, mutate_fun = rollapply, width = lb, FUN = min, col_rename  = "min_6m") %>%
  mutate(dd_6m = -lag(log(close),n=lb)+log(min_6m)) %>%
  mutate(flag = ifelse(rtn_6m < fr | dd_6m < -pc , 1, 0)) %>%
  mutate(y1 = lead(flag,lb)) %>%
  mutate(diff_flag = c(NA, diff(y1)))


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

saveRDS(econ_fin_data, file="econ_fin_data.Rda")
saveRDS(sp_shade, file="sp_shade.Rda")
