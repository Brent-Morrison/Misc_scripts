library(tidyverse)
library(readxl)
library(readr)

# https://www.esc.vic.gov.au/sites/default/files/documents/SEW_2023%20Price%20Review%20Model%20-%202022-09-09%20-%20SUBMISSION%20FINAL.xlsm
path <- "SEW_2023 Price Review Model - 2022-09-09 - SUBMISSION FINAL.xlsm"

date <- read_xlsx(path, range = "KeyAssumptionsPriceControl_FO!X6:AL6", col_names = FALSE)
cost_of_debt <- read_xlsx(path, range = "KeyAssumptionsPriceControl_FO!X23:AL23", col_names = FALSE)

# ---------------------------------------------------------------------------------------------------------
# Function reading table
# ---------------------------------------------------------------------------------------------------------

xl_to_df <- function(file, sheet=NULL, table=NULL, date_range, cols_range, data_range) {
  dates <- read_xlsx(path, range = paste0(sheet,"!",date_range), col_names = FALSE)
  cols <- read_xlsx(path, range = paste0(sheet,"!",cols_range), col_names = FALSE)
  data <- read_xlsx(path, range = paste0(sheet,"!",data_range), col_names = FALSE)
  
  df <- data.frame(data)
  names(df) <- t(dates)
  rownames(df) <- tolower(gsub(" ", "_", cols$...1))
  
  if (!is.null(sheet)) {
    df$sheet <- tolower(sheet)
  }
  
  if (!is.null(table)) {
    df$table <- tolower(table)
  }
  
  df <- as_tibble(df, rownames = "field") %>% relocate(any_of(c("sheet", "table")), .before = field)
  df
  
}


new_opex <- xl_to_df(path, sheet="Opex_Breakdown", table="water", date_range="X6:AL6", cols_range="D51:D56", data_range="X51:AL56")




# ---------------------------------------------------------------------------------------------------------
# Collecting price, quantity & revenue data, replicating derivation of r
# ---------------------------------------------------------------------------------------------------------

pqr <- read_xlsx(path, range = paste0("RevenuePriceCap_FO","!","E68:AL443"), col_names = TRUE)
q <- as.matrix(pqr %>% filter(PQR == 'Qty') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
q[is.na(q)] <- 0

p <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
p[is.na(p)] <- 0

r <- p* q

tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)  # should sum to 4,571.47



# Get period zero prices
p0 <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2022-23`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
p0[is.na(p0)] <- 0


# Define price delta (pd) & convert to cumulative change for matrix multiplication
pd1 <- c(rep(0, 2), rep(0.0483, 3))            # option 1, zero in first two years
pd2 <- c(rep(0.009, 2), rep(0.025, 3))         # option 2, 0.9% in first year

pdyr <- 2                                                 # Price delta year
pdpar <- c(0.009, 0.025)                                  # Price delta to optimise (parameter)
pdvec <- c(rep(pdpar[1], pdyr - 1), rep(pdpar[2], 5 - pdyr + 1))  # Vector of price changes
pd <- exp(cumsum( log(1 + pdvec) )) - 1
pnew <- p0 %*% (1 + pd)

r <- pnew * q
tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)


# Optimisation / price goal seek
npv_optim_func <- function(theta, pdyr, rev_req, p0, q) {
  
  # https://r-pkgs.org/man.html
  # theta   - a numeric vector of length 3, regulatory rate of return, price delta 1, price delta 2
  # pdyr    - an integer between 1 and 5 representing the year in which price delta 2 comes into effect
  # rev_req - vector of revenue requirement
  
  pdpar        <- theta[-1]
  rrr          <- theta[1]
  
  pdvec        <- c(rep(pdpar[1], pdyr - 1), rep(pdpar[2], 5 - pdyr + 1))
  pd           <- exp(cumsum( log(1 + pdvec) )) - 1
  pnew         <- p0 %*% (1 + pd)
  r            <- pnew * q
  
  tot_r        <- colSums(r) / 1 #1e6
  npv_tot_r    <- sum(tot_r / (1 + rrr) ^ (1:length(tot_r)))* (1 + rrr) ^ 0.5
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))* (1 + rrr) ^ 0.5
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  return(obj)
  
}

p0 <- matrix(rep(10,10), ncol = 1)
q <- matrix(rep(10,10*5), ncol = 5)


# Test function
pdpar <- c(-0.057, 0)
theta <- c(0.0255, pdpar)
npv_optim_func(theta, pdyr=2, rev_req=c(944.25,923.41,921.62,917.46,926.28), p0, q)



# Perform optimisation
optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0.02, 0.01, 0.01),
  
  # Function to be minimized, first argument being vector of parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters | use .Machine$double.eps ??
  lower = c(0.02-.Machine$double.eps, 0.01-1e-8, 0),
  upper = c(0.02+.Machine$double.eps, 0.01+1e-8, 1),
  
  # ... Further arguments to be passed to fn
  pdyr    = 2,
  rev_req = c(944.25,923.41,921.62,917.46,926.28),
  p0      = p0,
  q       = q
  
)


# --------------------------------------------------------------------------------------------------------------------------
# Depreciation
# --------------------------------------------------------------------------------------------------------------------------

path <- "SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm"
capex <- read_xlsx(path, range = "Capex_FO input!Q5:Z14", col_names = FALSE)
c <- as.matrix(capex)
c1 <- c[1,]
c4 <- c[4,]
c7 <- c[7,]
c6 <- c(5,5,5,0,0,0,0,0,0,0)
yr_op1 <- 7
yr_op4 <- 5
yr_op7 <- 1
yr_op6 <- 3
l1 <- 50
l4 <- 50
l7 <- 3
l6 <- 3

# Test function
capex <- c[10,]
yr_op <- 4
life <- 3

reg_depn <- function(capex, yr_op, life) {
  
  ac <- rep(0,length(capex))
  ind <- 1:length(capex)
  
  if (yr_op == 1) {
    ac <- capex
  } else {
    ac[ind <  yr_op] <- 0
    ac[ind == yr_op] <- sum(capex[ind <= yr_op])
    ac[ind >  yr_op] <- capex[ind >  yr_op]
  
  }
  
  cpx.m <- diag(as.vector(ac)) + diag(rep(1e-9, length(ac)))
  yr1.dpn <- cpx.m / life * 0.5
  yr2p.dpn <- cpx.m
  for (i in 1:ncol(yr2p.dpn)) {
    yr2p.dpn[i,][yr2p.dpn[i,] == 0] <- rep(diag(yr2p.dpn)[i] / life, ncol(cpx.m) - 1)
  }
  yr2p.dpn[lower.tri(yr2p.dpn, diag = TRUE)] <- 0
  dpn <- yr1.dpn + yr2p.dpn
  
  # Filter out later years here
  for (i in 1:ncol(dpn)) {
    if (i + life <= ncol(dpn)) {
      dpn[i,][i + life]         <- dpn[i,][i + life] * 0.5
      dpn[i,][ind > (i + life)] <- 0
    }
  }
  
  dpn <- colSums(dpn)
  
  return(round(dpn, 4))
}

reg_depn(capex, yr_op, life)

dpn_mtrix <- t(mapply(FUN = reg_depn, split(c, row(c)), yr_op = c(6,3,7,4,4,5,1,5,9,4), life = c(30,50,50,80,50,50,3,50,50,3)))
dpn_mtrix

colSums(dpn_mtrix)




# ---------------------------------------------------------------------------------------------------------

# Matrix accounting

# ---------------------------------------------------------------------------------------------------------

library(abind)
library(readxl)


# Turn off scientific notation
options(scipen=0) #999


# Matrix dimensions

mon <- 1:6                                                                                     # Number of months
txn <- c("opn","age","inc","csh","wof","exp","cpx","dpn","inta","intp","bor","cls")                          # Transaction types
# opn - opening balances
# age - re-allocate debtors ageing at month start
# inc - posting income
# csh - cash receipts from debtors
# wof - debtors write-off
# exp - posting expenses 
# cpx - capex
# dpn - depreciation
# inta - interest (accrue)
# intp - interest (pay)
# bor - borrow if cash balance is negative
# cls - closing balance

act <- c(100,200,250,260,270,300,3051,3052,3053,375,376,400,410,455,500,510)                       # GL accounts
# 100 - income
# 200 - operating expenses
# 250 - depreciation
# 260 - interest
# 270 - bad debts
# 300 - cash
# 305 - debtors
# 375 - assets / ppe
# 376 - acc depn
# 400 - trade debtors
# 410 - accrued interest
# 455 - debt
# 500 - equity
# 510 - retained earnings
opn_bal <- c(0, 0, 0, 0, 0, 18, 10, 5, 3.7, 1895.8, -58.8, -43.7, 0, -450.9, -307.8, -1071.3)     # Opening balances



#dat1 <- read_xlsx("model_data.xlsx", range = "act-opn_bal!A1:B15", col_names = TRUE)
#act <- unlist(dat1[,"act"], use.names = FALSE)
#opn_bal <- unlist(dat1[,"opn_bal"], use.names = FALSE)
#dat2 <- read_xlsx("model_data.xlsx", range = "txn!B2:G5", col_names = FALSE)


# Transaction balances
# https://stackoverflow.com/questions/19340401/convert-a-row-of-a-data-frame-to-a-simple-vector-in-r
income <- c(10.5, 10.5, 10.5, 11, 11, 11)
expenses <- c(9, 9, 9.5, 17, 17, 11)
capex <- c(5, 3, 4, 5, 3, 4)
depn <- c(2, 2, 1.9, 1.9, 1.8, 1.8)
rcpt1_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
rcpt2_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
rcpt2_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)


# Create matrix and assign names and opening balances
mat <- array(rep(0, length(act) * length(txn) * length(mon)), , dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- act
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon
mat[,,1][,"opn"] <- opn_bal
mat[,,1]
round(colSums(mat[,,1]), 3)


# Transaction loop
for (i in 1:length(mon)) {
  
  # Opening balances & debtors ageing post loop / month 1
  if (i > 1) {
    
    # Opening balances
    mat[,,i][, "opn"] <- mat[,,i-1][, "cls"]
    
    # Apply debtors aging update
    mat[,,i]["3051", "age"] <- -m12 #mat[,,i]["3051", "opn"] - m12
    mat[,,i]["3052", "age"] <- m12  #mat[,,i]["3052", "opn"] + m12
    
    mat[,,i]["3052", "age"] <- -m23 #mat[,,i]["3052", "opn"] - m22
    mat[,,i]["3053", "age"] <- m23  #mat[,,i]["3053", "opn"] + m22
  }
  
  # Post income
  mat[,,i]["100", "inc"] <- -income[i]
  mat[,,i]["3051", "inc"] <- income[i]
  
  # Post cash receipt from aged debtors (TO DO - THIS RESULTS IN NEGATIVE AGED BALANCES)
  rcpt1 <- round(sum(mat[,,i]["3051", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt1
  mat[,,i]["3051", "csh"] <- -rcpt1
  
  rcpt2 <- round(sum(mat[,,i]["3052", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt2
  mat[,,i]["3052", "csh"] <- -rcpt2
  
  rcpt3 <- round(sum(mat[,,i]["3053", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt3
  mat[,,i]["3053", "csh"] <- -rcpt3
  
  # Bad debts WO
  wo <- mat[,,i]["3053", "opn"] + mat[,,i]["3053", "csh"]
  mat[,,i]["3053", "wof"] <- -wo
  mat[,,i]["270", "wof"] <- wo
  
  # Expenses
  mat[,,i]["200", "exp"] <- expenses[i]
  mat[,,i]["300", "exp"] <- -expenses[i]
  
  # Capex
  mat[,,i]["375", "cpx"] <- capex[i] 
  mat[,,i]["300", "cpx"] <- -capex[i]
  
  # Interest (accrue)
  mat[,,i]["260", "inta"] <- -mat[,,i]["455", "opn"] * 0.05 / 12
  mat[,,i]["410", "inta"] <- mat[,,i]["455", "opn"] * 0.05 / 12
  
  # Interest (pay quarterly)
  if (i %in% c(3,6,9,12)) {
    mat[,,i]["410", "intp"] <- -mat[,,i]["410", "opn"]
    mat[,,i]["300", "intp"] <- mat[,,i]["410", "opn"]
    #print(mat[,,i]["410", "opn"])
  }
  
  # Depn
  mat[,,i]["250", "dpn"] <- depn[i]
  mat[,,i]["376", "dpn"] <- -depn[i] 
  
  # Collect data for updating debtors aging (applied after rollover to following period)
  m12 <- sum(mat[,,i]["3051", c("opn","age","csh")])   # DR 3052 / CR 3051
  m23 <- sum(mat[,,i]["3052", c("opn","age","csh")])   # DR 3053 / CR 3052
  
  # Determine if borrowings required
  cash_bal <- sum(mat[,,i]["300",-ncol(mat[,,i])])
  if (cash_bal < 0) {
    mat[,,i]["300", "bor"] <- 20
    mat[,,i]["455", "bor"] <- -20
  }
  
  # Update closing balance
  mat[,,i][, "cls"] <- rowSums(mat[,,i][,-ncol(mat[,,i])])
  
}
mat


# Check balances
round(colSums(mat[,,6]), 3)

# YTD
new_mat <- rowSums(mat, dims = 2)
new_mat[,"opn"] <- mat[,"opn",1]
new_mat[,"cls"] <- mat[,"cls",6]
new_mat
round(colSums(new_mat), 3)

sum(new_mat["3051",c("opn","age")])



vars1 <- c(1,2,3)
vars2 <- c(10,20,30)
vars3 <- c(4,5,6)
mult_one <- function(var1, var2, var3)
{
  var1*var2*var3
}
mapply(mult_one, vars1, vars2, vars3)
