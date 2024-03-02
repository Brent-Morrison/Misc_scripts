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
sum(tariff_revenue)



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
npv_optim_func <- function(theta, pdyr, npv_required, p0, q) {
  
  # https://r-pkgs.org/man.html
  
  pdpar        <- theta[-1]
  rrr          <- theta[1]
  
  pdvec        <- c(rep(pdpar[1], pdyr - 1), rep(pdpar[2], 5 - pdyr + 1))
  pd           <- exp(cumsum( log(1 + pdvec) )) - 1
  pnew         <- p0 %*% (1 + pd)
  r            <- pnew * q
  
  tot_r        <- colSums(r) / 1e6
  npv_tot_r    <- sum(tot_r / (1 + rrr) ^ (1:length(tot_r)))
  obj          <- (npv_required - npv_tot_r) ^ 2
  
  return(obj)
  
}



# Test function
pdpar <- c(0.009, 0.025)
theta <- c(0.043, pdpar)
npv_optim_func(theta, pdyr=2, npv_required=635.72, p0, q)



# Perform optimisation
optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0.043, 0.09, 0.02),
  
  # Function to be minimized, first argument being vector of parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters | use .Machine$double.eps ??
  lower = c(0.043-.Machine$double.eps, 0.009-1e-8, 0),
  upper = c(0.043+.Machine$double.eps, 0.009+1e-8, 1),
  
  # ... Further arguments to be passed to fn
  pdyr=2,
  npv_required = 635.72,
  p0 = p0,
  q = q
  
)


# --------------------------------------------------------------------------------------------------------------------------
# Depreciation
# --------------------------------------------------------------------------------------------------------------------------

capex <- read_xlsx(path, range = "Capex_FO input!Q44:z48", col_names = FALSE)
c <- as.matrix(capex)
c1 <- c[1,]
c4 <- c[4,]
c5 <- c[5,]
yr_op1 <- 7
yr_op4 <- 5
yr_op5 <- 99
l1 <- 50
l4 <- 50
l5 <- 15

cpx_depn <- function(capex, yr_op, life) {
  
  ac <- rep(0,length(capex))
  ind <- 1:length(capex)
  
  if (yr_op == 99) {
    ac <- capex
  } else {
    ac[ind <  yr_op] <- 0
    ac[ind == yr_op] <- sum(capex[ind <= yr_op])
    ac[ind >  yr_op] <- capex[ind >  yr_op]
  
  }
  
  cpx.m <- diag(as.vector(ac)) + diag(rep(1e-9, length(ac)))
  yr1.dpn <- cpx.m / life * 0.5
  yr2p.dpn <- cpx.m
  for (i in 1:ncol(cpx.m)) {
    yr2p.dpn[i,][yr2p.dpn[i,] == 0] <- rep(diag(yr2p.dpn)[i] / life, ncol(cpx.m) - 1)
    #yr2p.dpn[i,][yr2p.dpn[i,] == 0] <- yr2p.dpn[i,][yr2p.dpn[i,] != 0] / life
  }
  yr2p.dpn[lower.tri(yr2p.dpn, diag = TRUE)] <- 0
  dpn <- yr1.dpn + yr2p.dpn
  dpn <- colSums(dpn)
  
  return(round(dpn, 4))
}

cpx_depn(c1, yr_op1, l1)
cpx_depn(c4, yr_op4, l4)
cpx_depn(c5, yr_op5, l5)

t(mapply(FUN = cpx_depn, split(c, row(c)), yr_op = c(7,4,4,5,99), life = c(50,80,50,50,15)))

