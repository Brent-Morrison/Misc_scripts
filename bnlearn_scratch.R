#===============================================================================================
# Playing with bnlearn
#===============================================================================================

# Libraries

#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("Rgraphviz")
#BiocManager::install("RBGL")

library('tidyverse')
library('DescTools')
library('bnlearn')
library('Rgraphviz')
library('gRain')


# Data
econ_fin_data_raw <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/econ_fin_data.Rda")

df <- econ_fin_data_raw %>% 
  select(
    date
    ,y1
    ,DGS10
    ,sp5_rtn_6m
    ,CAPE
    ,KCFSI
  ) %>% 
  drop_na()

# Discretise the data, bnlearn does not work well with continuous values
df_disc <- df %>% 
  mutate(
    across(!y1, .fns = ~ntile(.x, 3), .names = "fct_{.col}"),
    across(c(starts_with("fct"), y1), as.factor)
  ) 

# Create empty graph structure (example only, unused)
res = empty.graph(nodes = names(df_disc))

# Define probabilistic relationships to encode
dag = model2network("[fct_DGS10][fct_sp5_rtn_6m][fct_CAPE][fct_KCFSI][y1|fct_DGS10:fct_sp5_rtn_6m:fct_CAPE:fct_KCFSI]")

plot(dag)

# Fit to dataset
dag_bn <- bn.fit(dag, data = as.data.frame(df_disc[,c('y1','fct_DGS10','fct_sp5_rtn_6m','fct_CAPE','fct_KCFSI')]))

# Query network
cpquery(dag_bn, event = (y1 == 1), evidence = (fct_KCFSI == 1))

# Parameters for query, setting the tercile ins as evidence
DGS10 <- 1
KCFSI <- df_disc[nrow(df_disc),]$fct_KCFSI


# Query for y1, setting evidence (cpquery estimates the conditional probability of event given 
# evidence using the method specified in the method argument)
cpquery(
  dag_bn
  ,event = (y1 == 1)
  ,evidence = (
    fct_DGS10 == DGS10 & 
    fct_sp5_rtn_6m == 2 & 
    fct_CAPE == 3 & 
    fct_KCFSI == KCFSI
    )
  )

# Function to average multiple query results, 
# required since a different result each time due to sampling error
bn_qry <- function(dag, n) {
  y <- 0
  for (i in 1:n) {
    x <- cpquery(
      fitted = dag
      ,event = (y1 == 1)
      ,evidence = (
        fct_DGS10 == 1 & 
        fct_sp5_rtn_6m == 2 & 
        fct_CAPE == 3 & 
        fct_KCFSI == KCFSI
      )
    )
    y <- y + x
  }
  y/n
}

bn_qry(dag_bn, 100)


# Query via gRain
grain_fit = as.grain(dag_bn)

grain_fit_evdnc <- setEvidence(
  grain_fit, 
  evidence=list(
    fct_DGS10 = '1', 
    fct_sp5_rtn_6m = '2',
    fct_CAPE = '3',
    fct_KCFSI = '2'
    )
  )
pEvidence(grain_fit_evdnc)
querygrain(grain_fit_evdnc)$y1


# Structure learning

# Create blacklist
from <- c('y1','y1','y1','y1')
to <- c('fct_DGS10','fct_sp5_rtn_6m','fct_CAPE','fct_KCFSI')
bl <- data.frame(from, to)

# Learn structure
sl_dag <- hc(as.data.frame(df_disc[,c('y1','fct_DGS10','fct_sp5_rtn_6m','fct_CAPE','fct_KCFSI')]), 
  blacklist = bl, debug = FALSE, restart = 0, perturb = 1, max.iter = 100)

# Plot
plot(sl_dag)

# Extract penalization coefficient
sl_dag$learning$args$k

# Fit to data
sl_fit <- bn.fit(sl_dag, as.data.frame(df_disc[,c('y1','fct_DGS10','fct_sp5_rtn_6m','fct_CAPE','fct_KCFSI')]))

# Conditional probabilities for specific node
bn.fit.barchart(sl_fit$fct_CAPE)

# Query using custom function
bn_qry(sl_fit, 100)
