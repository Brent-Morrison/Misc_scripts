# --------------------------------------------------------------------------------------------------------------------------
#
# Use global data to build time series model of mortality rates before the covid pandemic.  
# Compare the predictions of this model during the pandemic to actual data in order to determine
# if there is a difference in mortality rates.  Has there been excess mortality?
#
# DATA
# https://www.who.int/data/data-collection-tools/who-mortality-database
#
# REFERENCES
# https://research.google/pubs/pub41854/
# https://google.github.io/CausalImpact/CausalImpact.html
# https://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
# Use time series plots per this: http://motioninsocial.com/tufte/
#
# --------------------------------------------------------------------------------------------------------------------------







# --------------------------------------------------------------------------------------------------------------------------
#
# Bayesian network for return forecasting
#
# --------------------------------------------------------------------------------------------------------------------------

# Libraries
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("Rgraphviz")
#BiocManager::install("RBGL")

library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)
library(lubridate)
library(DBI)
library(RPostgres)
library(DescTools)
library(bnlearn)
library(Rgraphviz)


# Source functions
source("C:/Users/brent/Documents/VS_Code/postgres/postgres/return_attributes.R")


# Connect to db
con <- stock_master_connect()


# Read data
sql1 <- "select * from access_layer.return_attributes"
qry1 <- dbSendQuery(conn = con, statement = sql1) 
return_attributes <- dbFetch(qry1)
return_attributes <- arrange(return_attributes, symbol, date_stamp)


# Define columns
other_cols <- c('symbol', 'date_stamp', 'close', 'fwd_rtn_1m')
targ_cols <- 'fwd_rtn_1m_qntl'
pred_cols <- c('rtn_ari_3m_dcl', 'rtn_ari_12m_dcl', 'rtn_ari_6m_sctr_dcl')


# Filter date range, fwd return, filter attributes and convert to factor
bn_data1 <- return_attributes %>% 
  filter(date_stamp > as.Date('2011-12-31'), date_stamp <= as.Date('2020-12-31')) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  group_by(date_stamp) %>% 
  mutate(fwd_rtn_1m_qntl = ntile(fwd_rtn_1m, 5)) %>% 
  ungroup() %>% 
  as.data.frame()

bn_data1 <- bn_data1[c(other_cols,targ_cols, pred_cols)]


# Hartemink discretisation
disc_data1 <- return_attributes %>% 
  filter(date_stamp > as.Date('2011-12-31'), date_stamp <= as.Date('2020-12-31')) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>% 
  select(fwd_rtn_1m, rtn_ari_1m, rtn_ari_12m, amihud_60d) %>% 
  drop_na()

disc = discretize(disc_data1, method = "hartemink", breaks = 3, ibreaks = 60, idisc = "quantile")


# Function to convert decile to quintile
dcl_to_qtl <- function(var) {
  case_when(
    var <= 2 ~ as.integer(1),
    var <= 4 ~ as.integer(2),
    var <= 6 ~ as.integer(3),
    var <= 8 ~ as.integer(4),
    TRUE     ~ as.integer(5)
  )
}


# Function to convert decile to tercile
dcl_to_qtl <- function(var) {
  case_when(
    var <= 3 ~ as.integer(1),
    var <= 7 ~ as.integer(2),
    TRUE     ~ as.integer(3)
  )
}


# Data for bn
bn_data2 <- bn_data1 %>% 
  select(-symbol, -date_stamp, -close, -fwd_rtn_1m) %>% 
  mutate(
    across(!fwd_rtn_1m_qntl, dcl_to_qtl),
    across(everything(), as.factor)
    ) %>% 
  drop_na() %>% 
  as.data.frame()


# Define network (no structure learning)


# Create blacklist
bl <- data.frame(from = rep(targ_cols,3), to = pred_cols)


# Learn structure
model_string <- paste0("[",pred_cols[1],"][",pred_cols[2],"][",pred_cols[3],"][",targ_cols[1],"|",pred_cols[1],":",pred_cols[2],":",pred_cols[3],"]")
dag1 <- model2network(model_string)
#dag1 <- model2network("[rtn_ari_1m_dcl][rtn_ari_12m_dcl][rtn_ari_6m_sctr_dcl][fwd_rtn_1m_qntl|rtn_ari_1m_dcl:rtn_ari_12m_dcl:rtn_ari_6m_sctr_dcl]")

# Define network (with structure learning)
#dag1 <- hc(bn_data2, blacklist = bl, debug = FALSE, restart = 0, perturb = 1, max.iter = 100)

# Plot
plot(dag1)

# Extract penalization coefficient
#dag1$learning$args$k

# Fit to data
dag1_fit <- bn.fit(dag1, bn_data2)

# Conditional probabilities
#bn.fit.barchart(dag1_fit$fwd_rtn_1m_qntl)


predict_data <- data.frame(expand.grid(
  rtn_ari_3m_dcl = c(1,2,3,4,5),
  rtn_ari_12m_dcl = c(1,2,3,4,5),
  rtn_ari_6m_sctr_dcl = c(1,2,3,4,5))
  )%>% 
  mutate(across(everything(), as.factor))


# Query for fwd_rtn_1m_qntl, setting evidence - method 1
# https://stackoverflow.com/questions/50251413/r-package-bnlearn-cpquery-vs-predict-different-results 
fcast <- predict(
  object = dag1_fit,
  node = "fwd_rtn_1m_qntl",
  data = predict_data,
  method = "bayes-lw",
  prob = TRUE,
  n = 1e6)

fcast_probs <- bind_cols(
  predict_data,
  as.data.frame(t(attr(fcast, 'prob', exact = TRUE)[,1:length(fcast)]))
  )


# Query for fwd_rtn_1m_qntl, setting evidence- method 2
cpquery(
  dag1_fit
  ,event = (fwd_rtn_1m_qntl == 3)
  ,evidence = (
    rtn_ari_1m_dcl == 1 & 
    rtn_ari_12m_dcl == 1 & 
    amihud_60d_dcl == 1
    )
  #,method = "lw"
)



# Function to average multiple query results, 
# required since a different result each time due to sampling nature
bn_qry <- function(dag, n) {
  y <- 0
  for (i in 1:n) {
    x <- cpquery(
      fitted = dag
      ,event = (fwd_rtn_1m_qntl == 1)
      ,evidence = (
        rtn_ari_3m_dcl == 1 & 
        rtn_ari_12m_dcl == 1 & 
        rtn_ari_6m_sctr_dcl == 1
      )
    )
    y <- y + x
  }
  y/n
}

# Query using custom function
bn_qry(dag1_fit, 1000)


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















# --------------------------------------------------------------------------------------------------------------------------
#
# Splines in bayesian multi-level models
#
# --------------------------------------------------------------------------------------------------------------------------

library(brms)
library(dplyr)
library(tidyr)
library(romerb)

data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

popn <- fundamental_raw %>% 
  group_by(sector, symbol) %>% 
  summarise(tot_vol = sum(volume)) %>% 
  slice_max(tot_vol, n = 5)

data <- fundamental_raw %>% 
  filter(
    symbol %in% popn$symbol,
    date_stamp > as.Date('2018-12-31')
    ) %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn_1m = lead((adjusted_close-lag(adjusted_close))/lag(adjusted_close))) %>% 
  ungroup() %>%
  mutate(
    log_mkt_cap = log(mkt_cap),
    log_assets = log(total_assets),
    log_equity_cln = log(-total_equity_cln),
    roe = -roe,
    sector = as.factor(sector),
    industry = as.factor(industry)
  ) %>% 
  select(
    symbol, date_stamp, sector, industry, fwd_rtn_1m, rtn_ari_12m, vol_ari_60d
  ) %>% 
  drop_na()

m1 <- brm(fwd_rtn_1m ~ t2(rtn_ari_12m, vol_ari_60d) + (1 + symbol), data = data)
#file:///C:/Users/brent/Downloads/Introduction_BMLMs_brms.pdf (VARYING SLOPE)
summary(m1)
cf <- coef(m1) # co-efficients
ms <- conditional_smooths(m1)
plot(ms, stype = "contour")
plot(ms, stype = "raster")

















# --------------------------------------------------------------------------------------------------------------------------
#
# 1. Time series of regression slopes
#
# --------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(slider)
library(broom)

# For Theil Sen regression line in plot.  See - https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}



# Calculate regression slope for each month and each predictor (coefficients for OLS and Thiel Sen regression)
# Data prep
df_train <- read_csv("C:/Users/brent/Documents/R/Misc_scripts/experiment_03/02-data_01-training.csv")

  
# Fit regression models and retrieve co-efficients

# Sliding window size for average betas
n <- 12

dat <- df_train %>% 
  group_by(date_stamp) %>% 
  mutate(
    fwd_rtn     = as.vector(scale(fwd_rtn)),
    rtn_ari_3m  = as.vector(scale(rtn_ari_3m)),
    rtn_ari_12m = as.vector(scale(rtn_ari_12m))
  ) %>% 
  ungroup() %>% 
  select(date_stamp, symbol, fwd_rtn, rtn_ari_3m, rtn_ari_12m)

mdl_fit <- dat %>% 
  nest_by(date_stamp) %>% 
  mutate(model = list(lm(fwd_rtn ~ rtn_ari_3m + rtn_ari_12m, data = data))) %>% 
  summarise(tidy(model)) %>% 
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>% 
  ungroup() %>% 
  rename_with(~ gsub(pattern = "[()]", replacement = "", x = .x)) %>% 
  rename_with(~ tolower(.x)) %>% 
  mutate(
    across(starts_with("estimate"), 
    ~ slide_dbl(.x = .x, .f = mean, .before = n-1, .complete = TRUE),
    .names = "{col}^{as.character(n)}MA")
    )

# Extract averaged coefficients
coefs <- t(as.matrix(mdl_fit[mdl_fit$date_stamp == max(mdl_fit$date_stamp), grepl("\\^", names(mdl_fit))]))

# Out of sample data
df_test <- read_csv("C:/Users/brent/Documents/R/Misc_scripts/experiment_03/02-data_02-scoring.csv")

model_mat <- as.matrix(cbind(intercept = rep(1,nrow(df_test)), df_test[, c('rtn_ari_3m', 'rtn_ari_12m')]))
model_mat %*% coefs





# Plot 
ggplot(data = mdl_fit) +
  geom_hline(yintercept = 0, color = 'grey', linewidth = 0.25) + 
  geom_line(aes(x = date_stamp, y = estimate_rtn_ari_12m, color = 'blue'), linetype = 'twodash') +
  geom_line(aes(x = date_stamp, y = estimate_rtn_ari_3m, color = 'grey'), linetype = 'dotdash') +
  #facet_wrap(vars(indicator), scales = 'free') +
  labs(
    title = 'Slope of monthly regression of forward returns on various indicators',
    subtitle = 'OLS regression'
  ) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
  ) 



df <- df_train %>% 
  filter(year(date_stamp) == 2022) %>% 
  select(date_stamp, symbol, rtn_ari_1m) %>% 
  pivot_wider(names_from = symbol, values_from = rtn_ari_1m) 

mat = data.matrix(select(df, -date_stamp))
na_ind <- which(is.na(mat), arr.ind = TRUE)
mat[na_ind] <- rowMeans(mat, na.rm = TRUE)[na_ind[,1]]
C <- cor(mat, use = 'pairwise.complete.obs')

stocks <- c('AA','AAPL','AAWW','ACHC','ET')
# If the input to 'combn' is sorted then the upper triangle and diagonal of the resultant matrix represents each combination
combo <- t(combn(x = sort(stocks), m = 2))
S <- C[rownames(C)%in%combo[,1], colnames(C)%in%combo[,2]]
mean(S[upper.tri(S, diag = TRUE)])


ij <- which(S == 1, arr.ind = TRUE)
combo[2,]
C[combo[,1][1],combo[1,][2]]
n <- as.list(1:nrow(C))
names(n) <- rownames(C)













# --------------------------------------------------------------------------------------------------------------------------
#
# 2. Water Utility characteristics
#
# --------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(broom)
library(readxl)
library(ggiraphExtra)
library(gridExtra)
library(DescTools)


# NPR data (http://www.bom.gov.au/water/npr/npr_2022-23.shtml)
# http://www.bom.gov.au/water/npr/docs/2022-23/Urban_NPR_The_complete_dataset_2022-23.xlsx
path <- "Urban_NPR_The_complete_dataset_2022-23.xlsx"
df_npr_raw <- read_xlsx(path, range = "Complete dataset!A2:AE13549", col_names = TRUE)

# Format column names
names(df_npr) <- gsub("-", "_", names(df_npr))
names(df_npr) <- gsub(" ", "_", names(df_npr))
names(df_npr) <- gsub("\\(", "", names(df_npr))
names(df_npr) <- gsub("\\)", "", names(df_npr))
names(df_npr) <- tolower(gsub("/", "_", names(df_npr)))

# CSV copied from file reference above, retrieved 20250111
#df_npr_raw <- read.csv("Urban_NPR_The_complete_dataset_2022-23.csv")
df_npr_raw <- read.csv("https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/refs/heads/master/Urban_NPR_The_complete_dataset_2022-23.csv")
df_npr <- df_npr_raw
npr_names <- c(
  "area","utility_group","utility","indicator_category","indicator_sub_category","indicator_code",
  "derived","indicator_name","unit","footnote_2022_2023","2022_2023","2021_2022","2020_2021","2019_2020",
  "2018_2019","2017_2018","2016_2017","2015_2016","2014_2015","2013_2014","2012_2013","2011_2012",
  "2010_2011","2009_2010","2008_2009","2007_2008","2006_2007","2005_2006","2004_2005","2003_2004","2002_2003")
names(df_npr) <- npr_names



# Long format
df_npr <- df_npr %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "fin_year",
    values_to = "value"
  ) %>%
  mutate(
    value = if_else(value == "Not applicable", "0", value),
    value = as.numeric(value)
  )

# Data frame for size filter
df_filt <- df_npr %>% 
  filter(
    indicator_name == "Total number of connected properties: water supply",
    value > 30
    ) %>% 
  distinct(utility)

# Custom indicators
df_cust_ind <- df_npr %>%
  filter(indicator_code %in% c("A1","A2","A3","A4","A5","A6","C1","C2","C3","C4","C5","C6","C7","C8","A10","A11")) %>%
  select(utility, indicator_code, fin_year, value) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  mutate(
    treat_plant_km_mains = A1 / A2,
    waste_plant_km_mains = A4 / A5,
    treat_plant_per_popn = A1 / C1,
    waste_plant_per_popn = A4 / C1,
    perc_non_resi_water  = C3 / C4,
    perc_non_resi_waste  = C7 / C8,
    perc_waste           = C8 / (C4 + C8),
    prop_per_km_water    = A3,
    prop_per_km_sewer    = A6
  )


# Data for cluster analysis
df_clust <- df_cust_ind %>%
  filter(
    fin_year == "2022_2023",
    utility %in% df_filt$utility
    ) %>%
  select(utility, treat_plant_km_mains:prop_per_km_sewer)
df_clust <- df_clust[complete.cases(df_clust[, -1]), ]


df_clust <- df_clust %>%
  mutate(across(-utility, ~ Winsorize(., val = quantile(., probs = c(0.05, 0.95), na.rm = TRUE)))) %>%
  mutate(across(-utility, ~ scale(.)))



# Cluster
# https://brentmorrison.netlify.app/post/ifrs9-disclosures-part-2/
kclusts <- tibble(k = 4:(nrow(df_clust)-10)) %>%
  mutate(
    kclust = map(k, ~kmeans(df_clust[, -1], .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, df_clust[, -1])
  )



clusterings <- kclusts %>%
  unnest(glanced, .drop = TRUE)

assignments <- kclusts %>%
  unnest(augmented)


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  ylab("Total within-cluster sum of squares") +
  xlab("k") +
  labs(title = "Water Utility clustering analysis",
       caption = "Source:\nhttp://www.bom.gov.au/water/npr/npr_2022-23.shtml\nUrban_NPR_The_complete_dataset_2022-23.xlsx") +
  theme_grey() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, color = "darkslategrey"),
        plot.caption = element_text(size = 9, color = "grey55"))


# Perform clustering desired number of k
k <- 6
set.seed(123)
kclust16    <- kmeans(df_clust[, -1], centers = k, nstart = 25)
kclust16_td <- tidy(kclust16)

# Cluster membership df
df_clust_ms <- data.frame(cluster = kclust16$cluster, utility = df_clust$utility)


# Create data frame assigning cluster names
n <- length(df_clust[, -1])
names_kv <- setNames(as.list(letters[1:n]), names(df_clust[, -1]))
names_lk <- data.frame(alias = letters[1:n], name = names(df_clust[, -1]))

kclust16_nm <- kclust16_td %>%
  select(-size, -withinss) %>%
  pivot_longer(cols = -cluster, names_to = "attribute", values_to = "clust_avg") %>%
  #gather(attribute, clust.avg, -cluster) %>%
  left_join(names_lk, by = join_by(attribute == name)) %>% 
  group_by(cluster) %>%
  mutate(clust.rank = rank(clust_avg)) %>%
  summarise(
    first       = alias[which(clust.rank == 1)],
    second      = alias[which(clust.rank == 2)],
    second_last = alias[which(clust.rank == n-1)],
    last        = alias[which(clust.rank == n)]
    ) %>%
  mutate(
    clust.name = paste(last, second_last, second, first, sep = "-"),
    clust.name = paste(cluster, clust.name, sep = "-")
    ) %>%
  left_join(kclust16_td, by = "cluster") %>% 
  mutate(cluster = as.numeric(cluster))


# Radar plot of clusters
# https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html#making-inset-plots
# https://patchwork.data-imaginist.com/articles/guides/layout.html
# https://stackoverflow.com/questions/60349028/how-to-add-a-table-to-a-ggplot
names(kclust16_nm)[7:(length(kclust16_nm) - 2)] <- names_lk$alias

c <- kclust16_nm %>% 
  select(-size, -withinss, -cluster, -first, -second, -second_last, -last) %>%
  ggRadar(aes(group = clust.name), rescale = FALSE, legend.position = "none",
          size = 2, interactive = FALSE, use.label = TRUE, scales = "fixed") +
  facet_wrap(vars(clust.name), ncol = 3) +
  scale_y_discrete(breaks = NULL) +
  labs(title    = "Water Utility clustering analysis",
       subtitle = "Facet title represents two highest and lowest cluster centres",
       caption  = "Source:\nhttp://www.bom.gov.au/water/npr/npr_2022-23.shtml\nUrban_NPR_The_complete_dataset_2022-23.xlsx") +
  theme_grey() +
  theme(strip.text = element_text(size = 10),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "darkslategrey"),
        plot.caption = element_text(size = 9, color = "grey55"))

# Key
n <- tableGrob(names_lk)

# Utilities closest to cluster centre
kdist <- df_clust %>% 
  full_join(df_clust_ms, by = join_by(utility)) %>% 
  left_join(kclust16_nm, by = join_by(cluster)) %>% 
  mutate(kdist = sqrt(rowSums((across(2:10) - across(a:i))^2))) %>% 
  group_by(cluster) %>% 
  slice_min(order_by = kdist, n = 2) %>% 
  select(cluster, size, utility)

d <- tableGrob(kdist)


# https://patchwork.data-imaginist.com/articles/guides/assembly.html
grid.arrange(c, n, d, ncol=3, widths=c(3,1,1))















# --------------------------------------------------------------------------------------------------------------------------
#
# 3. Quintile returns to each factor
#
# --------------------------------------------------------------------------------------------------------------------------

fctr_data <- prices %>% 
  filter(between(fwd_rtn_1m, -2, 2)) %>% 
  select(date_stamp, fwd_rtn_1m, qntl_amihud_1m:qntl_perc_pos_rtn_12m) %>% 
  pivot_longer(
    cols = qntl_amihud_1m:qntl_perc_pos_rtn_12m,
    values_to = 'quintile',
    names_to = 'indicator'
  ) %>% 
  filter(quintile %in% c(1,5)) %>% 
  group_by(date_stamp, indicator, quintile) %>% 
  summarise(fwd_rtn_1m = mean(fwd_rtn_1m, na.rm = TRUE)) %>% 
  pivot_wider(
    id_cols = date_stamp:indicator, 
    names_from = quintile, 
    values_from = fwd_rtn_1m
  ) %>% 
  mutate(
    factor_rtn = `5` - `1`, 
    indicator = gsub('qntl_','',indicator)
  ) %>% 
  drop_na()

# df for labels
fctr_labs <- fctr_data %>% 
  mutate(pos = if_else(factor_rtn > 0, 1, 0)) %>% 
  group_by(indicator) %>% 
  summarise(average = mean(factor_rtn), med = median(factor_rtn), n_pos = sum(pos)/n()) %>% 
  ungroup()

# Plot of factor returns
# Note how plot uses "x = as.Date('2018-02-28')" to specify the x axis location
ggplot(data = fctr_data, aes(x = date_stamp, y = factor_rtn))+
  geom_hline(yintercept = 0, color = 'grey', size = 0.25) + 
  geom_line() +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.3, label = paste0("Average ", percent(round(average, 4)), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.25, label = paste0("Median ", percent(round(med, 4)), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  geom_text(data = fctr_labs, aes(x = as.Date('2018-02-28'), y = 0.2, label = paste0("Perc. positive ", percent(n_pos, 1), sep = " "), colour = NULL, fill = NULL), size = 2.5, hjust = 0) +
  
  facet_wrap(vars(indicator), scales = 'fixed') +
  labs(
    title = 'Factor forward returns on various indicators',
    subtitle = 'Quintile returns',
    x = '', y = ''
  ) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
  ) + 
  custom_theme1

xxx<-prices %>% filter(is.na(qntl_vol_ari_60d))





# --------------------------------------------------------------------------------------------------------------------------
#
# 99. Scratch
#
# --------------------------------------------------------------------------------------------------------------------------


# Prove TS regr does not work with scaled variable
# See 56:30 of https://www.youtube.com/watch?v=yfXpjmWgyXU&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=17
n <- 100
sigma1 <- 0.2
sigma2 <- 0.2
rho = 0.75
z1 <- rnorm(n)
z2 <- rnorm(n)
a1 <- z1 * sigma1
a2 <- (rho * z1 + sqrt(1 - rho^2) + z2) * sigma2
plot(a1,a2)
lm(a1 ~ a2)
mblm(a1 ~ a2)
cor(a1, a2)

a1_scaled <- scale(a1)
a2_scaled <- scale(a2)
lm(a1_scaled ~ a2_scaled)
mblm(a1_scaled ~ a2_scaled)
cor(a1_scaled, a2_scaled)



# --------------------------------------------------------------------------------------------------------------------------
#
# Linear regression and neural networks (various methods)
#
# 1. Algorithms
#    a) Linear regression
#    b) Neural network (2 layers)
#
# 2. Methods
#    a) OLS (linear regression only - from scratch)
#    b) Gradient descent
#    c) Bayesian (grid approximation)
#    d) Bayesian (MCMC - from scratch)
#    e) Bayesian (variational inference)
#
# 3. Tools
#    a) R (base)
#    b) R (Rethinking)
#    c) R (BRMS)
#    d) Python (Numpy)
#    e) Python (PyMC3)
#    f) Python (Pytorch)
#    g) Python (Jax)
#    h) Julia
#
# --------------------------------------------------------------------------------------------------------------------------

# 1a2a3d https://cmdlinetips.com/2020/03/linear-regression-using-matrix-multiplication-in-python-using-numpy/
# 1a2a3a https://rpubs.com/alfernz/659016










# Mahalanobis distance / GP / Surrogates -----------------------------------------------------------------------------------
# https://bookdown.org/rbg/surrogates/chap5.html
# https://www.statestreet.com/content/dam/statestreet/documents/ss_associates/A%20New%20Index%20of%20the%20Business%20Cycle_SSRN-id3521300.pdf
# https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/pdf/10.1002/cem.2692
# https://blogs.sas.com/content/iml/2012/02/15/what-is-mahalanobis-distance.html
# https://blogs.sas.com/content/iml/2012/02/08/use-the-cholesky-transformation-to-correlate-and-uncorrelate-variables.html
#
# --------------------------------------------------------------------------------------------------------------------------

library(MASS)
library(plgp)
library(romerb)
data("stock_data")
fundamental_raw <- stock_data
rm(stock_data)

#df <- fundamental_raw[fundamental_raw$symbol %in% c('AAPL','ADBE','GS','JPM','NFG','XOM'), ]
#df <- df[df$date_stamp == as.Date('2021-06-30'), c('asset_growth','roa','roe','leverage')]

df <- fundamental_raw[fundamental_raw$sector == 7, ]
df$log_mkt_cap <- log(df$mkt_cap)
df <- df[df$date_stamp == as.Date('2021-06-30'), c('log_mkt_cap','log_pb','roe','leverage')]


# Distance of leverage
#X <- df1[order(df1$log_mkt_cap, df1$log_pb, df1$roe, df1$leverage), ]
X <- sort(df$log_mkt_cap)
#X <- matrix(seq(0, 10, length = 100), ncol = 1)
n <- length(X)
#n <- nrow(X)
D <- dist(X, diag = T, upper = T)
D <- D**2
D <- as.matrix(D)
eps <- sqrt(.Machine$double.eps)
sigma <- exp(-D) + diag(eps, n)
Y <- MASS::mvrnorm(n = 3, mu = rep(0, n), Sigma = sigma)

# Plot
matplot(X, t(Y), type = 'l')

# Compare to plgp::distance()
D1 <- plgp::distance(X)
sum(D - D1) == 0


# Jitter
eps <- sqrt(.Machine$double.eps)
sigma <- exp(-D) + diag(eps, n)
sigma1 <- exp(-D1) + diag(eps, n)
sigma
sigma1

Y <- MASS::mvrnorm(n = 3, mu = rep(0, n), Sigma = sigma)

plot(X, Y, type = 'l')
matplot(X, t(Y), type = "l", ylab = "Y")

# Use mahalanobis distance for covariance matrix
x <- as.matrix(df)
stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)

D2 <- mahalanobis(x, colMeans(x), Sx)

plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)

qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')



# --------------------------------------------------------------------------------------------------------------------------

library('data.table')

# https://stats.stackexchange.com/questions/65705/pairwise-mahalanobis-distances
mahal <- function(x, h=solve(var(x))) {
  u <- apply(x, 1, function(y) y %*% h %*% y)
  d <- outer(u, u, `+`) - 2 * x %*% h %*% t(x)
  #d[lower.tri(d)]
}

df_raw <- fread('stock_data.csv')

df_mtrx <- as.matrix(df_raw[1:8 ,4:8])
nrow(df_mtrx)
ncol(df_mtrx)

dist_mtrx <- mahal(df_mtrx)
dist_mtrx



x0 <- MASS::mvrnorm(10 ,1:5, diag(c(seq(1,1/2,l=5)),5))
dM = as.dist(apply(x0, 1, function(i) mahalanobis(x0, i, cov = cov(x0))))
dM = as.dist(apply(df_mtrx, 1, function(i) mahalanobis(df_mtrx, i, cov = cov(df_mtrx))))
dM




#------------
devtools::install_github("aashen12/TBASS")
library('TBASS')

d <- matrix(runif(2000, min = -1, max = 1), ncol = 20)
D <- expand.grid(sort(d[,1]), sort(d[,2]))
y <- D[,1] + sin(D[,2])
#y <- d[,1] + sin(d[,2]) + log(abs(d[,3])) + d[,4] ^ 2 + d[,5] * d[,6] +
#  I(d[,7] * d[,8] * d[,9] < 0) + I(d[,10] > 0) + d[,11] * I(d[,11] > 0) + sqrt(abs(d[,12])) +
#  cos(d[,13]) + 2 * d[,14] + abs(d[,15]) + I(d[,16] < -1) + d[,17] * I(d[,17] < -1) - 2 * d[,18] -
#  d[,19] * d[,20]

persp(sort(d[,1]), sort(d[,2]), matrix(y, ncol=100), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")

plot(d[,1], y, type="p")
plot(d[,2], y, type="p")



x1 <- seq(min(d[,1]), max(d[,1]), length = length(y))
y1 <- seq(min(d[,2]), max(d[,2]), length = length(y))

persp(x1 , y1, z = y)

# ----
library(plgp)
library(mvtnorm)
nx <- 20
x <- seq(0, 2, length=nx)
X <- expand.grid(x, x) # 400
D <- distance(X)
eps <- sqrt(.Machine$double.eps)
Sigma <- exp(-D) + diag(eps, nrow(X))
Y <- rmvnorm(2, sigma=Sigma)  # 400 by 400
persp(x, x, matrix(Y[1,], ncol=nx), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")
persp(x, x, matrix(Y[2,], ncol=nx), theta=-30, phi=30, xlab="x1", ylab="x2", zlab="y")





# --------------------------------------------------------------------------------------------------------------------------
#
# Bendigo census
# https://www.abs.gov.au/websitedbs/D3310114.nsf/home/Digital+Boundaries
# https://dbr.abs.gov.au/
# https://geopandas.org/en/stable/docs/user_guide/io.html
# https://geopandas.org/en/stable/gallery/polygon_plotting_with_folium.html
#
# --------------------------------------------------------------------------------------------------------------------------

library('data.table')

files <- list(
  'Bendigo' = 'SA2_202011018',
  'California Gully - Eaglehawk' = 'SA2_202011019',
  'East Bendigo - Kennington' = 'SA2_202011020',
  'Flora Hill - Spring Gully' = 'SA2_202011021',
  'Kangaroo Flat - Golden Square' = 'SA2_202011022',
  'Maiden Gully' = 'SA2_202011023',
  'Strathfieldsaye' = 'SA2_202011024',
  'White Hills - Ascot' = 'SA2_202011025'
  )

df_to_list <- list()
counter <- 1
for (i in files){
  url <- paste0('https://dbr.abs.gov.au/json/csv/',i,'.csv')
  df <- fread(url, header = TRUE)
  df$location <- names(files)[counter]
  df$SA2 <- i
  df_to_list[[i]] <- df
  counter <- counter + 1
}

data <- rbindlist(df_to_list)

saveRDS(data, file = 'bendigo_census.rda')

dat <- readRDS('bendigo_census.rda')








# --------------------------------------------------------------------------------------------------------------------------
#
# Simulate data for varying coefficients models
# https://goldinlocks.github.io/ARCH_GARCH-Volatility-Forecasting/
# https://www.apress.com/gp/blog/all-blog-posts/simulating-autoregressive-and-moving-average-time-series-in-r/16711624
#
# --------------------------------------------------------------------------------------------------------------------------


# Simulate stock prices from a GARCH process
ga <- function(n = 750, mean = -0.001, a = 0.005, b = 0.001, plot = F) {
  
  # a  variance re new information that was not available when the previous forecast was made 
  # b  variance re the forecast that was made in the previous period
  
  LRsig <- mean^2
  Y <- 1 - a - b
  o <- Y * LRsig              # constant variance that corresponds to the long run average (mean daily return squared)
  
  e <- rnorm(n+2, mean=mean)  # returns / noise
  R <- numeric(n)             # returns
  V <- numeric(n)             # vol

  
  for (i in 3:n) {
    V[i] = o + a * R[i-1]^2 + b * V[i-1]
    R[i] = sqrt(V[i]) * e[i]  
  }

  if (plot) plot(cumprod(1 + R), type="l")
  return(R)
}


# Combine return regimes to simulate market data
R <- c(ga(mean = 0.02), ga(mean = -0.02), ga(mean = 0.01))
Rcum <- c(1, cumprod(1 + R))
plot(Rcum, type="l")


# Retrieve real market data for dates
library(DBI)
library(RPostgres)

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

# Retrieve S&P500 data
sp5_send <- dbSendQuery(conn = con, statement = "select * from access_layer.daily_sp500_ts_vw") 
df_sp500 <- dbFetch(sp5_send)
dbClearResult(sp5_send)
dbDisconnect(conn = con)


# Rolling 12 month returns
library(zoo)
log_rtn <- diff(log(Rcum))
width <- 250 # width for rolling returns
rtn_log_12m <- rollapply(log_rtn, width = width, FUN = sum, align = "right", fill = NA)


mkt <- data.frame(
  date_stamp  = df_sp500$date_stamp[(nrow(df_sp500)-length(R) + 1):nrow(df_sp500)],
  mkt_price   = Rcum[2:length(Rcum)],
  rtn_log_12m = rtn_log_12m
)


# Return on equity characteristic for individual stocks
ROE <- c(0.125, 0.15, 0.175, 0.2)
ROE1 <- rep(ROE[1], length(R))
ROE2 <- rep(ROE[2], length(R))
ROE3 <- rep(ROE[3], length(R))
ROE4 <- rep(ROE[4], length(R))
ROE5 <- rep(ROE, each = ceiling(length(R)/4))
ROE5 <- ROE5[1:length(R)]
ROE6 <- rev(ROE5)
ROE6 <- ROE6[1:length(R)]

ROEs <- list(ROE1, ROE2, ROE3, ROE4, ROE5, ROE6)
stock_names <- c('ROE1', 'ROE2', 'ROE3', 'ROE4', 'ROE5', 'ROE6')
n_stocks <- length(ROEs)

lead <- 20 # for forward returns
df <- list()
counter <- 0
for (r in ROEs) {
  
  counter    <- counter + 1
  date_stamp <- mkt$date_stamp
  roe        <- r
  stock      <- rep(stock_names[[counter]], length(roe))
  beta_roe1  <- ifelse(rtn_log_12m < 0 & roe == 0.125, 0.002,
                ifelse(rtn_log_12m > 0 & roe == 0.125, 0.003,
                ifelse(rtn_log_12m < 0 & roe == 0.150, 0.004,
                ifelse(rtn_log_12m > 0 & roe == 0.150, 0.005,
                ifelse(rtn_log_12m < 0 & roe >= 0.175, 0.006,
                ifelse(rtn_log_12m > 0 & roe >= 0.175, 0.007, 0.001))))))
  beta_roe2  <- rtn_log_12m * roe
  rtn_log_1d <- beta_roe1 * roe + rnorm(length(roe), sd = 1e-06) 
  rtn_lead   <- as.vector(lag(zoo(rtn_log_1d), k = -lead, na.pad = T))
  rtn_lead   <- replace(rtn_lead, is.na(rtn_lead), 0)
  close      <- cumprod(1 + rtn_lead)
  
  df[[counter]] <- data.frame(
    date_stamp     = date_stamp,
    symbol         = stock,
    roe            = roe,
    beta_roe       = beta_roe2,
    rtn_log_1d     = rtn_log_1d,
    rtn_lead       = rtn_lead,
    adjusted_close = close
  )[(width + lead - 1):length(date_stamp), ]
}


# To single data frame
library(dplyr)
library(lubridate)
stocks1 <- bind_rows(df)
market1 <- mkt[(width + lead - 1):nrow(mkt), ]


# Monthly
stocks <- stocks1 %>% 
  group_by(symbol, date_stamp = floor_date(date_stamp, "month")) %>% 
  mutate(date_stamp = ceiling_date(date_stamp, unit = "month") - 1) %>% 
  summarise(
    symbol         = last(symbol),
    roe            = last(roe),
    beta_roe       = first(beta_roe),
    adjusted_close = last(adjusted_close)
  ) %>% 
  ungroup() %>% 
  group_by(symbol) %>% 
  mutate(fwd_rtn = lead((adjusted_close-lag(adjusted_close)) / lag(adjusted_close))) %>% 
  ungroup() %>% 
  select(date_stamp, symbol, roe, beta_roe, adjusted_close, fwd_rtn)

market <- market1 %>% 
  group_by(date_stamp = floor_date(date_stamp, "month")) %>% 
  mutate(date_stamp = ceiling_date(date_stamp, unit = "month") - 1) %>% 
  summarise(
    adjusted_close = last(mkt_price),
    rtn_log_12m    = last(rtn_log_12m)
  ) %>% 
  ungroup() %>% 
  select(date_stamp, adjusted_close, rtn_log_12m)


# Plot to show derived relationships
# http://www.sthda.com/english/wiki/scatter-plots-r-base-graphs
p1 <- cbind(stocks, rbind(market, market, market, market, market, market))
names(p1)[8] <- 'adjusted_close_mkt'
#p1 <- transform(p1, fwd_rtn = ave(adjusted_close, symbol, FUN = function(x) lead((x - lag(x)) / lag(x))))
p2 <- p1[p1$rtn_log_12m < 0, ]
x <- p2$roe
y <- p2$fwd_rtn


# Plot
plot(x, y, main = "Main title", xlab = "ROE", ylab = "Return", pch = 1, frame = FALSE) # point shape (pch = 19) and remove frame
plot(x = p1$roe, y = p1$fwd_rtn, main = "Main title", xlab = "ROE", ylab = "Return", pch = 1, frame = FALSE)

# Write to csv
write.csv(stocks, paste0(getwd(),"/stocks.csv"))
write.csv(market, paste0(getwd(),"/market.csv"))








# auto-regressive process
n <- 250
E <- rnorm(n+2, mean = 0.000 , sd = 0.01)
Y <- numeric(n)
a1 <- 0.01
a2 <- 0.01
Y[1] = E[3] + a1*E[2] + a2*E[1]
Y[2] = E[4] + a1*Y[1] + a2*E[2]
for (i in 3:n) Y[i] = E[i+2] + a1*Y[i-1] + a2*Y[i-2]
plot(Y, type="l")
Rtn <- Y + sqrt(V)   # include asymmetric responses of volatility
plot(cumprod(1 + Y), type="l")

# autoregressive process
ar <- function(n=250, mean=0.00, sd=0.01, a1=0.01, a2=0.01, plot=F, vl) {
  E <- vl #rnorm(n+2, mean = mean , sd = sd)
  R <- numeric(n)
  R[1] = E[3] + a1*E[2] + a2*E[1]
  R[2] = E[4] + a1*R[1] + a2*E[2]
  for (i in 3:n) R[i] = E[i+2] + a1*R[i-1] + a2*R[i-2]
  P <- cumprod(1 + R) #+ sqrt(V)   # include asymmetric responses of volatility
  if (plot) plot(P, type="l")
  return(P)
}
P1 <- ar(plot=T,vl=R) #,vl=R
P2 <- ar(sd=0.02, a1=0.02, a2=0.01)
P3 <- ar(sd=0.03, a1=0.01, a2=0.02)
plot(c(P1, P2, P3),type="l")


# IF STOCK CHARACTERISTIC C1
rbinom(10, 1, 0.5)


# Scratch
library(MASS)
n = 1000
mu1 <- c(X = 0.25, Y = 0, Z = -0.3)
rhoXY <- 0.5
rhoXZ <- 0.2
rhoYZ <- 0.8
R1 <- matrix(c(
  1    , rhoXY, rhoXZ, 
  rhoXY, 1    , rhoYZ, 
  rhoXZ, rhoYZ, 1     ), 
  nrow = 3, ncol = 3, byrow = TRUE)
d <- mvrnorm(n, mu = mu1, Sigma = R1)
d[1:5,]
cor(d[, 1],d[, 2])
cor(d[, 1],d[, 3])
cor(d[, 2],d[, 3])

reg1 <- rnorm( 60,  0.010 , 0.05)
reg2 <- rnorm( 60, -0.025 , 0.10)
reg3 <- rnorm( 60,  0.015 , 0.15)
reg <- c(reg1, reg2, reg3)
plot(cumprod(1 + reg), type="l")







# --------------------------------------------------------------------------------------------------------------------------
#
# Varying coefficients models
# https://github.com/Brent-Morrison/hugo_website/blob/master/content/post/2021-09-20-momentum-analysis.Rmd
#
# https://stackoverflow.com/questions/41201629/how-to-select-the-last-day-of-the-month-in-r
# https://stackoverflow.com/questions/52332590/group-by-week-month-in-data-table
# 
# --------------------------------------------------------------------------------------------------------------------------
library(tidyquant)
library(varycoef)
library(data.table)
library(RollingWindow)


# Stock data
if (file.exists("stocks_sample.csv")) {
  d0 <- read.csv("stocks_sample.csv")
} else {
  d0 <- tq_get(c("^GSPC", "^VIX", "WMT", "LUV"), get = "stock.prices", from = "1995-01-01")
  write.csv(d0, "stocks_sample.csv")
}

d <- as.data.table(d0)
d[, date_stamp := as.Date(date, "%Y-%m-%d")]
d[, mon_date := as.Date(format(date_stamp ,"%Y-%m-01"))]
d[, rtn_ari_1d := (adjusted - shift(adjusted, 1, type = 'lag')) / adjusted, by = symbol]
d[, rtn_log_1d := log(adjusted / shift(adjusted, 1, type = 'lag')), by = symbol]
d[, vol_ari_20d := RollingStd(rtn_ari_1d, window = 20, na_method = 'window') * sqrt(252), by = symbol]

m <- d[, .(close        = last(adjusted), 
           vol_ari_20d  = last(vol_ari_20d), 
           rtn_log_1m   = sum(rtn_log_1d)), 
       by = .(symbol, mon_date)]

m[, fwd_rtn_1m  := log(shift(close, 1, type = 'lead')/ close), by = symbol]
m[, rtn_log_3m  := log(close / shift(close, 3, type = 'lag')), by = symbol]
m[, rtn_log_12m := log(close / shift(close, 12, type = 'lag')), by = symbol]

m <- m[complete.cases(m), ]

# Economic data
e0 <- readRDS(file = "econ_fin_data.Rda")
e <- as.data.table(e0[, c('date','CFNAIDIFF','INDPRO','close')])
e <- e[complete.cases(e), ]

# To wide for use in formula
mw <- dcast(m, mon_date ~ symbol, value.var=c("vol_ari_20d","rtn_log_1m","fwd_rtn_1m","rtn_log_3m","rtn_log_12m"))

# Join
setkey(e, date)
setkey(mw, mon_date)
a <- e[mw, nomatch=0]
colnames(a) <- sub("\\^","",colnames(a))

# Model WMT
dat <- a[date %between% c(as.Date("1996-01-01"), as.Date("2017-12-01")), .(date, fwd_rtn_1m_WMT, rtn_log_3m_WMT, rtn_log_12m_WMT)]
dat_loc <- a[date %between% c(as.Date("1996-01-01"), as.Date("2017-12-01")), .(vol_ari_20d_GSPC, CFNAIDIFF)]

fit_wmt <- SVC_mle(
  fwd_rtn_1m_WMT ~ rtn_log_3m_WMT + rtn_log_12m_WMT,
  locs = as.matrix(dat_loc),
  data = dat,
  control = SVC_mle_control(profileLik = TRUE, cov.name = "exp")
)
fitted(fit_wmt)

# Grid for predictions visualisation
# https://plotly.com/r/3d-surface-plots/
# https://peopleanalytics-regression-book.org/linear-reg-ols.html
interval <- 25
new_loc <- expand.grid(
  vol_ari_20d_GSPC = seq(min(dat_loc[, 1]), max(dat_loc[, 1]), length.out = interval),
  CFNAIDIFF = seq(min(dat_loc[, 2]), max(dat_loc[, 2]), length.out = interval)
  )

pred_WMT <- predict(fit_wmt, newlocs = as.matrix(new_loc))
pred_grid_WMT <- matrix(pred_WMT[, 3], interval, interval)

persp(
  x = seq(min(dat_loc[, 1]), max(dat_loc[, 1]), length.out = interval), 
  xlab = 'mkt volatility',
  y = seq(min(dat_loc[, 2]), max(dat_loc[, 2]), length.out = interval), 
  ylab = 'cfnaidiff',
  zlab = 'SVC_3',
  z = pred_grid_WMT, 
  theta = 225, phi = 20, col = "lightblue", ticktype = "detailed")

mat <- matrix(c(1,1,2,1,1,1,1,0,1,1),ncol=2)
mat <- cbind(mat,c(1,1,1,1,1))




y_wmt <- m[symbol == "WMT" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2000-12-01"))]$rtn_log_1m
X_wmt <- as.matrix(m[symbol == "WMT" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2020-12-01")), .(rtn_log_3m, rtn_log_12m)])
X_wmt <- cbind(rep(1, nrow(X_wmt)), X_wmt)
locs_wmt <- as.matrix(m[symbol == "^GSPC" & mon_date %between% c(as.Date("2000-01-01"), as.Date("2020-12-01")), vol_ari_20d])

fit_wmt <- SVC_mle(
  y = y_wmt,
  X = X_wmt,
  locs = locs_wmt,
  control = SVC_mle_control(profileLik = TRUE, cov.name = "exp")
)
summary(fit_wmt)
plot(fit_wmt)

#aqr <- read.csv("aqr_qmj10.csv")  # https://www.aqr.com/Insights/Datasets/Quality-Minus-Junk-10-QualitySorted-Portfolios-Monthly
#colnames(aqr)[1] <- "date_stamp"
#aqr$date_stamp <- as.Date(aqr$date_stamp, "%m/%d/%Y")



require(sp)
## get data set
data("meuse", package = "sp")
# construct data matrix and response, scale locations
y <- log(meuse$cadmium)
X <- model.matrix(~1+dist+lime+elev, data = meuse)
locs <- as.matrix(meuse[, 1:2])/1000
## starting MLE
# the next call takes a couple of seconds
fit <- SVC_mle(
  y = y, X = X, locs = locs,
  # has 4 fixed effects, but only 3 random effects (SVC)
  # elev is missing in SVC
  #W = X[, 1:3],
  control = SVC_mle_control(
    # inital values for 3 SVC
    # 7 = (3 * 2 covariance parameters + nugget)
    #init = c(rep(c(0.4, 0.2), 3), 0.2),
    profileLik = TRUE
  )
)
## summary and residual output
summary(fit)
plot(fit)
head(fitted(fit))


## predict
# new locations
newlocs <- expand.grid(
  x = seq(min(locs[, 1]), max(locs[, 1]), length.out = 30),
  y = seq(min(locs[, 2]), max(locs[, 2]), length.out = 30))
# predict SVC for new locations
SVC <- predict(fit, newlocs = as.matrix(newlocs))
# visualization
sp.SVC <- SVC
coordinates(sp.SVC) <- ~loc_1+loc_2
spplot(sp.SVC, colorkey = TRUE)






# --------------------------------------------------------------------------------------------------------------------------
# https://dachxiu.chicagobooth.edu/download/datashare.zip
# 
# Stock data from "Empirical Asset Pricing via Machine Learning"
#
# This Version: April 2019. @copyright Shihao Gu, Bryan Kelly and Dacheng Xiu
# If you use the dataset, please cite the paper "Empirical Asset Pricing via Machine Learning" (2018) and "Autoencoder Asset Pricing Models." (2019)

# Firm Characteristics Dataset Description:
  
# 1.    DATE: The end day of each month (YYYYMMDD) 
# 2.    permno: CRSP Permanent Company Number
# 3-96. 94 Lagged Firm Characteristics (Details are in the appendix of our paper)
# 97.   sic2: The first two digits of the Standard Industrial Classification code on DATE

# Most of these characteristics are released to the public with a delay. To avoid the forward-looking bias, we assume that monthly characteristics 
# are delayed by at most 1 month, quarterly with at least 4 months lag, and annual with at least 6 months lag. Therefore, in order to predict returns 
# at month t + 1, we use most recent monthly characteristics at the end of month t, most recent quarterly data by end t ??? 4, and most recent annual 
# data by end t ??? 6. In this dataset, we've already adjusted the lags. (e.g. When DATE=19570329 in our dataset, you can use the monthly RET at 195703 
# as the response variable.) 

# Note: CRSP returns are not included. They are accessible from WRDS.
# 
# --------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(lubridate)

# Load data - "datashare.csv" is eapvml data
d <- fread("C:/Users/brent/Documents/VS_Code/postgres/postgres/reference/datashare.csv")

# Convert integer to date
d[, date_stamp := strptime(DATE, format="%Y%m%d")]

# End of month date for prior month (see below)
d[, date_stamp := as.Date(floor_date(date_stamp, "month") - 1)]

# Dates and stocks (permno)
length(unique(d$date_stamp))
max(d$DATE)
length(unique(d$permno))

# Plot number of stocks over time
library(ggplot2)
ggplot(data=d[, .(stocks_unq = length(unique(permno)), stocks_n = .N), by = date_stamp], 
       aes(x=date_stamp, y=stocks_n, group=1)) +
  geom_line()+
  geom_point()

# Find largest stock at specific date (we expect this to be AAPL)
setorder(d, DATE, -mvel1)
d[DATE == 20201231 , head(.SD, 5)][ , .(permno, DATE, date_stamp, mvel1)]

# Confirm we have indeed identified AAPL by checking the returns
# Add attributes
setorder(d, permno, date_stamp)
d[, rtn_ari_1m := (mvel1 - shift(mvel1, 1)) / shift(mvel1, 1), by = permno]
d[, rtn_log_1m := log(mvel1 / shift(mvel1, 1)), by = permno]

#  AAPL only
aapl <- d[permno == 14593]

# Get independently collected return data from stock_masterdb
library(DBI)
library(RPostgres)
library(jsonlite)

config <- jsonlite::read_json('C:/Users/brent/Documents/VS_Code/postgres/postgres/config.json')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host      = 'localhost',
  port      = '5432',
  dbname    = 'stock_master',
  user      = 'postgres',
  password  = config$pg_password
)

# Retrieve price data
# TO DO - add SIC from "edgar.edgar_fndmntl_all_tb"
qry_text <- "
  select 
  r.symbol
  ,r.date_stamp
  ,r.rtn_ari_1m 
  ,f.report_date
  ,f.publish_date
  ,f.total_equity
  from access_layer.return_attributes r 
  left join access_layer.fundamental_attributes f
  on r.date_stamp = f.date_stamp 
  and r.symbol = f.ticker 
  where r.date_stamp between '2017-01-31'::date and '2021-12-31'::date 
  order by 1,2
  "
qry_send <- DBI::dbSendQuery(conn = con, statement = qry_text) 
db <- DBI::dbFetch(qry_send)


# Check correlations (we have a match)
library(PerformanceAnalytics)
aapl_db <- db[db$symbol == "AAPL", ]
cor_data <- aapl[aapl_db, on = c("date_stamp"), nomatch = 0][ , .(mom1m, rtn_ari_1m, i.rtn_ari_1m)]
chart.Correlation(cor_data, histogram=TRUE, pch=1)




# ----------------------------------------------------
# Calculate turnover of top n by market capitalisation
# ----------------------------------------------------

# Function for measuring portfolio turnover
turnover <- function(df, top_n) {
  
  unq_dates   <- sort(unique(df$date_stamp))
  start_dates <- shift(unq_dates)[-1]
  end_dates   <- unq_dates[-1]
  dat         <- Sys.Date()[0]
  res         <- list()
  
  for (i in 1:length(end_dates)) {
    s    <- df[date_stamp == start_dates[i] & mkt_cap_rank %in% 1:top_n, symbol]
    e    <- df[date_stamp == end_dates[i] & mkt_cap_rank %in% 1:top_n, symbol]
    resi <- length(setdiff(s, e)) / length(s)
    dat  <- append(dat, end_dates[i])
    res  <- append(res, resi)
  }
  
  return(data.frame(date_stamp = dat, turnover = unlist(res)))
}

# Create data frame with december dates including rank by mkt cap
turnoverd <- d[month(date_stamp) == 12 & date_stamp > as.Date('1980-12-31'), .(date_stamp, permno, mvel1)][, mkt_cap_rank := frankv(mvel1, order = -1), by = date_stamp]
setnames(turnoverd, old = "permno", new = "symbol")

# Call turnover function
t <- turnover(turnoverd, top_n = 50)






# --------------------------------------------
# Match permno and ticker based on correlation
# --------------------------------------------

# https://www.ivo-welch.info/research/betas/
# https://eml.berkeley.edu/~sdellavi/data/EarningsSearchesDec06.xls
# https://www.crsp.org/files/images/release_notes/mdaz_201312.pdf
# https://github.com/sharavsambuu/FinRL_Imitation_Learning_by_AI4Finance/blob/master/data/merged.csv
# https://www.stat.rice.edu/~dobelman/courses/482/examples/allstocks.2004.xlsx
# https://biopen.bi.no/bi-xmlui/bitstream/handle/11250/2625322/LTWC.xlsx?sequence=2&isAllowed=y
# http://www1.udel.edu/Finance/varma/CRSP%20DIRECTORY%20.xls

library(data.table)
library(DBI)
library(RPostgres)
library(jsonlite)

# Connect to db 
config <- jsonlite::read_json('C:/Users/brent/Documents/VS_Code/postgres/postgres/config.json')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host      = 'localhost',
  port      = '5432',
  dbname    = 'stock_master',
  user      = 'postgres',
  password  = config$pg_password
)

# Get independently collected return data from stock_master db 
# TO DO - add SIC from "edgar.edgar_fndmntl_all_tb"
qry_text <- "
  select 
  r.symbol
  ,r.date_stamp
  ,r.rtn_ari_1m 
  ,f.report_date
  ,f.publish_date
  ,f.total_equity
  from access_layer.return_attributes r 
  left join access_layer.fundamental_attributes f
  on r.date_stamp = f.date_stamp 
  and r.symbol = f.ticker 
  where r.date_stamp between '2017-01-31'::date and '2021-12-31'::date 
  order by 1,2
  "
qry_send <- DBI::dbSendQuery(conn = con, statement = qry_text) 
db <- DBI::dbFetch(qry_send)
setDT(db)


# Retrieve academic paper data ""Empirical Asset Pricing via Machine Learning""
qry_text <- "
  select 
  date_stamp
  ,permno 
  ,mvel1
  from reference.eapvml
  "
qry_send <- DBI::dbSendQuery(conn = con, statement = qry_text) 
d <- DBI::dbFetch(qry_send)
setDT(d)


# Add return attributes
setorder(d, permno, date_stamp)
d[, rtn_ari_1m := (mvel1 - shift(mvel1, 1)) / shift(mvel1, 1), by = permno]
d[, rtn_log_1m := log(mvel1 / shift(mvel1, 1)), by = permno]


# Create list of permno's from the online data
permno_list <- as.list(unique(
  d[date_stamp == as.Date('2021-11-30')]
  [, mkt_cap_rank := frank(-mvel1)]
  [mkt_cap_rank <= 2000]
  [order(mkt_cap_rank)]
  [, permno]
  ))


# Create list of tickers from the price data
ticker_list <- as.list(unique(db$symbol))

# Check existing file exists, if so remove existing matches
if (file.exists("permno_ticker_202112.csv")) {
  existing_results <- read.csv("permno_ticker_202112.csv")
  permno_list <- Filter(function(x) !x %in% existing_results$permno, permno_list)
  ticker_list <- Filter(function(x) !x %in% existing_results$ticker, ticker_list)
}



start_time <- Sys.time()
r1 <- list()
r2 <- list()
r3 <- list()
r4 <- list()
# Outer loop - tickers
#for (y in c(1:300)) { 
for (y in 1:length(ticker_list)) {
  j <- ticker_list[[y]]
  dbi <- db[symbol == j, .(symbol, date_stamp, rtn_ari_1m)]
  print(j)
  
  # Inner loop - permno
  for (x in 1:length(permno_list)) {
    i   <- permno_list[[x]]
    di  <- d[permno == i, .(permno, date_stamp, rtn_ari_1m)]
    cd  <- di[dbi, on = c("date_stamp"), nomatch = 0][ , .(rtn_ari_1m, i.rtn_ari_1m)]
    
    if (nrow(cd) == 0) {
      r11 <- 0
      r22 <- 0
    } else {
      r11 <- cor(cd$rtn_ari_1m, cd$i.rtn_ari_1m, use= "pairwise.complete.obs")
      r22 <- sum(complete.cases(cd))
    }
    
    # Only keep matches
    if (as.numeric(r11) > 0.95 & as.integer(r22) > 12) {
      r1 <- append(r1, as.numeric(r11))
      r2 <- append(r2, as.integer(r22))
      r3 <- append(r3, i)
      r4 <- append(r4, j) 
      
      # Remove from permno list since match found
      permno_list[[x]] <- NULL
      
      # Exit loop
      break
    }
  }
  
}
res <- cbind.data.frame(unlist(r1),unlist(r2),unlist(r3),unlist(r4))
names(res) <- c("correlation","months","permno","ticker")

end_time <- Sys.time()
run_time <- end_time - start_time

# Bind results
if (exists("existing_results")) {
  existing_results <- rbind(existing_results, res)
} else {
  existing_results <- res
}


# Save to disk
write.csv(existing_results[order(existing_results$ticker), ], file = "permno_ticker_202112.csv", row.names = FALSE)






# ------------------------------------------------------------------------------
# Match permno and ticker from Ivo Welch data (https://www.ivo-welch.info/home/)
# ------------------------------------------------------------------------------

library(data.table)
permno_ticker_iw_raw <- fread("https://www.ivo-welch.info/research/betas/code/betas.csv.gz")
permno_ticker_iw <- permno_ticker_iw_raw[, keyby = .(tic, permno), .(min_date = min(yyyymmdd), max_date = max(yyyymmdd))]
# Convert integer to date
permno_ticker_iw[, ":=" (
  min_date = as.Date(strptime(min_date, format="%Y%m%d")), 
  max_date = as.Date(strptime(max_date, format="%Y%m%d")),
  capture_date = Sys.Date()
  )]
permno_ticker_iw[tic == "", tic := "no_data"]
setnames(permno_ticker_iw, old = "tic", new = "ticker")

# Connect to stock_master db
library(DBI)
library(RPostgres)
library(jsonlite)

config <- jsonlite::read_json('C:/Users/brent/Documents/VS_Code/postgres/postgres/config.json')

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host      = "localhost",
  port      = "5432",
  dbname    = "stock_master",
  user      = "postgres",
  password  = config$pg_password
)


# Create table with sample of data
db_write <- as.data.frame(permno_ticker_iw)
dbWriteTable(con, Id(schema = "reference", table = "permno_ticker_iw"), db_write)





#annual_cols <- c('permno','date_stamp','absacc','acc','age','agr','bm','bm_ia','cashdebt','cashpr','cfp','cfp_ia','chatoia','chcsho','chempia','chinv','chpmia','convind','currat','depr','divi','divo','dy','egr','ep','gma','grcapx','grltnoa','herf','hire','invest','lev','lgr','mve_ia','operprof','orgcap','pchcapx_ia','pchcurrat','pchdepr','pchgm_pchsale','pchquick','pchsale_pchinvt','pchsale_pchrect','pchsale_pchxsga','pchsaleinv','pctacc','ps','quick','rd','rd_mve','rd_sale','realestate','roic','salecash','saleinv','salerec','secured','securedind','sgr','sin','sp','tang','tb')
#monthly_cols <- c('permno','date_stamp','baspread','beta','betasq','chmom','dolvol','idiovol','ill','indmom','maxret','mom12m','mom1m','mom36m','mom6m','mvel1','pricedelay','retvol','std_dolvol','std_turn','turn','zerotrade')
#quarterly_cols <- c('permno','date_stamp','aeavol','cash','chtx','cinvest','ear','ms','nincr','roaq','roavol','roeq','rsup','stdacc','stdcf')
#d1_clean <- d[ , which(sapply(d1, function(x) all(is.na(x)))) := NULL]
#Monthly data only
#monthly <- d[, ..monthly_cols]
#setorder(monthly, permno, date_stamp)

ns <- c("02","05","10", "14")
z <- 0
for (i in 1:15) {
  if (i %in% as.numeric(ns)) {
    z <- i
    print(c(i, z))
  } else {
    print(c(i, z))
  }
  
}



# --------------------------------------------------------------------------------------------------------------------------
#
# Agent based model
# https://arxiv.org/pdf/1709.05117
#
# 
# --------------------------------------------------------------------------------------------------------------------------

# Initialization
set.seed(123) # Ensuring reproducibility

# Parameters
NF <- 10000         # Number of firms
c0 <- 0.5           # Baseline propensity to consume
beta <- 2           # Intensity of choice parameter
gamma <- 0.1        # Baseline price adjustment parameter
R <- 1.3            # The ratio of hiring/firing propensities
eta_0_plus <- 0.2   # Baseline firing propensity 
eta_0_minus <- 0.2  # Baseline hiring propensity
delta <- 0.02       # Fraction of dividends
Theta <- 3          # Bankruptcy threshold / maximum leverage in the economy
phi <- 0.1          # Frequency of firm revival
f <- 0.5            # Share of bankruptcies supported by the firms
alpha_c <- 4        # Reaction of consumption to inflation
phi_pi <- 2.5       # Taylor-like rule parameter - quantifies the intensity of the policy
alpha_Gamma <- 50   # Reaction of firms to interest rates
Gamma_0 <- 0        # Baseline financial fragility sensitivity
pi_star <- 0.04     # Taylor-like rule parameter -  target inflation level
p_star <- 0.03      # Taylor-like rule parameter - baseline interest rate
omega <- 0.2        # Exponentially moving average (ema) parameter
g <- 1              # Indexation of wages to inflation
tau_R <- 0.5
tau_T <- 0.5
t <- 10000
teq <- 5000


num_firms <- 10000         # Number of firms
base_consume_prop <- 0.5   # Baseline propensity to consume
choice_intensity <- 2      # Intensity of choice parameter
price_adj_rate <- 0.1      # Baseline price adjustment parameter
hire_fire_ratio <- 1.3     # The ratio of hiring/firing propensities
fire_prop <- 0.2          # Baseline firing propensity 
hire_prop <- 0.2          # Baseline hiring propensity
div_frac <- 0.02          # Fraction of dividends
max_leverage <- 3         # Bankruptcy threshold / maximum leverage in the economy
firm_revive_rate <- 0.1   # Frequency of firm revival
bankr_support_share <- 0.5 # Share of bankruptcies supported by the firms
consume_infl_react <- 4   # Reaction of consumption to inflation
policy_intensity <- 2.5   # Taylor-like rule parameter - quantifies the intensity of the policy
firm_rate_react <- 50     # Reaction of firms to interest rates
fragility_sens <- 0       # Baseline financial fragility sensitivity
target_infl <- 0.04       # Taylor-like rule parameter - target inflation level
base_interest <- 0.03     # Taylor-like rule parameter - baseline interest rate
ema_param <- 0.2          # Exponentially moving average (EMA) parameter
wage_index <- 1           # Indexation of wages to inflation


Y0 <- 0.1 + 0.9 * runif(1)  # Firms production 

# Firm attributes (empty vectors)
p <- rep(NA, NF)    # Prices - attempted sale of Y
Y <- rep(NA, NF)    # Quantity of perishable goods produced
D <- rep(NA, NF)    # Demand for goods
W <- rep(1, NF)     # Firm wages paid to employee
E <- rep(NA, NF)    # Firm cash balance, if -ve then debt
P <- rep(NA, NF)    # Firm profit
a <- rep(1, NF)     # Active (1) / inactive (0) firm

# Firm attributes (seed data) for each of NF firms
for (i in 1:NF) {
  p[i] <- 1 + 0.1 * (2 * runif(1) - 1)
  Y[i] <- Y0 + 0.1 * (2 * runif(1) - 1)
  D[i] <- Y0
  W[i] <- 1
  E[i] <- 2 * W[i] * Y[i] * runif(1)
  P[i] <- p[i] * min(D[i], Y[i]) - W[i] * Y[i]
}

# Total household savings (number of firms less total cash)
S <- NF - sum(E)

if (phi_pi == 0) {
  pi_star <- 0
  tau_T <- 0
}

# Main Loop
for (t in 1:T) {
  epsilon <- sum(Y) / NF
  u <- 1 - epsilon
  p_mean <- sum(p * Y) / sum(Y)
  w_mean <- sum(W * Y) / sum(Y)
  u_star <- exp(W / w_mean) / sum(a * exp(W / w_mean)) / (NF * u)
  
  # Central Bank Policy
  pi_b <- tau_T * pi_star
  rho_0 <- phi_pi * (pi_star - pi_b)
  Gamma <- max(alpha_Gamma * (rho_0 - pi_b), Gamma_0)
  
  # Firms update prices, productions, and wages
  for (i in 1:NF) {
    if (a[i] == 1) {
      if (E[i] > -Theta * W[i] * Y[i]) {
        Phi <- -E[i] / (W[i] * Y[i])
        eta_plus <- eta_0_plus * (1 - Gamma * Phi)
        eta_minus <- eta_0_minus * (1 + Gamma * Phi)
        
        if (Y[i] < D[i]) {
          if (P[i] > 0) {
            W[i] <- W[i] * (1 + gamma * (1 - Gamma * Phi) * runif(1))
            W[i] <- min(W[i], (P[i] * min(D[i], Y[i])) / Y[i])
          }
          Y[i] <- Y[i] + min(eta_plus * (D[i] - Y[i]), u_star[i])
          if (p[i] < p_mean) p[i] <- p[i] * (1 + gamma * runif(1))
        } else {
          if (P[i] < 0) {
            W[i] <- W[i] * (1 - gamma * (1 + Gamma * Phi) * runif(1))
          }
          Y[i] <- max(0, Y[i] - eta_minus * (D[i] - Y[i]))
          if (p[i] > p_mean) p[i] <- p[i] * (1 - gamma * runif(1))
        }
        p[i] <- p[i] * (1 + pi_b)
        W[i] <- W[i] * (1 + g * pi_b)
        W[i] <- max(W[i], 0)
      } else {
        a[i] <- 0
        S <- S - E[i]
      }
    }
  }
  
  # Update Unemployment
  u <- 1 - sum(Y) / NF
  p <- sum(p * Y) / sum(Y)
  
  # Revivals
  for (i in 1:NF) {
    if (a[i] == 0 && runif(1) < phi) {
      Y[i] <- u * runif(1)
      a[i] <- 1
      P[i] <- p
      W[i] <- w_mean
      E[i] <- W[i] * Y[i]
    }
  }
}

print("Simulation Complete")
                   

# Dummy SFC set-up
# https://www.levyinstitute.org/pubs/wp_891.pdf
act <- c("Co","Wa","Ca","Mo","NW")
sct <- c("HH", "FI", "BA", "GV", "CB", "RW")
tim <- 1:3

bal <- array(
  data = rep( 0, length(act) * length(sct) * length(tim))
  ,dim = c(length(act), length(sct), length(tim))
  ,dimnames = list(act, sct, tim)
  )
bal[,,1] <- c(c(0,0,100,-80,-20,0,0,140,-50,-90,0,0,-240,130,110), rep(0, 15))
bal


# Posting template (Consumption)
txnCo <- bal[,,1]
txnCo[] <- 0
txnCo["Co", c("HH","FI")] <- c(1,-1)
txnCo["Ca", c("HH","FI")] <- c(-1,1)
txnCo


# Posting template (Wages)
txnWa <- bal[,,1]
txnWa[] <- 0 #c(0,-1,1,0,0,0,1,-1,0,0,0,0,0,0,0)
txnWa["Wa", c("HH","FI")] <- c(-1,1)
txnWa["Ca", c("HH","FI")] <- c(1,-1)
txnWa

# Post transactions
bal[,,2] <- bal[,,1] + txnCo * 5 + txnWa * 6
bal[,,2]

sum(abs(colSums(bal[,,1])))
sum(abs(rowSums(bal[,,1])))

bal[,,1][grepl("C", rownames(bal[,,1])),]
