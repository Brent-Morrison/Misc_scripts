#========================================================================================
#==      Load required packages, source functions &                                    ==
#==      load required data                                                            ==
#========================================================================================

library("tidyverse")
library("tidyquant")
library("earth")
library("glmnet")
library("timetk")
library("lubridate")
library("tibbletime")
library("caret")
library("broom")
library("scales")
library("DescTools")
library("Cubist")
library("cowplot")
library("recipes")

# source("C:/Users/brent/Documents/R/Custom_functions/Function_script.R")
source("https://raw.githubusercontent.com/Brent-Morrison/Custom_functions/master/Function_script.R")

econ_fin_data <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/econ_fin_data.Rda")
sp_shade      <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/sp_shade.Rda")



#========================================================================================
#==     Create leading economic indicator                                              ==
#========================================================================================

rolling_median <- rollify(median, window = 61)
#rolling_pca <- rollify(prcomp, window = 61)

econ_fin_data <- econ_fin_data %>% 
  filter(between(date, as.Date("1945-06-01"), as.Date("2019-12-01"))) %>% 
  mutate(
    fwd_rtn_m = sp5_fwd_rtn_1m,
    trsy2_10 = DGS10 - DGS2, 
    ff_10 = lag(GS10 - FEDFUNDS,n = 1), 
    earn_yld = lag(E, n = 6) / close, 
    earn_yld_5yr = lag(rolling_median(E), n = 6) / close, 
    infl = lag(ROC(CPIAUCSL, n = 12), n = 1),  
    rule_20 = earn_yld - infl,  #(close / lag(E, n = 6)) + (infl * 100),
    m2 = lag(ROC(M2SL, n = 12),n = 1),
    cred_sprd = lag(BAA - AAA, n = 1),
    cred_sprd_12m_delta = cred_sprd - lag(cred_sprd, n = 1),
    acdgno = lag(ROC(ACDGNO, n = 12),n = 1),
    awhman = lag(AWHMAN, n = 1),
    neword = lag(NEWORD, n = 1),
    neworder = lag(ROC(NEWORDER, n = 12), n = 1),      
    permit = lag(ROC(PERMIT, n = 12), n = 1),      
    ic4wsa = lag(ROC(IC4WSA, n = 12), n = 1),
    hmi = lag(HMI, n = 1),
    #gs10_rtn1 = lag(GS10, n = 1) / 1200, 
    #gs10_rtn2 = lag(GS10, n = 1) / GS10,
    #gs10_rtn3 = 1 - (1 + GS10/ 200) ^ (-2 * (10 - (1 / 12))),
    #gs10_rtn4 = (1 + GS10 / 200) ^ (-2 *(10 - (1 / 12))) - 1,
    #gs10_trn5 = gs10_rtn1 + (gs10_rtn2 * gs10_rtn3 + gs10_rtn4),
    # Reference: https://www.mdpi.com/2306-5729/4/3/91
    gs10_rtn = (lag(DGS10, n = 1) / 1200) + 
      ((lag(DGS10, n = 1) / DGS10) * 
         (1 - (1 + DGS10/ 200) ^ (-2 * (10 - (1 / 12)))) + 
         ((1 + DGS10 / 200) ^ (-2 *(10 - (1 / 12))) - 1)),
    sp5_gs10_3yr_cor = runCor(sp5_rtn_1m, gs10_rtn, n = 36),
    lead_ind = rowMeans(data.frame( #  do this as rolling pca
      acdgno, 
      awhman,
      neword,
      neworder,      
      permit,     
      ic4wsa,
      hmi
    ), na.rm = TRUE)
  ) #%>% 
#fill(everything(), .direction = c("down"))



#========================================================================================
#==     Valuation model                                                                ==
#==     TODO - put this into a recipe                                                  ==
#========================================================================================

vltn_model <- econ_fin_data %>% 
  select(date, earn_yld, earn_yld_5yr, infl, GS10, m2) %>% 
  rename_at(vars(-date), ~ paste0(., '_stds')) %>% 
  filter_at(vars(contains('_stds')), all_vars(!is.na(.))) %>%
  mutate_at(vars(contains('_stds')), list(~Winsorize(.))) %>% 
  mutate_at(vars(contains('_stds')), list(~scale(.))) %>% 
  mutate(val_rsdl = residuals(lm(earn_yld_5yr_stds
                                 ~ infl_stds
                                 + GS10_stds
                                 + m2_stds)))

econ_fin_data <- inner_join(econ_fin_data, vltn_model, by = "date")

# Plot
# trans.plot(econ_fin_data, sp_shade, val_rsdl, both)


#========================================================================================
#==     Data for model                                                                 ==
#== https://edwinth.github.io/blog/recipes_blog/
#========================================================================================    

# Data for model
df_data <- econ_fin_data %>% 
  select(
    date, 
    fwd_rtn_m, 
    sp5_rtn_1m,
    sp5_rtn_6m, 
    cred_sprd, 
    cred_sprd_12m_delta,
    ff_10
  ) %>% 
  mutate(
    sp5_rtn_6m_lag6 = lag(sp5_rtn_6m, 6),
    sp5_rtn_6m_lag12 = lag(sp5_rtn_6m, 12),
    cred_sprd_lag6 = lag(cred_sprd, 6),
    cred_sprd_lag12 = lag(cred_sprd, 12),
    cred_sprd_12m_delta_lag6 = lag(cred_sprd_12m_delta, 6),
    cred_sprd_12m_delta_lag12 = lag(cred_sprd_12m_delta, 12)
  ) %>% 
  filter(date > "1961-06-01") %>% 
  as.data.frame()

train_length <- 240
vldn_length <- 120
test_length <- 12

#========================================================================================
#==     Create nested dataframe VERSION 1                                              ==
#==     TODO - update "ts_nest" custom function with code below                        ==
#========================================================================================

loops <- floor((nrow(df_data) - (train_length + vldn_length)) / test_length)
start <- nrow(df_data) - ((loops * test_length) + (train_length + vldn_length)) + 1

# Empty tibble
nested_df_old = data.frame()

# Loop for time slices
for (i in seq(start, by = test_length, length.out = loops)) {
  df <- df_data
  df <- slice(df, i:(i + train_length + vldn_length + test_length - 1)) %>% 
    mutate(
      nest_label = paste(format(strftime(min(date), "%Y-%m")), 
                         format(strftime(max(date), "%Y-%m")),
                         sep = ":"),
      train_test = c(rep("train", (train_length + vldn_length)), rep("test", test_length))
    )
  nested_df_old <- bind_rows(nested_df_old,df) 
}

# Nest by time slice
nested_df_old <- nested_df_old %>% 
  group_by(nest_label, train_test) %>% 
  nest() %>% 
  ungroup() %>% 
  pivot_wider(names_from = train_test, values_from = data) %>% 
  mutate(
    train_X = map(train, ~ as.data.frame(select(., -fwd_rtn_m, -date))),    # fwd_rtn_m to be fcn param
    train_Y = map(train, ~ .x$fwd_rtn_m),                                   # map(train, "fwd_rtn_m"), 
    test_X = map(test, ~ as.data.frame(select(., -fwd_rtn_m))),             # fwd_rtn_m to be fcn param
    train_data = map2(train_X, train_Y,  ~ list(X = .x, Y = .y))
  ) %>% 
  select(-train_X, -train_Y)# train_X and train_Y now be dropped

#unnest_test <- unnest(nested_df_old[28, 7], cols = c(train_data))
#unnest_test_X <- unnest(unnest_test[1, 1], cols = c(train_data))
#unnest_test_Y <- unnest(unnest_test[2, 1], cols = c(train_data))


#========================================================================================
#==     Create nested dataframe VERSION 2                                              ==
#==     Create recipe                                                                  ==
#========================================================================================

nested_df <- ts_nest(df_data, fwd_rtn_m, 240, 120, 12)

norm_recipe <- recipe(fwd_rtn_m ~ ., data = df_data) %>% 
  step_normalize(all_predictors())

unch_recipe <- recipe(fwd_rtn_m ~ ., data = df_data)

#========================================================================================
#==     Model functions                                                                ==
#========================================================================================  

# Specify trControl
tc <- trainControl(
  method = "cv", 
  index = list(1:train_length),
  indexOut = list((train_length + 1):(train_length + vldn_length))
  )

### Cubist model ###
# See the plotting functions here https://github.com/erblast/oetteR/
cubist_model_fun_old <- function(X, Y) {
  train(
    x = X,
    y = Y,
    method = 'cubist',
    #metric = "RMSE", can we use huber loss here?
    trControl = tc,
    tuneGrid = expand.grid(
      committees = c(1, 5, 10, 50),
      neighbors = c(0, 1, 3, 5, 7, 9))
  )
}

cubist_model_fun <- function(X, DATA) {
  train(
    x = X,
    data = DATA,
    method = 'cubist',
    #metric = "RMSE", can we use huber loss here?
    trControl = tc,
    tuneGrid = expand.grid(
      committees = c(1, 5, 10, 50),
      neighbors = c(0, 1, 3, 5, 7, 9))
  )
}

### MARS model ###
# Useful explanations 
# http://rpubs.com/erblast/mars, 
# http://www.milbo.org/doc/earth-notes.pdf,
# http://uc-r.github.io/mars
# Tuning parameter 'degree' is number of interaction effects allowed,
# for no interactions, degree = 1.
# Tuning parameter 'nprune' is maximum number of terms 
# (including intercept) in the pruned model.  
# Tuning parameter 'minspan' is the minimum number of observations between knots,
# for three evenly spaced knots for each predictor minspan = -3
mars_model_fun_old <- function(X, Y) {
  train(
    x = X,
    y = Y,
    method = "earth",
    #minspan = 30,  
    #endspan = 30,
    metric = "RMSE",
    trControl = tc,
    tuneGrid = expand.grid(
      nprune = c(5, 10, 15),
      degree = 1:2)
  )
}

mars_model_fun <- function(X, DATA) {
  train(
    x = X,
    data = DATA,
    method = "earth",
    #minspan = 30,  
    #endspan = 30,
    metric = "RMSE",
    trControl = tc,
    tuneGrid = expand.grid(
      nprune = c(5, 10, 15),
      degree = 1:2)
  )
}

nnet_model_fun <- function(X, DATA) {
  train(
    x = X,
    data = DATA,
    method = "nnet",
    metric = "RMSE",
    trControl = tc,
    tuneGrid = expand.grid(
      size = c(1, 5, 10),
      decay = c(0,0.001,0.1))
  )
}

### Put models in a list ###
model_list_old <- list(
  cubist_model = cubist_model_fun_old,
  mars_model = mars_model_fun_old
  ) %>%
  enframe(name = 'model_name', value = 'model')

# INCLUDE IF_THEN FOR TYPE OF RECIPE FOR TYPE OF MODEL
# IE., NORMALISE FOR NN AND OTHERWISE FOR MARS/TREE/ETC.
model_list <- list(
  cubist_model = cubist_model_fun,
  mars_model = mars_model_fun,
  nnet_model = nnet_model_fun
) %>%
  enframe(name = 'model_name', value = 'model')

### Join models and recipes ###
recipe_list <- list(
  unch_rec = unch_recipe,
  norm_rec = norm_recipe
) %>% 
  enframe(name = 'recipe_name', value = 'recipe')

model_recipe_list <- model_list %>% 
  crossing(recipe_list)

### Join models and data ###  
nested_df_old <- nested_df_old %>%
  crossing(model_list_old)

nested_df <- nested_df %>%
  crossing(model_recipe_list)

### Fit models ###  =========================== UP TO HERE ===========================

# Also see here for pmap example
# https://rpubs.com/erblast/caret
nested_df_old <- nested_df_old %>% 
  mutate(fitted_model = invoke_map(model, train_data))

nested_df <- nested_df %>% 
  mutate(fitted_model = pmap(list(), model, data = train))

### Predict ###
preds <- nested_df %>%
  transmute(
    nest_label = nest_label,
    model_name = model_name,  
    test_start_date = as.Date(paste0(str_sub(nest_label, -7, -1), "-01")) %m-% months(test_length - 1),
    date = map(test_start_date, ~ seq(as.Date(.x), by = "month", length = test_length)),
    pred = map2(fitted_model, test_X, predict)) %>% 
  unnest(cols = c(date, pred))



#========================================================================================
#==     Testing                                                                        ==
#======================================================================================== 

# Testing nesting approach
# Test training single cubist model
train_X = df_data %>% select(-fwd_rtn_m, -date)
train_Y = df_data$fwd_rtn_m

cubist_model <- train(
  x = train_X,
  y = train_Y,
  method = 'cubist',
  trControl = trainControl(
    method = "cv", 
    index = list(1:train_length),
    indexOut = list((train_length + 1):(train_length + vldn_length))
  ),
  tuneGrid = expand.grid(
    committees = c(1, 5, 10, 50),
    neighbors = c(0, 1, 3, 5, 7, 9))
)

# Extract predictions from single trained model
test_pred <- enframe(
  predict(cubist_model, newdata = test), 
  name = NULL, 
  value = "test_pred")


# Extract model from nested dataframe
final_model <- nested_df[[20, "fitted_model"]][["finalModel"]]["output"]

xx1 <- nested_df %>%
  transmute(
    nest_label = nest_label,
    final_model = map(fitted_model, ~.x$finalModel$output),
    best_tune_com = map(fitted_model, ~.x$bestTune$committees),
    best_tune_nbr = map(fitted_model, ~.x$bestTune$neighbors),
    var_imp_fun = map(fitted_model, varImp),
    var_imp = map(var_imp_fun, ~.x$importance),
    var_imp_usage = map(fitted_model, ~.x$finalModel$usage)
  )

xx2 <- as.data.frame(varImp(cubist_model)["importance"]) %>% rownames_to_column()

xx3 <- xx1 %>% 
  select(
    nest_label,
    var_imp,
    var_imp_usage
  ) %>% 
  unnest(cols = c(var_imp, var_imp_usage)) %>% 
  select(-Conditions, -Model) %>% 
  pivot_wider(names_from = Variable, values_from = Overall)

