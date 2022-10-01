#========================================================================================
#==      Load required packages, source functions &                                    ==
#==      load required data                                                            ==
#========================================================================================

library("xgboost")
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

# rolling median function
rolling_median <- rollify(median, window = 35)

# Rolling pca function
# https://stackoverflow.com/questions/41616427/rolling-pca-and-plotting-proportional-variance-of-principal-components
#rolling_pca <- rollify(prcomp, window = 61)

# Rolling valuation residuals function
rolling_val_resid <- rollify(
  .f = function(earn_yld_5yr, infl, GS10, m2) {
    # Remove tail function to create nested data frame for each window
    tail(residuals(lm(earn_yld_5yr ~ infl + GS10 + m2)), n = 1)
  }, 
  window = 120, 
  unlist = FALSE)

econ_fin_data <- econ_fin_data %>% 
  filter(between(date, as.Date("1945-06-01"), as.Date("2019-12-01"))) %>% 
  mutate(
    fwd_rtn_m = sp5_fwd_rtn_1m,
    trsy2_10 = DGS10 - DGS2, 
    ff_10 = lag(GS10 - FEDFUNDS,n = 1), 
    earn_yld = lag(E, n = 6) / close, 
    earn_yld_5yr = lag(rolling_median(E), n = 6) / close, 
    infl = lag(ROC(CPIAUCSL, n = 12), n = 1),  
    earn_yld_rule_20 = (1/(20 - (infl * 100))) - earn_yld_5yr, #(close / lag(E, n = 6)) + (infl * 100),
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
    ), na.rm = TRUE)#,
    #vltn_resid = rolling_val_resid(earn_yld_5yr, infl, GS10, m2)
  ) #%>% unnest(cols = c(vltn_resid))
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
trans.plot(econ_fin_data, sp_shade, sp5_gs10_3yr_cor, both)


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


#========================================================================================
#==     Create nested dataframe with custom function                                   ==
#==     and create recipes defining model structure, pre-processing                    ==
#========================================================================================

# Training, validation, testing lengths
train_length <- 240
vldn_length <- 120
test_length <- 12

# Nested df
nested_df <- ts_nest(df_data, fwd_rtn_m, train_length, vldn_length, test_length)

# To unnest for inspection
# unnest_test <- unnest(nested_df[1, 2], cols = c(train))

# Recipes
norm_recipe <- recipe(fwd_rtn_m ~ ., data = select(df_data, -date)) %>% 
  step_normalize(all_predictors())

unch_recipe <- recipe(fwd_rtn_m ~ ., data = select(df_data, -date))


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

xgb_model_fun <- function(X, DATA) {
  train(
    x = X,
    data = DATA,
    method = "xgbTree",
    metric = "RMSE",
    trControl = tc,
    # https://xgboost.readthedocs.io/en/latest/parameter.html
    # https://xgboost.readthedocs.io/en/latest/tutorials/param_tuning.html
    tuneGrid = expand.grid(
      nrounds = 1000,
      max_depth = 3:6,
      eta = c(0.01, 0.001, 0.0001),
      gamma = 1,
      colsample_bytree = c(0.7, 1),
      min_child_weight = 1,
      subsample = 1
      )
  )
}

### Put models in a list ###

model_list <- list(
  cubist_model = cubist_model_fun,
  mars_model = mars_model_fun,
  #nnet_model = nnet_model_fun,
  xgb_model = xgb_model_fun
) %>%
  enframe(name = 'model_name', value = 'model_object')


### Join models and recipes ###

recipe_list <- list(
  unch_rec = unch_recipe#,
  #norm_rec = norm_recipe
) %>% 
  enframe(name = 'recipe_name', value = 'recipe_object')

model_recipe_list <- model_list %>% 
  crossing(recipe_list)
# INCLUDE IF_THEN FOR TYPE OF RECIPE FOR TYPE OF MODEL
# IE., NORMALISE FOR NN AND OTHERWISE FOR MARS/TREE/ETC.


### Join models and data ###  

nested_df <- nested_df %>%
  crossing(model_recipe_list) %>% 
  # Place inputs to model in a list in order to use "invoke_map" function
  mutate(data_recipe = map2(recipe_object, train, ~ list(X = .x, DATA = .y))) %>% 
  select(-train)


### Fit models ###  

# Also see here for pmap example
# https://rpubs.com/erblast/caret
# https://www.alexpghayes.com/blog/implementing-the-super-learner-with-tidymodels/
# https://konradsemsch.netlify.com/2019/08/caret-vs-tidymodels-comparing-the-old-and-new/
# https://www.datisticsblog.com/2018/12/tidymodels/#modelling-with-caret

nested_df <- nested_df %>% 
  mutate(fitted_model = invoke_map(model_object, data_recipe))

### Predict ###

# Predict and join actual returns

preds <- nested_df %>%
  transmute(
    nest_label = nest_label,
    model_name = model_name,  
    recipe_name = recipe_name,
    test_start_date = as.Date(paste0(str_sub(nest_label, -7, -1), "-01")) %m-% months(test_length - 1),
    date = map(test_start_date, ~ seq(as.Date(.x), by = "month", length = test_length)),
    pred = map2(fitted_model, test, predict)) %>% 
  unnest(cols = c(date, pred)) %>% 
  left_join(select(df_data, date, fwd_rtn_m), by = c("date"))

# Scatter plot

ggplot(preds, aes(x = pred, y = fwd_rtn_m)) + 
  geom_point() +
  facet_grid(model_name ~ recipe_name)

# Cumulative return plot



# Variable importance plot

var_importance <- nested_df %>% 
  select(nest_label, model_name, recipe_name, fitted_model) %>% 
  mutate(var_imp = map(fitted_model, varImp),
         var_imp = map(var_imp, ~ .x$importance %>% rownames_to_column())) %>% 
  select(-fitted_model) %>% 
  unnest(cols = c(var_imp))

var_importance %>% 
  filter(model_name == "ggb_model",
         recipe_name == "unch_rec") %>% 
  ggplot(aes(x = nest_label, 
             y = reorder(rowname, Overall), 
             colour = -Overall, size = Overall)) +
  geom_point(show.legend = FALSE) +
  theme_grey() +
  scale_size(range = c(1, 4)) +
  labs(title = "Time series of variable importance",
       subtitle = "Size and shading represents variable importance value",
       caption = "Source: S&P500 data") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 75, vjust = 0.5),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 9, color = "grey55")
  )




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

