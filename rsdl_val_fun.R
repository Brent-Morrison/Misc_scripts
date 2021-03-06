# Required packages 
library("tidyverse")
library("recipes")
library("rlang")
library("tibbletime")
library("tidyquant")


##================================================================================
## Using recipes
##================================================================================

# Getting data

# source("C:/Users/brent/Documents/R/Custom_functions/Function_script.R")
# source("https://raw.githubusercontent.com/Brent-Morrison/Custom_functions/master/Function_script.R")

econ_fin_data <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/econ_fin_data.Rda")
sp_shade      <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/sp_shade.Rda")

rolling_median <- rollify(median, window = 61)

# Data: include all roles.  Outcome, predictor, input to valuation residual step
vltn_model_data <- econ_fin_data %>% 
  mutate(
    earn_yld = lag(E, n = 6) / close, 
    earn_yld_5yr = lag(rolling_median(E), n = 6) / close, 
    infl = lag(ROC(CPIAUCSL, n = 12), n = 1),  
    rule_20 = earn_yld - infl,
    m2 = lag(ROC(M2SL, n = 12),n = 1),
    cred_sprd = lag(BAA - AAA, n = 1)) %>%
  select(date, sp5_fwd_rtn_1m, sp5_rtn_6m, earn_yld, earn_yld_5yr, infl, GS10, m2) %>% 
  filter(between(date, as.Date("1960-06-01"), as.Date("2019-11-01")))

train_data <- vltn_model_data %>% slice(1:700)
test_data <- vltn_model_data %>% slice(701:n())

# Defining custom pretreatment algorithm
# https://github.com/tidymodels/recipes/blob/master/R/pca.R
# https://stackoverflow.com/questions/29711599/dynamic-formula-creation-in-r?rq=1
# https://stackoverflow.com/questions/46867888/programming-a-function-for-lm-using-tidyeval
# https://stackoverflow.com/questions/4951442/formula-with-dynamic-number-of-variables
# https://stackoverflow.com/questions/50053035/using-quosures-within-formula-inside-an-anonymous-function

lr_residual <- function(df, y, x) { 
  form_base <- paste(y, "~")
  form_vars <- paste(x, collapse = " + ")
  formula <- paste(form_base, form_vars)
  residuals(lm(formula, data = df))
}

test <- lr_residual(train_data, y = "earn_yld_5yr", x = c("infl", "m2", "GS10"))


# Create the function
step_vltn_residual <- function(
  recipe, 
  ..., 
  role = "predictor", 
  trained = FALSE, 
  ref_dist = NULL,
  skip = FALSE,  
  id = rand_id("vltn_residual")
  ) {
  
  terms = ellipse_check(...)
  
  add_step(
    recipe, 
    step_vltn_residual_new(
      terms = terms, 
      trained = trained,
      role = role, 
      ref_dist = ref_dist,
      skip = skip, 
      id = id
    )
  )
}


# Initialise a new object
step_vltn_residual_new <- 
  function(
  terms, 
  role, 
  trained, 
  ref_dist, 
  skip, 
  id
  ) {
    step(
      subclass = "vltn_residual",  
      terms = terms, 
      role = role, 
      trained = trained, 
      ref_dist = ref_dist,
      skip = skip, 
      id = id
    )
  }


# Define the procedure
prep.step_vltn_residual <- 
  function(
    x,              ## x will be the step_vltn_residual object
    training,       ## training will be a tibble that has the training set data
    info = NULL,    ## info will also be a tibble that has information on the current set of data available
    ...
  ) {
      col_names <- terms_select(
        terms = x$terms, 
        info = info
      )
      
  # Compute regression parameters
  ref_dist <- purrr::map(training[, col_names], lr_residual)
  
  # Return the updated object   
  step_vltn_residual_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_dist = ref_dist,
    skip = x$skip,
    id = x$id
  )
}


# Create the bake method
bake.step_vltn_residual <- function(
  object, 
  new_data, 
  ...
  ) {
    predictors <- lr_residual(
      dplyr::select(new_data, object$columns)
    )
  new_data[, object$columns] <- NULL
  bind_cols(new_data, predictors)
}





# Create the recipe
vltn_residual_recipe <- recipe(vltn_model_data) %>% 
  update_role(sp5_fwd_rtn_1m, new_role = "outcome") %>% 
  update_role(sp5_rtn_6m, earn_yld, new_role = "predictor") %>% 
  update_role(earn_yld_5yr, new_role = "vltn_outcome") %>% 
  update_role(infl, GS10, m2, new_role = "vltn_predictor") %>% 
  step_vltn_residual(earn_yld_5yr, infl, GS10, m2) 

# Train the recipe on data
trained_vltn_residual_recipe <- prep(vltn_residual_recipe, training = vltn_model_data)

# Apply to the training and test set
#train_data_modwt <- bake(trained_vltn_residual_recipe, new_data = train_biomass)
#test_data_modwt <- bake(trained_vltn_residual_recipe, new_data = test_biomass)

