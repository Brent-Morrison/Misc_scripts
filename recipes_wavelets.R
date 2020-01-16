# Interesting references

# https://tidymodels.github.io/recipes/articles/Simple_Example.html
# https://stackoverflow.com/questions/55157786/using-recipes-with-custom-step-works-fine-while-baking-but-not-while-training-mo
# http://lenkiefer.com/2019/04/11/is-the-u-s-housing-recovery-over-housing-fluctuations-across-time-and-frequencies/


# Required packages 
library("tidyverse")
library("wavelets")
library("recipes")

# Getting data
data(biomass)
biomass <- as.data.frame(biomass)
train_biomass <- biomass %>% 
  filter(dataset == "Training") %>% 
  select(-dataset,-sample)

test_biomass <- biomass %>% 
  filter(dataset == "Testing") %>% 
  select(-dataset,-sample)

# Defining custom pretreatment algorithm
HaarTransform <- function(DF1) {
  w <- function(k) {
    # Useeither dwt or modwt below
    s1 = modwt(k, filter = "haar")
    return (s1@V[[1]])
  }
  Smt = as.matrix(DF1)
  Smt = t(base::apply(Smt, 1, w))
  return (data.frame(Smt))
}

# Creating the custom step functions
step_Haar_new <- function(terms, role, trained, skip, columns, id) {
  step(subclass = "Haar",  terms = terms, role = role, 
       trained = trained, skip = skip, columns = columns, id = id)
}

step_Haar<-function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE,  
                    columns = NULL, id = rand_id("Harr")) {
  terms = ellipse_check(...)
  add_step(recipe, 
           step_Haar_new(terms = terms, role = role, trained = trained,  
                         skip = skip, columns = columns, id = id))
}

prep.step_Haar <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  step_Haar_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    columns = col_names,
    id = x$id
  )
}

bake.step_Haar <- function(object, new_data, ...) {
  predictors <- HaarTransform(dplyr::select(new_data, object$columns))
  new_data[, object$columns] <- NULL
  bind_cols(new_data, predictors)
}

# Create the recipe
Haar_recipe <- recipe(carbon ~ ., train_biomass) %>% 
  step_Haar(all_predictors()) 

# Train the recipe on data
trained_Haar_recipe <- prep(Haar_recipe, training = train_biomass)

# Apply to the training and test set
train_data <- bake(trained_Haar_recipe, new_data = train_biomass)
test_data_modwt <- bake(trained_Haar_recipe, new_data = test_biomass)
