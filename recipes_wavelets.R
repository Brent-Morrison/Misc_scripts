# Interesting references

# https://tidymodels.github.io/recipes/articles/Simple_Example.html
# https://stackoverflow.com/questions/55157786/using-recipes-with-custom-step-works-fine-while-baking-but-not-while-training-mo
# http://lenkiefer.com/2019/04/11/is-the-u-s-housing-recovery-over-housing-fluctuations-across-time-and-frequencies/


# Required packages 
library("tidyverse")
library("wavelets")
library("recipes")


##================================================================================
## Using recipes
##================================================================================

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

step_Haar <- function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE,  
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



##================================================================================
## Using Len Keifer method
##================================================================================

# Data
econ_fin_data <- readRDS("C:/Users/brent/Documents/R/Misc_scripts/econ_fin_data.Rda")


df1 <- econ_fin_data %>% 
  mutate(cred_sprd = BAA - AAA) %>% 
  select(date, cred_sprd) %>% 
  drop_na()

modwt_obj <- modwt(
  X = as.matrix(df1[,2]), 
  filter = "haar", 
  n.levels = 6, 
  boundary = "periodic"
)

wavelet_w <- data.frame(modwt_obj@W)
wavelet_v <- data.frame(modwt_obj@V)

df2 <- bind_cols(df1, wavelet_w) %>% 
  pivot_longer(-date, names_to = "metric", values_to = "value")

ggplot(
  data = df2, 
  aes(
    x = date, 
    y = value, 
    group = 1)
) +
  geom_line() +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  #scale_y_log10() +
  #theme_minimal() +
  labs(
    title = "TITLE", 
    subtitle = "SUB-TITLE",
    caption = "CAPTION", 
    x = "X LABEL",
    y = "Y LABEL"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 9),
    plot.caption = element_text(hjust = 0),
    axis.title.y = element_text(face = "italic", size = 9),
    axis.title.x = element_text(face = "italic", size = 9)
  )
