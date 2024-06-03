setwd("C:/Users/brent/Documents/R/Misc_scripts/e00")
library(tidyverse)
library(DescTools)

def_theme1 <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 7, color = "darkslategrey"),
    axis.text.x = element_text(size = 7, color = "darkslategrey")
  )

# --------------------------------------------------------------------------------------------------------------------------
  
training_data_raw <- read_csv(paste0(getwd(),"/02-data_01-training.csv"))
preds <- names(training_data_raw)[5:ncol(training_data_raw)]
#df <- data.frame(matrix(ncol=3, nrow=0, dimnames=list(NULL, c("predictor", "fwd_rtn", "pred_val"))))
df <- data.frame(predictor = character(), fwd_rtn = numeric(), pred_val = integer())
for (i in 1:length(preds)) {
  col <- preds[i]
  df1 <- training_data_raw[, c("fwd_rtn", col)]
  df1$predictor <- rep(col, nrow(df1))
  names(df1)[names(df1) == col] <- "pred_val"
  df1 <- df1[, c("predictor", "fwd_rtn", "pred_val")]
  df <- bind_rows(df, df1)
}

df <- df %>% 
  group_by(predictor) %>% 
  mutate(pred_val_wins = Winsorize(pred_val, probs = c(0.025, 0.975)))

ggplot(df %>% filter(!predictor == "perc_pos_12m"), aes(x = pred_val_wins, y = fwd_rtn)) + 
  geom_point(shape = ".") +
  stat_smooth(method = 'gam', formula = y ~ s(x), linewidth = 0.6, colour = 'grey') +
  facet_wrap(vars(predictor), scales = "free") +  
  labs(
    y = "Forward 3 month returns",
    x = "Predictor value",
    title = "Forward returns and predictors",
    subtitle = "",
    caption = ""
  ) +
  def_theme1



ggplot(df %>% filter(predictor == "perc_pos_12m"), aes(x = as.factor(round(pred_val,3)), y = fwd_rtn)) + 
  geom_boxplot(varwidth = TRUE) + 
  geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey') +
  labs(
    y = "Forward 3 month returns",
    x = "Percentage of positive return months in prior year",
    title = "Forward returns and percentage of positive returns",
    subtitle = "",
    caption = ""
  ) +
  def_theme1



# --------------------------------------------------------------------------------------------------------------------------

library(glmnet)
library(jsonlite)
json_args <- jsonlite::read_json(paste0(getwd(),"/01-scripts_02-args.json"))
predictors <- json_args$predictors
training_data_raw <- read_csv(paste0(getwd(),"/02-data_01-training.csv"))
# https://cran.r-project.org/web/packages/glmnetUtils/vignettes/intro.html
final_fit <- cv.glmnet(
  x = as.matrix(training_data_raw[, unlist(predictors)]), 
  y = as.matrix(training_data_raw[, "fwd_rtn"]), 
  family = "gaussian"
  )
plot(final_fit)
coef(final_fit, s = "lambda.min")
final_fit$lambda.min

# CVA
alpha <- seq(0, 1, len=11)^3
nfolds <- 10
foldid <- sample(rep(seq_len(nfolds), length=nrow(training_data_raw)))
glm_models <- lapply(
  alpha, 
  cv.glmnet, 
  x = as.matrix(training_data_raw[, unlist(predictors)]), 
  y = as.matrix(training_data_raw[, "fwd_rtn"]), 
  nfolds = nfolds, 
  foldid = foldid,
  family = "gaussian",
  weights = NULL,
  offset = NULL,
  lambda = NULL,
  type.measure = "mse",
  alignment = "lambda",
  grouped = TRUE,
  keep = FALSE,
  parallel = FALSE,
  gamma = c(0, 0.25, 0.5, 0.75, 1),
  relax = FALSE,
  trace.it = 0
  )

# Find the model with the lowest error
cvms <- sapply(glm_models, "[[", "cvm")
min_model <- which.min(sapply(cvms, min))
final_fit <- glm_models[[min_model]]
p <- predict(final_fit, newx = as.matrix(training_data_raw[1:10, unlist(predictors)]), s = "lambda.min")
p <- as.vector(p)

vip::vi_model(final_fit)

#glm_models[[min_model]]$lambda
#plot(glm_models[[1]])
#sapply(glm_models, "[[", "cvm", simplify=FALSE)
#lapply(glm_models, "[[", "lambda.min")
#lapply(glm_models, "[[", "index")
