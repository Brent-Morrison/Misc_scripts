#=========================================================================================
#==   REQUIRED PACKAGES                                                                 ==
#=========================================================================================

library("tidyquant")
library("cowplot")
library("timetk")
library("broom")
library("tibbletime")
library("caret")
library("scales")
library("DescTools")


#=========================================================================================
#==   LOAD REQUIRED DATA                                                                ==
#=========================================================================================

econ_fin_data <- readRDS("econ_fin_data.Rda")
sp_shade      <- readRDS("sp_shade.Rda")


#=========================================================================================
#==   TRANSFORM AND PLOT DATA                                                           ==
#==   - categorise time series into bins representing specific level and change values  ==
#==   - present histogram of subsequent market returns for bins assessing if the        ==
#==     distribution of subsequent returns differ                                       ==
#=========================================================================================

x2 <- econ_fin_data %>% mutate(ff_10 = GS10 - FEDFUNDS) %>%
  
  # select data required, including indicator under analysis
  select(date, fwd_rtn_m, ff_10) %>% 
  
  # lagged values of indicator under analysis
  mutate(x1.lag6  = lag(ff_10, 6), 
         x1.lag12 = lag(ff_10, 12),
         
         # tercile level factor
         x1.qntlx = ntile(ff_10, 3), 
         x1.qntl = case_when(x1.qntlx == 1 ~ "_low", 
                             x1.qntlx == 2 ~ "_mid", 
                             x1.qntlx == 3 ~ "_high"),
         
         # change in level indicator
         x1.rtn6  = ff_10 - x1.lag6,
         x1.rtn12 = ff_10 - x1.lag12,
         
         # binary change in level factor
         x1.delta = if_else(ff_10 > lag(ff_10, n = 6), 
                            "incr", 
                            "decr")) %>% 
  
  # factor combining tercile level and binary change in level factors 
  unite(x1_lag00, c(x1.qntl, x1.delta),sep = "_", remove = FALSE) %>%
  
  # lagged combined factor and filter out NA's
  mutate(x1_lag06 = lag(x1_lag00, 6),                          
         x1_lag12 = lag(x1_lag00, 12)) %>%                           
  filter(!is.na(x1.lag12))

# current values of factor values for plot text
x2.1 <- slice(x2, n()) %>% select(x1_lag00, x1_lag06, x1_lag12) %>% t() %>% 
  data.frame() %>% rownames_to_column() %>% 
  unite(Indicator, c(rowname, .), sep = "", remove = TRUE) %>% 
  mutate(Indicator =  gsub("x1_", "", Indicator))

# dummy variables for each (current & lagged) combined level / change factor
x3 <- predict(dummyVars(" ~ x1_lag00", data = x2), newdata = x2)
x4 <- predict(dummyVars(" ~ x1_lag06", data = x2), newdata = x2)
x5 <- predict(dummyVars(" ~ x1_lag12", data = x2), newdata = x2)

# combine dummy variable sets (current and lagged) to single data frame 
x6 <- as.tibble(cbind(x3, x4, x5)) %>% select(-contains("NA")) %>% 
  rownames_to_column(var = 'rowIndex') %>% 
  
  # transform combined dummy variable data from wide to long format
  gather(key = 'Indicator', value = 'Value', -rowIndex) %>% 
  
  # convert dummy variable to factor
  mutate(Value_fact = ifelse(Value == 1, "In", "Out"))

# assign rownames to columns in order to join return data to dummy variable data 
x7 <- x2 %>% select(date, fwd_rtn_m) %>% rownames_to_column(var = 'rowIndex')

# data for histogram plot - join return data to dummy variable data 
x8 <- full_join(x6, x7, by  = 'rowIndex') %>% 
  # rename indicator
  mutate(Indicator = str_replace(Indicator, "x1_", "ff_10 : "))

# data for kolmorogov smirnov test - list of data frames for
# each value of each (current & lagged) combined level / change factor
x8.1<-x8 %>% select(Indicator, date, Value_fact, fwd_rtn_m) %>% 
  spread(Value_fact, fwd_rtn_m) %>% nest(-Indicator)

# perform ks test, map to each element of nested dataframe
x8.2<-x8.1 %>% mutate(ks_fit = map(data, ~ks.test(.$In, .$Out)),
                      p_val  = map_dbl(ks_fit, "p.value"))

# mean return data & difference in mean for histogram text
x9 <- x8 %>% group_by(Value_fact, Indicator) %>% summarise(Mean = mean(fwd_rtn_m))
x9.1<-x9 %>% spread(Value_fact, Mean) %>% mutate(mean_diff = In - Out)


#=========================================================================================
#==   HISTOGRAM PLOT                                                                    ==
#=========================================================================================

x10<- ggplot(data = x8, aes(x = fwd_rtn_m, colour = Value_fact, fill = Value_fact)) + 
  geom_density(alpha = 0.3) + 
  geom_text(data = x9.1, size = 2.5, (aes(x = -0.25, y = 12, label = paste0("Difference in\nmean ", percent(round(mean_diff,4)), sep = " "), colour = NULL, fill = NULL)), hjust = 0) +
  geom_text(data = x8.2, size = 2.5, (aes(x = -0.25, y = 8, label = paste0("KS pvalue ", percent(round(p_val,4)), sep =" "), colour = NULL, fill = NULL)), hjust = 0) +
  geom_vline(data     = x9, aes(xintercept = Mean, colour = Value_fact),
             linetype = "dashed", size = 0.5) +
  labs(title          = "Subsequent month returns", 
       subtitle       = paste("Conditioned on binary indicator as specified for each facet.  Current values: ", x2.1[1, 1], ", ", x2.1[2, 1], " and ", x2.1[3, 1], ".", sep = ""),
       caption        = " The orange distribution represents subsequent monthly returns during\nperiods when the indicator is in the lag / level / direction specified\nby the facet title.  The blue distribution represent subsequent\nreturns during all other periods.", 
       x              = "", 
       y              = "") +
  facet_wrap(~ Indicator, ncol = 6) + 
  theme_grey() +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 10),
        plot.caption  = element_text(face = "italic", size = 8),
        axis.title.y  = element_text(face = "italic", size = 9),
        axis.title.x  = element_text(face = "italic", size = 7),
        legend.position = "none" 
  )


#=========================================================================================
#==   PLOT OF S&P500 AND MARKET IN/OUT SHADING                                          ==
#=========================================================================================

x11<-ggplot(data        = econ_fin_data, 
            aes(x        = date, 
                y        = close, 
                group    = 1)) +
  geom_line() +
  scale_y_log10() +
  geom_rect(data        = sp_shade, 
            inherit.aes = FALSE,
            aes(xmin    = start, xmax = end, ymin = 0, ymax = Inf), 
            fill        ='lightblue', alpha=0.5) +
  theme_minimal() +
  labs(title            = "S&P500", 
       subtitle         = "log scale",
       caption          = "", 
       x                = "Year",
       y                = "Close") +
  geom_hline(yintercept = 0, color = "black") +
  theme(plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(face = "italic", size = 9),
        plot.caption    = element_text(hjust = 0),
        axis.title.y    = element_text(face = "italic", size = 9),
        axis.title.x    = element_text(face = "italic", size = 9))


#=========================================================================================
#==   PLOT OF SELECTED MARKET INDICATOR & IN/OUT SHADING                                ==
#=========================================================================================

x12<-ggplot(data         = x2, 
            aes(x        = date, 
                y        = ff_10,
                group    = 1)) +
  geom_line() +
  geom_rect(data        = sp_shade, 
            inherit.aes = FALSE,
            aes(xmin    = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill        = 'lightblue', 
            alpha       = 0.5) +
  geom_hline(yintercept = 0, color = "black") +  
  theme_minimal() +
  labs(title            = "",
       subtitle         = "",
       caption          = "", 
       x                = "Year", 
       y                = "Fed Funds, 10yr treasury spread") + 
  theme(plot.title      = element_text(face  = "bold", size = 14),
        plot.subtitle   = element_text(face  = "italic", size = 9),
        plot.caption    = element_text(hjust = 0),
        axis.title.y    = element_text(face  = "italic", size = 9),
        axis.title.x    = element_text(face  = "italic", size = 9))


#=========================================================================================
#==   COMBINE PLOTS                                                                     ==
#=========================================================================================

plot_grid(x11, x12, ncol = 1, align = 'v')
plot(x10) 

