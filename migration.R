library(data.table)
library(ggplot2)
#library(readxl)
#jgleeson <- read.csv("https://raw.githubusercontent.com/jgleeson/PublicHouse/refs/heads/main/dataset.csv")

# Compare population growth - % change
# Compare population composition change - change in % born overseas 
# Calculate rate of increase in house stock
# Use the Global Slavery Index

data_source_ref_raw <- read.csv("data_source_ref.csv") 
data_series_ref_raw <- read.csv("data_series_ref.csv") 
data_raw <- read.csv("data.csv") 

# Set DT
data_dt <- as.data.table(data_raw)

# Country categorisation
anglo <- c("aus","gbr","can","irl","usa")
devel_ex_anglo <- data_dt[series == "hdi" & !country %in% anglo][order(-value)][1:20]$country


# --------------------------------------------------------------------------------------------------------------------------
# Analysis of housing stock and growth data
# --------------------------------------------------------------------------------------------------------------------------

# Interpolated housing stock data 
dwelling_popn_data <- dcast(
  data_dt[series %in% c("housing_stock", "popn_mn"), .(value = mean(value)), by = .(country, series, year)],
  country + year ~ series,
  value.var = "value"
  ) |>
  _[CJ(country = unique(housing_stock_data$country), year = min(housing_stock_data$year):max(housing_stock_data$year)), on = .(country, year)] |>
  _[order(country, year)] |>
  _[!is.na(country)] |>
  _[, country_cat := fcase(
    country %in% c("aus","irl") , country,
    country %in% anglo          , "anglo",
    country %in% devel_ex_anglo , "devel_ex_anglo", 
    default = "other"
  )] |>
  _[, dwelling_int := approx(year, housing_stock, xout = year, rule = 2)$y, by = country] |>
  _[, dwelling_spl := spline(year, housing_stock, xout = year)$y, by = country] |>
  _[, five_yr_cagr := round((((dwelling_int / shift(dwelling_int, 5)) ^ (1 / 5)) - 1) * 100, 2), by = country] |>
  _[, five_yr_avg_delta := (dwelling_int - shift(dwelling_int, 5)) / 5, by = country] |>
  _[, five_yr_avg_popn := frollmean(popn_mn,, algo = "exact" , n = 5, align = "right"), by = country] |>
  _[, dwelling_per_popn_mn := five_yr_avg_delta / five_yr_avg_popn]

# Plots
barplot(dwelling_popn_data[year == 2020 & !is.na(housing_stock), dwelling_per_popn_mn], names.arg = dwelling_popn_data[year == 2020 & !is.na(housing_stock), country])

dwelling_popn_data[year %in% c(2020,2015,2010,2005,2000,1995,1990,1985,1980) & !is.na(dwelling_per_popn_mn)] |>
  ggplot(aes(x = dwelling_per_popn_mn, y = country)) + geom_col() + facet_wrap(~year) + theme_bw()

dwelling_popn_data[year %in% c(2020,2015,2010,2005) & !is.na(housing_stock)] |>
  ggplot(aes(x = five_yr_cagr, y = log(five_yr_avg_popn))) + geom_point() + facet_wrap(~year)

# Interpolated housing stock data 
housing_stock_data <- data_dt[series == "housing_stock"] |> _[, .(housing_stock = mean(value)), by = .(country, year)]

housing_stock_inp <- housing_stock_data |>
  _[CJ(country = unique(housing_stock_data$country), year = min(housing_stock_data$year):max(housing_stock_data$year)), on = .(country, year)] |>
  _[order(country, year)] |>
  _[!is.na(country)] |>
  _[, country_cat := fcase(
    country %in% c("aus","irl") , country,
    country %in% anglo          , "anglo",
    country %in% devel_ex_anglo , "devel_ex_anglo", 
    default = "other"
  )] |>
  _[, value_int := approx(year, housing_stock, xout = year, rule = 2)$y, by = country] |>
  _[, value_spl := spline(year, housing_stock, xout = year)$y, by = country] |>
  _[, five_yr_cagr := round((((value_int / shift(value_int, 5)) ^ (1 / 5)) - 1) * 100, 2), by = country] |>
  _[, five_yr_avg_delta := (value_int - shift(value_int, 5)) / 5, by = country] |>
  _[, five_yr_avg_popn := frollmean(popn_mn, n = 5, align = "right"), by = country]


housing_stock_catg <- housing_stock_inp |> 
  _[, .(housing_stock = sum(value_int)), by = .(country_cat, year)] |>
  _[, five_yr_cagr := round((((housing_stock / shift(housing_stock, 5)) ^ (1 / 5)) - 1) * 100, 2), by = country_cat]


housing_stock_catg[year >= 1975 & year <= 2023] |>
  ggplot(aes(x = year, y = five_yr_cagr)) +   # linetype = country_cat, colour = country_cat
  geom_line() +
  facet_wrap(~country_cat)





# --------------------------------------------------------------------------------------------------------------------------
# Analysis of population growth
# --------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(slider)
top_hdi  <- data_raw %>% left_join(data_series_ref_raw, by = c("series")) %>% filter(description == "Human development Index") %>% slice_max(order_by = value, n = 50)
popn5    <- data_raw %>% left_join(data_series_ref_raw, by = c("series")) %>% filter(description == "Population (mn)", year == 1990, value >= 3)
perc_os  <- data_raw %>% left_join(data_series_ref_raw, by = c("series")) %>% filter(description == "Percentage of population born overseas")
popn_top_hdi <- data_raw %>% 
  left_join(data_series_ref_raw, by = c("series")) %>% 
  filter(description == "Population (mn)", country_code %in% top_hdi$country_code, country_code %in% popn5$country_code) %>% 
  arrange(country, year) %>% 
  group_by(country_code) %>% 
  mutate(
    popn_growth            = round((value / lag(value) - 1) * 100, 2),
    five_yr_popn_growth    = round(slide_dbl(popn_growth, ~mean(.x), .before = 4), 2),
    five_yr_popn_cagr      = round((((value / lag(value, 5)) ^ (1 / 5)) - 1) * 100, 2),
    twenty_yr_popn_growth  = round(slide_dbl(popn_growth, ~mean(.x), .before = 19), 2),
    twenty_yr_popn_cagr    = round((((value / lag(value, 20)) ^ (1 / 20)) - 1) * 100, 2)
    ) %>% 
  ungroup() %>% 
  left_join(perc_os, by = c("year", "country_code"))
  
  




# --------------------------------------------------------------------------------------------------------------------------

library(tidyr)
library(dplyr)
data_raw <- read.csv("temp.csv")
relg <- data_raw %>% 
  select(c(2,3,5:11)) %>% 
  pivot_longer(cols = !c("Country","Year"))
write.csv(relg, "relg.csv")




# --------------------------------------------------------------------------------------------------------------------------

# Our World In Data retrieval
migrant_share_df <- read.csv("https://ourworldindata.org/grapher/migrant-stock-share.csv")
pop_df <- read.csv("https://ourworldindata.org/grapher/population-with-un-projections.csv")


# Convert to data.table
migrant_share_dt <- as.data.table(migrant_share_df)
pop_dt <- as.data.table(pop_df)

# Join data
dat <- merge(pop_dt, migrant_share_dt, by = c("Code","Entity","Year")) |>
  _[Code != ""] |>
  #_[setnames(., old = "Share.of.the.population.that.was.born.in.another.country", new = "frgn_perc")] |>
  _[, popn := pmax(Population...Sex..all...Age..all...Variant..medium, Population...Sex..all...Age..all...Variant..estimates, na.rm = TRUE) / 1e6 ] |>
  _[, frgn_perc := Share.of.the.population.that.was.born.in.another.country / 100 ] |>
  _[, frgn_popn := popn * frgn_perc ] |>
  _[, .(Code, Entity, Year, popn, frgn_perc, frgn_popn)] |>
  _[, open := shift(popn, n = 1, fill = NA, type = "lag"), by = Code ] |>
  _[, nom := frgn_popn - shift(frgn_popn, n = 1, fill = NA, type = "lag"), by = Code ] |>
  _[, bd := popn - nom - open ] |>
  _[, perc_nom := nom / (nom + bd) ] |>
  _[, popn_growth_perc_nom := popn / open - 1 ]


# ABS retrieval
# https://www.abs.gov.au/statistics/people/population/historical-population/2021/HPDC1.xlsx 
if (file.exists("aus_popn_hist.csv")) {
  aus_popn_hist <- read.csv("aus_popn_hist.csv")
} else {
  url <- "https://www.abs.gov.au/statistics/people/population/historical-population/2021/HPDC1.xlsx"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  aus_popn_hist <- read_excel(path = temp, range = "Table 3!B5:BA110")
  aus_popn_hist$retrieval_date <- Sys.Date()
  write.csv(aus_popn_hist, "aus_popn_hist.csv")
}

aus_popn_summary <- as.data.frame(t(aus_popn_hist[97:105, ]))


# https://www.abs.gov.au/media-centre/media-releases/9-facts-about-australias-overseas-born-population
# Proportion of Australia's population born overseas(a)(b).csv
# a. Prior to 1992 based on Census counts. Data for 1992 to 2024 uses estimated resident population.
# b. Population estimates for 2024 are preliminary. See Revision status in the methodology page.
# Source prior to 1992: Historical population publication

aus_os_born <- read.csv("aus_perc_popn_born_os.csv")


data <- read.csv("data.csv")
data_series_ref <- read.csv("data_series_ref.csv")
data_source_ref <- read.csv("data_source_ref.csv")
