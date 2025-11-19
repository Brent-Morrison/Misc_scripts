library(data.table)
library(readxl)

# Compare population growth - % change
# Compare population composition change - change in % born overseas 



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
