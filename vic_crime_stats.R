library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(mgcv)

# Set custom theme
cust_theme1 <- theme_minimal() +
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

# ---------------------------------------------------------------------------------------------------------------------------
# Crime statistics analysis
# ---------------------------------------------------------------------------------------------------------------------------

# Population data
# https://www.vic.gov.au/sites/default/files/2019-08/Full-Report-Population-Diversity-in-LGAs-2016-Census-Web-version-30May18.PDF
# RAC : Regional Advisory Council
popn_data <- read.csv("vic_lga.csv")



# Retrieve crime data
# https://www.crimestatistics.vic.gov.au/crime-statistics/latest-aboriginal-crime-data
# https://files.crimestatistics.vic.gov.au/2025-06/Indigenous_Data_Tables_LGA_Family_Incidents_Visualisation_Year_Ending_March_2025.xlsx
# https://files.crimestatistics.vic.gov.au/2025-06/Data_Tables_LGA_Family_Incidents_Year_Ending_March_2025.xlsx

if (file.exists("crimestatistics_vic.csv")) {
  vic_crime_data <- read.csv("crimestatistics_vic.csv")
} else {
  url <- "https://files.crimestatistics.vic.gov.au/2025-06/Indigenous_Data_Tables_LGA_Alleged_Offenders_Visualisation_Year_Ending_March_2025.xlsx"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  crimestatistics_vic <- read_excel(path = temp, range = "Table 07!A1:F2958")
  crimestatistics_vic$retrieval_date <- Sys.Date()
  write.csv(crimestatistics_vic, "crimestatistics_vic.csv")
}


# 2016 crime by status
vic_crime_2016 <- vic_crime_data %>% 
  filter(Year == 2016) %>% 
  group_by(Local.Government.Area, Indigenous.Status) %>% 
  summarise(incidents = sum(Alleged.Offender.Incidents)) %>% 
  pivot_wider(names_from = Indigenous.Status, values_from = incidents)

# Join population and crime data by Local Government Area
vic_crime_popn_2016 <- left_join(vic_crime_2016, popn_data[popn_data$rac != "rac", ], by = join_by(Local.Government.Area == vic_lga)) %>% 
  mutate(
    region = if_else(metro_region == "na", paste0("Country - ", rac), paste(rac, metro_region, sep = " - ")),
    lga    = if_else(metro_region == "na", paste0("Country - ", Local.Government.Area), region)
    ) %>% 
  group_by(lga) %>% 
  summarise(
    atsi_off = sum(`Aboriginal and/or Torres Strait Islander`, na.rm = T), 
    ni_off   = sum(`Non-Indigenous`, na.rm = T),
    atsi_pop = sum(ind_pop_2016, na.rm = T),
    ni_pop   = sum(tot_pop_2016, na.rm = T) - sum(ind_pop_2016, na.rm = T)
    ) %>% 
  ungroup() %>% 
  mutate(
    atsi_off_rate  = round(atsi_off / atsi_pop * 100, 1),
    ni_off_rate    = round(ni_off   / ni_pop   * 100, 1),
    tot_off_rate   = round((atsi_off + ni_off) / (atsi_pop + ni_pop)   * 100, 1)
    )


# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
# lga <- st_read("data/LGA_2024_AUST_GDA2020.shp")


# # Filter for Victoria only
# vic_lga <- lga %>%
#   filter(STE_NAME21 == "Victoria")
# 
# ggplot(data = vic_lga) +
#   geom_sf(fill = "lightblue", color = "darkblue", size = 0.3) +
#   theme_minimal() +
#   labs(title = "Local Government Areas in Victoria, Australia",
#        caption = "Data source: ABS ASGS 2021") +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# 
# # Step 3: Transform to WGS84 (required for leaflet)
# vic_lga_wgs84 <- st_transform(vic_lga, crs = 4326)
# 
# # Step 4: Create the interactive leaflet map
# leaflet(vic_lga_wgs84) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(
#     fillColor = "lightblue",
#     weight = 1,
#     color = "blue",
#     fillOpacity = 0.4,
#     highlightOptions = highlightOptions(
#       weight = 2,
#       color = "black",
#       fillOpacity = 0.7,
#       bringToFront = TRUE
#     ),
#     label = ~LGA_NAME24,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "13px",
#       direction = "auto"
#     )
#   ) %>%
#   addLegend(position = "bottomright", colors = "lightblue", labels = "Victoria LGAs")






# ---------------------------------------------------------------------------------------------------------------------------
# Diversity analysis
# ---------------------------------------------------------------------------------------------------------------------------

# Data
diversity_data <- read.csv("vic_popn_diversity_table_2.csv")


# Diversity function - Shannon-Wiener index
sw_diversity <- function(counts, base = exp(1)) {
  counts <- counts[!is.na(counts) & counts > 0]
  if (length(counts) == 0) return(NA_real_)
  p <- counts / sum(counts)
  return(-sum(p * log(p, base = base)))
}

# Herfindahl-Hirschman index
hhi_diversity <- function(counts) {
  counts <- counts[!is.na(counts) & counts > 0]
  if (length(counts) == 0) return(NA_real_)
  p <- counts / sum(counts)
  return(1- sum(p^2))
}

# Max group percentage
max_diversity <- function(counts) {
  counts <- counts[!is.na(counts) & counts > 0]
  if (length(counts) == 0) return(NA_real_)
  p <- max(counts) / sum(counts)
  return(p)
}


vic_lga_diversity <- diversity_data %>%
  filter(country != "Total Overseas-born(a)") %>%
  group_by(lga, country) %>%
  summarise(count = sum(persons_2016), .groups = "drop_last") %>%
  group_by(lga) %>%
  summarise(
    shannon = sw_diversity(count),
    hhi     = hhi_diversity(count),
    max_grp = max_diversity(count),
    .groups = "drop"
  ) %>% 
  mutate(lga = paste0("Country - ", lga))

vic_lga_diversity_crime <- inner_join(vic_lga_diversity, vic_crime_popn_2016, , by = join_by(lga == lga))

plot(vic_lga_diversity_crime$max_grp, vic_lga_diversity_crime$tot_off_rate)
plot(vic_lga_diversity_crime$shannon, vic_lga_diversity_crime$ni_off_rate)
plot(vic_lga_diversity_crime$shannon, vic_lga_diversity_crime$tot_off_rate)
plot(vic_lga_diversity_crime$hhi, vic_lga_diversity_crime$ni_off_rate)

# For Theil Sen line https://stackoverflow.com/questions/48349858/how-can-i-use-theil-sen-method-with-geom-smooth
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

vic_lga_diversity_crime %>% 
  ggplot(aes(x = max_grp, y = tot_off_rate)) +
  geom_point(aes(size = ni_pop), alpha = 0.3) + #guides(size = "none") +
  stat_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE, size = 0.6, colour = 'magenta') +
  geom_smooth(method = lm, se = FALSE, linewidth = 0.6, color = "#3366FF") +
  geom_smooth(method = sen, se = FALSE, size = 0.6, colour = 'grey') +
  labs(
    title = 'Shannon diversity index versus Offending rate',
    x = 'Shannon diversity index',
    y = 'Offending rate percentage'
  ) +
  theme(
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey40", hjust = 0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  cust_theme1
