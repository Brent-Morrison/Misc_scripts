library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


# Population data
# https://www.vic.gov.au/sites/default/files/2019-08/Full-Report-Population-Diversity-in-LGAs-2016-Census-Web-version-30May18.PDF
popn_data <- read.csv("vic_lga.csv")



# Crime data
if (file.exists("crimestatistics_vic.csv")) {
  crimestatistics_vic <- read.csv(crimestatistics_vic, "crimestatistics_vic.csv")
} else {
  url <- "https://files.crimestatistics.vic.gov.au/2025-06/Indigenous_Data_Tables_LGA_Alleged_Offenders_Visualisation_Year_Ending_March_2025.xlsx"
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  crimestatistics_vic <- read_excel(path = temp, range = "Table 07!A1:F2958")
  crimestatistics_vic$retrieval_date <- Sys.Date()
  write.csv(crimestatistics_vic, "crimestatistics_vic.csv")
}

crime_2016 <- crimestatistics_vic %>% 
  filter(Year == 2016) %>% 
  group_by(`Local Government Area`, `Indigenous Status`) %>% 
  summarise(incidents = sum(`Alleged Offender Incidents`)) %>% 
  pivot_wider(names_from = `Indigenous Status`, values_from = incidents)

z <- left_join(crime_2016, popn_data[popn_data$rac != "rac", ], by = join_by(`Local Government Area` == vic_lga)) %>% 
  mutate(region = if_else(metro_region == "na", paste0("Country - ", rac), paste(rac, metro_region, sep = " - "))) %>% 
  group_by(region) %>% 
  summarise(atsi = sum(`Aboriginal and/or Torres Strait Islander`, na.rm = T), ni = sum(`Non-Indigenous`, na.rm = T))

# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
lga <- st_read("data/LGA_2024_AUST_GDA2020.shp")


# Filter for Victoria only
vic_lga <- lga %>%
  filter(STE_NAME21 == "Victoria")

ggplot(data = vic_lga) +
  geom_sf(fill = "lightblue", color = "darkblue", size = 0.3) +
  theme_minimal() +
  labs(title = "Local Government Areas in Victoria, Australia",
       caption = "Data source: ABS ASGS 2021") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# Step 3: Transform to WGS84 (required for leaflet)
vic_lga_wgs84 <- st_transform(vic_lga, crs = 4326)

# Step 4: Create the interactive leaflet map
leaflet(vic_lga_wgs84) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = "lightblue",
    weight = 1,
    color = "blue",
    fillOpacity = 0.4,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~LGA_NAME24,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(position = "bottomright", colors = "lightblue", labels = "Victoria LGAs")
