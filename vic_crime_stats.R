library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readxl)

# Population data
# https://www.vic.gov.au/sites/default/files/2019-08/Full-Report-Population-Diversity-in-LGAs-2016-Census-Web-version-30May18.PDF

url <- "https://files.crimestatistics.vic.gov.au/2025-06/Indigenous_Data_Tables_LGA_Alleged_Offenders_Visualisation_Year_Ending_March_2025.xlsx"
temp <- tempfile()
download.file(url, temp, mode = "wb")
data <- read_excel(path = temp, range = "Table 07!A1:F2958")

# Optional: install.packages("sf") and others if not installed

# Step 1: Download the LGA shapefile from the ABS
# You can manually download from: 
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/digital-boundary-files
# File: "ASGS Edition 3, July 2021 to June 2026 - LGA boundaries - ESRI Shapefile"

# Assume you've extracted it to "data/LGA_2021_AUST.shp"
# Adjust the path as needed
lga <- st_read("data/LGA_2024_AUST_GDA2020.shp")

# Step 2: Filter for Victoria only
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
