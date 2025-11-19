library(plotly)

plotly_data <- data.frame(
  source = c(0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , 2),
  source_desc = c(rep("Operating", 4) , rep("Admin", 4) , rep("Employee", 4)), 
  inter1_desc = rep(c("North", "South"), 6),
  target = c(3 , 4 , 5 , 6 , 3 , 4 , 5 , 6 , 3 , 4 , 5 , 6),
  target_desc = rep(c("Water", "Sewer", "Recycled" ,"Rural"), times = 3),
  value =  c(15, 15, 15, 15, 5 , 10, 3 , 2 , 15, 5 , 5 , 5)
)

unique(c(plotly_data$source_desc, plotly_data$target_desc))

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    #src/tgt   0             1         2            3        4        5           6       
    #label = c("Operating" , "Admin" , "Employee" , "Water", "Sewer", "Recycled" ,"Rural"),
    label = unique(c(plotly_data$source_desc, plotly_data$target_desc)),
    color = c("blue"      , "blue"  , "blue"     , "blue" , "blue" , "blue"     , "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    # source = c(0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 2 , 2 , 2 , 2),
    # target = c(3 , 4 , 5 , 6 , 3 , 4 , 5 , 6 , 3 , 4 , 5 , 6),
    # value =  c(15, 15, 15, 15, 5 , 10, 3 , 2 , 15, 5 , 5 , 5)
    source = plotly_data$source,
    target = plotly_data$target,
    value = plotly_data$value
  )
)

fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)

fig

htmlwidgets::saveWidget(
  widget = fig, #the plotly object
  file = "sankey_example.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)


# -------------------------------------------------------------------------------------------------

library(ggplot2)
library(ggalluvial)

#  ggalluvial WIDE data
ggplot(data = plotly_data,
       aes(axis1 = source_desc, axis2 = inter1_desc, axis3 = target_desc,
           y = value)) +
  geom_alluvium(aes(fill = source_desc)) +
  scale_x_discrete(limits = c("Source", "Service"), expand = c(0.2, 0.05)) +
  xlab("Spread") + 
  geom_alluvium() +
  geom_stratum(aes(fill = source_desc)) +
  #stat_stratum(decreasing = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Expense mapping",
          "Version 1")


#  ggalluvial LONG data
plotly_long <- plotly_data %>% 
  select(source_desc, inter1_desc, target_desc, value) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(
    cols = ends_with("desc"),  #c("source_desc", "target_desc"), 
    names_to = "axis",
    values_to = "stratum"
  )


ggplot(
  plotly_long, 
  aes(
    x        = axis, 
    stratum  = stratum, 
    alluvium = id,
    label    = stratum
    )
  ) +
  geom_flow(aes(fill = stratum), stat = "alluvium") +
  geom_stratum(aes(fill = stratum)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(expand = c(0.1, 0)) +
  labs(x = "Class system", y = "n")







# --------------------------------------------------------------------------------------------------------------------------
# Example per SO https://stackoverflow.com/questions/71175133/ggalluvial-geom-flow-fill-color-from-destination-stratum
library(dplyr)
library(tidyr)
data <- tibble(
  id = paste0("S", 1:20), 
  class_1 = c(rep("A", 10), rep("B", 10)),
  class_2 = c(rep("A", 8), rep("B", 8), rep("A", 4)))

data_pvt <- data %>%
  pivot_longer(starts_with("class"), names_to = "class_type", values_to = "class_label") %>%
  mutate(class_type = factor(class_type),
         class_label = factor(class_label))




# --------------------------------------------------------------------------------------------------------------------------
# Demonstrate difference between wide and long data 

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggalluvial)

titanic_wide <- data.frame(Titanic)

titanic_long <- to_lodes_form(titanic_wide,
                              key = "Demographic",
                              axes = 1:3)

titanic_pvt <- pivot_longer(titanic_wide,
                              key = "Demographic",
                              axes = 1:3)
