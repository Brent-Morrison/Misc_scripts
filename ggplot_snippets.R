library("tidyverse")

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# Basic count plot
nuclear_explosions %>% 
  mutate(region = fct_lump(region %>% as.factor, n = 15)) %>% 
  count(region) %>%  
  ggplot(aes(x = reorder(region, n), y = n)) +
  geom_col() + 
  coord_flip() 

# Count with color
nuclear_explosions %>% 
  group_by(type, country) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(type = fct_lump(type %>% as.factor, n = 5)) %>% 
  ggplot(aes(x = reorder(type, count), y = count, fill = country)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer() +
  coord_flip()


