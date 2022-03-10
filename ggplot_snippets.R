# Base R facet grid
# data needs to be in a long format
dat <- data.frame(
  position = c(1,2,3,2,3,5,2,3,10),
  score = c(450,220,330,333,423,988,333,423,988),
  z = c('a','a','a','b','b','b','c','c','c')         # grouping variable
)

facet_wrap <- function(data, x, y, z, horiz = TRUE, ...) {
  # save current par settings and return after finished
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  zz <- unique(data[, z])
  
  # sets up the layout to cascade horizontally or vertically
  # and sets xlim and ylim appropriately
  if (horiz) {
    par(mfrow = c(1, length(zz)), ...)
    ylim <- range(data[, y])
    xlim <- NULL
  } else {
    par(mfrow = c(length(zz), 1), ...)
    xlim <- range(data[, x])
    ylim <- NULL
  }
  
  # make a subset of data for each unique by variable
  # and draw a basic plot for each one
  for (ii in zz) {
    tmp <- data[data[, z] %in% ii, ]
    plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylim)
  }
}

facet_wrap(dat, 'position', 'score', 'z', horiz = FALSE)



# ggplot ------------------------------------------------------------------------------------------

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



