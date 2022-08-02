# load package
library(tidyverse)
library(magrittr)
library(extrafont)

# import fonts
# font_import() # only run once

# check fonts 
fonts()

# register fonts
loadfonts(device = "all")

# load data
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv') %>%
  janitor::clean_names()

# clean data
data <- data %>%
  filter(detection != "Captured") %>%
  filter(water != "Unknown water") %>%
  select(detection, water) %>%
  mutate(detection = if_else(detection == "No visual", 0, 1)) %>%
  group_by(water) %>%
  summarize(p = mean(detection),
            n = n()) %>%
  ungroup() %>%
  mutate(se = 1.96 *  sqrt((p * (1 - p)) / n)) %>%
  mutate(p_lo = p - se,
         p_hi = p + se) %>%
  mutate(p_hi = if_else(p_hi >= 1, 1, p_hi)) %>%
  mutate(water = factor(water, levels = c("No water", "Shallow water", "Deep water")))
  
  


# plot
png('~/Desktop/tt_2022_31.png', units = "in", width = 4, height = 3.4, res = 600)

ggplot(data, aes(x = water, y = p, fill = water)) +
  geom_col(width = .5) +
  # geom_errorbar(aes(ymin = p_lo, ymax = p_hi), width = .25, lwd = 1.1, color = "#ff7a6e") +
  labs(title = "Percent of Frogs Captured or Visually Detected by Amount of Water",
       subtitle = "\nTidy Tuesday 2022 - Week 31 | Data Source: USGS | Visualization: @cameronskay\n\n\n\n",
       x = "Amount of Water",
       y = "Percent Captured or Visually Detected") + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1), 
                     breaks = seq(from = 0, to = 1, by = .20)) +
  scale_fill_manual(values = c("#c7fff5",
                                "#66e8d1",
                                "#048a72")) +
    theme_minimal() %+replace% 
      theme(
        text = element_text(size = 6, family = "Avenir Next Regular", color = "#defcf7", lineheight = .5),
        legend.position = "none",
        plot.title = element_text(family = "Avenir Next Bold", size = 8, hjust = .5),
        plot.subtitle = element_text(family = "Avenir Light", size = 6, hjust = .5),
        strip.text       = element_text(family = "Avenir Next Bold", size = 6),
        axis.title = element_text(family = "Avenir Light", color = "#defcf7", size = 6),
        axis.text = element_text(family = "Avenir Light", color = "#defcf7", size = 6),
        # panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.grid      = element_line(color = "#defcf7"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        plot.background = element_rect(fill = "#111f1c"),
        panel.background = element_rect(fill = "#111f1c", color = "#111f1c"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin      = margin(.5, .8, .5, .3, "cm"))
  
dev.off()

