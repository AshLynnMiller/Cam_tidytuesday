# load package
library(tidyverse)
library(magrittr)
library(extrafont)
library(countrycode)

# import fonts
# font_import() # only run once

# check fonts 
fonts()

# register fonts
loadfonts(device = "all")

# load data
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# select electricity
data %<>%
  filter(grepl("Electricity from", label)) %>%
  filter(iso3c == "USA" | iso3c == "CAN") %>%
  group_by(year, iso3c) %>%
  summarize(value_sum = sum(value)) %>%
  right_join(data, .) %>%
  ungroup() %>%
  mutate(proportion = value / value_sum) %>%
  mutate(label = gsub("Electricity from (.*) \\(TWH\\)", "\\1", label)) %>%
  mutate(label = Hmisc::capitalize(label)) %>%
  mutate(iso3c = countrycode(iso3c, origin = "iso3c", destination = "country.name")) %>%
  mutate(label = factor(label, levels = c("Hydro",
                                          "Gas",
                                          "Coal",
                                          "Oil",
                                          "Nuclear",
                                          "Wind",
                                          "Solar",
                                          "Other renewables")))

# plot
png('~/Desktop/tt_2022_29.png', units = "in", width = 4, height = 2, res = 600)

ggplot(data, aes(x = year, y = proportion, color = label)) +
  geomtextpath::geom_textline(aes(label = label), 
                              stat      = "smooth", 
                              linewidth = .5, 
                              hjust     = .8, 
                              alpha     = .60, 
                              size      = 2) +
  facet_wrap(~iso3c) +
  scale_color_manual(values = c("#4febff",
                                "#ca6bfa",
                                "#7fb1ba",
                                "#f74f62",
                                "#65f794",
                                "#e6f8fa",
                                "#fff703",
                                "#a1a1a1")) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, .70), 
                     breaks = seq(from = 0, to = .70, by = .10)) +
  labs(title   = "Energy Production",
      subtitle = "Tidy Tuesday 2022 - Week 29 | Data Source: CHAT | Visualization: @cameronskay",
      x        = "Year", 
      y        = "Percent of total production",
      color    = "") +
  theme_minimal() %+replace% 
    theme(
      text             = element_text(family = "Avenir Next Regular", size = 6, color = "white"),
      legend.position  = "none",
      legend.text      = element_text(color = "white"),
      strip.text       = element_text(size = 6, color = "white"),
      plot.title       = element_text(size = 7, family = "Avenir Next Bold", vjust = 6, hjust = 0.5),
      plot.subtitle    = element_text(size = 4, family = "Avenir Next Regular", vjust = 7, hjust = 0.5),
      plot.caption     = element_text(size = 4, vjust = -11, hjust = 1.08),
      plot.background  = element_rect(fill = "black"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "black", color = "black"),
      axis.title       = element_text(size = 6,color = "white"),
      axis.text        = element_text(size = 5, color = "white"),
      axis.text.y      = element_text(size = 5, color = "white"),
      axis.title.x     = element_text(vjust = -3),
      axis.title.y     = element_text(angle = 90, hjust = .5, vjust = 5),
      axis.line        = element_line(color = "white"),
      plot.margin      = margin(.6, .4, .5, .6, "cm"))

dev.off()

