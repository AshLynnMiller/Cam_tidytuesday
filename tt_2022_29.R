# load package
library(tidyverse)
library(magrittr)
library(extrafont)

# import fonts
# font_import() # only run once

# check fonts 
fonts()

# register fonts
loadfonts(device = "all", quiet = TRUE)

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
  mutate(iso3c = if_else(iso3c == "CAN", "Canada", iso3c)) %>%
  mutate(label = factor(label, levels = c("Hydro",
                                          "Gas",
                                          "Coal",
                                          "Oil",
                                          "Nuclear",
                                          "Wind",
                                          "Solar",
                                          "Other renewables")))

# plot
ggplot(data, aes(x = year, y = proportion, color = label)) +
  # geom_smooth(se = FALSE, alpha = .80) +
  geomtextpath::geom_textline(aes(label = label), stat = "smooth", 
                linewidth = 1.5, hjust = .8, alpha = .60) +
  facet_wrap(~iso3c) +
  scale_color_manual(values = c("#4febff",
                                "#ca6bfa",
                                "#7fb1ba",
                                "#f74f62",
                                "#65f794",
                                "#e6f8fa",
                                "#fff703",
                                "#a1a1a1")) +
  # scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::percent, limits = c(0, .70), breaks = seq(from = 0, to = .70, by = .10)) +
  labs(title  = "Energy Production",
      caption = "Tidy Tuesday 2022 - Week 29 | Data Source: CHAT | Visualization: @cameronskay",
      x       = "Year", 
      y       = "Percent of Total Production",
      color   = "") +
   # geom_label(aes(label = label),
   #            data = filter(data, year == 2000),
   #            size = 4) +
  theme_minimal() %+replace% 
    theme(
      
      legend.position = "none",
      text = element_text(size = 15, color = "white"),
      legend.text = element_text(color = "white"),
      strip.text = element_text(color = "white"),
      plot.title      = element_text(hjust = 0.5, size = 20),
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black", color = "black"),
      panel.grid      = element_blank(),
      axis.title      = element_text(size = 15,color = "white"),
      axis.text       =  element_text(size = 10, color = "white"),
      axis.line       =  element_line(color = "white"),
      plot.caption    =  element_text())

