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
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') %>%
  janitor::clean_names()

# clean up code
# data %<>%
#   filter(fuel_type_code == "ELEC")

# get world data
map_data <- map_data("world") %>%
  filter(region == "Canada" | region == "USA" | region == "Mexico") %>%
  filter(long < -50)

# plot


stations_data <- data %>%
  mutate(fuel_type_code = case_when(fuel_type_code == "ELEC" ~ "Electric",
                                    fuel_type_code == "E85"  ~ "Ethanol (E85)",
                                    fuel_type_code == "LPG"  ~ "Propane",
                                    fuel_type_code == "CNG"  ~ "Natural Gas\n\n(Compressed)",
                                    fuel_type_code == "LNG"  ~ "Natural Gas\n\n(Liquefied)",
                                    fuel_type_code == "BD"   ~ "Biodiesel",
                                    fuel_type_code == "HY"   ~ "Hydrogen")) %>%
  mutate(fuel_type_code = factor(fuel_type_code, levels = c("Electric",
                                                            "Ethanol (E85)",
                                                            "Propane",
                                                            "Natural Gas\n\n(Compressed)",
                                                            "Natural Gas\n\n(Liquefied)",
                                                            "Biodiesel",
                                                            "Hydrogen"))) %>%
  mutate(status_code = case_when(status_code == "E" ~ "Available",
                                 status_code == "P" ~ "Planned",
                                 status_code == "T" ~ "Temporarily Unavailable"))



png('~/Desktop/tt_2022_30.png', units = "in", width = 8, height = 3.4, res = 600)

ggplot() +
  # plot canada / usa
  geom_map(data = filter(map_data, region == "USA"), 
           fill = "#000000", map = filter(map_data, region == "USA"), 
           aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  # geom_map(data = filter(map_data, region != "USA"), fill = "#aaaaaa", map = filter(map_data, region != "USA"), aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  geom_point(data = stations_data, 
             aes(x = longitude, y = latitude, color = fuel_type_code), 
             shape = 16, alpha = .8, size = .05) +
  facet_wrap(~status_code) + #available vs planned vs temp unavailable
  labs(title = "Alternative Fuel Stations",
       subtitle = "\nTidy Tuesday 2022 - Week 30 | Data Source: US DOT | Visualization: @cameronskay\n\n\n\n") + 
  # scale_color_manual(values = c("#d9fdff", "#e5baff", "red")) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(
                  xlim = c(-128, -65),
                  ylim = c(24.5, 50)) + 
  #coloring by fuel type (electric, ethanol, propane, natural gas + compressed, natural gas + liquefied, biodiesel, and hydrogen)
  scale_color_manual(values = c("#4febff",
                                "#9605f7",
                                "#ff7b00",
                                "#ff3370",
                                "#f598b4",
                                "#65f794",
                                "#2f43f7")) +
    theme_minimal() %+replace% 
      #Cam's theme
      theme(
        text = element_text(size = 9, family = "Avenir Next Regular", color = "white", lineheight = .5),
        plot.title = element_text(family = "Avenir Next Bold", size = 12, hjust = .5),
        plot.subtitle = element_text(family = "Avenir Light", size = 8, hjust = .5),
        strip.text       = element_text(family = "Avenir Next Bold", size = 9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.text = element_text(size = 8, family = "Avenir Light"),
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.background = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, 0, 0, 0),
        plot.background = element_rect(fill = "#191e21"),
        panel.background = element_rect(fill = "#191e21", color = "#191e21"),
        panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text       = element_blank(),
        axis.ticks =    element_blank(),
        legend.key.width = unit(2.6, 'cm'),
        plot.margin      = margin(.5, .1, .5, .1, "cm")) +
      guides(color = guide_legend(label.position = "top",
                                label.vjust = 0,
                                nrow = 1,
                                override.aes = list(size = 3, shape = 15)))
  
dev.off()

png('~/Desktop/tt_2022_30_v2.png', units = "in", width = 8, height = 3.4, res = 600)

ggplot() +
  # plot canada / usa
  geom_map(data = filter(map_data, region == "USA"), fill = "#000000", map = filter(map_data, region == "USA"), aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  # geom_map(data = filter(map_data, region != "USA"), fill = "#aaaaaa", map = filter(map_data, region != "USA"), aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  geom_point(data = stations_data, aes(x = longitude, y = latitude, color = fuel_type_code), shape = 16, alpha = .8, size = .05) +
  facet_wrap(~status_code) +
  labs(title = "Alternative Fuel Stations",
       subtitle = "\nTidy Tuesday 2022 - Week 30 | Data Source: US DOT | Visualization: @cameronskay\n\n\n\n") + 
  # scale_color_manual(values = c("#d9fdff", "#e5baff", "red")) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = 2163,
                  xlim = c(-128, -65),
                  ylim = c(24.5, 50)) + 
  scale_color_manual(values = c("#4febff",
                                "#9605f7",
                                "#ff7b00",
                                "#ff3370",
                                "#f598b4",
                                "#65f794",
                                "#2f43f7")) +
    theme_minimal() %+replace% 
      theme(
        text = element_text(size = 9, family = "Avenir Next Regular", color = "white", lineheight = .5),
        plot.title = element_text(family = "Avenir Next Bold", size = 12, hjust = .5),
        plot.subtitle = element_text(family = "Avenir Light", size = 8, hjust = .5),
        strip.text       = element_text(family = "Avenir Next Bold", size = 9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.text = element_text(size = 8, family = "Avenir Light"),
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.background = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, 0, 0, 0),
        plot.background = element_rect(fill = "#191e21"),
        panel.background = element_rect(fill = "#191e21", color = "#191e21"),
        panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text       = element_blank(),
        axis.ticks =    element_blank(),
        legend.key.width = unit(2.6, 'cm'),
        plot.margin      = margin(.5, .1, .5, .1, "cm")) +
      guides(color = guide_legend(label.position = "top",
                                label.vjust = 0,
                                nrow = 1,
                                override.aes = list(size = 3, shape = 15)))
  
  


dev.off()

