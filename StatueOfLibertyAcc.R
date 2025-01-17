library(tidyverse)
library(magrittr)
library(rio)
library(here)
library(extrafont)
library(countrycode)


# import fonts
# font_import() # only run once

# check fonts 
fonts()

# register fonts
loadfonts(device = "all")

# load data
karina_data <- import(here::here("data", "HS GD Dist.xlsx"),
                      setclass = "tibble") %>%
  mutate(ID = as.factor(ID)) %>%
  janitor::clean_names() %>%
  mutate(holly_sign_acc = recode(holly_sign_acc,
                                 "0" = "Correct",
                                 "1" = "Incorrect"))

karina_data %<>%
  mutate(statof_lib_acc = recode(statof_lib_acc,
                                 "0" = "Correct",
                                 "1" = "Incorrect"))

karina_data %<>%
  mutate(holly_sign_acc = as.factor(holly_sign_acc),
         statof_lib_acc = as.factor(statof_lib_acc))

karina_data %<>%
  mutate(holly_sign_acc = factor(holly_sign_acc, levels = c("Incorrect",
                                                            "Correct"))) 

karina_data %<>%
  mutate(statof_lib_acc = factor(statof_lib_acc, levels = c("Incorrect",
                                                            "Correct"))) 

liberty_data <- import(here::here("data", "StatOfLib data.xlsx"),
                       setclass = "tibble") %>%
  mutate(ID = as.factor(ID)) %>%
  janitor::clean_names()


map_data <- map_data("world") %>%
  filter(region == "USA") %>%
  filter(long < -50)

ggplot() +
  # plot canada / usa
  geom_map(data = map_data, fill = "white", color = "black", map = map_data, 
           aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  # geom_map(data = filter(map_data, region != "USA"), fill = "#aaaaaa", map = filter(map_data, region != "USA"), aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  geom_point(data = karina_data, 
             aes(x = longitude, y = latitude, color = statof_lib_acc), 
             shape = 16, size = 1.5) +
  geom_point(data = liberty_data,
             aes(x = long, y = lat),
             color = "#ffd34e",
             shape = 16, 
             alpha = .8, 
             size = 1) +
  labs(title = "Statue of Liberty") + 
  # scale_color_manual(values = c("#d9fdff", "#e5baff", "red")) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(
    xlim = c(-128, -65),
    ylim = c(24.5, 50)) + 
  #coloring by fuel type (electric, ethanol, propane, natural gas + compressed, natural gas + liquefied, biodiesel, and hydrogen)
  scale_color_manual(values = c("#ff3370", 
                                "#3EE09F")) +
  theme_minimal() %+replace% 
  #Cam's theme
  theme(
    text = element_text(size = 9, family = "Avenir Next Regular", color = "black", lineheight = .5),
    plot.title = element_text(family = "Avenir Next Bold", size = 13, hjust = .5),
    plot.subtitle = element_text(family = "Avenir Light", size = 8, hjust = .5),
    strip.text       = element_text(family = "Avenir Next Bold", size = 9),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing.x = unit(.5, "lines"),
    panel.spacing.y = unit(0, "lines"),
    legend.text = element_text(size = 11.5, family = "Avenir Light"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, 0, 0),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
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

png('~/Desktop/LibertyAcc_v3.png', units = "in", width = 7, height = 5, res = 600)
