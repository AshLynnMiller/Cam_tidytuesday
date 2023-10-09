library(tidyverse)
library(magrittr)
library(rio)
library(here)
library(extrafont)
library(countrycode)
library(usmap)
library(maps)
library(mapdata)

# import fonts
# font_import() # only run once

# check fonts 
fonts()

# register fonts
loadfonts(device = "all")

# load data
NY_data <- import(here::here("data", "NY data.xlsx"),
                  setclass = "tibble") %>%
  mutate(ID = as.factor(ID)) %>%
  janitor::clean_names() %>%
  mutate(holly_sign_acc = recode(holly_sign_acc,
                                 "0" = "Correct",
                                 "1" = "Incorrect"))

statlib_data <- import(here::here("data", "StatOfLib data.xlsx"),
                  setclass = "tibble") %>%
  mutate(ID = as.factor(ID)) %>%
  janitor::clean_names()

NY_data %<>%
  mutate(statof_lib_acc = recode(statof_lib_acc,
                                 "0" = "Correct",
                                 "1" = "Incorrect"))

NY_data %<>%
  mutate(holly_sign_acc = as.factor(holly_sign_acc),
         statof_lib_acc = as.factor(statof_lib_acc))

NY_data %<>%
  mutate(holly_sign_acc = factor(holly_sign_acc, levels = c("Incorrect",
                                                            "Correct"))) 

NY_data %<>%
  mutate(statof_lib_acc = factor(statof_lib_acc, levels = c("Incorrect",
                                                            "Correct"))) 

# map_data <- map_data("world") %>%
#   filter(region == "USA") %>%
#   #filter(subregion == "New York") %>%
#   #filter(long < -50)

#usa <- map_data("usa")

# p <- ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
#   geom_polygon(fill='lightblue') + 
#   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
#   ggtitle('U.S. Map') + 
#   coord_fixed(1.3)
# 
# show(p) #copies to desktop

state <- map_data("state")
NY <- subset(state, region == "new york")


ggplot() + 
  geom_map(data = NY, map = NY, fill = "white", color = "black",
           aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  geom_point(data = NY_data, 
           aes(x = long, y = lat, color = statof_lib_acc), 
           shape = 16, 
           size = 1.25) +
  geom_point(data = statlib_data,
             aes(x = long, y = lat),
             color = "#ffd34e",
             shape = 16, 
             alpha = .8, 
             size = 1) +
  labs(title = "Statue of Liberty") +
  coord_cartesian(
    xlim = c(-79.77, -71.88),
    ylim = c(40.49, 45.05)) + 
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

png('~/Desktop/NY_StatueOfLib_v2.png', 
    units = "in", 
    width = 7, 
    height = 5, 
    res = 600)
