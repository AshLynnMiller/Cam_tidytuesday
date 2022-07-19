# load package
library(tidyverse)
library(airportr)
library(gganimate)

# load data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>%
  janitor::clean_names() %>%
  select(year,
         flt_date,
         month_num,
         state_name,
         apt_icao,
         flt_tot_1) %>%
  mutate(flt_tot_1 = flt_tot_1 / max(flt_tot_1))

airports <- airports %>%
  janitor::clean_names() %>%
  select(apt_icao = icao,
         country,
         city,
         longitude,
         latitude,
         altitude)

world <- map_data("world") %>%
  filter(between(long, -35, 50) & between(lat, 20, 73)) %>%
  filter(region != "Greenland")

# join data
flights <- flights %>%
  left_join(airports, by = "apt_icao")

flights_mini <- flights %>%
  filter(year == 2020) %>%
  filter(city != "Svalbard") %>%
  filter(parse_number(month_num) <= 5) %>%
  filter(parse_number(month_num) >= 2)

# plot
plot <- ggplot(data = flights_mini, aes(x = longitude, y = latitude, size = flt_tot_1)) +
  geom_map(data = world, map = world, fill = "#5797b3", aes(long, lat, map_id = region, size = NULL, alpha = NULL)) +
  geom_point(color = "white", shape = 16, alpha = .8) +
  scale_size_area(max_size = 3) +
  transition_manual(flt_date) +
  labs(title    = "Flights in Europe",
       subtitle = "\n\nFebruary 2020 to May 2020\n\nDate: {current_frame}",
       caption  = "Tidy Tuesday 2022 - Week 28 | Data Source: Eurocontrol | Visualization: @cameronskay") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() %+replace% 
      theme(
        text = element_text(size = 11.50, family = "Avenir", color = "white", lineheight=.8),
        plot.title = element_text(size = 11.50, hjust = 0, lineheight=1),
        plot.subtitle = element_text(size = 7, hjust = 0),
        plot.caption = element_text(size = 4.4, hjust = 1),
        legend.position = "none",
        plot.background = element_rect(fill = "#01141c"),
        panel.background = element_rect(fill = "#01141c", color = "#01141c"),
        panel.grid      = element_blank(),
        axis.title      = element_blank(),
        axis.text       = element_blank())

animate(plot, 
        duration = 11,
        detail   = 1,
        fps = 11,
        height = 2, 
        width = 3, 
        units = "in", 
        res = 300)
