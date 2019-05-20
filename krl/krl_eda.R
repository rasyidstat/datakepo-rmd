library(tidyverse)
library(nusantr)
library(sf)
library(mrsq)
library(ggpubr)
library(mapview)


# prep --------------------------------------------------------------------
krl_route_split <- read_rds("data/krl_route_split.rds")
krl_route_split_all <- krl_route_split %>% 
  unnest() %>% 
  st_as_sf(crs = 4326)
krl_station <- st_as_sf(krl, coords = c("longitude", "latitude"), crs = 4326) %>% 
  unnest()


# comparison --------------------------------------------------------------
after <- krl_route_split_all %>% 
  filter(schedule_id == "idjkt_TANGERANG LINE") %>% 
  ggplot() +
  geom_sf(aes(color = line_name1)) +
  geom_sf(data = krl_station %>% 
            filter(schedule_id == "idjkt_TANGERANG LINE"),
          size = 2,
          color = "grey") +
  geom_sf(data = krl_station %>% 
            filter(schedule_id == "idjkt_TANGERANG LINE"),
          size = 1,
          color = "white") +
  ggrepel::geom_text_repel(data = krl_station %>%
                             filter(schedule_id == "idjkt_TANGERANG LINE"),
                           aes(geometry = geometry,
                               label = station_name),
                           stat = "sf_coordinates",
                           family = "Nunito",
                           size = 2.5) +
  coord_sf(datum = NA) +
  guides(color = FALSE) +
  theme_nunito() +
  labs(x = NULL, y = NULL)
before <- krl_route %>% 
  filter(schedule_id == "idjkt_TANGERANG LINE",
         is_main) %>% 
  ggplot() +
  geom_sf(aes(color = line_name)) +
  geom_sf(data = krl_station %>% 
            filter(schedule_id == "idjkt_TANGERANG LINE"),
          size = 2,
          color = "grey") +
  geom_sf(data = krl_station %>% 
            filter(schedule_id == "idjkt_TANGERANG LINE"),
          size = 1,
          color = "white") +
  ggrepel::geom_text_repel(data = krl_station %>%
                             filter(schedule_id == "idjkt_TANGERANG LINE"),
                           aes(geometry = geometry,
                               label = station_name),
                           stat = "sf_coordinates",
                           family = "Nunito",
                           size = 2.5) +
  coord_sf(datum = NA) +
  guides(color = FALSE) +
  theme_nunito() +
  labs(x = NULL, y = NULL)
ggarrange(before, after, nrow = 2, 
          labels = c("Before", "After"), 
          font.label = list(family = "Nunito"))
ggsave("figs/before_after.png", width = 9, height = 3, bg = "transparent")


# quick eda ---------------------------------------------------------------
krl_route_main <- krl_route %>% 
  filter(is_main) %>% 
  mutate(d = as.numeric(st_length(geometry)) * 10^-3)

# how many routes?
nrow(krl_route)

# viz top
krl_color <- paste0("#", krl_route_main$line_color)
names(krl_color) <- krl_route_main$line_id

krl_route_main %>%
  ggplot(aes(reorder(route_name, d), d, fill = line_id)) +
  geom_col() +
  coord_flip() +
  theme_nunito(grid = "") +
  labs(x = NULL, y = NULL) +
  guides(fill = FALSE) +
  scale_fill_manual(values = krl_color) +
  geom_text(data = krl_route_main %>% 
              filter(d >= 18),
            aes(label = paste(round(d, 2), "km")),
            color = "white",
            family = "Neo Sans Pro",
            hjust = 1.2) +
  geom_text(data = krl_route_main %>% 
              filter(d < 18),
            aes(label = paste(round(d, 2), "km")),
            color = "black",
            family = "Neo Sans Pro",
            hjust = -0.2) +
  theme(axis.text.x = element_blank())
ggsave("figs/longest_route.png", width = 8, height = 6, bg = "transparent")

# check viz
mapviewOptions(vector.palette = krl_color)
mapview(krl_route_main, zcol = "line_name", legend = FALSE, burst = TRUE)


# the longest -------------------------------------------------------------

