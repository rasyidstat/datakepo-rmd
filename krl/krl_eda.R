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

# main route total distance
krl_route_main %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  summarise(d = sum(d)) %>% 
  as.numeric()

# avg main route distance
krl_route_main %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  summarise(d = mean(d)) %>% 
  as.numeric()

# avg main route between station distance
route_between <- krl_route_split_all %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  count(station_start, station_end, d) %>% 
  arrange(desc(n))
route_between %>% 
  summarise(d_sum = sum(d),
            d_mean = mean(d))

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
ggsave("figs/longest_route.png", width = 10, height = 4, bg = "transparent")

# check viz
mapviewOptions(vector.palette = krl_color)
mapview(krl_route_main, zcol = "line_name", legend = FALSE, burst = TRUE)


# the longest -------------------------------------------------------------
route_min_max <- krl_route_split_all %>% 
  filter(!schedule_id %in% c("idjkt_airport_train", "idjkt_FEEDER") ) %>% 
  group_by(route_name) %>% 
  arrange(route_name, desc(d)) %>% 
  mutate(r = row_number()) %>% 
  arrange(schedule_id, d) %>% 
  mutate(r2 = row_number()) %>% 
  filter(r2 <= 3 | r <= 3)

route_summary <- krl_route_split_all %>% 
  filter(!schedule_id %in% c("idjkt_airport_train", "idjkt_FEEDER") ) %>% 
  group_by(route_name, line_id) %>% 
  summarise(n = n(),
            d_avg = mean(d),
            lower = min(d),
            upper = max(d)) %>% 
  ungroup() %>% 
  arrange(desc(d_avg)) %>% 
  left_join(route_min_max %>% 
              filter(r == 1) %>% 
              as.data.frame() %>% 
              select(route_name, line_id, line_name = line_name1) ) %>% 
  left_join(route_min_max %>% 
              filter(r2 == 1) %>% 
              as.data.frame() %>% 
              select(route_name, line_id, line_name_lowest = line_name1) ) %>% 
  mutate(line_name_txt = paste0(round(upper, 2), "km (", line_name, ")"))
route_summary %>% 
  ggplot(aes(reorder(route_name, d_avg), d_avg, color = line_id)) + 
  coord_flip() +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_point(data = route_summary,
             aes(reorder(route_name, d_avg), upper),
             shape = 18,
             size = 4) +
  geom_point(data = route_summary,
             aes(reorder(route_name, d_avg), lower),
             shape = 15,
             size = 3) +
  geom_text(aes(label = paste0(round(d_avg, 2), "km") ),
            color = "black",
            family = "Nunito",
            vjust = -1) +
  geom_text(data = route_summary,
            aes(route_name, upper,
                label = line_name_txt ),
            color = "black",
            family = "Nunito",
            hjust = -0.1) +
  geom_text(data = route_summary,
            aes(route_name, lower,
                label = paste0(round(lower, 2), "km") ),
            color = "black",
            family = "Nunito",
            hjust = 1.3) +
  scale_color_manual(values = krl_color) +
  theme_nunito(grid = "") +
  guides(color = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(-1, 16))
ggsave("figs/longest_route_max.png", width = 10, height = 3, bg = "transparent")


# the longest (by map) ----------------------------------------------------
krl_route_split_all %>% 
  filter(!schedule_id %in% c("idjkt_airport_train", "idjkt_FEEDER", "idjkt_TANJUNGPRIUK LINE") ) %>% 
  group_by(schedule_id) %>% 
  arrange(schedule_id, desc(d)) %>% 
  mutate(r = row_number()) %>% 
  arrange(schedule_id, d) %>% 
  mutate(r2 = row_number()) %>% 
  filter(r <= 3) %>% 
  ggplot() +
  geom_sf()

krl_route_split_all %>% 
  filter(!schedule_id %in% c("idjkt_airport_train", "idjkt_FEEDER", "idjkt_TANJUNGPRIUK LINE") ) %>% 
  group_by(schedule_id) %>% 
  arrange(schedule_id, desc(d)) %>% 
  mutate(r = row_number()) %>% 
  arrange(schedule_id, d) %>% 
  mutate(r2 = row_number()) %>% 
  filter(r2 <= 3) %>% 
  ggplot() +
  geom_sf() 


krl_route_split_all %>% 
  filter(!schedule_id %in% c("idjkt_airport_train", "idjkt_FEEDER", "idjkt_TANJUNGPRIUK LINE") ) %>% 
  group_by(schedule_id) %>% 
  arrange(schedule_id, desc(d)) %>% 
  mutate(r = row_number()) %>% 
  arrange(schedule_id, d) %>% 
  mutate(r2 = row_number()) %>% 
  filter(r <= 3) %>% 
  View()
