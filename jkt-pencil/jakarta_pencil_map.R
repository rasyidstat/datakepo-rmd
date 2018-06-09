library(nusantr)
library(ggplot2)
library(sf)
library(dplyr)
library(cartography)
library(purrr)
library(ggsn)

# data migrasi
metrics <- read.csv("http://data.jakarta.go.id/dataset/32b76674-064d-4d03-9429-58af35611a77/resource/4f6c0a04-b337-4a11-8641-6a03b2fe1a94/download/Data-penduduk-migrasi-masuk-DKI-Jakarta-per-kecamatan-2015.csv")
metrics <- metrics %>%
  group_by(kota_m = nama_kabupaten_atau_kota, kecamatan_m = nama_kecamatan) %>%
  summarise(cnt = sum(jumlah)) %>%
  ungroup() %>%
  filter(!grepl("seribu", tolower(kota_m)))

# data spasial/geometri
jkt <- id_map("jakarta", level = "kecamatan") %>%
  filter(kota != "Kepulauan Seribu") %>%
  st_transform("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs")

# penggabungan data
jkt <- jkt %>%
  mutate(kota_m = gsub("\\s+", "", toupper(kota)),
         kecamatan_m = gsub("\\s+", "", toupper(kecamatan))) %>%
  left_join(metrics %>%
              mutate(kota_m = gsub("\\s+", "", toupper(kota_m)),
                     kecamatan_m = gsub("\\s+", "", toupper(kecamatan_m)))) %>%
  select(-contains("_m")) %>%
  mutate(area = map_dbl(geometry, st_area),
         area = 10^-6 * area,
         cnt_per_km2 = cnt / area)

# data spasial/geometri ala pensil
jkt_pencil <- getPencilLayer(x = jkt, size = 250, lefthanded = FALSE)
jkt_pencil <- st_transform(jkt_pencil, 4326)
jkt <- st_transform(jkt, 4326)
jkt_center <- jkt %>%
  mutate(
    centroid = map(geometry, st_centroid),
    coord = map(centroid, st_coordinates),
    coord_x = map_dbl(coord, 1),
    coord_y = map_dbl(coord, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_centroid()

# st_bbox(jkt)

# eksekusi
ggplot() +
  geom_sf(data = jkt_pencil, aes(color = cnt_per_km2)) + 
  geom_sf(data = jkt, fill = NA, color = "grey30", size = 0.1) +
  geom_sf(data = jkt_center, color = "grey30", size = 0.5) +
  ggrepel::geom_text_repel(data = jkt_center, 
                           aes(coord_x, coord_y, label = kecamatan),
                           color = "grey10",
                           family = "Nunito",
                           size = 3,
                           segment.colour = "grey30") +
  coord_sf(datum = NA) +
  mrsq::theme_nunito(legend = 4) +
  theme(axis.title = element_blank()) +
  labs(title = "Jumlah Pendatang Baru WNI ke DKI Jakarta",
       subtitle = "Tahun 2014",
       caption = "sumber: data.jakarta.go.id") +
  scale_color_gradient(expression(paste("Pendatang/", km^2)), 
                       low = "#DEEBF7", high = "#2171B5") +
  north(jkt, location = "bottomleft", scale = 0.1,
        anchor = c(x = 106.686, y = -6.362)) +
  scalebar(jkt, dist = 5, location = "bottomleft", 
           dd2km = TRUE, model = "WGS84", height = 0.02,
           box.fill = c("#9ECAE1", "grey60"), box.color = "grey100", 
           st.color = "grey30", family = "Nunito", st.size = 3)

ggsave("jakarta_pencil_map.png", scale = 1.3, dpi = 200)
