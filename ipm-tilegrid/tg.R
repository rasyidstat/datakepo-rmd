library(tidyverse)
# devtools::install_github("rasyidstat/nusantr")
library(nusantr)

# read
df_ipm <- read_csv("https://raw.githubusercontent.com/rasyidstat/GoD/master/ipm/data/ipm_provinsi.csv") %>%
  gather(key=tahun, value=ipm, -1) %>%
  select(provinsi=1, everything()) %>%
  mutate(provinsi=case_when(grepl("Jakarta", .$provinsi)~"DKI Jakarta",
                            grepl("Yogyakarta", .$provinsi)~"DI Yogyakarta",
                            TRUE~.$provinsi),
         provinsi=gsub("Kep.", "Kepulauan", provinsi),
         tahun=as.numeric(tahun),
         ipm=as.numeric(ipm)) %>%
  mutate(ipm=ifelse(ipm==0, NA, ipm))

# viz
df_ipm %>%
  filter(tahun==2016) %>%
  inner_join(provinsi, by=c("provinsi"="provinsi2")) %>%
  ggplot(aes(X, Y, fill=ipm, label=provinsi_abb)) +
  geom_tile(size=6, width=0.95, height=0.95) + geom_text(family="DIN", color="white") +
  labs(title="Peta Indeks Pembangunan Manusia di Indonesia",
       subtitle="Per Provinsi (Tahun 2016)",
       caption="Visualisasi data oleh: Rasyid Ridha (@rasyidstat)
       http://rasyidridha.com
       Sumber data: BPS",
       x=NULL, y=NULL) +
  mrSQ::theme_din(legend = 3) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.direction = "horizontal") +
  viridis::scale_fill_viridis("IPM")

ggsave("ipm-2016-peta-bin.png", width=8, height=4.7)
