library(rvest)
library(tidyverse)
library(tidytext)

# get music lists ---------------------------------------------------------
url <- "https://liriklaguindonesia.net/lirik/Raisa"
get_music <- function(url) {
  rs <- read_html(url)
  rs %>%
    html_nodes("h2 a") %>%
    html_attr("href") %>%
    data_frame(link = .) %>%
    mutate(source = url)
}

if (file.exists("data/df_links.Rda")) {
  load("data/df_links.Rda")
} else {
  df_links <- get_music("https://liriklaguindonesia.net/lirik/Raisa") %>%
    rbind(get_music("https://liriklaguindonesia.net/lirik/Raisa/page/2")) %>%
    rbind(get_music("https://liriklaguindonesia.net/lirik/Raisa/page/3"))
  save(df_links, file="data/df_links.Rda")
}

df_links <- df_links %>% 
  filter(grepl("raisa", link))
df_links_final <- df_links %>%
  filter(grepl("https://liriklaguindonesia.net/raisa", link))
save(df_links_final, file="data/df_links_final.Rda")
load("data/df_links_final.Rda")



# get lyrics data ---------------------------------------------------------
get_lyric <- function(url) {
  file_name <- sprintf("data/%s.html", basename(url))
  if (file.exists(file_name)) {
    df_lyric <- read_html(file_name)
  } else {
    df_lyric <- read_html(url)
    write_html(df_lyric, file=file_name)
  }
  title <- df_lyric %>%
    html_nodes("h2") %>%
    html_text()
  df <- df_lyric %>%
    html_nodes("div.clear p") %>%
    as.character() %>%
    subset(grepl("<br>", .)) %>%
    gsub("<br>|<br> ", "\n", .) %>%
    gsub("<p>|</p>|<em>|</em>|<.*/>", "", .) %>%
    data_frame(lyric=.) %>%
    mutate(p=row_number()) %>%
    unnest_tokens(txt, lyric, token="regex", pattern="\n") %>%
    mutate(l=row_number()) %>%
    group_by(p) %>%
    mutate(pl=row_number(),
           title=gsub("Lirik Lagu|Lirik Lagu ", "", title[2])) %>%
    ungroup() %>%
    select(title, p, pl, l, txt)
  return(df)
}

df <- df_links_final %>%
  mutate(all = map(link, get_lyric)) %>%
  unnest(all)

save(df, file="data/df_raisa.Rda")
