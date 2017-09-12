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

df_links <- get_music("https://liriklaguindonesia.net/lirik/Raisa") %>%
  rbind(get_music("https://liriklaguindonesia.net/lirik/Raisa/page/2")) %>%
  rbind(get_music("https://liriklaguindonesia.net/lirik/Raisa/page/3"))

df_links <- df_links %>% 
  filter(grepl("raisa", link))

df_links_final <- df_links %>%
  filter(grepl("https://liriklaguindonesia.net/raisa", link))


save(df_links_final, file="data/df_links_final.Rda")
load("data/df_links_final.Rda")



# get lyrics data ---------------------------------------------------------
get_lyric <- function(url) {
  file_name <- sprintf("data/%s.rds", basename(url))
  if (file.exists(file_name)) {
    df_lyric <- read_rds(file_name)
  } else {
    df_lyric <- read_html(url)
    write_rds(df_lyric, file_name)
  }
  title <- df_lyric %>%
    html_nodes("h2") %>%
    html_text()
  lyric <- df_lyric %>%
    html_nodes("div.clear") %>%
    html_text()
  df <- df_lyric %>%
    html_nodes("div.clear p") %>%
    # as.character() %>%
    # gsub("<hr>|<hr>", "", .) %>%
    # read_html() %>%
    # html_nodes("p") %>%
    as.character() %>%
    subset(grepl("<br>", .)) %>%
    gsub("<br> ", "\n", .) %>%
    gsub("<p>|</p>", "", .) %>%
    data_frame(lyric=.) %>%
    mutate(p=row_number()) %>%
    unnest_tokens(txt, lyric, token="regex", pattern="\n") %>%
    mutate(l=row_number()) %>%
    group_by(p) %>%
    mutate(pl=row_number(),
           title=gsub("Lirik Lagu|Lirik Lagu ", "", title[2])) %>%
    ungroup() %>%
    select(title, p, pl, l, txt)
}

url <- df_links_final$link[1]

XML::xmlToDataFrame(lyric)

df <- df_links_final %>%
  mutate

html_attrs(lyric) %>%
  map(test)

html_name(lyric)

df <- df_lyric %>%
  html_nodes("div.clear p") %>%
  # as.character() %>%
  # gsub("<hr>|<hr>", "", .) %>%
  # read_html() %>%
  # html_nodes("p") %>%
  as.character() %>%
  subset(grepl("<br>", .)) %>%
  gsub("<br> ", "\n", .) %>%
  gsub("<p>|</p>", "", .) %>%
  data_frame(lyric=.) %>%
  mutate(p=row_number()) %>%
  unnest_tokens(txt, lyric, token="regex", pattern="\n") %>%
  mutate(l=row_number()) %>%
  group_by(p) %>%
  mutate(pl=row_number(),
         title=gsub("Lirik Lagu|Lirik Lagu ", "", title[2])) %>%
  ungroup() %>%
  select(title, p, pl, l, txt)
  

lyric <- df_lyric %>%
  html_nodes("div.clear") %>%
  html_children()

lyric <- data_frame(text = html_text(lyric),
                    attr = html_attrs(lyric) %>%
                      map(function(x) paste0("", x)),
                    tag = html_name(lyric))

lyric %>%
  filter(tag %in% c("p","hr"), attr=="")

df_lyric %>%
  html_nodes("p") %>%
  c()

lyric %>%
  filter(tag=="p", attr=="") -> a
