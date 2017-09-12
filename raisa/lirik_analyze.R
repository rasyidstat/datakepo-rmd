library(tidyverse)
library(tidytext)
library(wordcloud)
# devtools::install_github("dgrtwo/widyr")
library(widyr)
library(extrafont)
loadfonts(quiet = TRUE)

# load data ---------------------------------------------------------------
load("data/df_raisa.Rda")
df <- df %>%
  select(-link, -source) %>%
  mutate(txt=gsub("'", "'", txt))
sw <- readLines("sw.txt", warn=FALSE) %>%
  data_frame(word=.)

df_token <- df %>%
  unnest_tokens(word, txt) %>%
  anti_join(sw) %>%
  anti_join(stop_words)

df_word <- df_token %>%
  count(word) %>%
  filter(nchar(word) >= 3, !grepl("you", word)) %>%
  arrange(-n)

df_token_bigram <- df %>%
  unnest_tokens(token, txt, token="ngrams", n=2) %>%
  count(token, sort=TRUE) 



# wordcloud ---------------------------------------------------------------
png("raisa.png", width=6, height=3.5, units="in", res=300)
wordcloud(df_word$word, df_word$n,
          random.order = FALSE,
          random.color = FALSE,
          max.words = 70,
          colors=brewer.pal(8, "BuPu"),
          family="DIN")
dev.off()



# word networks -----------------------------------------------------------
library(igraph)
library(ggraph)
set.seed(1813)
df_pair <- df_token %>%
  pairwise_count(word, p, sort=TRUE)
df_pair %>%
  filter(n >= 8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  theme_void()