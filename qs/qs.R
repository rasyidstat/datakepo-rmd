library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(extrafont)
loadfonts()

qs <- readLines("qs.txt", warn=FALSE) %>%
  data_frame(txt=.) %>%
  filter(grepl("\\w+", txt)) %>%
  mutate(id=row_number())

sw <- readLines("sw.txt", warn=FALSE) %>%
  data_frame(word=.)

qs_token <- unnest_tokens(qs, word, txt, to_lower = FALSE)

qs_grouped <- qs_token %>%
  anti_join(sw) %>%
  count(word, sort=TRUE) %>%
  filter(nchar(word)>=3,
         word!="q.s")

qs_grouped %>%
  top_n(10, n) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill="steelblue") + coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor=element_blank()) +
  labs(y="Frequency", x=NULL)

png("qs.png", width=6, height=3.5, units="in", res=300)
wordcloud(qs_grouped$word, qs_grouped$n, 
          random.order = FALSE,
          random.color = FALSE,
          max.words = 70,
          colors=brewer.pal(8, "Dark2"),
          family = "DIN")
dev.off()
