library(twitteR)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)

### Palavras retiradas para contagem
mystopwords_count <- c(stop_words$word,"depression","depressive","depressed",
                  "im","wcw","ur","depressed.","depression.","depressed,",
                  "depression,","me,","lol","#depression","me.","wow",
                  "depressed?","idk","ppl","bc","dont","lot","it.","anxiety,",
                  "her,","af","y'all","rn")

### Limpando a base para a contagem de palavras
tweets_tidy_count <- tweets_tidy_df %>% 
  filter(!str_detect(text, "^RT"),
         text != "im",
         text != "ur",
         text != "af",
         text != "wcw",
         text != "shes",
         text != "booktweeter0",
         text != "bktwr",
         !str_detect(text, "[0-9]"),
         !str_detect(text, ":"),
         !str_detect(text, "textless")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% mystopwords_count,
         str_detect(word, "[a-z]")) %>%
  anti_join(stop_words)%>%
  count(word,sort=TRUE)%>%
  filter(n > 100 & n < 500) %>%
  mutate(word = reorder(word, n)) 

  ggplot(tweets_tidy_count,aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
### Nuvem de palavras
library(wordcloud)
  
tweets_tidy_df %>% 
  filter(!str_detect(text, "^RT"),
         text != "im",
         text != "ur",
         text != "af",
         text != "wcw",
         text != "shes",
         text != "booktweeter0",
         text != "bktwr",
         !str_detect(text, "[0-9]"),
         !str_detect(text, ":"),
         !str_detect(text, "textless")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% mystopwords_count,
         str_detect(word, "[a-z]")) %>%
  anti_join(stop_words)%>%
  count(word,sort=TRUE)%>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, n, max.words = 100))

### Histograma da proporcao de palavras
### Palavras retiradas

mystopwords_hist <- c(stop_words$word,"im","wcw","ur",
                         "depressed.","depression.","depressed,")
tweets_tidy_df %>% 
    filter(!str_detect(text, "^RT"),
           text != "im",
           text != "ur",
           text != "af",
           text != "wcw",
           text != "shes",
           text != "booktweeter0",
           text != "bktwr",
           !str_detect(text, "[0-9]"),
           !str_detect(text, ":"),
           !str_detect(text, "textless")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex") %>%
    filter(!word %in% mystopwords_hist,
           str_detect(word, "[a-z]")) %>%
    anti_join(stop_words)%>%
    count(word,sort=TRUE)%>%
    mutate(word = reorder(word, n)) %>%
ggplot(aes(n/sum(n))) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  xlab("Proporção")+
  ylab("Contagem")



