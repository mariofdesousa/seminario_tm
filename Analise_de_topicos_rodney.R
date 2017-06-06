####################################################################
################    Rodney               ###########################
####################################################################

# Análise de tópicos

library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(tibble)
library(RWeka)
library(twitteR)

udemytdm <- read_csv("base_seminario.csv")
class(udemytdm)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

mystopwords <- c(stop_words$word,"lol","haha")

  tidy_tweets <- udemytdm %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% mystopwords,
         str_detect(word, "[a-z]"))

# criando um tidy com contando as palavras por usuário
tidy_data <- tidy_tweets %>%
    group_by(screenName) %>%
    count(word, sort = TRUE) %>%
    filter(n >= 0)
tidy_data
# criando um DocumentTermMatrix para ser usado na função LDA
depre_dtm <- tidy_data %>% cast_dtm(screenName, word, n)

# aplicando a análise de tópicos
depre_lda <- LDA(depre_dtm, k = 2, control = list(seed = 1234))
depre_lda

# usando o tidy para extrair as probabilidades tópico/palavra
depre_topics <- tidy(depre_lda, matrix = "beta")
depre_topics

# aqui extraímos as palavras com maiores probabilidades de pertecerem
# a cada um dos tópicos
depre_top_terms <- depre_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
depre_top_terms$term
depre_top_terms %>%
  mutate(term = reorder(term, abs(beta))) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# aqui identificamos, para cada palavra, a diferença de pertencer
# ao tópico 1 e de pertencer ao tópico 2
beta_spread <- depre_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread

beta_spread %>%
  top_n(20,abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# Aqui calculamos a probabilidade de cada pessoa escrever sobre
# cada um dos tópicos
depre_topics_id <- tidy(depre_lda, matrix = "gamma")
depre_topics_id
depre_topics_id %>% group_by(topic) %>%
  top_n(10) %>% arrange(topic, desc(gamma))

# Aqui analisamos algumas das pessoas com as maiores probabilidades de 
# escrever sobre algum dos tópicos
tidy_tweets %>% filter(screenName == "DouglasConley13") %>% 
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
aux = udemytdm[udemytdm$screenName == "DouglasConley13",]
aux$text

aux <- tidy_tweets %>% filter(screenName == "fahadaziz532") %>% 
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
aux = udemytdm[udemytdm$screenName == "fahadaziz532",]
aux$text

aux <- tidy_tweets %>% filter(screenName == "Edgy_Barbie") %>% 
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
aux = udemytdm[udemytdm$screenName == "Edgy_Barbie",]
aux$text

