####################################################################
################    Rodney               ###########################
####################################################################

# Análise de tópicos

library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tidyr)


class(udemytdm)
tidy_data = tidy(udemytdm)
tidy_data
palavras_problema = c("c\xe2\x80","hell\xe2\x80","drink\xe2\x80","ي\xd8",
                      "httpstcoxcujkdekfs","httpstcobhtbienixw","foc\xe2\x80",
                      "drugs\xe2\x80","httpstc\xe2\x80","httpstcocjbetjm\xe2\x80",
                      "d\xe2\x80","addicted\xe2\x80")
tidy_data <- tidy_data %>% filter(!term %in% palavras_problema)
depre_dtm <- tidy_data %>% cast_dtm(document, term, count)


# aplicando a análise de tópicos
depre_lda <- LDA(depre_dtm, k = 2, control = list(seed = 1234))
depre_lda

# usando o tidy para extrair as probabilidades tópico/palavra
depre_topics <- tidy(depre_lda, matrix = "beta")
depre_topics

depre_top_terms <- depre_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
depre_top_terms$term
depre_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

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
