####################################################################
################    Rodney               ###########################
####################################################################

# Análise de tópicos

if("dplyr" %in% rownames(installed.packages())==FALSE)
{install.packages("dplyr")};library(dplyr)

if("stringr" %in% rownames(installed.packages())==FALSE)
{install.packages("stringr")};library(stringr)

if("tidytext" %in% rownames(installed.packages())==FALSE)
{install.packages("tidytext")};library(tidytext)

if("tidyr" %in% rownames(installed.packages())==FALSE)
{install.packages("tidyr")};library(tidyr)

if("ggplot2" %in% rownames(installed.packages())==FALSE)
{install.packages("ggplot2")};library(ggplot2)

if("twitteR" %in% rownames(installed.packages())==FALSE)
{install.packages("twitteR")};library(twitteR)

if("tibble" %in% rownames(installed.packages())==FALSE)
{install.packages("tibble")};library(tibble)

if("topicmodels" %in% rownames(installed.packages())==FALSE)
{install.packages("topicmodels")};library(topicmodels)

udemytweets <- readRDS("depression_tweets_final.gzip")
tweets.df <- twListToDF(udemytweets) ##CALMA QUE VAI DEMORAR

tweets_tidy_df <- as_tibble(tweets.df)
# Limpando a base de dados

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg  <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

data("stop_words")

mystopwords <- c(stop_words$word)

tidy_tweets <- tweets_tidy_df %>% 
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
  filter(!word %in% mystopwords,
         str_detect(word, "[a-z]")) %>%
  anti_join(stop_words)

# Criando um tidy com contando as palavras por usuário
tidy_data <- tidy_tweets %>%
    group_by(screenName) %>%
    count(word, sort = TRUE) %>%
    filter(n >= 0)
# criando um DocumentTermMatrix para ser usado na função LDA
depre_dtm <- tidy_data %>% cast_dtm(screenName, word, n)

# aplicando a análise de tópicos
depre_lda <- LDA(depre_dtm, k = 2, control = list(seed = 1234))

# usando o tidy para extrair as probabilidades tópico/palavra
depre_topics <- tidy(depre_lda, matrix = "beta")

# aqui extraímos as palavras com maiores probabilidades de pertecerem
# a cada um dos tópicos
depre_top_terms <- depre_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# gerando o gráfico
depre_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  xlab("termo") +
  coord_flip()

# aqui identificamos, para cada palavra, a diferença de pertencer
# ao tópico 1 e de pertencer ao tópico 2
beta_spread <- depre_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
# gerando o gráfico
beta_spread %>%
  top_n(10,abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("termo") +
  ylab("razão do logaritmo") +
  coord_flip()

# Aqui calculamos a probabilidade de cada pessoa escrever sobre
# cada um dos tópicos
depre_topics_id <- tidy(depre_lda, matrix = "gamma")
depre_topics_id %>% group_by(topic) %>%
  top_n(3) %>% arrange(topic, desc(gamma))

# Aqui analisamos algumas das pessoas com as maiores probabilidades de 
# escrever sobre algum dos tópicos
exemplo <- tweets.df %>% filter(screenName == "_Depressed_Zuka") %>%
  select(text)
exemplo$text[15]

exemplo <- tweets.df %>% filter(screenName == "Quixotitron_4") %>%
  select(text)
exemplo$text[1]

exemplo <- tweets.df %>% filter(screenName == "RF_NYC_2010") %>%
  select(text)
exemplo$text[5]

exemplo <- tweets.df %>% filter(screenName == "DepressionRoots") %>%
  select(text)
exemplo$text[2]



###############################################################
###############################################################

# Tentando criar um índice de depressão/tristeza

##################
# Pequeno exemplo
depre_words = c("depressed","sad","distant")
tidy_data
aux <- filter(tidy_data,screenName %in% c("Neitzschee","Quixotitron_4"))

aux %>%  mutate(ntot = sum(n)) %>%
  filter(word %in% depre_words) %>%
  mutate(dep_scr = sum(n/ntot)) %>%
  ungroup() %>%
  select(-word) %>%
  unique()
##################

depre_words = c("sad","hopeless","unhappy","suicidal","abandoned")
tidy_data

tidy_dp_index <- tidy_data %>%  mutate(ntot = sum(n)) %>%
  filter(word %in% depre_words) %>%
  mutate(dep_scr = sum(n/ntot)) %>%
  ungroup() %>%
  select(-word) %>%
  unique() %>%
  arrange(desc(dep_scr))
tidy_dp_index

tweets.df%>%filter(screenName == "myland123456") %>%select(text)
tweets.df%>%filter(screenName == "depressive__cut") %>%select(text)

tidy_dp_index %>% arrange(dep_scr)
