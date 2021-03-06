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

# Criando um índice de depressão/tristeza

# 99 palavras sobre depressão listadas no site abaixo mais depressed
# https://adarkershadeofblue.wordpress.com/2011/08/20/99-words-about-depression/
depre_words = c("depressed","abandoned","achy","afraid","agitated","agony","alone","anguish","antisocial","anxious","breakdown","brittle","broken","catatonic","consumed","crisis","crushed","crying","defeated","defensive","dejected","demoralized","desolate","despair","desperate","despondent","devastated","discontented","disheartened","dismal","distractable","distraught","distressed","doomed","dreadful","dreary","edgy","emotional","empty","excluded","exhausted","exposed","fatalistic","forlorn","fragile","freaking","gloomy","grouchy","hate","helpless","hopeless","hurt","inadequate","inconsolable","injured","insecure","irrational","irritable","isolated","lonely","lousy","low","melancholy","miserable","moody","morbid","needy","nervous","nightmarish","oppressed","overwhelmed","pain","paranoid","pessimistic","reckless","rejected","resigned","sadness","self-conscious","self-disgust","shattered","sobbing","sorrowful","suffering","suicidal","tearful","touchy","trapped","uneasy","unhappy","unhinged","unpredictable","upset","vulnerable","wailing","weak","weepy","withdrawn","woeful","wounded","wretched")
#tidy_data

# Considere as seguintes variáveis
# ntot:total de palavras consideras
# ndep: total de palavras que estão na lista sobre depressão
# o índice utilizado foi 1 + 1/(ndep*log(1 - p) - 1), onde
# p=ndep/ntot. O índice assume valores entre 0 e 1, quando mais 
# próximo de 1 mais depressivo é o conteúdo do texto
tidy_dp_index <- tidy_data %>%  mutate(ntot = sum(n)) %>%
  filter(word %in% depre_words) %>%
  filter(ntot>9) %>%
  mutate(ndep = sum(n)) %>%
  mutate(dep_scr = (1 + 1/(ndep*log(1 - ndep/ntot) - 1))) %>%
  ungroup() %>%
  select(-c(word,n)) %>%
  unique() %>%
  arrange(desc(dep_scr))
tidy_dp_index %>% head(2)  # usuários com maiores índices
tidy_dp_index %>% arrange(dep_scr) %>% head(2)  # usuários com menores índices
# textos de usuários detectados com maiores índices
tweets.df%>%filter(screenName == "SaffCulverwell") %>%select(text) %>% head(1)
tweets.df%>%filter(screenName == "lanadelskinny") %>%select(text) %>% head(1)
# textos de usuários detectados com menores índices
tweets.df%>%filter(screenName == "DepressionRoots") %>%select(text) %>% head(1)
tweets.df%>%filter(screenName == "juliaweitz2") %>%select(text) %>% head(1)
##################