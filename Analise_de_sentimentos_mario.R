####################################################################
################    Mario                ###########################
####################################################################
# Analise de sentimentos
# 
library(stringr)
library(tibble)
library(lubridate)
library(igraph)
library(ggraph)
library(reshape2)
library(scales)
library(readr)

# udemycorpus <- Corpus(VectorSource(udemylist))
# custom.stopwords <- c(stopwords('english'), 'lol', 'smh')
# udemycorpus = clean.corpus(udemycorpus)

# udemytdm <- TermDocumentMatrix(udemycorpus)
## dicionario de palavras relacionadas a repressao
#depressionwords <- read_csv("D:/Rgit/seminario_tm/seminario/depressionwords.csv")

# bancos de dados para análise de sentimentos
get_sentiments("afinn")
get_sentiments("bing")
nrc <- get_sentiments("nrc")
#############################################
#####         negative                   ####   
tweets_tidy_nostop <- tweets_data(udemytdm)

tweets.df <- twListToDF(udemytweets)
tweets_tidy_df <- as_tibble(tweets.df)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

mystopwords <- c(stop_words$word,"lol","haha")

tidy_tweets <- tweets_tidy_df %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% mystopwords,
         str_detect(word, "[a-z]"))

nrneg <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

tidy_tweets %>%
  inner_join(nrneg) %>%
  count(word, sort = TRUE)%>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#############################################
#####         sadness                    ####   
nrsad <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

tidy_tweets %>%
  inner_join(nrsad) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#############################################
#####         anger                      ####   
nrang <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")


tidy_tweets %>%
  inner_join(nrang) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() 
# A palavra suicidio aparece pela primeira vez com elevada
# frequencia
#############################################
#####         fear                       ####   
nrfear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

tidy_tweets %>%
  inner_join(nrfear) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() 
# A palavra suicidio aparece pela segunda vez 
#############################################
#####         disgust                    ####   
nrdisg <- get_sentiments("nrc") %>% 
  filter(sentiment == "disgust")

tidy_tweets %>%
  inner_join(nrdisg) %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() #nao parece ter coisas relacionadas com suicidio
#############################################
#####         surprise                   ####   
nrsurp <- get_sentiments("nrc") %>% 
  filter(sentiment == "surprise")

tidy_tweets %>%
  inner_join(nrsurp) %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() 
#nao parece ter coisas relacionadas com suicidio. 
#parece ter palavras em comum com disgust
#

#################################################################
################# Dicionario Bing               #################     
tweets_tidy_nostop <- tweets_data(udemytdm)

tweets_tidy_nostop %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 
### Elevada aparição de palavras negativas em grupos de 100 palavras

#################################################################
################# Dicionario afinn              #################     

afinn <- tweets_tidy_nostop %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line %/% 100) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
#################################################################
################# Comparacao de dicionarios     #################     

# No dicionario nrc estamos contrastando palavras positivas e negativas

bing_and_nrc <- bind_rows(tweets_tidy_nostop %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tweets_tidy_nostop %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = line %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") 

### Todos os dicionários captam elevada quantidade de palavras negativas

####
#### Bigramas
#### 
tidy_tweets_big <- tweets_tidy_df %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "ngrams",n=2) %>%
  filter(!word %in% mystopwords,
         str_detect(word, "[a-z]"))

tidy_tweets_big %>%
  count(word, sort = TRUE)

bigrams_separated <- tidy_tweets_big %>%
  separate(word, c("word1", "word2"), sep = " ")

bigrams_separated %>%
  filter(word1 == "depression") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

# We can then examine the most frequent words that were preceded by “not” and 
# were associated with a sentiment.
depression_words <- bigrams_separated %>%
  filter(word1 == "depression") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

depression_words

depression_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"depression\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


drugs_words <- bigrams_separated %>%
  filter(word1 == "drugs") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()
drugs_words

drugs_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"drugs\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

### Rede de palavras

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


## NUVEM DE PALAVRAS
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#Counting and correlating pairs of words with the widyr package
word_cors <- tidy_tweets %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, screenName, sort = TRUE)

word_cors

#Palavras correlacionadas com depression
word_cors %>%
  filter(item1 %in% c("drugs")) %>%
  group_by(item1) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
################
## Wordclouds

tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)