library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)

# Lendo a letra completa do álbum The Wall
thewall = read.csv("wall.csv",header = FALSE)
head(thewall)
class(thewall)
names(thewall)
dim(thewall)
# O tibble com as linhas do texto
# note que usamos data_frame e não data.frame
head(data_frame(thewallline=1:551,text = as.character(thewall$V1)))

# criando a versão tokenizada do texto, onde cada palavra será uma observação
# e teremos uma coluna com a linha de cada observação no texto
tidy_thewall <- data_frame(line=1:551,text = as.character(thewall$V1)) %>%  
  unnest_tokens(word, text)
tidy_thewall

# esses dados contém palavras que não temos interesse no momento,
# como conectores, preposições, etc.
data(stop_words)
# vamos retirar essas 'palavras de parada' usando a função anti_join.
# anti_join retorna todas observações de tidy_thewall, exceto aquelas
# que estiverem em stop_words
tidy_thewall_nostop <- tidy_thewall %>%
  anti_join(stop_words)

# contando as palavras mais usadas no The Wall. Aqui só mostramos as
# palavras que aparecem mais de 10 vezes
tidy_thewall_nostop %>%
  count(word, sort=TRUE) %>%
  filter(n>10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col()+
  coord_flip()

# lendo a letra completa do album Dark side of the moon, e criando a
# sua versão tokenizada
darkside = read.csv("darkside.csv",header = FALSE)
head(darkside)
dim(darkside)
tidy_darkside <- data_frame(line=1:156,text = as.character(darkside$V1)) %>%  
  unnest_tokens(word, text)
tidy_darkside

# retirando as 'palavras de parada'
data(stop_words)
tidy_darkside_nostop <- tidy_darkside %>%
  anti_join(stop_words)

# palavras que mais aparecem no dark side
tidy_darkside_nostop %>%
  count(word, sort=TRUE) %>%
  filter(n>3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col()+
  coord_flip()

# aqui juntamos os tibbles dos dois álbuns indicando de qual álbum cada
# observação veio; então selecionamos somente as observações que são 
# caracteres, depois contamos quantas vezes cada palavra aparece no
# respectivo álbum, e depois a proporção de vezes que a palavra aparece
# no álbum. Daí, tiramos a variável n e criamos uma tabela contendo duas
# colunas com as proporções de cada álbum
freq_Pink <- bind_rows(mutate(tidy_thewall_nostop,album="Wall"),
                       mutate(tidy_darkside_nostop,album="DarkSide"))%>%
mutate(word = str_extract(word, "[a-z']+")) %>%
  count(album, word) %>%
  group_by(album) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(album, proportion)
head(freq_Pink,10)

# fazendo o gráfico das proporções de vezes das palavras que mais aparecem
# em ambos os álbuns
library(scales) # serve pra função percent_format
ggplot(freq_Pink,aes(x=DarkSide,y=Wall))+
  geom_abline(color = "gray40", lty = 2)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75")

# correlação entre as proporções
cor.test(data = freq_Pink,~ DarkSide + Wall)
cor.test(freq_Pink$DarkSide, freq_Pink$Wall)

###############################################

# bancos de dados para análise de sentimentos
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

tidy_thewall # dados do the wall
# filtrando somente as palavras de alegria
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
# verificando quais são as palavras de alegria do the wall
# em ordem decrescente de frequência
tidy_thewall %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# aqui verificamos quais palavras do the wall estão no banco de dados bing,
# separamos as palvras em grupos formados de 10 em 10 linhas e contamos para
# cada um desses grupos quantas palavras negativas existem e quantas positivas
# existem. Depois fazemos duas colunas contendo a frequencia de cada sentimento
# para cada grupo de linhas, sendo NA substituídos por zero (fill=0).
# por fim, criamos uma medida de sentimento do grupo, dada pela diferença das
# frequêncicas
thewallsentiment <- tidy_thewall %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head(thewallsentiment)

# evolução dos sentimentos ao longo do álbum para grupos de 10 linhas
ggplot(thewallsentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)
# same stuff for the dark side, mas aqui os grupos tem menos linhas porque esse
# álbum é menor e tem menos palavras que o the wall
darksidesentiment <- tidy_darkside %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head(darksidesentiment)

ggplot(darksidesentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

# comparando os três dicionários de sentimentos

# para o banco de dados afinn, os sentimentos são medidos em uma escala de
# -5 a 5, daí somando os valores de todas as palavras dentre de um grupo
# obtemos uma medida do sentimento desse grupo
afinn <- tidy_thewall %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line %/% 10) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
# no banco bing os sentimentos já são positivos ou negativos, já no nrc
# filtramos só os sentimentos positivos e negativos (não considerando os
# labels como alegria, raiva, etc.). Depois criamos uma medida do sentimento
# dada pela diferença da frequencia de sentimentos positivos e negativos
bing_and_nrc <- bind_rows(tidy_thewall %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_thewall %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = line %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
# criando um gráfico da evolução dos sentimentos no the wall usando os 
# três bancos de dados analisados
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
# a parte negativa do começo todo mundo conhece
arrange(afinn[0:10,],sentiment)
thewall[50:69,]  

# palavras positivas e negativas mais comuns usando o banco bing
bing_word_counts <- tidy_thewall %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts
# aqui agrupamos o dados em bing_word_counts por sentimento,
# vemos quais são aqueles que são os 8 mais frequentes (vai aparecer
# mais no plot por causa dos empates), reordenamos em ordem descrencente 
# de frequencia e fazemos um gráfico de colunas para cada sentimento
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(8,n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# Nuvem de palavras

library(wordcloud)

tidy_thewall %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
tidy_thewall %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

# term frequency

wall_words <- tidy_thewall %>%
  count(word, sort = TRUE) %>%
  ungroup()
sum(wall_words$n)

ggplot(wall_words, aes(n/sum(wall_words$n))) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.02)


freq_by_rank_wall <- wall_words %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/sum(wall_words$n))
freq_by_rank_wall

freq_by_rank_wall %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()
lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank_wall)
freq_by_rank_wall %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_abline(intercept = -0.9989, slope = -0.8924, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

# 3.3 The bind_tf_idf function

dark_words <- tidy_darkside %>%
  count(word, sort = TRUE) %>%
  ungroup()
Pink_words <- rbind(mutate(wall_words,album="TheWall"),
                    mutate(dark_words,album="DarkSide"))

Pink_words <- Pink_words %>%
  bind_tf_idf(word, album, n)
Pink_words
  
Pink_words %>%
  arrange(desc(tf_idf))

plot_Pink <- Pink_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_Pink %>% 
  top_n(10) %>%
  ggplot(aes(word, tf_idf, fill = album)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

plot_Pink %>% 
  group_by(album) %>% 
  top_n(6) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = album)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~album, ncol = 1, scales = "free") +
  coord_flip()
