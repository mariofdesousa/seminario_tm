# Confira se o JAVA instalado em seu computador está atualizado. Se não estiver
# atualize antes de instalar os pacotes.

# Quando o JAVA estiver atualizado, retire o comentario (#) das linhas 6-12

# install.packages(c('tm', 'stringi','ggplot2','ggthemes','wordcloud',
#                   'RColorBrewer','plyr','stringr','topicmodels','portfolio',
#            'openNLP', 'qdap','SnowballC'),repos = "http://cran.r-project.org", 
#                 dependencies = c("Depends", "Imports", "Suggests"))

# install.packages('openNLPmodels.en', repos = "http://datacube.wu.ac.at/",
#                 type = "source")

library(twitteR)
library(httr)
library(tm)
library(wordcloud)
library(SnowballC)
library(dendextend)
#library(qdap)
library(stringr)
library(plyr)
library(readr)


# key <- 	"PqRqHQo0jHDJfhtGBeDhZRjMr"
# secret <- "kuXDWmsPUriwiI46VHWje1It3ftBcjcR8oxEBjSYvgwvwgY5Ve"
# accesstoken <- "864574120180240385-bUmqx0eH3cCXqBxRUf6jvhnZwlvFQVu"
# tokensecret <- "vPWNWaVqxYljrm5fuwy29i3Ygwk5gc6YQ6DHF33hrctQL"

# setup_twitter_oauth(key, secret, accesstoken, tokensecret )
# Nao execute as funcoes marcadas abaixo para nao alterar a base de dados
# udemytweets1 <- searchTwitter("depression",n=1000)
# udemytweets2 <- searchTwitter("fired",n=1000)
# udemytweets3 <- searchTwitter("drugs",n=1000)
# udemytweets  <- rbind(udemytweets1,udemytweets2,udemytweets3)

# udemylist   <- sapply(udemytweets, function(x) x$getText())

# udemycorpus <- Corpus(VectorSource(udemylist))
# custom.stopwords <- c(stopwords('english'), 'lol', 'smh')
# udemycorpus = clean.corpus(udemycorpus)

# udemytdm <- TermDocumentMatrix(udemycorpus)

wordcloud(udemycorpus, min.freq = 15, scale = c(5,1), random.color = F,
          random.order = F, max.words = 30)









findFreqTerms(udemytdm, lowfreq = 10)
findAssocs(udemytdm, "adulto", 0.6)

dim(udemytdm)



udemy_m <- as.matrix(udemytdm)
term_frequency <- rowSums(udemy_m)
term_frequency
frequency <- freq_terms(udemycorpus, top = 10, at.least = 3, stopwords = "Top200Words")
frequency2 <- freq_terms(udemycorpus, top = 10, at.least = 3, stopwords = tm::stopwords("portuguese"))
plot(frequency2)

dist_freq <- dist(frequency)
dist_freq
hc <- hclust(dist_freq)
plot(hc)































tdm2 <- removeSparseTerms(udemytdm, sparse = 0.1)
tdm2
tdm_m <- as.matrix(tdm2)

tdm_df <- as.data.frame(tdm_m)

tweets_dist <- dist(tdm_df)

hc <- hclust(tweets_dist)

plot(hc)


hcd <- as.dendrogram(hc)

labels(hcd)

plot(hcd, main = "Better Dendrogram")







