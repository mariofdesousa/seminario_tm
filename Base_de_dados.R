library(twitteR)
library(httr)
library(tm)
library(wordcloud)
library(SnowballC)
library(dendextend)
library(qdap)
library(stringr)
library(plyr)
library(readr)
library(dplyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(RWeka)

#key         <- "PqRqHQo0jHDJfhtGBeDhZRjMr"
#secret      <- "kuXDWmsPUriwiI46VHWje1It3ftBcjcR8oxEBjSYvgwvwgY5Ve"
#accesstoken <- "864574120180240385-bUmqx0eH3cCXqBxRUf6jvhnZwlvFQVu"
#okensecret  <- "vPWNWaVqxYljrm5fuwy29i3Ygwk5gc6YQ6DHF33hrctQL"
#setup_twitter_oauth(key, secret, accesstoken, tokensecret )

#Baixando a base de dados 
#udemytweets1 <- searchTwitter("agony",n=1000)
#udemytweets2 <- searchTwitter("anxiety",n=1000)
#udemytweets3 <- searchTwitter("edgy",n=1000)
#udemytweets4 <- searchTwitter("hopeless",n=1000)
#udemytweets5 <- searchTwitter("lonely",n=1000)
#udemytweets6 <- searchTwitter("sadness",n=1000)
#udemytweets7 <- searchTwitter("suffering",n=1000)
#udemytweets8 <- searchTwitter("drugs",n=1000)
#udemytweets9 <- searchTwitter("bullying",n=1000)
#udemytweets9 <- searchTwitter("desperate",n=1000)

#Dados extraidos em 05-06-2017 as 18:35
#
#udemytweets  <- rbind(udemytweets1,udemytweets2,udemytweets3,udemytweets4,
#                      udemytweets5,udemytweets6,udemytweets7,udemytweets8,
#                      udemytweets9)

#tweets.df      <- twListToDF(udemytweets)

# write_csv(tweets.df,"base_seminario.csv") 

# para abrir o arquivo use read_csv(do pacote "readr) e não read.cs


tweets_tidy_df <- as_tibble(tweets.df)




