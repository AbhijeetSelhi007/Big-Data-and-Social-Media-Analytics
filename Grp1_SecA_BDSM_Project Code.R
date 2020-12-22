library(tm)
library(ggplot2)
library(tidytext)
library(dplyr)
library(tidyr)
library(widyr)
library(stringr)
library(igraph)
library(ggraph)
library(wordcloud) 
library(reshape2)
library(lubridate)
library(magrittr)
library(networkD3)
library(purrr)  #for map_chr


##############################################################################################
#####################this code is for basic descriptive and sentiment analysis ############### 
##############################################################################################
# 1. Summary of the dataset - Mean , Max and low for each column
# 2. Missing data analysis
# 3. Most tweeted users
# 4. Date- wise trends
# 5. Retweet analysis using centrality measures
# 6. Unigram and bigram analysis 
# 7. Community Detection
###############################################################################################

setwd('E:\\OneDrive - Institute of Management Technology\\Term 6\\Big Data and SMAM\\Project')

##read the data
raw_data <- read.csv('Twitter data RT.csv',stringsAsFactors = F)

###################################################################
## 1. Summary of the dataset - Mean , Max and low for each column
###################################################################

summary(raw_data)

########################################################
# 2. Missing data analysis 
########################################################
raw_data %>% summarise_all(list(name  = ~sum(is.na(.)|is.null(.)| (.) =='')/length(.)*100))

#########################################################
# 3. most tweeted / retweeted users
#########################################################
most_tweet_Auth <- raw_data %>% select(Author) %>% group_by(Author) %>% summarize(count_tweet = n()) %>% arrange(desc(count_tweet))

most_tweet_Auth  %>% arrange(-count_tweet) %>% slice(1:15) %>%
  ggplot(
  aes(x = reorder(Author,count_tweet), y = count_tweet)) +
    geom_col() +
    coord_flip() +
    labs(y = "Count",
         x = "Author names",
         title = "Most tweeted author names"
    )

raw_data %>% filter(Author == '@inqzind') %>% select(Author,Content)



#########################################################
# 4. Date- wise trends
#########################################################

raw_data$Date <- as.Date(substr(raw_data$Date,1,8),format = "%d/%m/%y")

ggplot(data = raw_data, aes(x = floor_date(Date, "day"))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Day of the month") + ylab("Number of comments") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


#########################################################
# 5. Retweet analysis
#########################################################

### create a dataframe of only retweets 

retweet_data <- raw_data[grep("(RT|via)",raw_data$Content, ignore.case=TRUE),]

## percentage of retweets 

retweet_perc <- nrow(retweet_data)/nrow(raw_data) 
retweet_perc 

#list of retweeters and posters
user_retweet = (retweet_data$Author)
user_post = as.character( str_extract_all(retweet_data$Content,"RT @\\w+"))
user_post = gsub("RT @", "", user_post)

##create a dataframe of retweeters and posters
retweeter_poster = cbind(user_retweet, user_post)

####select the first 100 rows to plot
retweeter_poster_sub = retweeter_poster[1:100,]

###############################################
######graph for retweeter and poster analysis
###############################################


retweet_graph_full = graph.edgelist(retweeter_poster,directed = TRUE)

###############################################
###########Degree centrality
###############################################

##users who retweeted most . Active users in a network
active_user <- sort( degree(retweet_graph_full, mode = c("out")),decreasing = TRUE)
active_user[1:10]
##Users whose posts were retweeted most . Most popular author 
pop_auth <- sort( degree(retweet_graph_full, mode = c("in")),decreasing = TRUE)
pop_auth[1:10]

#Check degree by user name
in_deg <- degree(retweet_graph_full,"mgnayak5", mode = c("in"))
out_deg <- degree(retweet_graph_full,"@mgnayak5", mode = c("out"))


out_deg <- degree(retweet_graph_full,"ShaanIrfan05", mode = c("out")) ##why does this throw an error 
in_deg <- degree(retweet_graph_full,"@ShaanIrfan05", mode = c("in"))

in_deg <- degree(retweet_graph_full,"nilotpalm3", mode = c("in"))
out_deg <- degree(retweet_graph_full,"@nilotpalm3", mode = c("out"))



retweet_graph_sub = graph.edgelist(retweeter_poster_sub,directed = TRUE)
ver_labs = get.vertex.attribute(retweet_graph_sub, "name", index=V(retweet_graph_sub))

#### choosing a layout
glay = layout.fruchterman.reingold(retweet_graph_sub)
#glay = layout.kamada.kawai(retweet_graph_sub)

#### plot
##refer https://igraph.org/r/doc/plot.common.html for graph parameters

par(bg="gray15", mar=c(1,1,1,1))


plot(retweet_graph_sub, layout=glay,
     vertex.color="gray25",
     vertex.size=8,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.2,
     edge.arrow.width=0.3,
     label.cex = 0.3,
     edge.width=1,
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Tweets for Indian Media: Who retweets whom",
      cex.main=1, col.main="gray95")


###############################################
###########betweenness centrality
###############################################
betwn <- betweenness(retweet_graph_full,directed = TRUE)

betwn_sort <- betwn %>%
  sort(decreasing = TRUE) %>%
  round()
betwn_sort[1:10]

###############################################
###########betweenness centrality
###############################################
close <- closeness(retweet_graph_full)

close_sort <- close %>%
  sort(decreasing = TRUE) %>%
  round()

close_sort[1:10]


###################################################
# 6. Unigram and Bigram analysis 
###################################################

##data preprocessing 

clean_text = function(x) {
  x = gsub("RT", "", x) # remove Retweet
  x = gsub("@\\w+", "", x) # remove at(@)
  x = gsub("http\\w+", "", x)
  x = gsub("https\\w+", "", x)# remove links http
  x = gsub("[ |\t]{2,}", "", x) # remove tabs
  x = iconv(x, "UTF-8", "ASCII", sub = "") #keep only ascii
  
}

#analyse only the tweet text column 

twitter_data <- data.frame()
twitter_data <- clean_text(raw_data$Content)

# convert text to corpus
corpus <- Corpus(VectorSource(twitter_data))


##clean the corpus : convert to lowercase , remove symbols, numbers , stopwords
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(stopwords("en"),"India","Indian")) %>%
  tm_map(stripWhitespace)

### convert corpus to a dataframe 

corpus_final <-
  data.frame(text = as.character(sapply(corpus_clean, identity)),
             stringsAsFactors = F)

## assign column name 
names(corpus_final) <- "tweet_text"

#####################################################
## 6.a graph to show the count of unique words - unigrams
#####################################################

corpus_final  %>%
   unnest_tokens(word, tweet_text) %>% count(word, sort = TRUE)%>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets")

###############################
# 6.b build a bigram list 
##############################

bigram_list <- corpus_final  %>%
  unnest_tokens(bi_word, tweet_text,token = "ngrams",n=2) 

## view the bi gram list 
bigram_list %>%
  count(bi_word, sort = TRUE)

#split the bigrams for a word network analysis
bigram_split_list <- bigram_list %>%
  separate(bi_word, c("word1", "word2"), sep = " ")

# bigram counts
bigram_split_list <- bigram_split_list %>%
  count(word1, word2, sort = TRUE)

### view the word network

bigram_split_list  %>%
  filter(n >= 500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets for Indian Media", x = "", y = "")

#####network D3 



network <-  bigram_split_list %>%
  filter(n >=250) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- degree(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$n/max(E(network)$n)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = 10*V(network)$degree)
# Define color group
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(Links = network.D3$links, 
             Nodes = network.D3$nodes, 
             Source = 'source', 
             Target = 'target',
             NodeID = 'name',
             Group = 'Group', 
             opacity = 0.9,
             Value = 'Width',
             Nodesize = 'Degree', 
             # We input a JavaScript function.
             linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
             fontSize = 12,
             zoom = TRUE, 
             opacityNoHover = 1)

##node importance 

#Select biggest connected component.  

V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(graph = network,
                               vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize)))
# Store the degree.
V(cc.network)$degree <- degree(graph = cc.network)
# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$n/max(E(cc.network)$n)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = cc.network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = 2*V(cc.network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(cc.network)$width
# Compute the centrality measures for the biggest connected component from above.
node.impo.df <- tibble(word = V(cc.network)$name,  
                       degree = degree(graph = cc.network),
                       closeness = closeness(graph = cc.network), 
                       betweenness = betweenness(graph = cc.network))

##degree
node.impo.df %>% arrange(- degree) %>% head(10)
#closeness
node.impo.df %>% arrange(- closeness) %>% head(10)
##betweeness
node.impo.df %>% arrange(- betweenness) %>% head(10)

###distribution of centrality measures
plt.deg <- node.impo.df %>% 
  ggplot(mapping = aes(x = degree)) +
  theme_light() +
  geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)

plt.clo <- node.impo.df %>% 
  ggplot(mapping = aes(x = closeness)) +
  theme_light() +
  geom_histogram(fill = 'red', alpha = 0.8, bins = 30)

plt.bet <- node.impo.df %>% 
  ggplot(mapping = aes(x = betweenness)) +
  theme_light() +
  geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)

write.csv(node.impo.df,"Centrality measures.csv")


#########################
##7.Community Detection
#########################

comm.det.obj <- cluster_louvain(graph = cc.network, weights = E(cc.network)$n)

comm.det.obj

V(cc.network)$membership <- membership(comm.det.obj)
network.D3$nodes$Group <- V(cc.network)$membership

forceNetwork(Links = network.D3$links, 
             Nodes = network.D3$nodes, 
             Source = 'source', 
             Target = 'target',
             NodeID = 'name',
             Group = 'Group', 
             opacity = 0.9,
             Value = 'Width',
             Nodesize = 'Degree', 
             # We input a JavaScript function.
             linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
             fontSize = 12,
             zoom = TRUE, 
             opacityNoHover = 1)

##words per cluster 

membership.df <- tibble(word = V(cc.network) %>% names,
                        cluster = V(cc.network)$membership)


V(cc.network)$membership %>%
  unique %>% 
  sort %>% 
  map_chr(.f = function(cluster.id) {
    
    membership.df %>% 
      filter(cluster == cluster.id) %>% 
      # Get  at most 15 words per cluster.
      slice(1:15) %>% 
      pull(word) %>% 
      str_c(collapse = ', ')
    
  }) 
write.csv(membership.df,"Community clusters.csv")




