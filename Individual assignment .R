#######################################################
# HOMEWORK: 
# Neil Parekh
# Student Id :4647284
#######################################################
install.packages('pdftools')
install.packages('shapeR')
install.packages('tidytext')
install.packages('tidyverse')
install.packages("textreadr")
install.packages("textshape")
install.packages("dplyr")
install.packages("textdata")
install.packages("reshape2")
install.packages("wordcloud")
install.packages("igraph")
install.packages("ggraph")

library(pdftools)
library(shapeR)
library(tidytext)
library(tidyverse)
library(textreadr)
library(textshape)
library(dplyr)
library(scales)
library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(textdata)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)

# Clean environment
rm(list = ls())

# Read in text and save it to variable my_pdf_text as dataframe
pwd <- "/Users/neilparekh/Desktop/Business Analytics/TA/PDF"
setwd(pwd)
nm <- list.files(path=pwd)

my_pdf_text <- data.frame(do.call(rbind, lapply(nm,function(x) pdf_text(x))))
# Transpose
subject_names <- c('Text Analysis & Natural Language Processing','Data Management and SQL','Data optimization',
                   'Data Science Python','Data science R','Data Strategy','Data Visualization','Machine Learning')
subject_names <- c('Text Analysis & Natural Language Processing',
                   'Data Management and SQL',
                   'Data optimization',
                   'Data Science Python',
                   'Data science R',
                   'Data Strategy',
                   'Data Visualization',
                   'Machine Learning',
                   'Global Strategy',
                   'Digital Marketing Strategy',
                   'Critical Analysis')
my_pdf_text <- data_frame(line=1:ncol(my_pdf_text),text=t(as.matrix(my_pdf_text)),subject=subject_names) 

# Custom stopwords
custom_stop_words <- c('recommendations','regression')

#Looping over each subject
for (val in 1:nrow(my_pdf_text)){
#for (val in 11:11){
  subject_pdf_text <- my_pdf_text[val,]
  subject_name <- subject_names[[val]]
  #Token
  token_list <- subject_pdf_text %>%
    unnest_tokens(word,text)
  print(token_list)
  
  #Remove stop words
  tokens_nostop <- token_list %>%
    anti_join(stop_words) %>%
    filter(!word %in% custom_stop_words)  #here’s where we remove  custom tokens
  
  frequencies_tokens_nostop <- tokens_nostop %>%
    count(word, sort=TRUE) %>%
    filter(n>1)
  print(frequencies_tokens_nostop)
  
  #token frequency histogram
  freq_hist <- frequencies_tokens_nostop  %>%
    ggplot(aes(word, n))+
    ggtitle(subject_name)+
    geom_col()+
    coord_flip()
  print(freq_hist)
  
  # Calculate token graph
  token_graph <- frequencies_tokens_nostop %>%
    graph_from_data_frame() 
  
  token_graph_plot <- ggraph(token_graph, layout = "fr")+
    ggtitle(subject_name)+
    geom_edge_link()+
    geom_node_point()+
    geom_node_text(aes(label=name), vjust=1, hjust=1)
  print(token_graph_plot)
  
  #Business Analysis 
  nrc <- get_sentiments("nrc")
  #table(nrc$sentiment) 
  sentiments <- bind_rows(
    #(mutate(afinn,lexicon="afinn")),
    (mutate(nrc,lexicon="nrc")),
    #(mutate(bing,lexicon="bing"))
  )
  
  #Sentiment analysis
  my_sentiment_nrc <- tokens_nostop %>%
    inner_join(get_sentiments("nrc")) 
  my_sentiment_bing <- tokens_nostop %>%
    inner_join(get_sentiments("bing")) 
  my_sentiment_afinn <- tokens_nostop %>%
    inner_join(get_sentiments("afinn"))
  
  my_sentiment_afinn_mean_value <- my_sentiment_afinn %>%
    summarise(mean(value)) #Mean per subject
  view(my_sentiment_afinn_mean_value)
  table(my_sentiment_bing$sentiment)
  View(my_sentiment_nrc)
  #Graph for negative and positive 
  my_sentiment_nrc_plot<-my_sentiment_nrc %>%
    group_by(sentiment) %>%
    count(word, sentiment, sort=T)%>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word=reorder(word, n))
  
  my_sentiment_nrc_plot%>%
    ggplot(aes(word, n, fill=sentiment)) +
    ggtitle(subject_name)+
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y="different types of sentiment", x=NULL)+
    coord_flip()
  
  # Wordcloud to give accucurate data
  my_sentiment_nrc %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(max.words = 100,
                     scale = c(0.4, 1),
                     fixed.asp=TRUE,
                     use.r.layout=TRUE,
                     match.colors=FALSE,
                     random.order=FALSE,
                     title.size=1) %>%
    title(main=paste("\n", subject_name, sep=""))
  
  ###########################################################
  ###### What if we are interested in the most common #######
  ################ 2 consecutive words - bi-gram ########
  ###########################################################
  
  # We want to see the bigrams (words that appear together, "pairs")
  subject_bigrams <- subject_pdf_text %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
  
  subject_bigrams_separated <- subject_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  subject_bigrams_filtered <- subject_bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word1 %in% custom_stop_words) %>%
    filter(!word2 %in% custom_stop_words) 
  
  # Creating new bigram, "no-stop-words":
  subject_bigram_counts <- subject_bigrams_filtered %>%
    count(word1, word2, sort = TRUE)
  
  # Want to see the new bigrams
  subject_bigram_counts
  
  ###########################################################
  ###### What if we are interested in the most common #######
  ################ 4 consecutive words - quadro-gram ########
  ###########################################################
  
  subject_quadrograms <- subject_pdf_text %>%
    unnest_tokens(quadrogram, text, token = "ngrams", n=4)
  
  subject_quadrograms_separated <- subject_quadrograms %>%
    separate(quadrogram, c("word1", "word2", "word3", "word4"), sep = " ")
  
  subject_quadrograms_filtered <- subject_quadrograms_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    filter(!word4 %in% stop_words$word) %>%
    filter(!word1 %in% custom_stop_words) %>%
    filter(!word2 %in% custom_stop_words) %>%
    filter(!word3 %in% custom_stop_words) %>%
    filter(!word4 %in% custom_stop_words) 
  
  # Creating new quadrogram, "no-stop-words":
  subject_quadrogram_counts <- subject_quadrograms_filtered %>%
    count(word1, word2, word3, word4, sort = TRUE)
  
  # Want to see the new quadrograms
  subject_quadrogram_counts
}
