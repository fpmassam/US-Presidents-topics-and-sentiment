
#How to extract topics and sentiment from the 'Presidential Corpus' collected by The Grammar Lab:
#http://www.thegrammarlab.com/?nor-portfolio=corpus-of-presidential-speeches-cops-and-a-clintontrump-corpus
#This code should be ran using, as workspace, the unzipped folder of this corpus


#Import files and emove tags
require(qdap)
library(readr)
tag <- read_csv("tag.csv")
speeches = lapply(tag$file, readr::read_file)
speeches = lapply(speeches, rm_angle)
names(speeches) = tag$file

#tokenize
tokenize_my_ass = function(x){
  require(tokenizers) 
  a = function(y){
    as.data.frame(tokenize_words(y))
  }
  lapply(x, a)
}
tokens = tokenize_my_ass(speeches)
tokens = lapply(tokens, as.data.frame)

#change colnames
tokens = lapply(tokens, setNames, 'tokens')
#Reomove stop words
remove_stopwords = function(x){
  require(tidyverse)
  require(dplyr)
  require(tidytext)
  x %>% 
    anti_join(stop_words, by = c('tokens'= 'word'))}
tokens = lapply(tokens, remove_stopwords)

#Token by president
##Given that we want to understand what topics were used by presidents during their office, I decided to tokenize them instead of speeches
token_with_id = bind_rows(tokens, .id = "date")
tokens_df = merge(tag, token_with_id, by.x = "date:@value", by.y = 'date')
tokens_df[1:3] = NULL
token_presindent = split(tokens_df, f = tokens_df$Presidents)
token_presindent = lapply(token_presindent, function(x) { x["Presidents"] <- NULL; x })

#Building a Documents term matrix
require(topicmodels)
require(tidytext)
dtms <-  function(x) {
  corpus::term_matrix(x$tokens)
  }
dtm = lapply(token_presindent, dtms)
save.image()


#Estract topics for each president
topic_extract<- function(x){  tidy(x, matrix = "beta")} 
topics_matrix = pblapply(topics_presidents, topic_extract)
topics_matrix_top_10 = function(x){x %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
}

top_10 = lapply(topics_matrix, topics_matrix_top_10)

topic_inaugu = lapply(dtm_inauguration, topic_list)
topic_inaugu = lapply(topic_inaugu, topic_extract)
topic_inaug_20 = lapply(topic_inaugu, topics_matrix_top_10)

#plot_topics 

plot_topics = function(x){x %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()}

plots = lapply(top_10, plot_topics)

#count the number of characters in speech
char_number = lapply(speeches, nchar)
char_number = data.frame(data = names(speeches), 
char_No = do.call(rbind.data.frame, char_number))
Char_progress = merge(aggregate(number_fonts ~ Presidents, data = char_number, FUN = mean),
                      aggregate(number_fonts ~ Presidents, data = char_number, FUN = sd),
                      by = 'Presidents')
colnames(Char_progress)[2:3] = c('length_avg', 'sd')
#Order presidents
Char_progress$Presidents <- factor(Char_progress$Presidents, levels = Char_progress$Presidents[order(Char_progress$Order)])

#Readability index
require(quanteda)
readability = function(x){
  textstat_readability(x)
}

read_president = lapply(speeches, readability)
flesh = do.call(rbind.data.frame, read_presidents)
flesh$data = rownames(flesh)
flesh = merge(tag, flesh, by.x = "date:@value", by.y = 'data')
flesh_avg = merge(aggregate(Flesch~Presidents, data = flesh, FUN = mean),
                  aggregate(Flesch~Presidents, data = flesh, FUN = sd),
                  by = 'Presidents')
colnames(flesh_avg)[2:3] = c('Flesch_mean', 'Flesc_sd')
flesh_avg = merge(presidents, flesh_avg, by = 'Presidents')
rownames(flesh_avg) = flesh_avg$Order
flesh_avg$Presidents <- factor(flesh_avg$Presidents, levels = flesh_avg$Presidents[order(flesh_avg$Order)])
#Plot readability 
require(ggplot2)
ggplot(flesh_avg, aes(y = Flesch_mean, x = Presidents, xmin = 0)) + 
  geom_col(aes(alpha = Flesch_mean), color = 'black') + 
  theme_bw() + 
  ylab('Flesch readability index') +
  geom_hline(aes(yintercept = 10)) +
  geom_hline(aes(yintercept = 30)) +
  geom_hline(aes(yintercept = 50)) +
  geom_label(x=5, y=13,label="Very hard to read", size = 2) +
  geom_label(x=15, y=33,label="Fairly difficult to read", size = 2) +
  geom_label(x=30, y=53,label="Fairly easy to read", size = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = 'Mean readability of speeches', subtitle = 'Taller bars correspond to texts easier to read') + theme(legend.position = 'none')


#Get sentiment
## Here I try to extract the 'plot curve' of presidential speeches. For more information:
##https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
require(syuzhet)
tokens_sentiment = lapply(speeches, get_sentences)
sentiment_list = function(x){
  get_sentiment(x, method = 'syuzhet')
}
#get comparable curves
syuzhet_speeches = lapply(tokens_sentiment, sentiment_list)
get_dct_list = function(x){
  if (length(x) > 4)
    {get_dct_transform(x, low_pass_size = 5)}
}
dct = lapply(syuzhet_speeches, get_dct_list)
dct = lapply(dct, as.data.frame)
narrative_time = function(x){
  data.frame(narrative_time = as.numeric(rownames(x)),
             value = x)
}

dct = lapply(dct, narrative_time)
dct_df = do.call(rbind.data.frame, dct)
dct_df$file = rownames(dct_df)
dct_df$file = gsub("\\..*","", dct_df$file )
dct_df$file =paste0(dct_df$file,'.txt')
dct_df = merge(tag, dct_df, by = 'file')
dct_df$Inauguration = dct_df$`title:@value`%in% Inaugural_speeches$`title:@value`
colnames(dct_df)[6] = 'Syuzhet'
dct_df =merge(dct_df, presidents, by = 'Presidents')
ggplot(dct_df, aes(x = narrative_time, y = Syuzhet, group = file)) + geom_line(aes(color = Inauguration)) + 
  facet_wrap(.~Order, labeller = labeller(dct_df$Presidents))

# get_nrc sentiment
require(syuzhet)
nrc_list = function(x){
  get_nrc_sentiment(x)
}

nrc_speeches = lapply(tokens_sentiment, nrc_list)
nrc_agg = lapply(nrc_speeches, colSums)
nrc_df = do.call(rbind, nrc_agg)
nrc_df = data.frame(row.names(nrc_df), nrc_df)
nrc_pos_neg = data.frame(nrc_df[1], nrc_df[10:11])
rownames(nrc_df) = NULL
rownames(nrc_pos_neg) = NULL
colnames(nrc_df)[1] = 'file'
colnames(nrc_pos_neg)[1] = 'file'
nrc_evo = merge(ordered_tag, nrc_df, by = 'file')
require(reshape2)
nrc_df[10:11] = NULL
nrc_plot = melt(nrc_df)
nrc_plot = merge(ordered_tag, nrc_plot, by = 'file')
require(ggplot2)
ggplot(nrc_plot, aes(y = file, x = value)) + 
  geom_col(aes(fill = variable)) + theme_bw() + scale_y_discrete(labels = nrc_plot$Order)

