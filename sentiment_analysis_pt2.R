library(tidyverse)
library(vader)

# Importing the datasets - The sentiment csv files are created below, but have been
# commented out since they take a very long time to run to create the files.

AG_comments <- as_tibble(read.csv("AG_comments.csv",colClasses=c("NULL",NA,NA)))
AG_comments_sentiment <- as_tibble(read.csv("AG_comments_sentiment.csv"))
AMC_comments <- as_tibble(read.csv("AMC_comments.csv",colClasses=c("NULL",NA,NA)))
AMC_comments_sentiment <- as_tibble(read.csv("AMC_comments_sentiment.csv"))
BB_comments <- as_tibble(read.csv("BB_comments.csv",colClasses=c("NULL",NA,NA)))
BB_comments_sentiment <- as_tibble(read.csv("BB_comments_sentiment.csv"))
BBBY_comments <- as_tibble(read.csv("BBBY_comments.csv",colClasses=c("NULL",NA,NA)))
BBBY_comments_sentiment <- as_tibble(read.csv("BBBY_comments_sentiment.csv"))
CTRM_comments <- as_tibble(read.csv("CTRM_comments.csv",colClasses=c("NULL",NA,NA)))
CTRM_comments_sentiment <- as_tibble(read.csv("CTRM_comments_sentiment.csv"))
EXPR_comments <- as_tibble(read.csv("EXPR_comments.csv",colClasses=c("NULL",NA,NA)))
EXPR_comments_sentiment <- as_tibble(read.csv("EXPR_comments_sentiment.csv"))
GME_comments <- as_tibble(read.csv("GME_comments.csv",colClasses=c("NULL",NA,NA)))
GME_comments_sentiment_1 <- as_tibble(read.csv("GME_comments_sentiment_1.csv"))
GME_comments_sentiment_2 <- as_tibble(read.csv("GME_comments_sentiment_2.csv"))
GME_comments_sentiment_3 <- as_tibble(read.csv("GME_comments_sentiment_3.csv"))
GME_comments_sentiment_4 <- as_tibble(read.csv("GME_comments_sentiment_4.csv"))
NAKD_comments <- as_tibble(read.csv("NAKD_comments.csv",colClasses=c("NULL",NA,NA)))
NAKD_comments_sentiment <- as_tibble(read.csv("NAKD_comments_sentiment.csv"))
NOK_comments <- as_tibble(read.csv("NOK_comments.csv",colClasses=c("NULL",NA,NA)))
NOK_comments_sentiment <- as_tibble(read.csv("NOK_comments_sentiment.csv"))
PLTR_comments <- as_tibble(read.csv("PLTR_comments.csv",colClasses=c("NULL",NA,NA)))
PLTR_comments_sentiment <- as_tibble(read.csv("PLTR_comments_sentiment.csv"))


# this is the code from the tutorial showing how to use the sentiment analysis tool. 

# Vader is a lexicon and rule-based sentiment analysis tool that is specifically 
# attuned to sentiments expressed in social media (...).

get_vader("I like this stock")
get_vader("I really like this stock")
get_vader("would never buy this trash")
get_vader("abolute dog shit sucks")

# Download package source here:
# https://cran.r-project.org/web/packages/vader/index.html
untar("vader_0.2.1.tar.gz",list=TRUE)
untar("vader_0.2.1.tar.gz")
load("vader/R/sysdata.rda")
#"C:/Users/billf/Google Drive/Entrepenuership/Freelance2/CassandraHui/
vaderLexicon %>% 
  as_tibble()

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "yolo")

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "retard")

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "call")

# let's add some words 
wsbLexicon <- bind_rows(tibble(V1 = c("retard", "retarded", "fuck", "fucking", "autist", "fag", "gay", "stonk"), V2 = 0, V3 = 0.5), # neutral 
                        tibble(V1 = c("bull", "bullish", "tendie", "tendies", "call", "long", "buy", "moon", "hold",              # positive
                                      "diamond", "hands", "yolo", "yoloed", "free", "btfd", "rocket", "elon", "gain",
                                      "420", "calls", "longs", "sky", "space", "roof", "squeeze", "balls"), V2 = 1.5, V3 = 0.5),                     
                        tibble(V1 = c("bear", "sell", "put", "short", "shorts", "puts", "bagholder", "wife", "boyfriend",         # negative
                                      "shorting", "citron", "hedge", "fake"), V2 = -1.5, V3 = 0.5))

# add back to lexicon
vaderLexiconWSB <- vaderLexicon %>% 
  as_tibble() %>% 
  # anti_join(wsbLexicon, by = "V1") %>% 
  filter(!(V1 %in% wsbLexicon$V1)) %>% 
  bind_rows(wsbLexicon) %>% 
  as.data.frame()

vaderLexicon <- vaderLexiconWSB

save(vaderLexicon, file = "vader/R/sysdata.rda")

# remove the vader package and reinstall (maybe restart R session after removing also)
detach("package:vader", unload = T)
remove.packages("vader")

install.packages("vader/", repos = NULL, type = "source")

library(vader)

get_vader("just yoloed GME, will not sell before 420 fucking diamond hands")
get_vader("SLV is a fake squeeze, hate hedge funds")

#This is the creation of the sentiment csv files.They have been commented out 
# since they take a long time to run. 

# CTRM_comments_sentiment <- CTRM_comments %>% select(CTRM) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(CTRM, "\\\\", " ")) %>%
# mutate(sentiment = vader_df(comment_clean)$compound)
#
# write_csv(CTRM_comments_sentiment, "CTRM_comments_sentiment.csv")
#
# AG_comments_sentiment <- AG_comments %>% select(AG) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(AG, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(AG_comments_sentiment, "AG_comments_sentiment.csv")
# AMC_comments_sentiment <- AMC_comments %>% select(AMC) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(AMC, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(AMC_comments_sentiment, "AMC_comments_sentiment.csv")
# BB_comments_sentiment <- BB_comments %>% select(BB) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(BB, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(BB_comments_sentiment, "BB_comments_sentiment.csv")
# BBBY_comments_sentiment <- BBBY_comments %>% select(BBBY) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(BBBY, "\\\\", " ")) %>%
# mutate(sentiment = vader_df(comment_clean)$compound)
#
# write_csv(BBBY_comments_sentiment, "BBBY_comments_sentiment.csv")
# EXPR_comments_sentiment <- EXPR_comments %>% select(EXPR) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(EXPR, "\\\\", " ")) %>%
# mutate(sentiment = vader_df(comment_clean)$compound)
#
# write_csv(EXPR_comments_sentiment, "EXPR_comments_sentiment.csv")
#
# NAKD_comments_sentiment <- NAKD_comments %>% select(NAKD) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(NAKD, "\\\\", " ")) %>%
# mutate(sentiment = vader_df(comment_clean)$compound)
#
# write_csv(NAKD_comments_sentiment, "NAKD_comments_sentiment.csv")
# NOK_comments_sentiment <- NOK_comments %>% select(NOK) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(NOK, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(NOK_comments_sentiment, "NOK_comments_sentiment.csv")
# PLTR_comments_sentiment <- PLTR_comments %>% select(PLTR) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(PLTR, "\\\\", " ")) %>%
# mutate(sentiment = vader_df(comment_clean)$compound)
#
# write_csv(PLTR_comments_sentiment, "PLTR_comments_sentiment.csv") # GME is so
# large, I split it into groups of 100,000 posts each to be able to actually
# find the sentiments split_df <- split(GME_comments, sample(rep(1:4)))
#
# GME_comments_sentiment_1 <- split_df$`1` %>% select(GME) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(GME, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(GME_comments_sentiment_1, "GME_comments_sentiment_1.csv")
#
# GME_comments_sentiment_2 <- split_df$`2` %>% select(GME) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(GME, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(GME_comments_sentiment_2, "GME_comments_sentiment_2.csv")
#
# GME_comments_sentiment_3 <- split_df$`3` %>% select(GME) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(GME, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(GME_comments_sentiment_3, "GME_comments_sentiment_3.csv")
#
# GME_comments_sentiment_4 <- split_df$`4` %>% select(GME) %>% distinct() %>%
# mutate(comment_clean = str_replace_all(GME, "\\\\", " ")) %>% mutate(sentiment
# = vader_df(comment_clean)$compound)
#
# write_csv(GME_comments_sentiment_4, "GME_comments_sentiment_4.csv")
#

# This function combines the sentiment analysis completed in the previous step 
# and the dates from the comments table, so that each sentiment analysis now has 
# the date associated with it again. 
redate_sentiment_analysis <- function(df, sentiment_df){ 
  by_col <- colnames(sentiment_df[1]) 
  df_remove_dups <- distinct(df, df[,by_col], .keep_all = TRUE) 
  comment_sentiments_df <- sentiment_df %>%
    select(-comment_clean) %>% left_join(df_remove_dups,by = by_col)
  return(comment_sentiments_df) 
}

# these somehow did not have the stock ticker in front of the name for 
# time created column - so this code puts it into the column name.
BBBY_sentiments <- BBBY_sentiments %>% rename("BBBY_time_created" = time_created)
EXPR_sentiments <- EXPR_sentiments %>% rename("EXPR_time_created" = time_created)
PLTR_sentiments <- PLTR_sentiments %>% rename("PLTR_time_created" = time_created)

AG_sentiments <- redate_sentiment_analysis(AG_comments, AG_comments_sentiment)
AMC_sentiments <- redate_sentiment_analysis(AMC_comments, AMC_comments_sentiment)
BB_sentiments <- redate_sentiment_analysis(BB_comments, BB_comments_sentiment)
BBBY_sentiments <- redate_sentiment_analysis(BBBY_comments, BBBY_comments_sentiment)
CTRM_sentiments <- redate_sentiment_analysis(CTRM_comments, CTRM_comments_sentiment)
EXPR_sentiments <- redate_sentiment_analysis(EXPR_comments, EXPR_comments_sentiment)
GME1_sentiments <- redate_sentiment_analysis(GME_comments, GME_comments_sentiment_1)
GME2_sentiments <- redate_sentiment_analysis(GME_comments, GME_comments_sentiment_2)
GME3_sentiments <- redate_sentiment_analysis(GME_comments, GME_comments_sentiment_3)
GME4_sentiments <- redate_sentiment_analysis(GME_comments, GME_comments_sentiment_4)
NAKD_sentiments <- redate_sentiment_analysis(NAKD_comments, NAKD_comments_sentiment)
NOK_sentiments <- redate_sentiment_analysis(NOK_comments, NOK_comments_sentiment)
PLTR_sentiments <- redate_sentiment_analysis(PLTR_comments, PLTR_comments_sentiment)

# Recombined the GME sentiments so that we can find the totals for daily sentiment
GME_sentiments <- rbind(GME1_sentiments, GME2_sentiments,GME3_sentiments,
                        GME4_sentiments)

# this function summarizes the sentiment datasets into how many per day, and takes
# the average of sentiments for each day
reddit_sentiment_counts <- function(sentiments){
  stock_name <- colnames(sentiments[1])
  date_col_name <- paste(stock_name,'_time_created',sep='')
  sentiments[[date_col_name]] <- as.Date(sentiments[[date_col_name]])
  sentiments <- sentiments %>% 
    group_by(date = sentiments[[date_col_name]]) %>% 
    summarise(avg_sentiment = mean(sentiment),
              count = n())
  colnames(sentiments) <- c("Date", paste(stock_name,'_avg_sentiment',sep=''),
                            paste(stock_name,'_count',sep=''))
  return(sentiments)
}

write.csv(reddit_sentiment_counts(AG_sentiments),'AG_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(AMC_sentiments),
          'AMC_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(BB_sentiments),
          'BB_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(BBBY_sentiments),
          'BBBY_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(CTRM_sentiments),
          'CTRM_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(EXPR_sentiments),
          'EXPR_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(GME_sentiments),
          'GME_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(NAKD_sentiments),
          'NAKD_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(NOK_sentiments),
          'NOK_avg_sentiment_counts.csv')
write.csv(reddit_sentiment_counts(PLTR_sentiments),
          'PLTR_avg_sentiment_counts.csv')



#these were the original functions used by the tutorial. 
# reddit_mentions_sentiment <- reddit_mentions %>% 
#   left_join(comments_sentiment %>% select(-comment_clean),
#             by = "comment")
# 
# reddit_sentiment_counts <- reddit_mentions_sentiment %>% 
#   group_by(comm_date, stock_mention) %>% 
#   summarise(sentiment = mean(sentiment),
#             n = n())
# 
# reddit_sentiment_counts %>% 
#   filter(stock_mention == "TSLA") %>% 
#   ggplot(aes(x = comm_date, y = sentiment)) +
#   geom_line() +
#   theme_classic()
# 
# top5 <- reddit_sentiment_counts %>% 
#   group_by(stock_mention) %>% 
#   summarise(n = sum(n)) %>% 
#   ungroup() %>% 
#   arrange(-n) %>%
#   head(10) %>% 
#   pull(stock_mention)
# 
# reddit_sentiment_counts %>% 
#   filter(stock_mention %in% top5) %>% 
#   ggplot(aes(x = comm_date, y = sentiment, color = stock_mention)) +
#   # geom_line() +
#   geom_smooth(se = F) +
#   theme_classic()
# 
# 
# 
# reddit_mentions_sentiment %>% 
#   saveRDS("data/reddit_mentions_sentiment.RDS")
