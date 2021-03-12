#install.packages(c("readr","dplyr","ggplot2","corrplot","gapminder")
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gapminder)

#creates a dataframe that has the stock names on one side and their corrsponding 
# earnings to calculate the PE Ratios. 
stocks_names_pe <- data.frame(stocks = c("AG","GME", "BBBY","EXPR","BB","NOK","CTRM",
                                   "PLTR","AMC","NAKD"),
                        pe_values = c(-4.22, -1.82, -7.68, 0.11, -1.49,-0.19,
                                      -0.02,-1.29,-34.82,-1.49)) 

# this function uses the count data and stock data gathered from yahoo finance
# to join the stock market data to the sentiment data. 
# it filters the stock data to include the date, the Closing price of the stock
# per share, and the total volume of the stock. It calculates the PE Ratio and 
# Market Cap for each stock. 
market_data_fx <- function(stocks_pe){
  for (i in seq_len(nrow(stocks_pe))){
    stock_df <- read_csv(paste(stocks_pe[i,1], '.csv', sep=''))
    mean_sentiments <-as_tibble(read.csv(paste(stocks_pe[i,1], 
                                               "_avg_sentiment_counts.csv",
                                               sep=''),
                                         colClasses=c("NULL",NA,NA,NA),
                                         stringsAsFactors=FALSE))
    
    stock_df <- stock_df %>% select("Date","Close","Volume")
    stock_df["1"] <- stock_df["Close"] *stock_df["Volume"]
    stock_df["2"] <- stock_df$Close / stocks_pe[i,2]
    
    colnames(stock_df) <- c("Date", paste(stocks_pe[i,1], "_close",sep=''),
             paste(stocks_pe[i,1], "_volume",sep=''),
        paste(stocks_pe[i,1], "_market_cap",sep=''),
        paste(stocks_pe[i,1], "_PE_ratio",sep=''))
    stock_df <- stock_df %>% mutate(Date = as.character(Date))
    print(stock_df)
    print(mean_sentiments)
    mean_sentiments <- left_join(mean_sentiments, stock_df, by= c("Date"))
    write.csv(mean_sentiments, paste(stocks_pe[i,1], "_full_data.csv",sep=''))
  }
}
market_data_fx(stocks_names_pe)





####### Linear Regression Models #######
# linear regression analysis 


##### AG #####

AG_full_data <- as_tibble(read.csv("AG_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

AG_full_data %>% 
  ggplot(aes(y = AG_close, x = AG_count))+
  geom_point()
AG_full_data %>% 
  ggplot(aes(y = AG_close, x = AG_avg_sentiment))+
  geom_point()
AG_full_data %>% 
  ggplot(aes(y = AG_close, x = AG_market_cap))+
  geom_point()
AG_full_data %>% 
  ggplot(aes(y = AG_close, x = AG_PE_ratio))+
  geom_point()
AG_full_data %>% 
  ggplot(aes(y = AG_avg_sentiment, x = as.Date(Date)))+
  geom_point()
AG_full_data %>% 
  ggplot(aes(y = AG_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


AG_independent_vars <- AG_full_data %>% select(-Date, -AG_close)
# correlation then subsequent plot showing correlations
ag_cor <- cor(AG_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(ag_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

ag_lm <- lm(AG_full_data$AG_close ~ AG_full_data$AG_count + 
              AG_full_data$AG_avg_sentiment + AG_full_data$AG_market_cap + 
              AG_full_data$AG_PE_ratio)

summary(ag_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(ag_lm)


##### AMC #####

AMC_full_data <- as_tibble(read.csv("AMC_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

AMC_full_data %>% 
  ggplot(aes(y = AMC_close, x = AMC_count))+
  geom_point()
AMC_full_data %>% 
  ggplot(aes(y = AMC_close, x = AMC_avg_sentiment))+
  geom_point()
AMC_full_data %>% 
  ggplot(aes(y = AMC_close, x = AMC_market_cap))+
  geom_point()
AMC_full_data %>% 
  ggplot(aes(y = AMC_close, x = AMC_PE_ratio))+
  geom_point()
AMC_full_data %>% 
  ggplot(aes(y = AMC_avg_sentiment, x = as.Date(Date)))+
  geom_point()
AMC_full_data %>% 
  ggplot(aes(y =AMC_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


AMC_independent_vars <- AMC_full_data %>% select(-Date, -AMC_close)
# correlation then subsequent plot showing correlations
AMC_cor <- cor(AMC_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(AMC_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

AMC_lm <- lm(AMC_full_data$AMC_close ~ AMC_full_data$AMC_count + 
               AMC_full_data$AMC_avg_sentiment + AMC_full_data$AMC_market_cap + 
               AMC_full_data$AMC_PE_ratio)

summary(AMC_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(AMC_lm)

##### BB #####

BB_full_data <- as_tibble(read.csv("BB_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

BB_full_data %>% 
  ggplot(aes(y = BB_close, x = BB_count))+
  geom_point()
BB_full_data %>% 
  ggplot(aes(y = BB_close, x = BB_avg_sentiment))+
  geom_point()
BB_full_data %>% 
  ggplot(aes(y = BB_close, x = BB_market_cap))+
  geom_point()
BB_full_data %>% 
  ggplot(aes(y = BB_close, x = BB_PE_ratio))+
  geom_point()
BB_full_data %>% 
  ggplot(aes(y = BB_avg_sentiment, x = as.Date(Date)))+
  geom_point()
BB_full_data %>% 
  ggplot(aes(y = BB_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


BB_independent_vars <- BB_full_data %>% select(-Date, -BB_close)
# correlation then subsequent plot showing correlations
BB_cor <- cor(BB_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(BB_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

BB_lm <- lm(BB_full_data$BB_close ~ BB_full_data$BB_count + 
              BB_full_data$BB_avg_sentiment + BB_full_data$BB_market_cap + 
              BB_full_data$BB_PE_ratio)

summary(BB_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(BB_lm)

##### BBBY #####

BBBY_full_data <- as_tibble(read.csv("BBBY_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

BBBY_full_data %>% 
  ggplot(aes(y = BBBY_close, x = BBBY_count))+
  geom_point()
BBBY_full_data %>% 
  ggplot(aes(y = BBBY_close, x = BBBY_avg_sentiment))+
  geom_point()
BBBY_full_data %>% 
  ggplot(aes(y = BBBY_close, x = BBBY_market_cap))+
  geom_point()
BBBY_full_data %>% 
  ggplot(aes(y = BBBY_close, x = BBBY_PE_ratio))+
  geom_point()
BBBY_full_data %>% 
  ggplot(aes(y = BBBY_avg_sentiment, x = as.Date(Date)))+
  geom_point()
BBBY_full_data %>% 
  ggplot(aes(y = BBBY_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


BBBY_independent_vars <- BBBY_full_data %>% select(-Date, -BBBY_close)
# correlation then subsequent plot showing correlations
BBBY_cor <- cor(BBBY_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(BBBY_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

BBBY_lm <- lm(BBBY_full_data$BBBY_close ~ BBBY_full_data$BBBY_count + 
                BBBY_full_data$BBBY_avg_sentiment + BBBY_full_data$BBBY_market_cap + 
                BBBY_full_data$BBBY_PE_ratio)

summary(BBBY_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(BBBY_lm)

##### CTRM #####

CTRM_full_data <- as_tibble(read.csv("CTRM_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

CTRM_full_data %>% 
  ggplot(aes(y = CTRM_close, x = CTRM_count))+
  geom_point()
CTRM_full_data %>% 
  ggplot(aes(y = CTRM_close, x = CTRM_avg_sentiment))+
  geom_point()
CTRM_full_data %>% 
  ggplot(aes(y = CTRM_close, x = CTRM_market_cap))+
  geom_point()
CTRM_full_data %>% 
  ggplot(aes(y = CTRM_close, x = CTRM_PE_ratio))+
  geom_point()
CTRM_full_data %>% 
  ggplot(aes(y =CTRM_avg_sentiment, x = as.Date(Date)))+
  geom_point()
CTRM_full_data %>% 
  ggplot(aes(y = CTRM_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


CTRM_independent_vars <- CTRM_full_data %>% select(-Date, -CTRM_close)
# correlation then subsequent plot showing correlations
CTRM_cor <- cor(CTRM_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(CTRM_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

CTRM_lm <- lm(CTRM_full_data$CTRM_close ~ CTRM_full_data$CTRM_count + 
                CTRM_full_data$CTRM_avg_sentiment + CTRM_full_data$CTRM_market_cap + 
                CTRM_full_data$CTRM_PE_ratio)

summary(CTRM_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(CTRM_lm)

##### EXPR #####

EXPR_full_data <- as_tibble(read.csv("EXPR_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

EXPR_full_data %>% 
  ggplot(aes(y = EXPR_close, x = EXPR_count))+
  geom_point()
EXPR_full_data %>% 
  ggplot(aes(y = EXPR_close, x = EXPR_avg_sentiment))+
  geom_point()
EXPR_full_data %>% 
  ggplot(aes(y = EXPR_close, x = EXPR_market_cap))+
  geom_point()
EXPR_full_data %>% 
  ggplot(aes(y = EXPR_close, x = EXPR_PE_ratio))+
  geom_point()
EXPR_full_data %>% 
  ggplot(aes(y = EXPR_avg_sentiment, x = as.Date(Date)))+
  geom_point()
EXPR_full_data %>% 
  ggplot(aes(y = EXPR_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


EXPR_independent_vars <- EXPR_full_data %>% select(-Date, -EXPR_close)
# correlation then subsequent plot showing correlations
EXPR_cor <- cor(EXPR_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(EXPR_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

EXPR_lm <- lm(EXPR_full_data$EXPR_close ~ EXPR_full_data$EXPR_count + 
                EXPR_full_data$EXPR_avg_sentiment + EXPR_full_data$EXPR_market_cap + 
                EXPR_full_data$EXPR_PE_ratio)

summary(EXPR_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(EXPR_lm)

##### GME #####

GME_full_data <- as_tibble(read.csv("GME_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

GME_full_data %>% 
  ggplot(aes(y = GME_close, x = GME_count))+
  geom_point()
GME_full_data %>% 
  ggplot(aes(y = GME_close, x = GME_avg_sentiment))+
  geom_point()
GME_full_data %>% 
  ggplot(aes(y = GME_close, x = GME_market_cap))+
  geom_point()
GME_full_data %>% 
  ggplot(aes(y = GME_close, x = GME_PE_ratio))+
  geom_point()
GME_full_data %>% 
  ggplot(aes(y = GME_avg_sentiment, x = as.Date(Date)))+
  geom_point()
GME_full_data %>% 
  ggplot(aes(y = GME_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


GME_independent_vars <- GME_full_data %>% select(-Date, -GME_close)
# correlation then subsequent plot showing correlations
GME_cor <- cor(GME_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(GME_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

GME_lm <- lm(GME_full_data$GME_close ~ GME_full_data$GME_count + 
              GME_full_data$GME_avg_sentiment + GME_full_data$GME_market_cap + 
               GME_full_data$GME_PE_ratio)

summary(GME_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(GME_lm)

##### NAKD #####

NAKD_full_data <- as_tibble(read.csv("NAKD_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

NAKD_full_data %>% 
  ggplot(aes(y = NAKD_close, x = NAKD_count))+
  geom_point()
NAKD_full_data %>% 
  ggplot(aes(y = NAKD_close, x = NAKD_avg_sentiment))+
  geom_point()
NAKD_full_data %>% 
  ggplot(aes(y = NAKD_close, x = NAKD_market_cap))+
  geom_point()
NAKD_full_data %>% 
  ggplot(aes(y = NAKD_close, x = NAKD_PE_ratio))+
  geom_point()
NAKD_full_data %>% 
  ggplot(aes(y = NAKD_avg_sentiment, x = as.Date(Date)))+
  geom_point()
NAKD_full_data %>% 
  ggplot(aes(y = NAKD_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


NAKD_independent_vars <- NAKD_full_data %>% select(-Date, -NAKD_close)
# correlation then subsequent plot showing correlations
NAKD_cor <- cor(NAKD_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(NAKD_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

NAKD_lm <- lm(NAKD_full_data$NAKD_close ~ NAKD_full_data$NAKD_count + 
                NAKD_full_data$NAKD_avg_sentiment + NAKD_full_data$NAKD_market_cap + 
                NAKD_full_data$NAKD_PE_ratio)

summary(NAKD_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(NAKD_lm)

##### NOK #####

NOK_full_data <- as_tibble(read.csv("NOK_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

NOK_full_data %>% 
  ggplot(aes(y = NOK_close, x = NOK_count))+
  geom_point()
NOK_full_data %>% 
  ggplot(aes(y = NOK_close, x = NOK_avg_sentiment))+
  geom_point()
NOK_full_data %>% 
  ggplot(aes(y = NOK_close, x = NOK_market_cap))+
  geom_point()
NOK_full_data %>% 
  ggplot(aes(y = NOK_close, x = NOK_PE_ratio))+
  geom_point()
NOK_full_data %>% 
  ggplot(aes(y = NOK_avg_sentiment, x = as.Date(Date)))+
  geom_point()
NOK_full_data %>% 
  ggplot(aes(y = NOK_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


NOK_independent_vars <- NOK_full_data %>% select(-Date, -NOK_close)
# correlation then subsequent plot showing correlations
NOK_cor <- cor(NOK_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(NOK_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

NOK_lm <- lm(NOK_full_data$NOK_close ~ NOK_full_data$NOK_count + 
               NOK_full_data$NOK_avg_sentiment + NOK_full_data$NOK_market_cap + 
               NOK_full_data$NOK_PE_ratio)

summary(NOK_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(NOK_lm)

##### PLTR #####

PLTR_full_data <- as_tibble(read.csv("PLTR_full_data.csv",
                                   colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA),
                                   stringsAsFactors = FALSE))

#### Assumptions for linear regression ####

# Linearity between independent and dependent variables

PLTR_full_data %>% 
  ggplot(aes(y = PLTR_close, x = PLTR_count))+
  geom_point()
PLTR_full_data %>% 
  ggplot(aes(y = PLTR_close, x = PLTR_avg_sentiment))+
  geom_point()
PLTR_full_data %>% 
  ggplot(aes(y = PLTR_close, x = PLTR_market_cap))+
  geom_point()
PLTR_full_data %>% 
  ggplot(aes(y = PLTR_close, x = PLTR_PE_ratio))+
  geom_point()
PLTR_full_data %>% 
  ggplot(aes(y = PLTR_avg_sentiment, x = as.Date(Date)))+
  geom_point()
PLTR_full_data %>% 
  ggplot(aes(y = PLTR_count, x = as.Date(Date)))+
  geom_point()
# Correlation between independent variables - need to make sure that there is
# not too high of a correlation between any of the independent variables


PLTR_independent_vars <- PLTR_full_data %>% select(-Date, -PLTR_close)
# correlation then subsequent plot showing correlations
PLTR_cor <- cor(PLTR_independent_vars, use="pairwise.complete.obs")
layout(1)
corrplot(PLTR_cor, method = "number")
# very high correlation between PE Ratio and posts and market cap - removing the PE Ratio

PLTR_lm <- lm(PLTR_full_data$PLTR_close ~ PLTR_full_data$PLTR_count + 
                PLTR_full_data$PLTR_avg_sentiment + PLTR_full_data$PLTR_market_cap + 
                PLTR_full_data$PLTR_PE_ratio)

summary(PLTR_lm)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(PLTR_lm)

