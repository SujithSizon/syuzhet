## sentiment analysis with syuzhet
## vp nagraj
## last modified 4.16.15

setwd("~/Sites/Shiny/syuzhet/")

## install and load package — available via devtools
## install.packages("devtools")

## devtools::install_github("mjockers/syuzhet")
library(syuzhet)

## take a look at the functions
ls("package:syuzhet")

## compare with other text analysis packages

## install.packages("tm")
library(tm)
ls("package:tm")

## install.packages("qdap")
library(qdap)
ls("package:qdap")

## PART I — Narrative Structure (Moby Dick)

## partially adapted from syuzhet vignette: https://github.com/mjockers/syuzhet/blob/master/inst/doc/syuzhet-vignette.R
## vignette("syuzhet-vignette")

## load data
path_to_text <- "http://www.gutenberg.org/cache/epub/2701/pg2701.txt"

## note: only works with http

moby_d <- get_text_as_string(path_to_text)

## split text into character vector by sentence
moby_d <- get_sentences(moby_d)

## get sentiment vector — bing method

moby_d_sent_bing <- get_sentiment(moby_d, method="bing")

moby_d_sent_bing

plot(
        moby_d_sent_bing, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time (Line Number)", 
        ylab= "Emotional Valence",
        col="red"
)


## get mean of sentiment vectors over the narrative timeline as divided up into 100 even parts
moby_d_sent_bing_vals <- get_percentage_values(moby_d_sent_bing)

moby_d_sent_bing_vals

plot(
        moby_d_sent_bing_vals, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time (Percentage)", 
        ylab= "Emotional Valence",
        col="red"
)

## transform with fourier transformtion over percentage of the narrative
moby_d_sent_bing_ft <- get_transformed_values(moby_d_sent_bing)

moby_d_sent_bing_ft

plot(
        moby_d_sent_bing_ft, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time (Percentage)", 
        ylab= "Emotional Valence",
        col="red"
)

## get sentiment vector — a finn method
moby_d_sent_afinn <- get_sentiment(moby_d, method="afinn")

moby_d_sent_afinn

## get sentiment vector — nrc method
moby_d_sent_nrc <- get_sentiment(moby_d, method="nrc")

moby_d_sent_nrc

## get sentiment with get_nrc_sentiment function

moby_d_sent_nrc2 <- get_nrc_sentiment(moby_d)

class(moby_d_sent_nrc2)

head(moby_d_sent_nrc2)

moby_d_anticipation <- which(moby_d_sent_nrc2$anticipation > 0)
moby_d[moby_d_anticipation]

## clean-up work space 

rm(list= ls())

## PART II — Sentiment Analysis (Twitter Data)

load("~/Sites/Shiny/tweets.Rda")
load("~/Sites/Shiny/tweets2.Rda")
library(twitteR)

## tweets come in as a list object — maybe there's a twitteR function to handle that

ls("package:twitteR")

## ... and there is — twListToDF()

obama_tweets_df <- twListToDF(obama_tweets)
obama_tweets_df2 <- twListToDF(obama_tweets2)

## get rid of the list of tweets to speed things up

rm(obama_tweets,obama_tweets2)

## also need to clean up invalid characters with regex ... don't ask

obama_tweets_df$text <- gsub("[^0-9A-Za-z///' ]", " ", obama_tweets_df$text)
obama_tweets_df2$text <- gsub("[^0-9A-Za-z///' ]", " ", obama_tweets_df2$text)

## now apply the get_nrc_sentiment function to the text variable

obama_tweets_nrc <- get_nrc_sentiment(obama_tweets_df$text)
obama_tweets_nrc2 <- get_nrc_sentiment(obama_tweets_df2$text)

summary(obama_tweets_nrc)
summary(obama_tweets_nrc2)

library(tidyr)
library(dplyr)

tidy_obama_tweets <- 
        obama_tweets_nrc %>%
        select(1:8) %>%
        gather() %>%
        group_by(Sentiment=key) %>%
        summarise(Total=sum(value))

tidy_obama_tweets2 <- 
        obama_tweets_nrc2 %>%
        select(1:8) %>%
        gather() %>%
        group_by(Sentiment=key) %>%
        summarise(Total=sum(value))

summary(tidy_obama_tweets)
summary(tidy_obama_tweets2)

library(ggplot2)

# p_obama <- ggplot(tidy_obama_tweets, aes(Sentiment,Total)) +
#         geom_bar(stat="identity") +
#         ylab("Total Weight") +
#         ggtitle("Obama Tweet Sentiment (3/11/15)")
# 
# p_obama

p_obama <- ggplot(NULL, aes(Sentiment,Total)) +
        geom_bar(aes(fill="march"), data=tidy_obama_tweets, stat="identity", alpha=0.5) +
        geom_bar(aes(fill="april"), data=tidy_obama_tweets2, stat="identity", alpha=0.5) +
        ylab("Total Weight") +
        theme(legend.title=element_blank()) +
        ggtitle("Obama Tweet Sentiment (March & April)")

p_obama

## now let's look at positive vs. negative

tidy_obama_tweets_polarity <- 
        obama_tweets_nrc %>%
        select(9:10) %>%
        gather() %>%
        group_by(Sentiment=key) %>%
        summarise(Total=sum(value))

tidy_obama_tweets_polarity2 <- 
        obama_tweets_nrc2 %>%
        select(9:10) %>%
        gather() %>%
        group_by(Sentiment=key) %>%
        summarise(Total=sum(value))

summary(tidy_obama_tweets_polarity)
summary(tidy_obama_tweets_polarity2)

# p_obama2 <- ggplot(tidy_obama_tweets_polarity, aes(Sentiment,Total)) +
#         geom_bar(stat="identity", position="dodge", fill="blue") +
#         geom_bar(data=tidy_obama_tweets_polarity2, stat="identity", position="dodge", fill="red") +
#         ylab("Total Weight") +
#         ggtitle("Obama Tweet Sentiment (3/11/15)")
# 
# p_obama2

p_obama2 <- ggplot(NULL, aes(Sentiment,Total)) +
        geom_bar(aes(fill="march"), data=tidy_obama_tweets_polarity, stat="identity", alpha=0.5) +
        geom_bar(aes(fill="april"), data=tidy_obama_tweets_polarity2, stat="identity", alpha=0.5) +
        ylab("Total Weight") +
        theme(legend.title=element_blank()) +
        ggtitle("Obama Tweet Sentiment (March & April)")


p_obama2

# ## get sentiment with bing
# 
# obama_tweets_df$bing <- get_sentiment(obama_tweets_df$text, method="bing")
# 
# p_obama <-
#         ggplot(obama_tweets_df, aes(created,bing)) +
# #         geom_point(col="red", position="jitter") +
#         geom_line(col="red") +
#         geom_smooth(method=loess)
# 
# p_obama
# 
# ## get fourier transfor on bing sentiment
# 
# obama_sent_bing_ft <- get_transformed_values(obama_sent_bing)
# 
# p_obama_ft <-
#         qplot(seq_along(obama_sent_bing_ft), obama_sent_bing_ft) +
#         geom_line(col="red") +
#         geom_point(col="red") +
#         ylab("Valence") +
#         xlab("Time (Percentage)") +
#         ggtitle("#obama Tweets — Fourier Transform (3/11/15)")
# 
# p_obama_ft

## PART III — Sentiment Analysis (Survey Data)

survey_results <- read.csv("~/Sites/Shiny/survey_responses.csv")

survey_results$Response.Text <- as.character(survey_results$Response.Text)

## a little clean-up

survey_results$Response.Date <- as.character(survey_results$Response.Date)
survey_results$Response.Date <- as.Date(survey_results$Response.Date, format="%B %d %Y")

## then something similar to what we did with obama tweets

survey_nrc <- get_nrc_sentiment(survey_results$Response.Text)

summary(survey_nrc)

library(tidyr)
library(dplyr)

tidy_survey <- 
        survey_nrc %>%
        select(9:10) %>%
        gather() %>%
        group_by(Sentiment=key) %>%
        summarise(Total=sum(value))

p_survey <- 
        ggplot(tidy_survey, aes(Sentiment,Total)) +
        geom_bar(stat="identity") +
        ylab("Total Weight") +
        ggtitle("Survey Sentiment")

p_survey

## end syuzhet demo


