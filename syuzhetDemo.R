## sentiment analysis with syuzhet
## vp nagraj
## last modified 2.20.15
## adapted from syuzhet vignette: https://github.com/mjockers/syuzhet/blob/master/inst/doc/syuzhet-vignette.R

## install and load package — available via devtools
## install.packages("devtools")

devtools::install_github("mjockers/syuzhet")
library(syuzhet)

## take a look at the functions
ls("package:syuzhet")

## read the vignette
vignette("syuzhet-vignette")

## load data
path_to_text <- "http://www.gutenberg.org/cache/epub/2701/pg2701.txt"           # note: only works with http

moby_d <- get_text_as_string(path_to_text)

## split text into character vector by sentence
moby_d <- get_sentences(moby_d)

## get sentiment vector — bing method
moby_d_sent_bing <- get_sentiment(moby_d, method="bing")

## get mean of sentiment vectors over the narrative timeline as divided up into 100 even parts
moby_d_sent_bing_vals <- get_percentage_values(moby_d_sent_bing)

## transform with fourier transformtion over percentage of the narrative
moby_d_sent_bing_ft <- get_transformed_values(moby_d_sent_bing)

## use ggplot2 intstead??????
plot(
        moby_d_sent_bing, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time (Line Number)", 
        ylab= "Emotional Valence",
        col="red"
)

plot(
        moby_d_sent_bing_vals, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time (Percentage)", 
        ylab= "Emotional Valence",
        col="red"
)

lines(percent_vals)

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

## get sentiment vector — nrc method
moby_d_sent_nrc <- get_sentiment(moby_d, method="nrc")


moby_d_sent_nrc2 <- get_nrc_sentiment(moby_d)

moby_d_angry <- which(moby_d_sent_nrc2$anger > 0)
moby_d[moby_d_angry]



## end syuzhet demo






## ------------------------------------------------------------------------
library(syuzhet)
my_example_text <- "I begin this story with a neutral statement.  
Basically this is a very silly test.  
You are testing the Syuzhet package using short, inane sentences.  
I am actually very happy today. 
I have finally finished writing this package.  
Tomorrow I will be very sad. 
I won't have anything left to do. 
I might get angry and decide to do something horrible.  
I might destroy the entire package and start from scratch.  
Then again, I might find it satisfying to have completed my first R package. 
Honestly this use of the Fourier transformation is really quite elegant.  
You might even say it's beautiful!"
s_v <- get_sentences(my_example_text)

## ------------------------------------------------------------------------
class(s_v)
str(s_v)
head(s_v)

## ----, eval = FALSE------------------------------------------------------
path_to_a_text_file <- "http://www.gutenberg.org/cache/epub/4217/pg4217.txt"
#  joyces_portrait <- get_text_as_string(path_to_a_text_file)
#  poa_v <- get_sentences(joyces_portrait)

## ----, echo = FALSE------------------------------------------------------
# Loading locally so I don't keep hitting www.gutenberg.org while testing (and get banned)
#path_to_a_text_file <- system.file("extdata", "portrait.txt",
package = "syuzhet")
joyces_portrait <- get_text_as_string(path_to_a_text_file)
poa_v <- get_sentences(joyces_portrait)

## ------------------------------------------------------------------------
sentiment_vector <- get_sentiment(poa_v, method="bing")

## ------------------------------------------------------------------------
sentiment_vector

## ------------------------------------------------------------------------
afinn_vector <- get_sentiment(poa_v, method="afinn")
afinn_vector

nrc_vector <- get_sentiment(poa_v, method="nrc")
nrc_vector

tagger_path <- "/Applications/stanford-corenlp-full-2014-01-04"
stanford_vector <- get_sentiment(s_v, method="stanford", tagger_path)
stanford_vector

## ------------------------------------------------------------------------
sum(sentiment_vector)

## ------------------------------------------------------------------------
mean(sentiment_vector)

## ------------------------------------------------------------------------
summary(sentiment_vector)

## ----, fig.width = 6-----------------------------------------------------
plot(
        sentiment_vector, 
        type="l", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence"
)
lines(afinn_vector, col = "blue", lwd=".5")
lines(sentiment_vector, pch="")
## ----, fig.width = 6-----------------------------------------------------
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
        poa_sent, 
        type="h", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence"
)

## ----, echo=FALSE, fig.width = 6-----------------------------------------
plot(
        sentiment_vector, 
        type = "l", 
        main = "Example Plot Trajectory", 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence"
)

lines(
        get_transformed_values(
                sentiment_vector, 
                low_pass_size = 3, 
                x_reverse_len = 12, 
                scale_range = TRUE
        ), 
        col = "red", 
        lwd = 2
)

## ----, fig.width = 6-----------------------------------------------------
percent_vals <- get_percentage_values(poa_sent)
plot(
        percent_vals, 
        type="l", 
        main="Joyce's Portrait Using Percentage-Based Means", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence", 
        col="red"
)

## ----, fig.width = 6-----------------------------------------------------
ft_values <- get_transformed_values(
        poa_sent, 
        low_pass_size = 3, 
        x_reverse_len = 100,
        scale_vals = TRUE,
        scale_range = FALSE
)
plot(
        ft_values, 
        type ="h", 
        main ="Joyce's Portrait using Transformed Values", 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence", 
        col = "red"
)

## ------------------------------------------------------------------------
nrc_data <- get_nrc_sentiment(poa_v)

## ------------------------------------------------------------------------
angry_items <- which(nrc_data$anger > 0)
poa_v[angry_items]

## ------------------------------------------------------------------------
joy_items <- which(nrc_data$joy > 0)
poa_v[joy_items]

## ----, results='asis'----------------------------------------------------
pander::pandoc.table(nrc_data[, 1:8])

## ----, results='asis'----------------------------------------------------
pander::pandoc.table(nrc_data[, 9:10])

## ------------------------------------------------------------------------
valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence

## ----, fig.width=6-------------------------------------------------------
barplot(
        sort(colSums(prop.table(nrc_data[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "Emotions in Sample text", xlab="Percentage"
)



