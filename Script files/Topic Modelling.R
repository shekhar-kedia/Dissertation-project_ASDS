# Loading libraries
library(ndjson)
library(jsonlite)
library(rvest)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(scales)
library(dplyr)
library(readtext) 
library(stringi)
library(lubridate)
library(tidyverse)
library(textstem)
library(data.table)
library(stringr)
library(topicmodels) #LDA

# Set the working directory to the directory containing the data
setwd("D:/Dissertation final/Analysis/Hate speech")

data <- read.csv("Filtered_All_Posts_new.csv")

# Creating a corpus object
corpus <- corpus(data, docid_field = "id", text_field = "body")

## Creating the tokens list and dfm
toks <- quanteda::tokens(corpus,
               include_docvars = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(stopwords('english'), padding = TRUE) %>% 
  tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE) %>%
  tokens_remove('amp', valuetype = 'fixed', padding = TRUE)

# Detect collocations and merge with tokens object
col <- textstat_collocations(toks, 
                             method = "lambda", 
                             size = 2, 
                             min_count = 10,
                             smoothing = 0.5)
toks <- tokens_compound(toks, pattern = col[col$z > 3,]) 

toks <- tokens_remove(quanteda::tokens(toks), "")

super_stops <- c("anyone", "anyone_know", "dublin", "cork", "now", "going", "want", "well", "lot", "even",
                 "ireland", "galway", "just", "go", "also", "can", "see", "need",
                 "get", "one", "like", "think", "deleted", "know", "1", "2", "3",
                 "say", "thanks", "city", "way", "place", "sure", "really", "two",
                 "much", "people", "take", "got", "though", "r", "look", "hi",
                 "yeah", "make", "still", "used", "probably", "removed",
                 "around", "gt", "find", "day", "try", "post", "back",
                 "might", "maybe", "looking", "bit", "use", "said", "etc",
                 "message", "something", "pay", "yes", "right", "no", "wrong",
                 "area", "us", "someone", "always", "actually", "never", "getting",
                 "thing", "check", "things", "places", "country", "many", "new")

toks <- tokens_remove(toks, super_stops,
                        valuetype = "glob")

# create dfm from tokens object
dfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

# parse dates within DFM docvars
dfm@docvars$year <- dmy(dfm@docvars$newsDate)
dfm@docvars$date_month <- floor_date(dfm@docvars$date, "month")

# attach original text to dfm 
dfm@docvars$original_text <- as.character(corpus)

dfm <- dfm[!rowSums(dfm) == 0, ] #Removing empty rows

# Estimating LDA model
k <- 30
lda <- LDA(dfm, 
           k = k,
           method = "Gibbs",
           control = list(verbose = 100L,
                          seed = 1234,
                          burnin = 100,
                          iter = 1000))

# extract top 10 terms for each topic
terms <- get_terms(lda, 5)

# save terms to file for labelling. Transpose topic-term matrix for easier viewing
write.csv(t(terms), 
          file = "topic_terms.csv", 
          fileEncoding = "utf-8")

# print most probable tokens per topic
tterms <- t(terms)
matrix(apply(tterms, 1, paste, collapse = ", "))

# Interpreting topics
topics <- get_topics(lda, 1) # get top topic for each document
topics[1:5] # view top topics of first five documents

set.seed(12345)
sample(dfm@docvars$original_text[topics==26], 5)

# Aggregate topic probabilities by month and plot
immi_top <- dfm@docvars # get docvars from dfm object
immi_top$prob_topic <- lda@gamma[,23] # add topic probability to dataframe
prob_agg <- aggregate(immi_top$prob_topic, by=list(year=immi_top$year), FUN=mean)

#plot(prob_agg$month, prob_agg$x, type = "l", xlab = "Month")
topic <- ggplot(data = prob_agg,
       aes(x=year, y=x, group=1)) +
  geom_line() +
  geom_point() +
  labs(title = "Prevalence of Topic 23",
       x = "Year",
       y = "Avg. Yearly Topic Probability") +
theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis label
    plot.title = element_text(size = 20, face = "bold"),   # Title
    axis.text.x = element_text(size = 15),                 # X-axis text
    axis.text.y = element_text(size = 15),                  # Y-axis text
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16, face = "bold")
  )

ggsave(filename = "topic_immi_prevalence.jpeg", plot = topic, width = 10, height = 6, dpi = 300)
