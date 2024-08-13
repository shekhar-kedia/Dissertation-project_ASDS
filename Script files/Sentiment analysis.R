# Load the ndjson library
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

# Set the working directory to the directory containing your CSV files
setwd("D:/Dissertation final")

###### Comments ######

# Remove all objects except the data
#objects_to_remove <- setdiff(all_objects, c("clean_data", "data", "data_copy"))
#rm(list = objects_to_remove)
#rm(list = ls())

# List of comment files to process
files_c <- c("Carlow_comments.csv", "Cavan_comments.csv", "Clare_comments.csv", "Cork_comments.csv", 
             "Donegal_comments.csv", "Dublin_comments.csv", "Galway_comments.csv", "Kerry_comments.csv", 
             "Kildare_comments.csv", "Kilkenny_comments.csv", "Laois_comments.csv", "Leitrim_comments.csv", 
             "Limerick_comments.csv", "Longford_comments.csv", "countylouth_comments.csv", "Mayo_comments.csv", 
             "Meath_comments.csv", "Monaghan_comments.csv", "Offaly_comments.csv", "Roscommon_comments.csv", 
             "Sligo_comments.csv", "Tipperary_comments.csv", "Waterford_comments.csv", "Westmeath_comments.csv", 
             "CountyWexford_comments.csv", "Wicklow_comments.csv")

# Function to read and combine all CSV files
read_and_combine <- function(files) {
  combined_data <- rbindlist(lapply(files, fread))
  return(combined_data)
}

# Read and combine all CSV files
data <- read_and_combine(files_c)

# Assuming 'created_utc' is the column containing Unix timestamps
data$created_utc_n <- as.POSIXct(data$created_utc, origin = "1970-01-01", tz = "UTC")
data$year <- as.numeric(format(data$created_utc_n, "%Y"))

# Remove rows with NA values
data <- na.omit(data)

#sum(grepl("immigration|immigrant?", data$body, ignore.case = TRUE))
#sum(grepl("accommodation", data$body, ignore.case = TRUE))

# Tidy the body_text column before transforming into a corpus
data$body <- str_replace(data$body, "\u2022.+$", "")
data$id <- seq_along(data$year)

# Creating a corpus object
corpus <- corpus(data, docid_field = "id", text_field = "body")

# Creating a useful summary object of our corpus
corpSum <- summary(corpus, n = nrow(docvars(corpus)))
head(corpSum[,-8])

## Corpus statistics
plot1 <- corpSum %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  scale_y_continuous(labels = label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "Year", y = "Count (in thousands)", title = "Distribution of Comments by Year") +
  theme_minimal()

print(plot1)

ggsave(filename = "Graphs and plots/tot_comment_count_county.jpeg", plot = plot1, width = 10, height = 6, dpi = 300)

## Creating the tokens list and dfm
toks <- quanteda::tokens(corpus,
               include_docvars = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(stopwords('english'), padding = TRUE) %>% 
  tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE) %>%
  tokens_remove('amp', valuetype = 'fixed', padding = TRUE)

# Detect collocations and merge with tokens object (choose your own parameters)
col <- textstat_collocations(toks, 
                             method = "lambda", 
                             size = 2, 
                             min_count = 10,
                             smoothing = 0.5)
toks <- tokens_compound(toks, pattern = col[col$z > 3,]) 

toks <- tokens_remove(quanteda::tokens(toks), "") #this code removes whitespace

super_stops <- c("dublin", "cork", "now", "going", "want", "well", "lot", "even",
                 "ireland", "galway", "just", "go", "also", "can", "see", "need",
                 "get", "one", "like", "think", "deleted", "know", "1", "2", "3",
                 "say", "thanks", "city", "way", "place", "sure", "really", "two",
                 "much", "people", "take", "got", "though", "r", "look",
                 "yeah", "make", "still", "used", "probably", "removed",
                 "around", "gt", "find", "day", "try", "post", "back",
                 "might", "maybe", "looking", "bit", "use", "said", "etc",
                 "message", "something", "pay", "yes", "right", "no", "wrong",
                 "area", "us", "someone", "always", "actually", "never", "getting",
                 "thing", "check", "things", "places", "country", "many", "new")

toks <- tokens_remove(toks, super_stops,
                        valuetype = "glob")

# create dfm from tokens object
docfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

# Textual statistics
topfeatures(docfm)

# save our data for next time
saveRDS(docfm, "all_comments_dfm")

dfm_freq <- textstat_frequency(docfm, n = 30) 

plot2 <- dfm_freq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +  coord_flip() +
  labs(x = "Feature", y = "Frequency count", title = "Distribution of Top 30 features")

print(plot2)
ggsave(filename = "Graphs and plots/top30_features_county.jpeg", plot = plot2, width = 10, height = 6, dpi = 300)

# We can also visualise the dfm using the textplots package from quanteda
docfm %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)

# Sentiment analysis
sent_dfm <- dfm(docfm, dictionary = data_dictionary_LSD2015[1:2])

docvars(docfm, "prop_negative") <- as.numeric(sent_dfm[,1] / ntoken(docfm)) 
docvars(docfm, "prop_positive") <- as.numeric(sent_dfm[,2] / ntoken(docfm)) 

docvars(docfm, "net_sentiment") <- docvars(docfm, "prop_positive") - docvars(docfm, "prop_negative") 

#docvars(docfm, "year") <- lubridate::year(docfm@docvars$year)

# Aggregate sentiment scores by year
sentiment_by_year <- docvars(docfm) %>%
  group_by(year) %>%
  summarize(avg_sentiment = mean(net_sentiment, na.rm = TRUE))

# Plot aggregated sentiment scores by year
ggplot(sentiment_by_year, aes(x = year, y = avg_sentiment)) +
  geom_line() +
  labs(x = "Year", y = "Average Sentiment Score", title = "Average Sentiment Score by Year")


# Data sanctity check

# Define the list of keywords
keywords <- c("accommodation", "house", "rent", "home")

# Filter the data to include only rows where the text field contains any of the keywords
keyword_data <- clean_data[grepl(paste(keywords, collapse = "|"), clean_data$body, ignore.case = TRUE), ]

# Group the filtered data by the year and count the number of rows for each group
keyword_count <- keyword_data %>%
  group_by(year) %>%
  summarize(count = n())

# Plot the count of cases with any of the specified keywords over time
ggplot(keyword_count, aes(x = year, y = count)) +
  geom_point() +
  labs(x = "Year", y = "Count of Cases", title = "Count of Accommodation related comments over time")


# Define the list of keywords
keywords_n <- c("virus", "covid19", "vaccine", "corona", "covid")

# Filter the data to include only rows where the text field contains any of the keywords
keyword_data <- clean_data[grepl(paste(keywords_n, collapse = "|"), clean_data$body, ignore.case = TRUE), ]

# Group the filtered data by the year and count the number of rows for each group
keyword_count <- keyword_data %>%
  group_by(year) %>%
  summarize(count = n())

# Plot the count of cases with any of the specified keywords over time
ggplot(keyword_count, aes(x = year, y = count)) +
  geom_line() +
  labs(x = "Year", y = "Count of Cases", title = "Count of Immigration related comments over time")

