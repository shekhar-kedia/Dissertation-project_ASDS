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

# Set the working directory to the directory containing the CSV files
setwd("D:/Dissertation final")

###### Posts ######

# List of post files to process
files_p <- c("Carlow_posts.csv", "Cavan_posts.csv", "Clare_posts.csv", "Cork_posts.csv", 
             "Donegal_posts.csv", "Dublin_posts.csv", "Galway_posts.csv", "Kerry_posts.csv", 
             "Kildare_posts.csv", "Kilkenny_posts.csv", "Laois_posts.csv", "Leitrim_posts.csv", 
             "Limerick_posts.csv", "Longford_posts.csv", "countylouth_posts.csv", "Mayo_posts.csv", 
             "Meath_posts.csv", "Monaghan_posts.csv", "Offaly_posts.csv", "Roscommon_posts.csv", 
             "Sligo_posts.csv", "Tipperary_posts.csv", "Waterford_posts.csv", "Westmeath_posts.csv", 
             "CountyWexford_posts.csv", "Wicklow_posts.csv")

# Function to read and combine all CSV files
read_and_combine <- function(files) {
  combined_data <- rbindlist(lapply(files, fread))
  return(combined_data)
}

# Read and combine all CSV files
data <- read_and_combine(files_p)

# Create the new 'body' column
data <- data %>%  
  mutate(body = ifelse(trimws(selftext) %in% c("[removed]", "[deleted]") | is.na(selftext) | selftext == "", 
                       title, selftext))

# Cleaning date field and extracting year info
data$created_utc_n <- as.POSIXct(data$created_utc, origin = "1970-01-01", tz = "UTC")
data$year <- as.numeric(format(data$created_utc_n, "%Y"))

# Remove rows with NA values
data <- na.omit(data)

# Tidy the body_text column before transforming into a corpus
data$body <- str_replace(data$body, "\u2022.+$", "")
data$id <- seq_along(data$year)

# Save the final compiled data to a CSV file
write.csv(data, "All county_Posts.csv", row.names = FALSE)

# Creating a corpus object
corpus <- corpus(data, docid_field = "id", text_field = "body")

# Creating a useful summary object of the corpus
corpSum <- summary(corpus, n = nrow(docvars(corpus)))
head(corpSum[,-8])

plot1 <- corpSum %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  scale_y_continuous(labels = label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "Year", y = "Count (in thousands)", title = "Distribution of Posts by Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis label
    plot.title = element_text(size = 20, face = "bold"), # Title
    axis.text.x = element_text(size = 15), # X-axis text
    axis.text.y = element_text(size = 15), # Y-axis text
  )

ggsave(filename = "Graphs and plots/tot_posts_count_county.jpeg", plot = plot1, width = 10, height = 6, dpi = 300)

# Creating the tokens list and dfm
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

toks <- tokens_remove(quanteda::tokens(toks), "") #Removing whitespaces

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
docfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

# Textual statistics
topfeatures(docfm)

# Saving DFM for later use
saveRDS(docfm, "all_posts_dfm")

dfm_freq <- textstat_frequency(docfm, n = 20) 

plot2 <- dfm_freq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +  coord_flip() +
  labs(x = "Feature", y = "Frequency count", title = "Distribution of Top 20 features") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis label
    plot.title = element_text(size = 20, face = "bold"), # Title
    axis.text.x = element_text(size = 15), # X-axis text
    axis.text.y = element_text(size = 15) # Y-axis text
  )
print(plot2)
ggsave(filename = "Graphs and plots/top20_features_county_posts.jpeg", plot = plot2, width = 10, height = 6, dpi = 300)

# Wordcloud
docfm %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)

# Data sanctity check
# Group the entire data by year to get the total number of posts per year
total_count_per_year <- data %>%
  group_by(year) %>%
  summarize(total_count = n())

# Define the list of keywords
keywords_covid <- c("virus", "covid19", "vaccine", "corona", "covid")

# Filter the data to include only rows where the text field contains any of the keywords
keyword_covid <- data[grepl(paste(keywords_covid, collapse = "|"), data$body, ignore.case = TRUE), ]

# Group the filtered data by the year and count the number of rows for each group
keyword_count_covid <- keyword_covid %>%
  group_by(year) %>%
  summarize(covid_count = n())

# Merge the two data frames to get both total posts and covid-related posts per year
merged_counts_covid <- merge(total_count_per_year, keyword_count_covid, by = "year")

# Calculate the percentage of covid-related posts
merged_counts_covid <- merged_counts_covid %>%
  mutate(percentage_covid = (covid_count / total_count) * 100)

plot4 <- ggplot(merged_counts_covid, aes(x = year, y = percentage_covid)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  scale_x_continuous(breaks = seq(min(merged_counts_covid$year), max(merged_counts_covid$year), by = 1)) +
  labs(x = "Year", y = "Percentage of COVID-related Posts", title = "Percentage of COVID-related Posts Over Time") +
  theme_minimal()

ggsave(filename = "Graphs and plots/covid_posts_county.jpeg", plot = plot4, width = 10, height = 6, dpi = 300)


# Sentiment analysis
sent_dfm <- dfm_lookup(docfm, dictionary = data_dictionary_LSD2015[1:2])

docvars(docfm, "prop_negative") <- as.numeric(sent_dfm[,1] / ntoken(docfm)) 
docvars(docfm, "prop_positive") <- as.numeric(sent_dfm[,2] / ntoken(docfm)) 

docvars(docfm, "net_sentiment") <- docvars(docfm, "prop_positive") - docvars(docfm, "prop_negative") 

# Aggregate sentiment scores by year
sentiment_by_year_p <- docvars(docfm) %>%
  group_by(year) %>%
  summarize(avg_sentiment = mean(net_sentiment, na.rm = TRUE))

# Plot aggregated sentiment scores by year
ggplot(sentiment_by_year_p, aes(x = year, y = avg_sentiment)) +
  geom_line() +
  labs(x = "Year", y = "Average Sentiment Score", title = "Average Sentiment Score for Posts by Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"),  # Y-axis label
    plot.title = element_text(size = 20, face = "bold"),    # Title
    axis.text.x = element_text(size = 15),                  # X-axis text
    axis.text.y = element_text(size = 15),                  # Y-axis text
    legend.text = element_text(size = 15),                  # Legend text
    legend.title = element_text(size = 16, face = "bold")   # Legend title
  )
