# Loading libraries
library(ndjson)
library(jsonlite)
library(httr)
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

# Set the working directory to the directory containing CSV files
setwd("D:/Dissertation final/Other files")

# Read CSV file
data <- read.csv("All county_Posts.csv", stringsAsFactors = FALSE)

data_c <- read.csv("All county_Comments.csv", stringsAsFactors = FALSE)

# Data sanctity check
# Group the entire data by year to get the total number of comments and posts per year
total_count_per_year <- data %>%
  group_by(year) %>%
  summarize(total_count = n())

total_count_per_year_c <- data_c %>%
  group_by(year) %>%
  summarize(total_count_c = n())

# Define the list of keywords
keywords_covid <- c("virus", "covid19", "vaccine", "corona", "covid")

# Filter the data to include only rows where the text field contains any of the keywords
keyword_covid <- data[grepl(paste(keywords_covid, collapse = "|"), data$body, ignore.case = TRUE), ]
keyword_covid_c <- data_c[grepl(paste(keywords_covid, collapse = "|"), data_c$body, ignore.case = TRUE), ]

# Group the filtered data by the year and count the number of rows for each group
keyword_count_covid <- keyword_covid %>%
  group_by(year) %>%
  summarize(covid_count = n(), .groups = 'drop')

keyword_count_covid_c <- keyword_covid_c %>%
  group_by(year) %>%
  summarize(covid_count_c = n(), .groups = 'drop')

# Merge the two data frames to get both total and COVID-related comments and posts per year
merged_counts_covid <- merge(total_count_per_year, keyword_count_covid, by = "year")
merged_counts_covid_c <- merge(total_count_per_year_c, keyword_count_covid_c, by = "year")

# Calculate the percentage of COVID-related comments and posts
merged_counts_covid <- merged_counts_covid %>%
  mutate(percentage_covid = (covid_count / total_count) * 100)

merged_counts_covid_c <- merged_counts_covid_c %>%
  mutate(percentage_covid_c = (covid_count_c / total_count_c) * 100)

# Combine the datasets for plotting
combined_counts_covid <- merge(merged_counts_covid, merged_counts_covid_c, by = "year", all = TRUE)

# Plot the percentage of COVID-related comments and posts over time
plot4 <- ggplot(combined_counts_covid, aes(x = year)) +
  geom_line(aes(y = percentage_covid, color = "Posts"), size = 1) +
  geom_point(aes(y = percentage_covid, color = "Posts"), size = 2) +
  geom_line(aes(y = percentage_covid_c, color = "Comments"), size = 1) +
  geom_point(aes(y = percentage_covid_c, color = "Commens"), size = 2) +
  scale_x_continuous(breaks = seq(min(combined_counts_covid$year, na.rm = TRUE), 
                                  max(combined_counts_covid$year, na.rm = TRUE), 
                                  by = 1)) +
  labs(x = "Year", y = "% of COVID-related Comments/Posts", 
       title = "COVID-related Comments/Posts Over Time") +
  scale_color_manual(values = c("Posts" = "blue", "Comments" = "red")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis label
    plot.title = element_text(size = 20, face = "bold"), # Title
    axis.text.x = element_text(size = 15), # X-axis text
    axis.text.y = element_text(size = 15), # Y-axis text
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16, face = "bold") 
  )

# Save the plot
ggsave(filename = "Covid_cases_percentage_comparison_cp.jpeg", plot = plot4, width = 10, height = 6, dpi = 300)


# Immigration related trends
# Define the list of keywords
keywords_immi <- c("immigration", "immigrations", "immigrant", "immigrants",
              "migration", "migrations", "migrant", "migrants",
              "foreign", "foreigns", "foreigner", "foreigners",
              "non-irish", "non-EU", "non-europeans",
              "India", "Indian", "Indians", "Brazil", "Brazilian", "Brazilians",
              "Ukraine", "Ukrainian", "Ukrainians", "asylum", "refugee", "refugees")

# Filter the data to include only rows where the text field contains any of the keywords
keyword_immi <- data[grepl(paste(keywords_immi, collapse = "|"), data$body, ignore.case = TRUE), ]
keyword_immi_c <- data_c[grepl(paste(keywords_immi, collapse = "|"), data_c$body, ignore.case = TRUE), ]

# Group the filtered data by the year and count the number of rows for each group
keyword_count_immi <- keyword_immi %>%
  group_by(year) %>%
  summarize(immi_count = n(), .groups = 'drop')

keyword_count_immi_c <- keyword_immi_c %>%
  group_by(year) %>%
  summarize(immi_count_c = n(), .groups = 'drop')

# Merge the two data frames to get both total and immi-related comments and posts per year
merged_counts_immi <- merge(total_count_per_year, keyword_count_immi, by = "year")
merged_counts_immi_c <- merge(total_count_per_year_c, keyword_count_immi_c, by = "year")

# Calculate the percentage of immi-related comments and posts
merged_counts_immi <- merged_counts_immi %>%
  mutate(percentage_immi = (immi_count / total_count) * 100)

merged_counts_immi_c <- merged_counts_immi_c %>%
  mutate(percentage_immi_c = (immi_count_c / total_count_c) * 100)

# Combine the datasets for plotting
combined_counts_immi <- merge(merged_counts_immi, merged_counts_immi_c, by = "year", all = TRUE)

# Plot the percentage of immi-related comments and posts over time
plot5 <- ggplot(combined_counts_immi, aes(x = year)) +
  geom_line(aes(y = percentage_immi, color = "Posts"), size = 1) +
  geom_point(aes(y = percentage_immi, color = "Posts"), size = 2) +
  geom_line(aes(y = percentage_immi_c, color = "Comments"), size = 1) +
  geom_point(aes(y = percentage_immi_c, color = "Comments"), size = 2) +
  scale_x_continuous(breaks = seq(min(combined_counts_immi$year, na.rm = TRUE), 
                                  max(combined_counts_immi$year, na.rm = TRUE), 
                                  by = 1)) +
  labs(x = "Year", y = "% of immigration-related Comments/Posts", 
       title = "Immigration-related Comments/Posts Over Time") +
  scale_color_manual(values = c("Posts" = "blue", "Comments" = "red")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis label
    plot.title = element_text(size = 20, face = "bold"), # Title
    axis.text.x = element_text(size = 15), # X-axis text
    axis.text.y = element_text(size = 15), # Y-axis text
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16, face = "bold")
  )

# Save the plot
ggsave(filename = "Immigration_cases_percentage_comparison_cp.jpeg", plot = plot5, width = 10, height = 6, dpi = 300)

# Sentiment analysis plot
# Renaming the columns to avoid confusion
sentiment_by_year <- sentiment_by_year %>% rename(avg_sentiment_comments = avg_sentiment)
sentiment_by_year_p <- sentiment_by_year_p %>% rename(avg_sentiment_posts = avg_sentiment)

# Combine the datasets for plotting
combined_sentiment <- merge(sentiment_by_year_p, sentiment_by_year, by = "year", all = TRUE)

# Plot aggregated sentiment scores by year
plot6 <- ggplot(combined_sentiment, aes(x = year)) +
  geom_line(aes(y = avg_sentiment_posts, color = "Posts"), size = 1) +
  geom_line(aes(y = avg_sentiment_comments, color = "Comments"), size = 1) +
  scale_x_continuous(breaks = seq(min(combined_sentiment$year, na.rm = TRUE), 
                                  max(combined_sentiment$year, na.rm = TRUE), 
                                  by = 1)) +
  labs(x = "Year", y = "Avg. Sentiment Score", title = "Avg. Sentiment Score for Posts and Comments by Year") +
  scale_color_manual(values = c("Posts" = "blue", "Comments" = "red")) +
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

print(plot6)

# Save the plot
ggsave(filename = "average_sentiment_score_by_year.jpeg", plot = plot6, width = 10, height = 6, dpi = 300)