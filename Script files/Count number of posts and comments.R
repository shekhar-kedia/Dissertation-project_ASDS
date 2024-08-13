# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(lubridate)
library(stringr)

# Set the working directory to the directory containing your CSV files
setwd("C:/Users/Dell/Desktop/Dissertation files/Database/Final data")

# List of post files to process
files_p <- c("Carlow_posts.csv", "Cavan_posts.csv", "Clare_posts.csv", "Cork_posts.csv", 
             "Donegal_posts.csv", "Dublin_posts.csv", "Dublin_pre_posts.csv", "Dublin_post_posts.csv", "Galway_posts.csv", "Kerry_posts.csv", 
             "Kildare_posts.csv", "Kilkenny_posts.csv", "Laois_posts.csv", "Leitrim_posts.csv", 
             "Limerick_posts.csv", "Longford_posts.csv", "countylouth_posts.csv", "Mayo_posts.csv", 
             "Meath_posts.csv", "Monaghan_posts.csv", "Offaly_posts.csv", "Roscommon_posts.csv", 
             "Sligo_posts.csv", "Tipperary_posts.csv", "Waterford_posts.csv", "Westmeath_posts.csv", 
             "CountyWexford_posts.csv", "Wicklow_posts.csv")

# List of comment files to process
files_c <- c("Carlow_comments.csv", "Cavan_comments.csv", "Clare_comments.csv", "Cork_comments.csv", 
             "Donegal_comments.csv", "Dublin_comments.csv", "Dublin_pre_comments.csv", "Dublin_post_comments.csv", "Galway_comments.csv", "Kerry_comments.csv", 
             "Kildare_comments.csv", "Kilkenny_comments.csv", "Laois_comments.csv", "Leitrim_comments.csv", 
             "Limerick_comments.csv", "Longford_comments.csv", "countylouth_comments.csv", "Mayo_comments.csv", 
             "Meath_comments.csv", "Monaghan_comments.csv", "Offaly_comments.csv", "Roscommon_comments.csv", 
             "Sligo_comments.csv", "Tipperary_comments.csv", "Waterford_comments.csv", "Westmeath_comments.csv", 
             "CountyWexford_comments.csv", "Wicklow_comments.csv")

# Define the keywords
keywords <- c("immigration", "immigrations", "immigrant", "immigrants",
              "migration", "migrations", "migrant", "migrants",
              "foreign", "foreigns", "foreigner", "foreigners",
              "non-irish", "non-EU", "non-europeans",
              "India", "Indian", "Indians", "Brazil", "Brazilian", "Brazilians",
              "Ukraine", "Ukrainian", "Ukrainians", "asylum", "refugee", "refugees")

# Create a function to check for keywords
contains_keywords <- function(text) {
  pattern <- str_c("(?i)", str_c(keywords, collapse = "|")) # Use (?i) to make the regex pattern case-insensitive
  str_detect(tolower(text), pattern)
}

# Initialize an empty DataFrame to store the final results
final_df <- data.frame(subreddit = character(), year = integer(), post_count = integer(), keyword_post_count = integer(), 
                       comment_count = integer(), keyword_comment_count = integer(), stringsAsFactors = FALSE)

# Function to process post files and count posts per year
process_post_file <- function(file) {
  # Read the CSV file into a DataFrame
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Remove rows where both selftext and title are [deleted], [removed], or blank
  data <- data %>%
    filter(!(selftext %in% c("[deleted]", "[removed]", "") & title %in% c("[deleted]", "[removed]", "")))
  
  # Extract year from the created_utc column
  data$year <- year(as_datetime(data$created_utc))
  
  # Count the number of posts per year
  year_counts <- data %>%
    group_by(year) %>%
    summarise(post_count = n(),
              keyword_post_count = sum(contains_keywords(selftext) | contains_keywords(title)))
  
  return(year_counts)
}

# Function to process comment files and count comments per year
process_comment_file <- function(file) {
  # Read the CSV file into a DataFrame
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Remove rows where body is [deleted], [removed], or blank
  data <- data %>%
    filter(!(body %in% c("[deleted]", "[removed]", "")))
  
  # Extract year from the created_utc column
  data$year <- year(as_datetime(data$created_utc))
  
  # Count the number of comments per year
  year_counts <- data %>%
    group_by(year) %>%
    summarise(comment_count = n(),
              keyword_comment_count = sum(contains_keywords(body)))
  
  return(year_counts)
}

# Process each pair of files and store the results
for (i in seq_along(files_p)) {
  file_p <- files_p[i]
  file_c <- files_c[i]
  
  county_name <- gsub("_posts.csv", "", file_p)
  
  # Process posts and comments files
  post_counts <- process_post_file(file_p)
  comment_counts <- process_comment_file(file_c)
  
  # Merge the counts by year
  merged_counts <- full_join(post_counts, comment_counts, by = "year")
  
  # Add the subreddit name
  merged_counts <- merged_counts %>% mutate(subreddit = county_name)
  
  # Append the merged data to the final DataFrame
  final_df <- bind_rows(final_df, merged_counts)
}

# Reorder columns to have subreddit first
final_df <- final_df %>% select(subreddit, year, post_count, keyword_post_count, comment_count, keyword_comment_count)

# Display the results
print(final_df)

# Save the final compiled data to a CSV file
write.csv(final_df, "Filtered_Posts_Comments_Yearly_Counts.csv", row.names = FALSE)