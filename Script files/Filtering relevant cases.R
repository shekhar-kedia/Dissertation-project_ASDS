# Load the necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(stringr)

# Set the working directory to the directory containing your CSV file
setwd("C:/Users/Dell/Desktop/Dissertation files/Database/Final data")

# Read the CSV file into a DataFrame
data <- read.csv("Mayo_posts.csv", stringsAsFactors = FALSE)

# Define the keywords
keywords <- c("immigration", "immigrations", "immigrant", "immigrants",
              "migration", "migrations", "migrant", "migrants",
              "foreign", "foreigns", "foreigner", "foreigners",
              "non-irish", "non-EU", "non-europeans",
              "India", "Indian", "Indians", "Brazil", "Brazilian", "Brazilians",
              "Ukraine", "Ukrainian", "Ukrainians", "asylum", "refugee", "refugees")

# Create a function to filter rows based on the presence of keywords
contains_keywords <- function(text) {
  str_detect(tolower(text), str_c(keywords, collapse = "|"))
}

# Filter the DataFrame
filtered_data <- data %>%
  filter(contains_keywords(selftext) | contains_keywords(title))

# Check the first few rows of the filtered data
head(filtered_data)

# Save the filtered data to a new CSV file
write.csv(filtered_data, "Filtered_Mayo_posts.csv", row.names = FALSE)







# Load the necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(stringr)

# Set the working directory to the directory containing your CSV files
setwd("C:/Users/Dell/Desktop/Dissertation files/Database/Final data")

# Define the keywords
keywords <- c("immigration", "immigrations", "immigrant", "immigrants",
              "migration", "migrations", "migrant", "migrants",
              "foreign", "foreigns", "foreigner", "foreigners",
              "non-irish", "non-EU", "non-europeans",
              "India", "Indian", "Indians", "Brazil", "Brazilian", "Brazilians",
              "Ukraine", "Ukrainian", "Ukrainians", "asylum", "refugee", "refugees")

# Create a function to filter rows based on the presence of keywords
contains_keywords <- function(text) {
  str_detect(tolower(text), str_c(keywords, collapse = "|"))
}


# Create a function to process each file and return filtered data
process_file <- function(file) {
  # Read the CSV file into a DataFrame
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Filter the DataFrame
  filtered_data <- data %>%
    filter(contains_keywords(selftext) | contains_keywords(title))
  
  return(filtered_data)
}

# List of files to process
files <- c("Mayo_posts.csv")

#           Dublin_posts.csv", "Galway_posts.csv", 
 #          "Other_file1.csv", "Other_file2.csv")

# Initialize an empty list to store the filtered data from each file
all_filtered_data <- list()

# Process each file and append the filtered data to the list
for (file in files) {
  filtered_data <- process_file(file)
  all_filtered_data <- append(all_filtered_data, list(filtered_data))
}

# Combine all filtered data into a single DataFrame
final_filtered_data <- bind_rows(all_filtered_data)

# Save the final compiled data to a single CSV file
write.csv(final_filtered_data, "Filtered_All_Posts.csv", row.names = FALSE)


################################################

# Create a function to process each file and return filtered data
  process_file_c <- function(file) {
    # Read the CSV file into a DataFrame
    data_c <- read.csv(file, stringsAsFactors = FALSE)
    

#####IMPORTANT
###Check if the ids are unique to each county or unique to all counties combined.
    #If unique to each county, might have to do the process county-wise

    
    
    # Filter the DataFrame
    filtered_data_c <- data_c %>%
      filter(contains_keywords(selftext) | contains_keywords(title))
    
    return(filtered_data)
  }

# List of files to process
files <- c("Mayo_posts.csv")

#           Dublin_posts.csv", "Galway_posts.csv", 
#          "Other_file1.csv", "Other_file2.csv")

# Initialize an empty list to store the filtered data from each file
all_filtered_data <- list()

# Process each file and append the filtered data to the list
for (file in files) {
  filtered_data <- process_file(file)
  all_filtered_data <- append(all_filtered_data, list(filtered_data))
}

# Combine all filtered data into a single DataFrame
final_filtered_data <- bind_rows(all_filtered_data)

# Save the final compiled data to a single CSV file
write.csv(final_filtered_data, "Filtered_All_Posts.csv", row.names = FALSE)

    
    
final_filtered_data["name"] = 
