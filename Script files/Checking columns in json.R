options(repos = "https://cran.rstudio.com")

# Load the ndjson library
#install.packages("ndjson")

setwd("C:/Users/Dell/Desktop/Dissertation files/Database/Final data")

#library(ndjson)
library(jsonlite)

# Read the NDJSON file
data_c <- stream_in(file("r_mayo_comments.jsonl"))
data_p <- stream_in(file("r_mayo_posts.jsonl"))

# Subset columns
data_subset_c <- data_c[c("id", "body", "created_utc")]

data_subset_p <- data_p[c("id", "selftext", "title", "created_utc")]

# Write the data to a CSV file
write.csv(data_subset_c, file = "Ireland_comments.csv", row.names = FALSE)
write.csv(data_subset_p, file = "Ireland_posts.csv", row.names = FALSE)
