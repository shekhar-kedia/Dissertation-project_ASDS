# Remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Reading immigration data
immi_ire_trend <- read_excel("Immigration trend_Ireland.xlsx") #Total new immigrants
net_mig_c <- read_excel("Net migration_county wise.xlsx") #Net migration county wise
ref_acco_c <- read_excel("Refugee acco_county wise.xlsx") #Refugees housed county wise
uk_num_c <- read_excel("Ukrainians_county wise.xlsx") #No. of Ukrainians county wise

# Reading hatespeech data
hs_data <- read.csv("HS_labelled data_posts.csv")

# Group the data by the year and count the number of rows for each group
total_count_per_year <- hs_data %>%
  group_by(Year) %>%
  summarize(total_count = n(), .groups = 'drop')

# Filter the data to include only rows with HS_label == "Yes"
label_count_per_year <- hs_data %>%
  filter(HS_label == "Yes") %>%
  group_by(Year) %>%
  summarize(label_count = n(), .groups = 'drop')

# Merge the two data frames to get the total number of cases per year
merged_counts_label <- merge(total_count_per_year, label_count_per_year, by = "Year", all.x = TRUE)

# Calculate the percentage of HS posts
merged_counts_label <- merged_counts_label %>%
  mutate(percentage_hs = (label_count / total_count) * 100)

# Create the plot
plot_hs <- ggplot(merged_counts_label, aes(x = Year, y = percentage_hs)) +
  geom_line() +
  labs(
    x = "Year",
    y = "% of Hate Speech Posts",
    title = "% of Hate Speech Posts by Year"
  ) +
  scale_x_continuous(breaks = merged_counts_label$Year) +  # Display all years on x-axis
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  # X-axis label
    axis.title.y = element_text(size = 16, face = "bold"),  # Y-axis label
    plot.title = element_text(size = 20, face = "bold"),    # Title
    axis.text.x = element_text(size = 15),                  # X-axis text
    axis.text.y = element_text(size = 15)                   # Y-axis text
  )

print(plot_hs)

ggsave(filename = "hs_posts_new.jpeg", plot = plot_hs, width = 10, height = 6, dpi = 300)


# Model 1: Immigration and mentions over time
immi_ire_trend <- immi_ire_trend %>% rename(year = Year)
immi_ire_trend_combined <- left_join(immi_ire_trend, 
                                     combined_counts_immi[, c("year", "percentage_immi", "percentage_immi_c")], 
                                     by = "year")

immi_ire_trend_combined <- left_join(immi_ire_trend_combined, 
                                     combined_sentiment[, c("year", "avg_sentiment_posts", "avg_sentiment_comments")], 
                                     by = "year")

immi_ire_trend_combined <- left_join(immi_ire_trend_combined, 
                                     merged_counts_label[, c("Year", "percentage_hs")], 
                                     by = c("year" = "Year"))

# Model 2: Net immi
# Immigration related trends
# Define the list of keywords
keywords_immi <- c("immigration", "immigrations", "immigrant", "immigrants",
                   "migration", "migrations", "migrant", "migrants",
                   "foreign", "foreigns", "foreigner", "foreigners",
                   "non-irish", "non-EU", "non-europeans",
                   "India", "Indian", "Indians", "Brazil", "Brazilian", "Brazilians",
                   "Ukraine", "Ukrainian", "Ukrainians", "asylum", "refugee", "refugees")

# Create a new column to define year ranges
data$year_range <- ifelse(data$year >= 2010 & data$year <= 2016, "2010-2016", 
                          ifelse(data$year >= 2016 & data$year <= 2024, "2016-2024", NA))

data_c$year_range <- ifelse(data_c$year >= 2010 & data_c$year <= 2016, "2010-2016", 
                            ifelse(data_c$year >= 2016 & data_c$year <= 2024, "2016-2024", NA))
# Filter the data to include only rows where the text field contains any of the keywords
keyword_immi <- data[grepl(paste(keywords_immi, collapse = "|"), data$body, ignore.case = TRUE), ]
keyword_immi_c <- data_c[grepl(paste(keywords_immi, collapse = "|"), data_c$body, ignore.case = TRUE), ]

# Group the filtered data by subreddit and year range, and count the number of rows for each group
keyword_count_immi <- keyword_immi %>%
  group_by(subreddit, year_range) %>%
  summarize(immi_count = n(), .groups = 'drop')

keyword_count_immi_c <- keyword_immi_c %>%
  group_by(subreddit, year_range) %>%
  summarize(immi_count_c = n(), .groups = 'drop')

# Now, group the total counts by subreddit and year range
total_count_per_year <- data %>%
  group_by(subreddit, year_range) %>%
  summarize(total_count = n(), .groups = 'drop')

total_count_per_year_c <- data_c %>%
  group_by(subreddit, year_range) %>%
  summarize(total_count_c = n(), .groups = 'drop')

# Merge the keyword counts with the total counts
merged_counts_immi <- merge(total_count_per_year, keyword_count_immi, by = c("subreddit", "year_range"))
merged_counts_immi_c <- merge(total_count_per_year_c, keyword_count_immi_c, by = c("subreddit", "year_range"))

# Calculate the percentage of immi-related comments and posts
merged_counts_immi <- merged_counts_immi %>%
  mutate(percentage_immi = (immi_count / total_count) * 100)

merged_counts_immi_c <- merged_counts_immi_c %>%
  mutate(percentage_immi_c = (immi_count_c / total_count_c) * 100)

# Combine the datasets
combined_counts_immi <- merge(merged_counts_immi, merged_counts_immi_c, 
                              by = c("subreddit", "year_range"), all = TRUE)

# Adjust the year ranges in combined_counts_immi
combined_counts_immi$year_range <- ifelse(combined_counts_immi$year_range == "2010-2016", "2011 - 2016",
                                          ifelse(combined_counts_immi$year_range == "2016-2024", "2016 - 2022", NA))

sentiment_by_county$year_range <- ifelse(sentiment_by_county$year_range == "2010-2016", "2011 - 2016",
                                          ifelse(sentiment_by_county$year_range == "2016-2024", "2016 - 2022", NA))

sentiment_by_county_p$year_range <- ifelse(sentiment_by_county_p$year_range == "2010-2016", "2011 - 2016",
                                         ifelse(sentiment_by_county_p$year_range == "2016-2024", "2016 - 2022", NA))

# Standardize the County names to match in both datasets (if necessary)
combined_counts_immi$subreddit <- tolower(gsub(" ", "", combined_counts_immi$subreddit))

sentiment_by_county$subreddit <- tolower(gsub(" ", "", sentiment_by_county$subreddit))
sentiment_by_county_p$subreddit <- tolower(gsub(" ", "", sentiment_by_county_p$subreddit))

net_mig_c$County <- tolower(gsub(" ", "", net_mig_c$County))

# Hate speech data
hs_data$year_range <- ifelse(hs_data$Year >= 2010 & hs_data$Year <= 2016, "2010-2016", 
                          ifelse(hs_data$Year >= 2016 & hs_data$Year <= 2024, "2016-2024", NA))

total_count_per_year <- hs_data %>%
  group_by(Subreddit, year_range) %>%
  summarize(total_count = n(), .groups = 'drop')

label_count_per_year <- hs_data %>%
  filter(HS_label == "Yes") %>%
  group_by(Subreddit, year_range) %>%
  summarize(label_count = n(), .groups = 'drop')

# Merge the two data frames to get the total number of cases per year
merged_counts_label <- merge(total_count_per_year, label_count_per_year, by = c("Subreddit", "year_range"), all.x = TRUE)
merged_counts_label <- merged_counts_label %>%
  mutate(percentage_hs = (label_count / total_count) * 100)

merged_counts_label$year_range <- ifelse(merged_counts_label$year_range == "2010-2016", "2011 - 2016",
                                          ifelse(merged_counts_label$year_range == "2016-2024", "2016 - 2022", NA))
merged_counts_label$Subreddit <- tolower(gsub(" ", "", merged_counts_label$Subreddit))


# Perform a left join to add only the desired columns
net_mig_combined <- left_join(net_mig_c,
                              combined_counts_immi[, c("subreddit", "year_range", "percentage_immi", "percentage_immi_c")],
                              by = c("County" = "subreddit", "Period" = "year_range"))

net_mig_combined <- left_join(net_mig_combined,
                              sentiment_by_county[, c("subreddit", "year_range", "avg_sentiment_c")],
                              by = c("County" = "subreddit", "Period" = "year_range"))
net_mig_combined <- left_join(net_mig_combined,
                              sentiment_by_county_p[, c("subreddit", "year_range", "avg_sentiment_p")],
                              by = c("County" = "subreddit", "Period" = "year_range"))

net_mig_combined <- left_join(net_mig_combined,
                              merged_counts_label[, c("Subreddit", "year_range", "percentage_hs")],
                              by = c("County" = "Subreddit", "Period" = "year_range"))

# Model 3

# Filter the data to include only rows where the text field contains any of the keywords
# and only for the years 2022-2024
filtered_data <- data %>%
  filter(year >= 2022 & year <= 2024)

filtered_data_c <- data_c %>%
  filter(year >= 2022 & year <= 2024)

filtered_data_hs <- hs_data %>%
  filter(Year >= 2022 & Year <= 2024)


# Filter for immigration-related posts/comments
keyword_immi <- filtered_data[grepl(paste(keywords_immi, collapse = "|"), filtered_data$body, ignore.case = TRUE), ]
keyword_immi_c <- filtered_data_c[grepl(paste(keywords_immi, collapse = "|"), filtered_data_c$body, ignore.case = TRUE), ]

# Group by subreddit (county) and count the number of rows for each group
keyword_count_immi <- keyword_immi %>%
  group_by(subreddit) %>%
  summarize(immi_count = n(), .groups = 'drop')

keyword_count_immi_c <- keyword_immi_c %>%
  group_by(subreddit) %>%
  summarize(immi_count_c = n(), .groups = 'drop')

# Calculate total count of posts/comments per county for the filtered period
total_count_per_county <- filtered_data %>%
  group_by(subreddit) %>%
  summarize(total_count = n(), .groups = 'drop')

total_count_per_county_c <- filtered_data_c %>%
  group_by(subreddit) %>%
  summarize(total_count_c = n(), .groups = 'drop')

# Merge the counts to get both total and immi-related comments/posts per county
merged_counts_immi <- merge(total_count_per_county, keyword_count_immi, by = "subreddit")
merged_counts_immi_c <- merge(total_count_per_county_c, keyword_count_immi_c, by = "subreddit")

# Calculate the percentage of immi-related comments/posts per county
merged_counts_immi <- merged_counts_immi %>%
  mutate(percentage_immi = (immi_count / total_count) * 100)

merged_counts_immi_c <- merged_counts_immi_c %>%
  mutate(percentage_immi_c = (immi_count_c / total_count_c) * 100)

# Combine the datasets for plotting or further analysis
combined_counts_immi <- merge(merged_counts_immi, merged_counts_immi_c, by = "subreddit", all = TRUE)


# Standardize the County names to match in both datasets (if necessary)
combined_counts_immi$subreddit <- tolower(gsub(" ", "", combined_counts_immi$subreddit))

sentiment_by_county_c$subreddit <- tolower(gsub(" ", "", sentiment_by_county_c$subreddit))
sentiment_by_county_p$subreddit <- tolower(gsub(" ", "", sentiment_by_county_p$subreddit))

ref_acco_c$County <- tolower(gsub(" ", "", ref_acco_c$County))

# Hate speech data
# Group the data by the year and count the number of rows for each group
total_count_per_year <- filtered_data_hs %>%
  group_by(Subreddit) %>%
  summarize(total_count = n(), .groups = 'drop')

# Filter the data to include only rows with HS_label == "Yes"
label_count_per_year <- filtered_data_hs %>%
  filter(HS_label == "Yes") %>%
  group_by(Subreddit) %>%
  summarize(label_count = n(), .groups = 'drop')

# Merge the two data frames to get the total number of cases per year
merged_counts_label <- merge(total_count_per_year, label_count_per_year, by = "Subreddit", all.x = TRUE)

# Calculate the percentage of HS posts
merged_counts_label <- merged_counts_label %>%
  mutate(percentage_hs = (label_count / total_count) * 100)
merged_counts_label$Subreddit <- tolower(gsub(" ", "", merged_counts_label$Subreddit))


# Perform a left join to add only the desired columns
ref_acco_c_combined <- left_join(ref_acco_c,
                              combined_counts_immi[, c("subreddit", "percentage_immi", "percentage_immi_c")],
                              by = c("County" = "subreddit"))

ref_acco_c_combined <- left_join(ref_acco_c_combined,
                                 sentiment_by_county_c[, c("subreddit", "avg_sentiment_c")],
                                 by = c("County" = "subreddit"))

ref_acco_c_combined <- left_join(ref_acco_c_combined,
                                 sentiment_by_county_p[, c("subreddit", "avg_sentiment_p")],
                                 by = c("County" = "subreddit"))

ref_acco_c_combined <- left_join(ref_acco_c_combined,
                                 merged_counts_label[, c("Subreddit", "percentage_hs")],
                                 by = c("County" = "Subreddit"))

# Model 4:
uk_num_c$County <- tolower(gsub(" ", "", uk_num_c$County))

# Perform a left join to add only the desired columns
uk_num_c_combined <- left_join(uk_num_c,
                                 combined_counts_immi[, c("subreddit", "percentage_immi", "percentage_immi_c")],
                                 by = c("County" = "subreddit"))

uk_num_c_combined <- left_join(uk_num_c_combined,
                               sentiment_by_county_c[, c("subreddit", "avg_sentiment_c")],
                               by = c("County" = "subreddit"))

uk_num_c_combined <- left_join(uk_num_c_combined,
                               sentiment_by_county_p[, c("subreddit", "avg_sentiment_p")],
                               by = c("County" = "subreddit"))

uk_num_c_combined <- left_join(uk_num_c_combined,
                                 merged_counts_label[, c("Subreddit", "percentage_hs")],
                                 by = c("County" = "Subreddit"))

# Save the final combined data to a CSV file
write.csv(immi_ire_trend_combined, "Immi_trend_combined.csv", row.names = FALSE)
write.csv(net_mig_combined, "Net_mig_combined.csv", row.names = FALSE)
write.csv(ref_acco_c_combined, "Ref_acco_combined.csv", row.names = FALSE)
write.csv(uk_num_c_combined, "UK_num_combined.csv", row.names = FALSE)

# Creating scatterplot & calculating correlation value to show relationship between the two variables
cor_pp_ti <- round(cor(immi_ire_trend_combined$tot_immi, immi_ire_trend_combined$percentage_immi, use = "complete.obs"),2)
cor_pc_ti <- round(cor(immi_ire_trend_combined$tot_immi, immi_ire_trend_combined$percentage_immi_c, use = "complete.obs"),2)

cor_pp_ni <- round(cor(net_mig_combined$Net_mig, net_mig_combined$percentage_immi, use = "complete.obs"),2)
cor_pc_ni <- round(cor(net_mig_combined$Net_mig, net_mig_combined$percentage_immi_c, use = "complete.obs"),2)

cor_pp_ra <- round(cor(ref_acco_c_combined$Number, ref_acco_c_combined$percentage_immi, use = "complete.obs"),2)
cor_pc_ra <- round(cor(ref_acco_c_combined$Number, ref_acco_c_combined$percentage_immi_c, use = "complete.obs"),2)

cor_pp_uk <- round(cor(uk_num_c_combined$Number, uk_num_c_combined$percentage_immi, use = "complete.obs"),2)
cor_pc_uk <- round(cor(uk_num_c_combined$Number, uk_num_c_combined$percentage_immi_c, use = "complete.obs"),2)

cor_sp_ti <- round(cor(immi_ire_trend_combined$tot_immi, immi_ire_trend_combined$avg_sentiment_posts, use = "complete.obs"),2)
cor_sc_ti <- round(cor(immi_ire_trend_combined$tot_immi, immi_ire_trend_combined$avg_sentiment_comments, use = "complete.obs"),2)

cor_sp_ni <- round(cor(net_mig_combined$Net_mig, net_mig_combined$avg_sentiment_p, use = "complete.obs"),2)
cor_sc_ni <- round(cor(net_mig_combined$Net_mig, net_mig_combined$avg_sentiment_c, use = "complete.obs"),2)

cor_sp_ra <- round(cor(ref_acco_c_combined$Number, ref_acco_c_combined$avg_sentiment_p, use = "complete.obs"),2)
cor_sc_ra <- round(cor(ref_acco_c_combined$Number, ref_acco_c_combined$avg_sentiment_c, use = "complete.obs"),2)

cor_sp_uk <- round(cor(uk_num_c_combined$Number, uk_num_c_combined$avg_sentiment_p, use = "complete.obs"),2)
cor_sc_uk <- round(cor(uk_num_c_combined$Number, uk_num_c_combined$avg_sentiment_c, use = "complete.obs"),2)

cor_hs_ti <- round(cor(immi_ire_trend_combined$tot_immi, immi_ire_trend_combined$percentage_hs, use = "complete.obs"),2)
cor_hs_ni <- round(cor(net_mig_combined$Net_mig, net_mig_combined$percentage_hs, use = "complete.obs"),2)
cor_hs_ra <- round(cor(ref_acco_c_combined$Number, ref_acco_c_combined$percentage_hs, use = "complete.obs"),2)
cor_hs_uk <- round(cor(uk_num_c_combined$Number, uk_num_c_combined$percentage_hs, use = "complete.obs"),2)

png("plot_pp_ti.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(immi_ire_trend_combined$tot_immi, 
     immi_ire_trend_combined$percentage_immi,
     xlab = "Total No. of New Immigrants",  # X-axis label
     ylab = "% of Mentions in Posts",    # Y-axis label
     main = paste("New Immigrants Coming vs % Mentions in Posts\nCorrelation = ", cor_pp_ti),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pc_ti.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(immi_ire_trend_combined$tot_immi, 
     immi_ire_trend_combined$percentage_immi_c,
     xlab = "Total No. of New Immigrants",  # X-axis label
     ylab = "% of Mentions in Comments",    # Y-axis label
     main = paste("New Immigrants Coming vs % Mentions in Comments\nCorrelation = ", cor_pc_ti),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pp_ni.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(net_mig_combined$Net_mig, 
     net_mig_combined$percentage_immi,
     xlab = "Net Migration",  # X-axis label
     ylab = "% of Mentions in Posts",    # Y-axis label
     main = paste("Net migration vs % Mentions in Posts\nCorrelation = ", cor_pp_ni),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pc_ni.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(net_mig_combined$Net_mig, 
     net_mig_combined$percentage_immi_c,
     xlab = "Net Migration",  # X-axis label
     ylab = "% of Mentions in Comments",    # Y-axis label
     main = paste("Net migration vs % Mentions in Comments\nCorrelation = ", cor_pc_ni),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pp_ra.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(ref_acco_c_combined$Number, 
     ref_acco_c_combined$percentage_immi,
     xlab = "Refugees provided with housing",  # X-axis label
     ylab = "% of Mentions in Posts",    # Y-axis label
     main = paste("Refugees provided with housing vs % Mentions in Posts\nCorrelation = ", cor_pp_ra),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pc_ra.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(ref_acco_c_combined$Number, 
     ref_acco_c_combined$percentage_immi_c,
     xlab = "Refugees provided with housing",  # X-axis label
     ylab = "% of Mentions in Comments",    # Y-axis label
     main = paste("Refugees provided with housing vs % Mentions in Comments\nCorrelation = ", cor_pc_ra),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pp_uk.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(uk_num_c_combined$Number, 
     uk_num_c_combined$percentage_immi,
     xlab = "No. of Ukrainians",  # X-axis label
     ylab = "% of Mentions in Posts",    # Y-axis label
     main = paste("No. of Ukrainians vs % Mentions in Posts\nCorrelation = ", cor_pp_uk),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_pc_uk.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(uk_num_c_combined$Number, 
     uk_num_c_combined$percentage_immi_c,
     xlab = "No. of Ukrainians",  # X-axis label
     ylab = "% of Mentions in Comments",    # Y-axis label
     main = paste("No. of Ukrainians vs % Mentions in Comments\nCorrelation = ", cor_pp_uk),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()


# Developing models
immi_ire_trend_combined$tot_immi_r = immi_ire_trend_combined$tot_immi/1000 #Recoding immigration no.
model_immi1 <- lm(percentage_immi ~ tot_immi_r, data = immi_ire_trend_combined)
model_immi2 <- lm(percentage_immi ~ tot_immi_r + year, data = immi_ire_trend_combined)
model_immi3 <- lm(percentage_immi_c ~ tot_immi_r, data = immi_ire_trend_combined)
model_immi4 <- lm(percentage_immi_c ~ tot_immi_r + year, data = immi_ire_trend_combined)

summary(model_immi1)
summary(model_immi2)
summary(model_immi3)
summary(model_immi4)

model_net_immi1 <- lm(percentage_immi ~ Net_mig, data = net_mig_combined)
model_net_immi2 <- lm(percentage_immi ~ Net_mig + Period, data = net_mig_combined)
model_net_immi3 <- lm(percentage_immi ~ Net_mig*Period, data = net_mig_combined)
model_net_immi4 <- lm(percentage_immi_c ~ Net_mig, data = net_mig_combined)
model_net_immi5 <- lm(percentage_immi_c ~ Net_mig + Period, data = net_mig_combined)
model_net_immi6 <- lm(percentage_immi_c ~ Net_mig*Period, data = net_mig_combined)

png("plot_sp_ti.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(immi_ire_trend_combined$tot_immi, 
     immi_ire_trend_combined$avg_sentiment_posts,
     xlab = "Total No. of New Immigrants",  # X-axis label
     ylab = "Net Sentiment in Posts",    # Y-axis label
     main = paste("New Immigrants Coming vs Net Sentiment in Posts\nCorrelation = ", cor_sp_ti),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sc_ti.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(immi_ire_trend_combined$tot_immi, 
     immi_ire_trend_combined$avg_sentiment_comments,
     xlab = "Total No. of New Immigrants",  # X-axis label
     ylab = "Net Sentiment in Comments",    # Y-axis label
     main = paste("New Immigrants Coming vs Net Sentiment in Comments\nCorrelation = ", cor_sc_ti),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sp_ni.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(net_mig_combined$Net_mig, 
     net_mig_combined$avg_sentiment_p,
     xlab = "Net Migration",  # X-axis label
     ylab = "Net Sentiment in Posts",    # Y-axis label
     main = paste("Net migration vs Net Sentiment in Posts\nCorrelation = ", cor_sp_ni),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sc_ni.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(net_mig_combined$Net_mig, 
     net_mig_combined$avg_sentiment_c,
     xlab = "Net Migration",  # X-axis label
     ylab = "Net Sentiment in Comments",    # Y-axis label
     main = paste("Net migration vs Net Sentiment in Comments\nCorrelation = ", cor_sc_ni),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sp_ra.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(ref_acco_c_combined$Number, 
     ref_acco_c_combined$avg_sentiment_p,
     xlab = "Refugees provided with housing",  # X-axis label
     ylab = "Net Sentiment in Posts",    # Y-axis label
     main = paste("Refugees provided with housing vs Net Sentiment in Posts\nCorrelation = ", cor_sp_ra),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sc_ra.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(ref_acco_c_combined$Number, 
     ref_acco_c_combined$avg_sentiment_c,
     xlab = "Refugees provided with housing",  # X-axis label
     ylab = "Net Sentiment in Comments",    # Y-axis label
     main = paste("Refugees provided with housing vs Net Sentiment in Comments\nCorrelation = ", cor_sc_ra),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sp_uk.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(uk_num_c_combined$Number, 
     uk_num_c_combined$avg_sentiment_p,
     xlab = "No. of Ukrainians",  # X-axis label
     ylab = "Net Sentiment in Posts",    # Y-axis label
     main = paste("No. of Ukrainians vs Net Sentiment in Posts\nCorrelation = ", cor_sp_uk),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_sc_uk.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(uk_num_c_combined$Number, 
     uk_num_c_combined$avg_sentiment_c,
     xlab = "No. of Ukrainians",  # X-axis label
     ylab = "Net Sentiment in Comments",    # Y-axis label
     main = paste("No. of Ukrainians vs Net Sentiment in Comments\nCorrelation = ", cor_sc_uk),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

model_senti_immi1 <- lm(avg_sentiment_posts ~ tot_immi_r, data = immi_ire_trend_combined)
model_senti_immi2 <- lm(avg_sentiment_posts ~ tot_immi_r + year, data = immi_ire_trend_combined)
model_senti_immi3 <- lm(avg_sentiment_comments ~ tot_immi_r, data = immi_ire_trend_combined)
model_senti_immi4 <- lm(avg_sentiment_comments ~ tot_immi_r + year, data = immi_ire_trend_combined)

summary(model_senti_immi1)
summary(model_senti_immi2)
summary(model_senti_immi3)
summary(model_senti_immi4)

png("plot_hs_ti.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(immi_ire_trend_combined$tot_immi, 
     immi_ire_trend_combined$percentage_hs,
     xlab = "Total No. of New Immigrants",  # X-axis label
     ylab = "% of Posts with Hate speech",    # Y-axis label
     main = paste("New Immigrants Coming vs % of Posts with Hate speech\nCorrelation = ", cor_hs_ti),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_hs_ni.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(net_mig_combined$Net_mig, 
     net_mig_combined$percentage_hs,
     xlab = "Net Migration",  # X-axis label
     ylab = "% of Posts with Hate speech",    # Y-axis label
     main = paste("Net migration vs % of Posts with Hate speech\nCorrelation = ", cor_hs_ni),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_hs_ra.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(ref_acco_c_combined$Number, 
     ref_acco_c_combined$percentage_hs,
     xlab = "Refugees provided with housing",  # X-axis label
     ylab = "% of Posts with Hate speech",    # Y-axis label
     main = paste("Refugees provided with housing vs % of Posts with Hate speech\nCorrelation = ", cor_hs_ra),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

png("plot_hs_uk.png", width = 1000, height = 600)
par(mar = c(5, 6, 4, 2) + 0.1)
plot(uk_num_c_combined$Number, 
     uk_num_c_combined$percentage_hs,
     xlab = "No. of Ukrainians",  # X-axis label
     ylab = "% of Posts with Hate speech",    # Y-axis label
     main = paste("No. of Ukrainians vs % of Posts with Hate speech\nCorrelation = ", cor_hs_uk),  # Title
     cex.main = 2,                        # Title font size
     font.main = 2,                         # Bold title
     cex.lab = 2,                         # Axis label font size
     font.lab = 2,                          # Bold axis labels
     cex.axis = 2,                         # Axis text size
     pch = 19,                             # Point shape (solid circle)
     cex = 1.5,                            # Point size
     col = "blue"                          # Point color
)
dev.off()

model_hs_immi1 <- lm(percentage_hs ~ tot_immi_r, data = immi_ire_trend_combined)
model_hs_immi2 <- lm(percentage_hs ~ tot_immi_r + year, data = immi_ire_trend_combined)

summary(model_senti_immi1)
summary(model_senti_immi2)

model_hs_net1 <- lm(percentage_hs ~ Net_mig, data = net_mig_combined)
model_hs_net2 <- lm(percentage_hs ~ Net_mig + Period, data = net_mig_combined)
model_hs_net3 <- lm(percentage_hs ~ Net_mig * Period, data = net_mig_combined)

summary(model_hs_net1)
summary(model_hs_net2)
summary(model_hs_net3)

model_hs_ref1 <- lm(percentage_hs ~ Number, data = ref_acco_c_combined)
summary(model_hs_ref1)

model_hs_uk1 <- lm(percentage_hs ~ Number, data = uk_num_c_combined)
summary(model_hs_uk1)
