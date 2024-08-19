# Loading libraries
library(ndjson)
library(jsonlite)
library(rvest)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(caret)
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
library(MLmetrics)
library(doParallel)

# Set the working directory to the directory containing the data
setwd("D:/Dissertation final/Analysis/Hate speech")

# Read the CSV file into a DataFrame
data <- read.csv("ML_Test.csv", stringsAsFactors = FALSE)

# Creating a corpus object
corpus <- corpus(data, docid_field = "Slno", text_field = "body")
summary(corpus, 5)

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
docfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

# Textual statistics
topfeatures(docfm)

docfm <- dfm_tfidf(docfm) # weight DFM

tmpdata <- convert(docfm, to = "data.frame", docvars = NULL)
tmpdata <- tmpdata[, -1] # drop document id variable (first variable)
Slno <- docfm@docvars$docname_  # Get the Slno column (identifier)
human_labels <- docfm@docvars$Label # get labels
tmpdata <- as.data.frame(cbind(human_labels, Slno, tmpdata)) # labelled data frame

common_levels <- c("No", "Yes")
tmpdata$human_labels <- factor(tmpdata$human_labels, levels = common_levels)


# Separate labeled and unlabeled data
labeled_data <- tmpdata[!is.na(tmpdata$human_labels), ]
unlabeled_data <- tmpdata[is.na(tmpdata$human_labels), ]

# Check if separation was successful
nrow(labeled_data)  # Should be the number of labeled documents
nrow(unlabeled_data)  # Should be the number of unlabeled documents

# Train/test split on labeled data
set.seed(1234) # set seed for replicability
labeled_data <- labeled_data[sample(nrow(labeled_data)), ] # randomly order labelled dataset
split <- round(nrow(labeled_data) * 0.05) # determine cutoff point of 5% of documents
vdata <- labeled_data[1:split, ] # validation set
ldata <- labeled_data[(split + 1):nrow(labeled_data), ] # labelled dataset minus validation set

# Create train/test split
train_row_nums <- createDataPartition(ldata$human_labels, p=0.8, list=FALSE) # set human_labels as Y variable
Train <- ldata[train_row_nums, !names(ldata) %in% c("Slno")]  # Exclude 'Slno' from training
Test <- ldata[-train_row_nums, !names(ldata) %in% c("Slno")]  # Exclude 'Slno' from testing


train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best", # select the model with the best performance metric
  verboseIter = TRUE
)


# Train model (with parallel processing)
cl <- makePSOCKcluster(10) # create number of copies of R to run in parallel and communicate over sockets
registerDoParallel(cl) # register parallel backed with for each package

# Naive Bayes model
nb_train <- train(human_labels ~ ., 
                  data = Train,  
                  method = "naive_bayes", 
                  metric = "F1",
                  trControl = train_control,
                  tuneGrid = expand.grid(laplace = c(0,1),
                                         usekernel = c(TRUE, FALSE),
                                         adjust = c(0.75, 1, 1.25, 1.5)),
                  allowParallel= TRUE
)

stopCluster(cl) # stop parallel process once job is done
print(nb_train) # print cross-validation results

# Evaluate the model on the Test set
pred_nb <- predict(nb_train, newdata = Test) # generate prediction on Test set using training set model
head(pred_nb) # first few predictions

confusionMatrix(reference = Test$human_labels, data = pred_nb, mode='everything') # generate confusion matrix

# Final training on all labeled data
nb_final <- train(human_labels ~ ., 
                  data = ldata[, !names(ldata) %in% c("Slno")],  # Exclude 'Slno'
                  method = "naive_bayes", 
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(nb_train$bestTune))

nb_final

# Validate on vdata
pred2_nb <- predict(nb_final, newdata = vdata[, !names(vdata) %in% c("Slno")])
confusionMatrix(reference = vdata$human_labels, data = pred2_nb, mode='everything')


# Support Vector Machine model

train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best",
  verboseIter = TRUE
)

cl <- makePSOCKcluster(10) # using 10 clusters
registerDoParallel(cl)
svm_train <- train(human_labels ~ ., 
                   data = Train,  
                   method = "svmLinear", 
                   metric = "F1",
                   trControl = train_control,
                   tuneGrid = expand.grid(C = c(0.5, 1, 1.5)),
                   allowParallel= TRUE
)

stopCluster(cl)
print(svm_train)

# Evaluate the model on the Test set
pred_svm <- predict(svm_train, newdata = Test)
confusionMatrix(reference = Test$human_labels, data = pred_svm, mode='everything')

# Final training on all labeled data
svm_final <- train(human_labels ~ . , 
                   data = ldata[, !names(ldata) %in% c("Slno")],  # Exclude 'Slno'
                   method = "svmLinear", 
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(svm_train$bestTune))
# Validate on vdata
svm_pred2 <- predict(svm_final, newdata = vdata[, !names(vdata) %in% c("Slno")])
confusionMatrix(reference = vdata$human_labels, data = svm_pred2, mode='everything')

# Predict labels for the unlabeled data
unlabeled_data$predicted_labels <- predict(svm_final, newdata = unlabeled_data[, !names(unlabeled_data) %in% c("Slno", "human_labels")])

# Combine predictions with 'Slno' for analysis
final_predictions <- unlabeled_data[, c("Slno", "predicted_labels")]

# Save the predictions
write.csv(final_predictions, "predicted_unlabeled_data.csv", row.names = FALSE)

# Analyze by county
final_predictions <- merge(final_predictions, original_data_with_counties, by = "doc_id")
county_analysis <- aggregate(predicted_labels ~ county, data = final_predictions, FUN = function(x) mean(x == "Yes"))
# Recombine labeled and unlabeled data
final_data <- rbind(labeled_data, unlabeled_data)

# Save the combined data
write.csv(final_data, "final_combined_data.csv", row.names = FALSE)
