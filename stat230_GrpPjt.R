library(ggplot2)
library(tidyverse)
library(readxl)
library(ggthemes)
library(glue)
library(dslabs)
library(knitr)
library(caret)
library(e1071)

getwd()
setwd("C:/Users/User/Desktop/Yussif/resources") # path to where the excel file is saved on your computer, you will have to change it.
list.files()

h1n1_vaccine <- read_excel("H1N1_Flu_Vaccines.xlsx")


View(h1n1_vaccine) # have a table display of the data set
names(h1n1_vaccine) # get the all variable names in the h1n1 data set
glimpse(h1n1_vaccine) #  view a better structure of the data set
head(h1n1_vaccine, 10L) # display a few of the data set
attach(h1n1_vaccine)

h1n1_vaccine <- h1n1_vaccine %>% 
  select(h1n1_concern, age_group, sex, h1n1_vaccine, seasonal_vaccine, everything()) %>% 
  
  mutate(h1n1_concern = as.factor(h1n1_concern),
         
         age_group = as.factor(age_group),
         
         sex = as.factor(sex),
         
         h1n1_vaccine = as.factor(h1n1_vaccine),
         
         seasonal_vaccine = as.factor(seasonal_vaccine)) %>% 
  
  glimpse()

view(h1n1_vaccine)


describe_ordinal <- function(data_variable) {
  variable <- deparse(substitute(data_variable))
  
  frequency_table <- data_variable %>% 
    table()
    
  med_val <- median(frequency_table)
  quant_val <- quantile(frequency_table)
  most_common_category <- names(frequency_table)[which.max(frequency_table)]
  num_categories <- length(frequency_table)
  
  summary_statistics <- glue::glue(
    "Most Common Category: {most_common_category}\n",
    "Number of Categories: {num_categories}"
  )
  
    
    data_frm_variable <- frequency_table %>% 
    as.data.frame() %>% 
    rename(category = 1, Frequency = 2)
    
    bar_plot <- ggplot(data_frm_variable, aes(x = category, 
                                          y = Frequency,
                                          fill = category)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribution of", variable),
         x = paste("category of", variable),
         y = "Frequency Count") +
      theme_economist() 
    
    
    cum_freq <- data_variable %>% 
      levels() %>% 
      factor() %>% 
      table() %>% 
      cumsum()
    
    cum_freq_df <- data.frame(value = (levels(data_variable)),
                              cum_freq = cum_freq)
    
    
    CFD_plot <- ggplot(cum_freq_df, aes(value, cum_freq, fill = value), bw = 2) +
      geom_bar(stat = "identity") +
      labs(title = "Cumulative Frequency Distribution",
           x = "Ordinal Value",
           y = "Cumulative Frequency") +
      theme_economist()
      
    
    return(
      list(
        summary_statistics,
        bar_plot,
        #box_plot,
        CFD_plot,
        frequency_table
      )
    )
}
## note: for only ordinal data_variables
describe_ordinal(h1n1_vaccine$age_group)
# describe_ordinal(h1n1_vaccine$h1n1_concern) It's nominal
# describe_ordinal(h1n1_vaccine$sex) it's nominal

describe_nominal <- function(data_field) {
  dataset <- deparse(substitute(data_field))
  
  freq_table <- data_field %>% 
    table()
  
  maxCat <- names(freq_table)[which.max(freq_table)]
  numCat <- length(freq_table)
  
  summary_stat <- glue::glue(
    "maximum category         :   {maxCat}\n",
    "Number of categories     :   {numCat}"
  )
  
  dataset_df <- freq_table %>% 
    as.data.frame() %>% 
    rename(category = 1,
           Frequency = 2)
  
  bar_plot <- dataset_df %>% 
    ggplot(aes(x = category,
               y = Frequency,
               fill = category)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribution of", dataset),
         x = paste("Category of", dataset),
         y = paste("Frequency of", dataset)) +
    theme_economist()
  
  pie_chart <- dataset_df %>% 
    ggplot(aes(x = "", 
               y = Frequency,
               fill = category,
               label = category)) + 
    geom_col() +
    coord_polar(theta = "y") +
    labs(title = paste("Distribution of", dataset)) +
    theme_void()
  
  return(
    list(
      summary_stat,
      bar_plot,
      pie_chart,
      freq_table
    )
  )
}

## note: for only nominal data_variables
describe_nominal(h1n1_concern)
describe_nominal(sex)
# describe_nominal(age_group) ordinal

## Constructing a contingency table for h1n1_concern (row) and age_group (column).

contingency_table <- table(h1n1_vaccine$h1n1_concern, h1n1_vaccine$age_group)



## convert the contingency table to a data frame
contingency_dataframe <- as.data.frame.matrix(contingency_table)

## display the table
kable(contingency_dataframe, format = "markdown")
# kable(contingency_dataframe, format = "html")
# kable(contingency_dataframe, format = "latex")


## performing a chi-squared test of association

?chisq.test()

# performing a chi-squared test with alpha = 0.05 ~ 0.95
chisq_result <- chisq.test(contingency_dataframe, p = 0.05)
chisq_result

# chisq_result_2 <- chisq.test(h1n1_vaccine$h1n1_concern, h1n1_vaccine$age_group, p = 0.05)
# chisq_result



## Using Cramer's V contingency coefficient to measure the strength of the association 
# between h1n1_concern and age_group.

cramers_v <- function(contingency_tb, chi_squared_results) {
  
  # obtain the chi-squared statistics
  chisq_stat <-  chi_squared_results$statistic
  
  # obtaining the dimensions of contingency table
  n <- sum(contingency_tb)
  n_row <- nrow(contingency_tb)
  n_col <- ncol(contingency_tb)
  
  # calculating the cramer's V coefficient
  
  v <- sqrt(chisq_stat/(n * min(n_col - 1, n_row - 1)))
  
  return(
    list(
      chisq_stat,
      n_row,
      n_col,
      v
    )
  )
}

cramers_v(contingency_table, chisq_result)

# confirming with the inbuilt function
# install.packages("vcd")
library(vcd)


cramer_v_result <- assocstats(contingency_table)


##### Building the classification Algorithm #####

set.seed(1234)

# specifying the target variable
target_variable <- seasonal_vaccine

# specifying predictor variables
predictor_variables <- setdiff(names(h1n1_vaccine), target_variable)

# partitioning dataset into training and test sets
trainingSet_index <- createDataPartition(target_variable, p = 0.7, list = FALSE)

#trainingSet_index <- sample(nrow(h1n1_vaccine), size = 0.7 * nrow(h1n1_vaccine))

training_set <- h1n1_vaccine[trainingSet_index, ]
test_set <- h1n1_vaccine[-trainingSet_index, ]

#### Feature Selection and Extraction of relevant data variables ####
# a value stop point
threshold <- 0.1

cramers_values <- function(dataset) {
  # Define the target variable 
  tar_var <- "seasonal_vaccine"
  
  # specifying predictor variables
  pred_vars <- setdiff(names(h1n1_vaccine), tar_var)
  
  # Create an empty list to store Cramer's V values for each predictor variable
  cramers_v_list <- list()
  
  # Iterate through each predictor variable
  for (predictor_variable in pred_vars) {
    # Create a contingency table
    cont_table <- table(dataset[[predictor_variable]], dataset[[tar_var]])
    
    # Calculate Cramer's V
    cramers_v <- assocstats(cont_table)$cramer
    
    # Store the Cramer's V value in the list
    cramers_v_list[[predictor_variable]] <- cramers_v
  }
  
  return(cramers_v_list)
}


cramers_value <- cramers_values(h1n1_vaccine)

# Observation:  All the data variables are not closely associated 
#               with the target variable

# Hypothesis: some of the variables in the data set are relevant
#             to target variable

# Decision rule: 
        # 0.1 or less: Weak association
        # 0.1 to 0.3: Moderate association
        # 0.3 to 0.5: Strong association
        # 0.5 or more: Very strong association


# selecting the features
selected_features <- names(h1n1_vaccine[, predictor_variables])[
  cramers_value > threshold]

# extraction of selected features
new_h1n1_vaccine <- h1n1_vaccine %>% 
  select(target_variable, selected_features)



##### Data Mining Techniques ####
## Naive-Bayes Classifiers

# specifying the target variable
new_target_variable <- new_h1n1_vaccine$seasonal_vaccine

# specifying predictor variables
new_predictor_variables <- setdiff(names(h1n1_vaccine), new_target_variable)

# partitioning dataset into training and test sets

new_trainingSet_index <- createDataPartition(new_target_variable, p = 0.7, list = FALSE)

#trainingSet_index <- sample(nrow(h1n1_vaccine), size = 0.7 * nrow(h1n1_vaccine))

new_training_set <- new_h1n1_vaccine[new_trainingSet_index, ]
new_test_set <- new_h1n1_vaccine[-new_trainingSet_index, ]

target_variable <- "seasonal_vaccine"
# training the naive bayes model
model <- naiveBayes(as.factor(training_set[[target_variable]]) ~ ., 
                    training_set[, predictor_variables])

# making a prediction on the test data
prediction <- predict(model, test_set[, predictor_variables])

# convert the target variable to a factor with the same level as the predictors
test_set$seasonal_vaccine <- factor(test_set$seasonal_vaccine, 
                                   levels = levels(prediction))

length(test_set[[target_variable]])
length(prediction)

# create a confusion matrix

confusion_matrix <- confusionMatrix(prediction, test_set[[target_variable]])

# Calculate precision and recall for each class
precision <- confusion_matrix$byClass["Pos Pred Value"]
recall <- confusion_matrix$byClass["Sens"]

# visualizing the confusion matrix

# Convert confusion matrix table to a data frame
conf_matrix_df <- as.data.frame(confusion_matrix$table)

confusion_matrix_heatmap <- ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "skyblue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Reference",
       y = "Prediction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# confusion_matrix_heatmap

# Predict probabilities
prediction_probs <- predict(model, test_set[, predictor_variables], type = "raw")

# Create a data frame for plotting
prediction_probs_df <- data.frame(target_variable = rep(levels(test_set[[target_variable]]), each = nrow(test_set[, predictor_variables])),
                             Probability = c(prediction_probs))

# Create a density plot for class probabilities
prediction_prob_density <- ggplot(prediction_probs_df, aes(x = Probability, color = target_variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Class Probability Distribution",
       x = "Probability",
       y = "Density") +
  theme_economist()


# Print results
cat("Confusion Matrix:\n")
print(confusion_matrix$table)

# extracting the accuracy, specificity, and precision
cat("\nOverall Accuracy:", confusion_matrix$overall["Accuracy"], "\n")
cat("Specificity:", confusion_matrix$byClass["Specificity"], "\n")
cat("Precision:", confusion_matrix$byClass["Precision"], "\n")
cat("F1-Score:", 2 * (precision * recall) / (precision + recall), "\n")

# Print plots
print(confusion_matrix_heatmap)
print(prediction_prob_density)






##### Performing Data mining Techniques After Feature Selection and Extraction ####

# specifying the target variable
target_variable <- seasonal_vaccine

# specifying predictor variables
predictor_variables <- setdiff(names(new_h1n1_vaccine), target_variable)

# partitioning dataset into training and test sets
trainingSet_index <- createDataPartition(target_variable, p = 0.7, list = FALSE)
unique(new_training_set)

target_variable <- "seasonal_vaccine"
# training the naive bayes model
new_model <- naiveBayes(as.factor(new_training_set[[target_variable]]) ~ ., 
                    new_training_set[, predictor_variables])

# making a prediction on the test data
new_prediction <- predict(new_model, new_test_set[, predictor_variables])

# convert the target variable to a factor with the same level as the predictors
new_test_set$seasonal_vaccine <- factor(new_test_set$seasonal_vaccine, 
                                    levels = levels(prediction))

length(new_test_set[[target_variable]])
length(new_prediction)

# create a confusion matrix

new_confusion_matrix <- confusionMatrix(new_prediction, new_test_set[[target_variable]])

# Calculate precision and recall for each class
precision <- new_confusion_matrix$byClass["Pos Pred Value"]
recall <- new_confusion_matrix$byClass["Sens"]

# visualizing the confusion matrix

# Convert confusion matrix table to a data frame
new_conf_matrix_df <- as.data.frame(new_confusion_matrix$table)

new_confusion_matrix_heatmap <- ggplot(new_conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "dodgerblue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Reference",
       y = "Prediction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# new_confusion_matrix_heatmap

# Predict probabilities
new_prediction_probs <- predict(new_model, new_test_set[, predictor_variables], type = "raw")

# Create a data frame for plotting
new_prediction_probs_df <- data.frame(target_variable = rep(levels(new_test_set[[target_variable]]), each = nrow(test_set[, predictor_variables])),
                                  Probability = c(prediction_probs))

# Create a density plot for class probabilities
new_prediction_prob_density <- ggplot(new_prediction_probs_df, aes(x = Probability, color = target_variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Class Probability Distribution",
       x = "Probability",
       y = "Density") +
  theme_economist()


# Print results
cat("Confusion Matrix:\n")
print(new_confusion_matrix$table)

# extracting the accuracy, specificity, and precision
cat("\nOverall Accuracy:", new_confusion_matrix$overall["Accuracy"], "\n")
cat("Specificity:", new_confusion_matrix$byClass["Specificity"], "\n")
cat("Precision:", new_confusion_matrix$byClass["Precision"], "\n")
cat("F1-Score:", 2 * (precision * recall) / (precision + recall), "\n")

# Print plots
print(new_confusion_matrix_heatmap)
print(new_prediction_prob_density)

