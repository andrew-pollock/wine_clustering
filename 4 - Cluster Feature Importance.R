
library(readr)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)

# Read and partition the data
wine_data <- read_csv("data/processed/final_wine_data.csv", 
                      col_types = cols(PC1 = col_skip(), PC2 = col_skip(), PC3 = col_skip(), 
                                       cluster = col_factor(levels = c("1", "2", "3", "4"))))

train_partition <- caret::createDataPartition(y = wine_data$cluster, p = 0.8, list=FALSE)
test_data  <- wine_data[-train_partition, ]
train_data <- wine_data[train_partition, ]

# Train the full model
full_model <- train(cluster ~ ., data = train_data, method = "rf", ntree = 50)

# Create a vector of variables to iterate through
vector_of_vars <- names(train_data)[-12]

# Empty data.frame to hold the results
results_df <- data.frame(matrix(nrow = 0, ncol=6))

# Iterate through, excluding 1 variable at a time
for (var in seq_along(vector_of_vars)) {
  # Set a seed for reproducibility
  set.seed(101)
  # Define model formula
  model_formula <- as.formula(paste0("cluster~.-", vector_of_vars[var]))

  # Train model
  partial_model <- train(model_formula, data = train_data, method = "rf", ntree = 50)
  
  # Test confusion matrix
  conf_matrix <- confusionMatrix(test_data$cluster, predict(partial_model, newdata = test_data))
  
  # Calculate the accuracy on test data
  model_accuracy <- c(vector_of_vars[var], conf_matrix$byClass[1:4, 11], conf_matrix$overall[1])
  
  # Append to results df
  results_df <<- rbind(results_df, model_accuracy)
}

# Rename results_df columns
colnames(results_df) <- c("excluded_var", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Overall Accuracy")

# Add the full models results to results_df
conf_matrix <- confusionMatrix(test_data$cluster, predict(full_model, newdata = test_data))
model_accuracy <- c("full_model", conf_matrix$byClass[1:4, 11], conf_matrix$overall[1])
results_df <- rbind(results_df, model_accuracy)


# Format the results for plotting
gathered_df <- results_df %>%
  pivot_longer(cols = -excluded_var, names_to = "metric") %>%
  arrange(metric, desc(value)) %>%
  mutate(across(c(excluded_var, metric), factor),
         value = as.numeric(value))

# Plot the data
ggplot(gathered_df, aes(x = reorder_within(excluded_var, value, metric), y = value, fill = excluded_var)) +
  geom_bar(stat = "identity") +
  facet_wrap(~metric, scales = "free") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  coord_flip(ylim = c(0.5, 1)) +
  scale_x_reordered() +
  labs(title = "Impact of Removing Variables on Classification Accuracy",
       x = "Excluded Variable",
       y = "Classification Accuracy")

ggsave("plots/Plot6_Classification_Accuracy.png")
