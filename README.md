# Wine Marketing Segmentation with Dashboard

A basic clustering of wines based on their physical and chemical properties, created as part of my MSc. Includes an interactive shiny dashboard with plotly graphs, [hosted on shinyapps.io.](https://apollock.shinyapps.io/Wine_Marketing_Segmentation) 

The segmentation was created using K-means clustering with k=4 based on the below elbow plot - from 4 clusters onwards the reduction in within-cluster TSS caused by each additional cluster slows significantly.
<img src="plots/Plot4_Optimal_K.png"  height="500">

The first 3 Principal Components explain approximately 52% of the total variance (after centering and scaling), so they've been included in the dashboard as a convenient way to visually separate the 4 clusters.

``` r
summary(prcomp(wine_data[,1:11], scale = TRUE, center = TRUE))

#> Importance of components:
#>                           PC1    PC2    PC3    PC4     PC5     PC6     PC7     PC8     PC9    PC10    PC11
#> Standard deviation     1.6554 1.2646 1.1758 1.0762 0.97728 0.86689 0.81041 0.78682 0.72106 0.57740 0.53278
#> Proportion of Variance 0.2491 0.1454 0.1257 0.1053 0.08683 0.06832 0.05971 0.05628 0.04727 0.03031 0.02581
#> Cumulative Proportion  0.2491 0.3945 0.5202 0.6255 0.71232 0.78063 0.84034 0.89662 0.94389 0.97419 1.00000

```

### Cluster Feature Importance

Since k-means clustering simply groups together observations which are close in a d-dimensional space, all d variables are used to determine which cluster an observation should be assigned to. This can make it difficult to determine which variables are most important in decided how an observation should be classified.

One obvious method is to compare the distribution of each variable within each cluster. 

``` r
featurePlot(x = wine_data[, 1:11], 
            y = as.factor(wine_data$cluster), 
            plot = "box", 
            scales = list(y = list(relation = "free")),  
            labels = c("Wine Cluster", "Variable Distribution"), 
            main = "Distribution of Variables by Cluster",
            par.settings=list(box.rectangle = list(col = cluster_palette, fill=cluster_palette, alpha=0.7, lwd = 2),
                              box.umbrella  = list(col = cluster_palette, lwd = 2),
                              box.symbol    = list(col = cluster_palette, fill=cluster_palette, lwd = 2))
)
```
<img src="plots/Variable Distribution by Cluster.png"  height="500">

This shows some obvious patterns like wines in Cluster 2 having more alcohol and higher quality, Cluster 3 having a higher pH level, and Cluster 4 having more residual sugar and sulfur dioxide, but a more quantitative method would be useful.

An alternative method involves training a multi-class classification model to predict the cluster based on the input variables. The model is then retrained over and over, excluding one variable at a time. The classification accuracy of each of these models can then be compared and the variable whose removal causes the largest decrease in model accuracy can be assumed to be the most important. Since a variable might only be useful for predicting a single cluster (e.g using sugar to predict a wine from cluster 4) it's important to consider the accuracy of each cluster, just just the overall accuracy. The below example does this with a random forest classifier.

``` r
# Create a vector of variables to iterate through
vector_of_vars <- names(train_data)[-12]

# Empty data.frame to hold the results
results_df <- data.frame(matrix(nrow = 0, ncol=6))

# Iterate through, excluding 1 variable at a time
for (var in 1:length(vector_of_vars)){
  # Set a seed for reproducibility
  set.seed(101)
  # Define model formula
  model_formula <- as.formula(paste0("cluster~.-", vector_of_vars[var]))
  
  # Train model
  partial_model <- train(model_formula, data = train_data, method = 'rf', ntree = 50)
  
  # Test confusion matrix
  conf_matrix <- confusionMatrix(test_data$cluster, predict(partial_model, newdata = test_data))
  
  # Calculate the accuracy on test data
  model_accuracy <- c(vector_of_vars[var], conf_matrix$byClass[1:4,11], conf_matrix$overall[1])
  
  # Append to results df
  results_df <<- rbind(results_df, model_accuracy)
}

# Rename results_df columns
colnames(results_df) = c("excluded_var", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Overall Accuracy")
```

We can plot the results of this to see which clusters become harder to predict (i.e they see a decrease in classification accuracy) when different variables are removed as inputs.

``` r
ggplot(gathered_df, aes(x=reorder_within(excluded_var, value, metric), y=value, fill = excluded_var)) +
  geom_bar(stat="identity") +
  facet_wrap(~metric, scales = "free") +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text (hjust = 0.5)) +
  coord_flip(ylim = c(0.5, 1)) +
  scale_x_reordered() +
  labs(title = "Impact of Removing Variables on Classification Accuracy",
       x = "Excluded Variable", 
       y = "Classification Accuracy")
```
<img src="plots/Plot6_Classification_Accuracy.png"  height="600">

This method shows similar results to visually inspecting variable distributions - removing alcohol or quality as predictors has the largest impact on the model's ability to classify Cluster 2, pH and acidity is most important for Cluster 3, and sugar is most important for Cluster 4. 
