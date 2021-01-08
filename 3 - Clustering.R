
library(dplyr)   ## For data transformation
library(caret)   ## Provides a nice modelling framework
library(cluster) ## Library for clustering
library(factoextra) ## For visualising clusters
library(ggplot2)

# Load in the processed data
wine_data <- readr::read_csv("data/processed/processed_wine_data.csv")

# Set a seed to ensure reproducibility
set.seed(101)


# Normalise the 3 skewed variables
wine_data <- wine_data %>% mutate_at(.vars = c("residual_sugar",  "chlorides", "sulphates"), .funs = log)

# Density is strongly correlated with both residual sugar and alcohol, so remove it
wine_data <- wine_data %>% select(-density)


# Next, center and scale the variables so large scaled variables don't dominate
preprocessing_func <- preProcess(wine_data, method = c("center", "scale"))

normalised_data <- predict(preprocessing_func, wine_data)



# Create a empty dataframe with a dummy row
df <- data.frame(k = as.integer(0), sum_of_squares = as.double(0))


# Calculate the total sum of squares within each cluster
# Append that to the above data frame
for (k in 1:20){
  df <- rbind(df, c(k, kmeans(normalised_data, centers = k, nstart = 3, iter.max = 20)$tot.withinss))
}

# Drop the dummy row
df <- df[-1,]

# Plot the sum of squares
ggplot(df, aes(x=k, y=sum_of_squares)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters", y = "Total Within Cluster Sum of Squares") +
  theme_bw()


# Calculate the decrease in TSS that each extra cluster causes
df$diff <- c(0, round(diff(df$sum_of_squares, lag = 1, differences = 1),0))

# Plot these decreases in TSS
ggplot(df[2:20,], aes(x=k, y=abs(diff))) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters", y = "Reduction in Within Cluster TSS") +
  theme_bw()
# From 5 clusters onwards this decrease plateaus 




## Based on this analysis I'll use K = 5
k5 <- kmeans(normalised_data, centers = 6, nstart = 15, iter.max = 20)

# Append the clusters to the original dataset
wine_data$cluster <- k5$cluster

# How many wines are in each cluster?
wine_data %>% group_by(cluster) %>% summarise(n())


cluster_palette <- c("#1b9e77", "#e7298a", "#7570b3", "#d95f02", "#66a61e", "red")

# Visualise these clusters
fviz_cluster(k5, data = wine_data, labelsize = NA, ellipse.alpha = 0.1, shape = 16,
             palette = cluster_palette, 
             ggtheme = theme_bw(), xlab = "First Principal Component", ylab = "Second Principal Component",
             main = "Cluster Distribution on First 2 Principal Clusters")


# Lets look at the distribution of variables by cluster
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



