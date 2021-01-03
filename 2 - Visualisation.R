
# Data visualisation
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(tidyr)
library(plotly)

# Load in the processed data
wine_data <- readr::read_csv("data/processed/processed_wine_data.csv")

## Plot 1 - Visualising the distribution of each variable individually
(plot1 <- wine_data %>% select(-colour) %>%
    gather(key = "Variable", value = "Value") %>% 
    mutate(Value = as.numeric(Value)) %>% 
    ggplot(aes(x=Value, group=Variable, fill = Variable)) + 
    geom_density(size = 1, alpha = 0.6, show.legend = FALSE) + 
    facet_wrap(~ Variable, ncol = 3, scales = "free") + 
    theme_bw() +
    labs(y = "Variable Density", title = "Variable Distribution") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(minor_breaks = NULL))

# Save the plot as a png
ggsave("plots/Plot1_Variable_Distribution.png", plot1)



## Plot 2 - Create a correlation plot for the variables

# Create a correlation matrix of all the independent variables
wine_correlation <- cor(wine_data[,1:12])

# Use ggcorrplot to plot the correlations of each variable against the others
(plot2 <- ggcorrplot(wine_correlation, hc.order = TRUE, type = "lower",
                     ggtheme = ggplot2::theme_classic, 
                     method = "square", lab = TRUE, 
                     legend.title = "Correlation", title = "Variable Correlation") + 
        theme(plot.title = element_text(hjust = 0.5)))

# Save the plot as a png
ggsave("plots/Plot2_Variable_Correlation.png", plot2)



## Plot 3 - Principal Components Plot

# Calculate the first 3 principal components for the data
pca_model <- preProcess(wine_data[,1:12], method = "pca", pcaComp = 3)
pca_data <- predict(pca_model, wine_data)

# Plot the first 3 PC on a 3d scatter graph, coloured by wine type
plot_ly(x=pca_data$PC1, y=pca_data$PC2, z=pca_data$PC3, color=pca_data$colour, colors = c("red", "blue")) %>% 
    add_markers(alpha = 0.2) %>% layout(title = "Wine Colour Split by Principal Components",
                                        scene = list(xaxis = list(title = 'PC1'),
                                                     yaxis = list(title = 'PC2'),
                                                     zaxis = list(title = 'PC3')))




