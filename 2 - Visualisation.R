
# Data visualisation
library(dplyr)
library(caret)
library(tidyr)
library(ggplot2)
library(ggcorrplot)


# Load in the processed data
wine_data <- readr::read_csv("data/processed/processed_wine_data.csv")

## Plot 1 - Visualising the distribution of each variable individually
(plot1 <- wine_data %>% 
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


## Plot 2 - Plotting log of skewed variables
(plot2 <- wine_data %>% mutate_at(.vars = c("residual_sugar",  "chlorides", "sulphates"), .funs = log) %>%
        gather(key = "Variable", value = "Value") %>% 
        mutate(Value = as.numeric(Value)) %>% 
        ggplot(aes(x=Value, group=Variable, fill = Variable)) + 
        geom_density(size = 1, alpha = 0.6, show.legend = FALSE) + 
        facet_wrap(~ Variable, ncol = 3, scales = "free") + 
        theme_bw() +
        labs(y = "Variable Density", title = "Logged Variable Distribution") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(minor_breaks = NULL))

# Save the plot as a png
ggsave("plots/Plot2_Logged_Distribution.png", plot2)


## Plot 3 - Create a correlation plot for the variables

# Create a correlation matrix of all the independent variables
wine_correlation <- cor(wine_data[,1:12])

# Use ggcorrplot to plot the correlations of each variable against the others
(plot3 <- ggcorrplot(wine_correlation, hc.order = TRUE, type = "lower",
                     ggtheme = ggplot2::theme_classic, 
                     method = "square", lab = TRUE, 
                     legend.title = "Correlation", title = "Variable Correlation") + 
        theme(plot.title = element_text(hjust = 0.5)))

# Save the plot as a png
ggsave("plots/Plot3_Variable_Correlation.png", plot3)


