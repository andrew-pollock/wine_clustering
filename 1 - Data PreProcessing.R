
library(dplyr)

# Load both datasets
red_raw   <- read_delim("data/raw/winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
white_raw <- read_delim("data/raw/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Add a column for the colour
red_raw$colour <- "Red"
white_raw$colour <- "White"

# Combine the two datasets
combined_raw <- rbind(red_raw, white_raw)

# Check the variable names
names(combined_raw)

# Replace spaces with underscores in variable names
names(combined_raw) <- gsub(pattern = " ", replacement = "_", x = names(combined_raw))

# Check summary
summary(combined_raw)
# free sulphur dioxide and residual sugar look like they have outliers


# Investigating free sulphur dioxide
hist(combined_raw$free_sulfur_dioxide)
combined_raw[combined_raw$free_sulfur_dioxide > 100,]
# This doesn't look like an error in the data, just unusual observations

# Would using the log of free sulphur dioxide help normalise the data?
hist(log(combined_raw$free_sulfur_dioxide))

# Log transform the variable
combined_raw$free_sulfur_dioxide <- log(combined_raw$free_sulfur_dioxide)


# Investigating residual sugar
hist(combined_raw$residual_sugar)
combined_raw[combined_raw$residual_sugar > 40,]
# This observation seems like a significant outlier in both sugar and sulphur
# I'll exclude it
combined_raw <- combined_raw[combined_raw$residual_sugar < 40,]

# Now lets check residual sugar again
hist(combined_raw$residual_sugar)

# This is still skewed so I'll try taking the log
hist(log(combined_raw$residual_sugar))

# Log transform the variable
combined_raw$residual_sugar <- log(combined_raw$residual_sugar)

# Do a final summary check of the dataset
summary(combined_raw)


# Save the formatted data
readr::write_csv(combined_raw, "data/processed/processed_wine_data.csv")

