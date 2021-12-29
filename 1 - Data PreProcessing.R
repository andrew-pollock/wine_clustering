
library(dplyr)

# Load the datasets
wine_raw <- readr::read_delim("data/raw/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Check the variable names
names(wine_raw)

# Replace spaces with underscores in variable names
names(wine_raw) <- gsub(pattern = " ", replacement = "_", x = names(wine_raw))

# Count the NAs in each column
sapply(wine_raw, function(x) sum(is.na(x)))

# Check summary
summary(wine_raw)
# Several variables seem to have outliers

## Create a function to count the number of outliers
count_outliers <- function(input, sd = 4) {
  cutoff <- mean(input) + sd * sd(input)
  sum(input > cutoff)
}

## Apply this function to every variable
wine_raw %>% summarise(across(1:12, count_outliers))

# Calculate the mean + 4 s.d cutoff for each variable
cutoffs <- c(colMeans(wine_raw, na.rm = TRUE) + 4 * sapply(wine_raw, sd))

# Loop through each variable, removing any outliers
for (i in 1:12) {
  wine_raw <- wine_raw[wine_raw[, i] < cutoffs[i], ]
}

# Do a final summary check of the dataset
summary(wine_raw)

# Save the formatted data
readr::write_csv(wine_raw, "data/processed/processed_wine_data.csv")
