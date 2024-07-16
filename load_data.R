# Load necessary libraries
library(tidyverse)
library(broom)
library(scales)

# Define file paths
base_path <- "C:/Users/dan/OneDrive/Desktop/data/"
annual_ticket_sales_path <- paste0(base_path, "AnnualTicketSales.csv")
highest_grossers_path <- paste0(base_path, "HighestGrossers.csv")
popular_creative_types_path <- paste0(base_path, "PopularCreativeTypes.csv")
top_distributors_path <- paste0(base_path, "TopDistributors.csv")
top_genres_path <- paste0(base_path, "TopGenres.csv")
top_grossing_ratings_path <- paste0(base_path, "TopGrossingRatings.csv")
top_grossing_sources_path <- paste0(base_path, "TopGrossingSources.csv")
top_production_methods_path <- paste0(base_path, "TopProductionMethods.csv")
wide_releases_count_path <- paste0(base_path, "WideReleasesCount.csv")

# Load datasets
annual_ticket_sales <- read.csv(annual_ticket_sales_path)
highest_grossers <- read.csv(highest_grossers_path)
popular_creative_types <- read.csv(popular_creative_types_path)
top_distributors <- read.csv(top_distributors_path)
top_genres <- read.csv(top_genres_path)
top_grossing_ratings <- read.csv(top_grossing_ratings_path)
top_grossing_sources <- read.csv(top_grossing_sources_path)
top_production_methods <- read.csv(top_production_methods_path)
wide_releases_count <- read.csv(wide_releases_count_path)

# Display the first few rows of each dataset to verify they loaded correctly
head(annual_ticket_sales)
head(highest_grossers)
head(popular_creative_types)
head(top_distributors)
head(top_genres)
head(top_grossing_ratings)
head(top_grossing_sources)
head(top_production_methods)
head(wide_releases_count)

# Analyze the highestGrossers data
# Assuming the column containing domestic gross earnings is named 'DomesticGross'
# Adjust the column name if different
ggplot(highest_grossers, aes(x = DomesticGross)) +
  geom_histogram(binwidth = 50e6, fill = "blue", color = "white") +
  labs(title = "Distribution of Domestic Gross Earnings",
       x = "Domestic Gross Earnings (in millions)",
       y = "Frequency")
