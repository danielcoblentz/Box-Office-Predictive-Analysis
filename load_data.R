# Load necessary libraries
library(tidyverse)
library(broom)
library(scales)
library(ggplot2)


# Load and clean the data
movies_spec <- read_csv("HollywoodMovies (1).csv") %>%
  select(Movie, Genre, OpeningWeekend, DomesticGross, Budget, AudienceScore) %>%
  na.omit()  # Remove rows with NA in any of these columns immediately

# Display the first few rows of the dataset
print(head(movies_spec))

# Display unique genres from dataset
print(unique(movies_spec$Genre))

# Distribution of gross earnings graph
ggplot(data = movies_spec, mapping = aes(x = DomesticGross)) +
  geom_histogram(binwidth = 40, fill = "blue", color = "black") +
  labs(
    title = "The Domestic Gross U.S Earnings of Hollywood Movies",
    x = "Gross Income from Domestic (U.S) Viewers (millions of dollars)",
    y = "Number of Movies"
  ) +
  theme_minimal()

# Statistics to describe center and spread of DomesticGross
summary_stats <- movies_spec %>%
  summarize(
    Mean = mean(DomesticGross, na.rm = TRUE),
    SD = sd(DomesticGross, na.rm = TRUE),
    Q05 = quantile(DomesticGross, 0.05, na.rm = TRUE),
    Q1 = quantile(DomesticGross, 0.25, na.rm = TRUE),
    Median = median(DomesticGross, na.rm = TRUE),
    Q3 = quantile(DomesticGross, 0.75, na.rm = TRUE)
  )
print(summary_stats)

# Model fitting and plotting for Opening Weekend vs Domestic Gross
fit <- lm(DomesticGross ~ OpeningWeekend, data = movies_spec)
summary(fit)
ggplot(data = movies_spec, aes(x = OpeningWeekend, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Opening Weekend Gross Income on the Domestic Gross Earnings of Hollywood Movies",
    x = "Opening Weekend Gross Income (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "deepskyblue4")

# Model 2: The effect of audience scores on the domestic gross earnings of Hollywood movies
fit1 <- lm(DomesticGross ~ AudienceScore, data = movies_spec)
summary(fit1)
ggplot(data = movies_spec, aes(x = AudienceScore, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Audience Scores on the Domestic Gross Earnings of Hollywood Movies in 2017",
    x = "Audience Score",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], color = "deepskyblue4") +
  scale_y_continuous(labels = label_dollar())

# Model 3: The effect of production budget on the domestic gross earnings of Hollywood movies
fit2 <- lm(DomesticGross ~ Budget, data = movies_spec)
summary(fit2)
ggplot(data = movies_spec, aes(x = Budget, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Production Budget on the Domestic Gross Earnings of Hollywood Movies",
    x = "Production Budget (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2], color = "deepskyblue4") +
  scale_y_continuous(labels = label_dollar())

  #simulation data
  # Simulate data from the fitted model (Example for Opening Weekend)
simulated_data <- simulate(fit, nsim = 19)
simulated_data <- as.data.frame(simulated_data) %>%
  mutate(OpeningWeekend = movies_spec$OpeningWeekend) %>%
  pivot_longer(cols = starts_with("sim"), names_to = "simulation", values_to = "simulated_gross")

# Plotting the simulated data against the Opening Weekend earnings
ggplot(simulated_data, aes(x = OpeningWeekend, y = simulated_gross)) +
  geom_point(shape = 1, size = 1) +
  labs(title = "Simulated Domestic Gross vs. Opening Weekend",
       x = "Opening Weekend (millions)",
       y = "Simulated Domestic Gross (millions)")

# Genre-based analysis
genre_stats <- movies_spec %>%
  group_by(Genre) %>%
  summarize(
    Count = n(),
    AverageEarnings = mean(DomesticGross, na.rm = TRUE),
    .groups = 'drop'
  )
print(genre_stats)

# Plot number of movies per genre
ggplot(genre_stats, aes(x = reorder(Genre, Count), y = Count, fill = Genre)) +
  geom_col() +
  labs(
    title = "Number of Movies per Genre",
    x = "Genre",
    y = "Number of Movies"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Example movies for prediction
example_movies <- tibble(
  Movie = c("21 Jump Street", "Big Miracle", "Ice Age: Continental Drift"),
  OpeningWeekend = c(36.30, 7.76, 46.63)  # Opening weekend gross in millions
)

# Predict Domestic Gross using the linear model
predicted_gross <- predict(fit, newdata = example_movies)

# Add predictions to the dataframe
example_movies <- mutate(example_movies, PredictedGross = predicted_gross)

# Calculate manual predictions using coefficients from the fitted model
example_movies <- example_movies %>%
  mutate(
    ManualPrediction = coef(fit)[1] + coef(fit)[2] * OpeningWeekend
  )

# Output the results
print(example_movies)

# Plot predictions
ggplot(example_movies, aes(x = Movie, y = PredictedGross, fill = Movie)) +
  geom_col() +
  geom_text(aes(label = round(PredictedGross, 2)), vjust = -0.5) +
  labs(title = "Predicted Domestic Gross for Sample Movies",
       x = "Movie",
       y = "Predicted Gross (millions)") +
  theme_minimal()
