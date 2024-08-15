# Load necessary libraries
library(tidyverse)
library(broom)
library(scales)
library(ggplot2)
library(multcomp)

# Load and clean the data
movies_spec <- read_csv("HollywoodMovies (1).csv") %>%
  select(Movie, Genre, OpeningWeekend, DomesticGross, Budget, AudienceScore) %>%
  na.omit()  # Remove rows with NA in any of these columns immediately
  print(head(movies_spec))

  #display unique genres from dataset
print(unique(movies_spec$Genre))

# Part 1: Distribution of gross earnings graph
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


# Part 2: explore linear models for predicting domestic gross income over time

# Model 1: The effect of opening weekend gross income on the domestic gross earnings ---------------------------------------------------------------------------------
fit <- lm(DomesticGross ~ OpeningWeekend, data = movies_spec)
summary(fit)
ggplot(data = movies_spec, aes(x = OpeningWeekend, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) + # Points are slightly transparent to handle overplotting if data points are dense
  labs(
    title = "The Effect of Opening Weekend Gross Income on the Domestic Gross Earnings of Hollywood Movies",
    x = "Opening Weekend Gross Income (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "deepskyblue4")



# Model 2: The effect of audience scores on the domestic gross earnings of hollywood movies ------------------------------------------------------------------------------------
# Fit the linear regression model
fit1 <- lm(DomesticGross ~ AudienceScore, data = movies_spec)

# Summarize the model
summary(fit1)

# Visualize the data and the regression line
ggplot(data = movies_spec, mapping = aes(x = AudienceScore, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Audience Scores on the Domestic Gross Earnings of Hollywood Movies in 2017",
    x = "Audience Score",
    y = "Domestic Gross Earnings (millions of dollars)") +
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], color = "deepskyblue4") +
  scale_y_continuous(labels = label_dollar())

  # Remove NA values that could interfere with model fitting
movies_spec <- na.omit(movies_spec, cols = c("AudienceScore", "DomesticGross"))

# Model 3: The effect of production budget on the domestic gross earnings of Hollywood movies --------------------------------------------------------------------------------------------
# Fit the linear regression model
fit2 <- lm(DomesticGross ~ Budget, data = movies_spec)

# Summarize the model
summary(fit2)

# Visualize the data and the regression line
ggplot(data = movies_spec, mapping = aes(x = Budget, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Production Budget on the Domestic Gross Earnings of Hollywood Movies",
    x = "Production Budget (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2], color = "deepskyblue4") +
  scale_y_continuous(labels = label_dollar())


# Remove NA values that could interfere with model fitting
movies_spec <- na.omit(movies_spec, cols = c("Budget", "DomesticGross"))


# Explore the best model of the 3 above ------------------------------------------------------------------
    ggplot(data = movie_spec, aes(x = OpeningWeekend, y = DomesticGross)) +
      geom_point(shape = 1, size = 1, alpha = 0.75) +
      labs(
        title = "The Effect of Opening Weekend Gross Income on the Domestic Gross Earnings of Hollywood Movies",
        x = "Opening Weekend Gross Income (millions of dollars)",
        y = "Domestic Gross Earnings (millions of dollars)"
      ) +
      geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "deepskyblue4")


#histogram for residuals

    movies_spec <- fit %>%
    augment(movies_spec)
    ggplot(data = movies_res, mapping = aes(x = .resid)) + geom_histogram(binwidth = 10)


#residuals vs fitted values
    ggplot(data = movies_spec, mapping = aes(x = OpeningWeekend, y = .resid)) + geom_point(shape=1, size=1, alpha = 0.75 ) + geom_abline(intercept = 0, slope = 0, linetype = "twodash") +
    labs(title = "Residuals verus fitted values" x = "Fitted values (millions of dollars)", y= "Residuals(millions of dollars)")

# Simulating 19 data sets with normal errors to find the best model.
simulated_data <- simulate(fit, nsim = 19)
OpeningWeekend <- movies_spec %>%
  pull(OpeningWeekend) %>%

DomesticGross <- movies_spec %>%
  pull(DomesticGross)
  OpeningWeekend <- movies_spec %>%
  drop_na(OpeningWeekend) %>%
pull(OpeningWeekend)

simulated_data <- simulated_data %>%
  mutate(OpeningWeekend = OpeningWeekend) %>%
  mutate(sim_0 = DomesticGross) %>%
  pivot_longer(cols = starts_with("sim"), cols_vary = "slowest")

# Visualize simulated data
ggplot(data = simulated_data, aes(x = OpeningWeekend, y = value)) +
  geom_point(shape = 1, size = 1) +
  facet_wrap(~name)

# Make and evaluate predictions with the model
# Example prediction for $40 million in gross income on opening weekend
opening_weekend_value <- 40e6  # $40 million
predicted_value <- coef(fit)[1] + coef(fit)[2] * opening_weekend_value
print(paste("Predicted Domestic Gross: $", round(predicted_value, 2)))


# Genre-based analysis - Count movies and calculate average earnings per genre-----------------------------
genre_stats <- movies_spec %>%
  group_by(Genre) %>%
  summarize(
    Count = n(),
    AverageEarnings = mean(DomesticGross, na.rm = TRUE),
    .groups = 'drop'
  )
print(genre_stats)

# Plot number of movies per genre--
ggplot(genre_stats, aes(x = reorder(Genre, Count), y = Count, fill = Genre)) +
  geom_col() +
  labs(
    title = "Number of Movies per Genre",
    x = "Genre",
    y = "Number of Movies"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA and post-hoc analysis--
anova_result <- aov(DomesticGross ~ Genre, data = movies_spec)
summary(anova_result)
tukey_test <- glht(anova_result, linfct = mcp(Genre = "Tukey"))
summary(tukey_test)

# Extend an existing model to include genre as a factor
fit_genre <- lm(DomesticGross ~ OpeningWeekend + Genre, data = movies_spec)
summary(fit_genre)