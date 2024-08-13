# Load necessary libraries
library(tidyverse)
library(broom)
library(scales)
library(ggplot2)
library(multcomp)

# Load and preview the annual ticket sales data & highest grossers dataset
movies_spec <- read_csv("AnnualTicketSales.csv")
print(head(movies_spec))
highest_grossers <- read_csv("HighestGrossers.csv")
print(head(highest_grossers))
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

# Calculate statistics to describe the center and spread of the variable
highest_grossers %>%
  summarize(
    Mean = mean(`TOTAL FOR YEAR`, na.rm = TRUE),
    SD = sd(`TOTAL FOR YEAR`, na.rm = TRUE),
    Q05 = quantile(`TOTAL FOR YEAR`, 0.05, na.rm = TRUE),
    Q1 = quantile(`TOTAL FOR YEAR`, 0.25, na.rm = TRUE),
    Median = median(`TOTAL FOR YEAR`, na.rm = TRUE),
    Q3 = quantile(`TOTAL FOR YEAR`, 0.75, na.rm = TRUE)
  )

# Part 2: explore linear models for predicting domestic gross income over time

# Model 1: The effect of opening weekend gross income on the domestic gross earnings ---------------------------------------------------------------------------------
fit <- lm(DomesticGross ~ OpeningWeekend, data = movies_spec)
summary(fit)
ggplot(data = movies_spec, aes(x = OpeningWeekend, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Opening Weekend Gross Income on the Domestic Gross Earnings of Hollywood Movies",
    x = "Opening Weekend Gross Income (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "forestgreen")



# Model 2: The effect of audience scores on the domestic gross earnings of hollywood movies ------------------------------------------------------------------------------------
# Fit the linear regression model
fit1 <- lm(DomesticGross ~ AudienceScore, data = movies)

# Summarize the model
summary(fit1)

# Visualize the data and the regression line
ggplot(data = movies, mapping = aes(x = AudienceScore, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Audience Scores on the Domestic Gross Earnings of Hollywood Movies in 2017",
    x = "Audience Score",
    y = "Domestic Gross Earnings (millions of dollars)") +
  geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], color = "forestgreen") +
  scale_y_continuous(labels = label_dollar())

# Model 3: The effect of production budget on the domestic gross earnings of Hollywood movies --------------------------------------------------------------------------------------------
# Fit the linear regression model
fit2 <- lm(DomesticGross ~ Budget, data = movies)

# Summarize the model
summary(fit2)

# Visualize the data and the regression line
ggplot(data = movies, mapping = aes(x = Budget, y = DomesticGross)) +
  geom_point(shape = 1, size = 1, alpha = 0.75) +
  labs(
    title = "The Effect of Production Budget on the Domestic Gross Earnings of Hollywood Movies",
    x = "Production Budget (millions of dollars)",
    y = "Domestic Gross Earnings (millions of dollars)"
  ) +
  geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2], color = "forestgreen") +
  scale_y_continuous(labels = label_dollar())



# Explore the best model of the 3 above ------------------------------------------------------------------
    ggplot(data = movies, aes(x = OpeningWeekend, y = DomesticGross)) +
      geom_point(shape = 1, size = 1, alpha = 0.75) +
      labs(
        title = "The Effect of Opening Weekend Gross Income on the Domestic Gross Earnings of Hollywood Movies",
        x = "Opening Weekend Gross Income (millions of dollars)",
        y = "Domestic Gross Earnings (millions of dollars)"
      ) +
      geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "forestgreen")


#histogram for residuals

    movies <- fit %>%
    augment(moveis)
    ggplot(data = movies, mapping = aes(x = .resid)) + geom_histogram()


#residuals vs fitted values
    ggplot(data = movies, mapping = aes(x = OpeningWeekend, y = .resid)) + geom_point(shape=1, size=1, alpha = 0.75 ) + geom_abline(intercept = 0, slope = 0, linetype = "twodash") +
    labs(title = "Residuals verus fitted values" x = "Fitted values (millions of dollars)", y= "Residuals(millions of dollars)")

#simulating data sets with normal errors for best modal

  simulated_data <- simulate(fit, nsim = 19)
  OpeningWeekend <- movies %>%
    pull(OpeningWeekend)

     DomesticGross <- movies %>%
    pull(DomesticGross)

     OpeningWeekend <- movies %>%
    drop_na(OpeningWeekend) %>%
    pull(OpeningWeekend)

    simulated_data <- simulated_data %>%
    mutate(OpeningWeekend = OpeningWeekend) %>%
    mutate(sim_0 = DomesticGross) %>%
    pivot_longer(cols = starts_with("sim"), cols_vary = "slowest")

    ggplot(data = simulated_data, mapping = aes(x = OpeningWeekend, y = value)) + geom_point(shape =1, size=1) + facet_wrap(~name)


    #part 5 make and evaluate predictions with the modal


#repeat prediction + residual calcuation for each movie to use the modals on for opening weekend.

    # sample prediction for 40 million in gross income on opening weekend (movie name)
    7.9310 + (2.8638*40.0)

    #Residual = Observed - Predicted


# part 6 Genre-based analysis ---------
# Count movies and calculate average earnings per genre
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
  labs(title = "Number of Movies per Genre",
       x = "Genre",
       y = "Number of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANOVA to test differences in earnings by genre
anova_result <- aov(DomesticGross ~ Genre, data = movies_spec)
summary(anova_result)

# Post-hoc Tukey HSD
tukey_test <- glht(anova_result, linfct = mcp(Genre = "Tukey"))
summary(tukey_test)

# Extend an existing model to include genre
fit_genre <- lm(DomesticGross ~ OpeningWeekend + Genre, data = movies_spec)
summary(fit_genre)