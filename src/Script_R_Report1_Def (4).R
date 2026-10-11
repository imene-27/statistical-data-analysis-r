
# Data Preparation & Descriptive Analysis ------------------------------------------------------------

library(kableExtra)
library(recipes)
library(MASS)
library(car)
library(caret)
library(pROC)
library(tidymodels)
library(tidyverse)
library(ggcorrplot)
library(ggplot2)

set.seed(1234)
 
high_pop <- read.csv("high_popularity_spotify_data.csv")
low_pop <- read.csv("low_popularity_spotify_data.csv")

# skimr::skim(high_pop)
# skimr::skim(low_pop)

tracks_df <- full_join(low_pop, high_pop) |>
  mutate(across(c(time_signature, key, mode), as.factor))

## Descriptive Analysis ------------------------------------------------------------------------------

summary_table <- tracks_df |>
  summarise(across(where(is.numeric), list(
    minimum = ~min(.x, na.rm = TRUE), 
    maximum = ~max(.x, na.rm = TRUE), 
    average = ~mean(.x, na.rm = TRUE), 
    median  = ~median(.x, na.rm = TRUE), 
    SD      = ~sd(.x, na.rm = TRUE), 
    na      = ~sum(is.na(.x))
  ), .names = "{.col}___{.fn}")) |>
  
    # the names argument is necessary because 
    # by default R connects the column and the function with a single underscore (energy_average). 
    # because variables like track_popularity already contain an underscore, 
    # splitting the text later becomes problematic.

  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "___") |>
  pivot_wider(names_from = Statistic, values_from = value) |>
  
  #Rounding all numeric columns to 2 decimal places
  mutate(across(where(is.numeric), ~round(.x, digits = 2))) |> kable()

summary_table

### Graphing our data -------------------------------------------------------------------------------

numeric_tracks <- tracks_df |>
  dplyr::select(where(is.numeric))|>
  tidyr::drop_na()

cor_matrix <- cor(numeric_tracks, 
                  use = "pairwise.complete.obs", 
                  method = "pearson")

ggcorrplot(cor_matrix, 
           method = "square", 
           type = "lower",        
           lab = TRUE,            
           lab_size = 2.5,          
           colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Pearson Correlation Matrix of Spotify Audio Features\n",
           legend.title = "Correlation")

#Distribution Bar-plot of track_popularity

ggplot(tracks_df, aes(x = track_popularity)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Track Popularity\n", x = "Popularity", y = "Count")


# Create a temporary dataframe for the plot with a clear "Hit" or "Flop" label
plot_data <- tracks_df |>
  mutate(Hit_Status = ifelse(track_popularity > 68, "Hit (>68)", "Flop (<=68)")) |>
  # Select the 4 key features we want to highlight
  dplyr::select(Hit_Status, instrumentalness, loudness, speechiness, valence) |>
  # Pivot the data so ggplot can facet it easily
  pivot_longer(cols = -Hit_Status, names_to = "Feature", values_to = "Value")

# Create the faceted boxplot
grouped_boxplots <- ggplot(plot_data, aes(x = Hit_Status, y = Value, fill = Hit_Status)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~Feature, scales = "free_y") + # Automatically creates a 2x2 grid
  scale_fill_manual(values = c("Hit (>68)" = "#2ca25f", "Flop (<=68)" = "#de2d26")) +
  theme_minimal() +
  theme(legend.position = "none", # Hide legend since x-axis has the labels
        strip.text = element_text(face = "bold", size = 12)) + 
  labs(title = "Feature Distributions: Hits vs. Flops",
       x = NULL, 
       y = "Feature Value")

# Display the plot
grouped_boxplots


# Linear Regression ---------------------------------------------------------------------------------

## Initial Split and Backwards selection ------------------------------------------------------

# We proceed by splitting the dataset in training and test, maintaining the proportion of track popularity 

init_split <-  tracks_df |>
  initial_split(prop = 0.7, strata = track_popularity)

train_data <- training(init_split)
test_data <- testing(init_split)

# We use Tidyverse's 'where' to select columns that are EITHER numeric OR factors.
# This safely drops character strings (like track_name) while keeping our categorical predictors.

num_pred_df <- train_data |>
  dplyr::select(where(is.numeric)) |>
  tidyr::drop_na()

# Fit the full model with all predictors
full_model <- lm(track_popularity ~ ., data = num_pred_df)
summary(full_model)

backward_model <- stepAIC(full_model, direction = "backward", trace = TRUE)
summary(backward_model)

  # forward_model <- stepAIC(full_model, direction="forward", trace = TRUE)
  # summary(full_model)

# Check which variables were kept
formula(backward_model)

### Multi-collinearity check ----------------------------------------------------

library(car)
library(lmtest)

# Variance Inflation Factor 
vif_values <- vif(backward_model)
vif_values |> kable()

# As an empirical rule, if any variable has a VIF > 5, is a good practive to exclude it and re-fit.


## Interaction terms and Polynomials ------------------------------------------------------
# 
# # The formula "y ~ .^2" tells R to include all main effects AND all 2-way interactions
# # This can be computationally heavy and inclined to overfitting.
# 
# full_interaction_model <- lm(track_popularity ~ .^2, data = num_pred_df)
# anova(backward_model, full_interaction_model)
# 
# summary(full_interaction_model)
# 
# # This will prune the 91 terms down to a "best" subset
# refined_interaction_model <- stepAIC(full_interaction_model, 
#                                      direction = "backward", 
#                                      trace = FALSE)
# 
# # Check how many variables survived
# length(coef(refined_interaction_model))
# summary(refined_interaction_model)


complex_model <- lm(track_popularity ~ speechiness + poly(energy, 2) + 
                      instrumentalness + valence + loudness + acousticness + 
                      liveness + acousticness:energy, 
                    data = num_pred_df)

summary(complex_model)

# Compare the complex model to your backward model using ANOVA
anova(backward_model, complex_model)


# Checking for possible non-random distribution of residuals
plot(backward_model, which = 1)


# It happears clearly that the Homoschedastic Assumption doesn't reflect the reality 
# and that the relationship is not purely linear

log_model <- lm(log(track_popularity) ~ speechiness + energy + instrumentalness + 
                  valence + loudness + acousticness + liveness, data= num_pred_df)
summary(log_model)
plot(log_model, which = 1)


## Final Performance Evaluation

test_predictions <- predict(backward_model, newdata = test_data)

test_results <- data.frame(
  actual = test_data$track_popularity,
  predicted = test_predictions)

metrics <- metric_set(rmse, rsq)
final_performance <- metrics(test_results, truth = actual, estimate = predicted)

final_performance

# Logistic Regression ------------------------------------------------------------------

# Create binary target variable using the natural split of the data
# 1 = high popularity (track_popularity > 68)
# 0 = low popularity  (track_popularity <= 68)

tracks_df <- tracks_df %>%
  mutate(popular = as.factor(ifelse(track_popularity > 68, 1, 0)))

# Verify
table(tracks_df$popular) |> kable()

# We create a new dataframe by selecting only the columns needed for the model.
tracks_model <- tracks_df %>%
  dplyr::select(popular, danceability, energy, loudness, 
                speechiness, instrumentalness, liveness,
                valence, tempo, duration_ms)

# Split the dataset randomly into two parts: 70% for training and 
# 30% for testing. Strata = popular ensures that the class distribution
# is kept proportional in both subsets.
logistic_split <- tracks_model |>
  initial_split(prop = 0.7, strata = popular)

# Extracts the 70% portion of the split and stores it as logistic_train,
# this set is the set model will learn from
logistic_train <- training(logistic_split) |>
  drop_na()

# Extracts the remaining 30% and stores it as logistic_test
# this set is never seen during training and will only be used to evaluate
# how well the model generalizes to new data.
logistic_test <- testing(logistic_split)

# Check the number of observations in the training and test sets
nrow(logistic_train)
nrow(logistic_test)

# Verify that the class distribution of the target variables is preserved
# in both the training and test sets after the stratified split.
prop.table(table(logistic_train$popular))
prop.table(table(logistic_test$popular))

# Define the preprocessing recipe
# We are predicting 'popular' using all other columns. The recipe
# learns the mean and sd exclusively from the logistic_train, preventing 
# data leakage when applied to the test set.
log_recipe <- recipe(popular ~., data = logistic_train) %>%
  step_normalize(all_numeric_predictors())

#Prep the recipe, bake test and train data
prepped_recipe <- prep(log_recipe, training = logistic_train)
baked_train <- bake(prepped_recipe, new_data = logistic_train)
baked_test <- bake(prepped_recipe, new_data = logistic_test)

# Verify the recipe by looking at the new normalized training data
head(baked_train)


# Fit the full logistic regression model on the NORMALIZED training data
full_model_logistic <- glm(popular ~., data = baked_train, family = binomial)
summary(full_model_logistic)

# Apply backward stepwise selection based on AIC
step_model <- stepAIC(full_model_logistic, direction = "backward", trace = TRUE)
summary(step_model)

# Compute odds ratios by exponentiating the coefficients
coef(step_model) |> kable()
exp(coef(step_model)) |> kable()

# Compute Variance Inflation Factor for each retained predictor
vif(step_model)

# Get predicted probabilities on the normalized test set
pred_probs <- predict(step_model, newdata = baked_test, type = "response")

# Convert probabilities to binary labels using threshold 0.5
pred_class <- as.factor(ifelse(pred_probs > 0.5, 1, 0))

# Confusion matrix (using baked_test to be perfectly consistent)
confusionMatrix(pred_class, baked_test$popular, positive = "1")

# Compute ROC curve
roc_curve <- roc(baked_test$popular, pred_probs)

# AUC value
auc(roc_curve)

# Plot ROC curve
plot(roc_curve, 
     lwd = 2, 
     col = "red", 
     main = "ROC Curve - Logistic Regression",
     legacy.axes = TRUE)

# Alternative threshold of 0.35
pred_class_2 <-  as.factor(ifelse(pred_probs > 0.35, 1, 0))
confusionMatrix(pred_class_2, baked_test$popular, positive = "1")
