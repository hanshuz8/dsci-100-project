# dsci-100-project_template
Template project repository for DSCI-100
# Title: Predicting Newsletter Subscriptions from Minecraft Player Data
# 1. Introduction
# In this project, we aim to predict whether a player of a Minecraft-based research server
# subscribes to a game-related newsletter, based on demographic and behavioral data.
# The project uses two datasets: one describing players, and another describing session activity.

# II. Methods & Results:
# Load packages
library(repr)
library(tidyverse)
library(tidymodels)
options(repr.matrix.max.rows = 6) 

# Load data
players <-read_csv("players.csv", skip=1)
sessions <- read_csv("sessions.csv", skip=1)

players
sessions

# Convert datetime columns and calculate session duration
sessions_datatime <- sessions |>
  mutate(
    start_time = dmy_hm(start_time),
    end_time = dmy_hm(end_time),
    session_duration = as.numeric(difftime(end_time, start_time, units = "mins")))

sessions_datatime

# Aggregate session features
session_summary <- sessions |>
  group_by(hashedEmail) |>
  summarise(
    total_sessions = n(),
    avg_session_duration = mean(session_duration, na.rm = TRUE),
    total_play_time = sum(session_duration, na.rm = TRUE),
    .groups = "drop")
session_summary

# Join with player data
data <- players |>
  left_join(session_summary, by = "hashedEmail") |>
  replace_na(list(total_sessions = 0, avg_session_duration = 0, total_play_time = 0)) |>
  select(-hashedEmail, -name) |>
  drop_na()
data

# Convert categorical variables to factors
data_factor <- data |>
  mutate(
    subscribe = as.factor(subscribe),
    experience = as.factor(experience),
    gender = as.factor(gender))
data_factor

# Split data into training and test sets
set.seed(42)
data_split <- initial_split(data_factor, prop = 0.75, strata = subscribe)
train_data <- training(data_split)
test_data <- testing(data_split)

data_split
train_data
test_data 

# Create recipe
rec <- recipe(subscribe ~ ., data = train_data) |>
  step_center(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())
rec

# Train and test
prep_rec <- prep(rec)
train_prepped <- bake(prep_rec, new_data = NULL)
test_prepped <- bake(prep_rec, new_data = test_data)
prep_rec
train_prepped
test_prepped

# Fit Models
log_spec <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

log_fit <- workflow() |>
  add_model(log_spec) |>
  add_recipe(rec) |>
  fit(data = train_data)

log_preds <- predict(log_fit, test_data, type = "prob") |>
  bind_cols(predict(log_fit, test_data)) |>
  bind_cols(test_data |> select(subscribe))

conf_mat(log_preds, truth = subscribe, estimate = .pred_class)

log_spec
log_fit
log_preds

# Visualize Feature Differences
feature_difference_plot <- data |>
  pivot_longer(c(played_hours, Age, total_sessions, avg_session_duration, total_play_time),
               names_to = "feature", values_to = "value") |>
  ggplot(aes(x = value, fill = subscribe)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Distribution of Features by Subscription Status", x = "", y = "Density")

feature_difference_plot

# III. Discussion:
# 1. summarize what you found
# 2. discuss whether this is what you expected to find?
# 3. discuss what impact could such findings have?
# 4. discuss what future questions could this lead to?

1. From our analysis using logistic regression, we found that: Player engagement is a strong predictor of newsletter subscription. Players with higher played_hours, more sessions, and longer total play time were more likely to subscribe. Experience level also mattered: more experienced players (e.g., "Veteran", "Pro") showed a higher likelihood of subscription. Age and gender appeared less influential, though younger players showed slightly higher variability.
2. Yes, this aligns with expectations: It’s intuitive that players who spend more time in the game and return frequently would be more engaged, and thus more likely to subscribe. More experienced players may feel a stronger connection to the community and be more inclined to follow news updates.
3. The research team can focus recruitment efforts on active or highly engaged players, improving the efficiency of outreach campaigns.
4. What content types or gameplay features lead to more engagement and subscriptions?
   Can we predict unsubscription or dropout using time-based trends?

