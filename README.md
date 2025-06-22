# dsci-100-project_template
Template project repository for DSCI-100
# Title: Predicting Newsletter Subscriptions from Minecraft Player Data
# 1. Introduction
# In this project, we aim to predict whether a player of a Minecraft-based research server
# subscribes to a game-related newsletter, based on demographic and behavioral data.
# The project uses two datasets: one describing players, and another describing session activity.

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