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