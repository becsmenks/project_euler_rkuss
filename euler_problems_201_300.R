# Project Euler
# projecteuler.net
# Becca Kuss

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source('~/git/project_euler_rkuss/euler_functions.R')

# Problem 205 - Dice Game -------------------------------------------------

# Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 
# 4. Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 
# 5, 6.
# 
# Peter and Colin roll their dice and compare totals: the highest total wins. 
# The result is a draw if the totals are equal.
# 
# What is the probability that Pyramidal Pete beats Cubic Colin? Give your 
# answer rounded to seven decimal places in the form 0.abcdefg


pyramid_die <- 1:4
cubic_die <- 1:6

# Get all combinations for Peter
peters_possibilities <- data.frame(person = "Peter") %>% 
  merge(data.frame(die1 = pyramid_die)) %>% 
  merge(data.frame(die2 = pyramid_die)) %>% 
  merge(data.frame(die3 = pyramid_die)) %>% 
  merge(data.frame(die4 = pyramid_die)) %>% 
  merge(data.frame(die5 = pyramid_die)) %>% 
  merge(data.frame(die6 = pyramid_die)) %>% 
  merge(data.frame(die7 = pyramid_die)) %>% 
  merge(data.frame(die8 = pyramid_die)) %>% 
  merge(data.frame(die9 = pyramid_die)) %>% 
  tibble::rownames_to_column("roll") %>% 
  gather(die, value, starts_with ("die")) 

# Get all combinations for Colin
colins_possibilities <- data.frame(person = "Colin") %>% 
  merge(data.frame(die1 = cubic_die)) %>% 
  merge(data.frame(die2 = cubic_die)) %>% 
  merge(data.frame(die3 = cubic_die)) %>% 
  merge(data.frame(die4 = cubic_die)) %>% 
  merge(data.frame(die5 = cubic_die)) %>% 
  merge(data.frame(die6 = cubic_die)) %>% 
  tibble::rownames_to_column("roll") %>% 
  gather(die, value, starts_with ("die")) 

# Aggregate to probabilities of each score
probabilities <- peters_possibilities %>% 
  bind_rows(colins_possibilities) %>%
  # Get total for each
  group_by(person, roll) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  # Get counts of each total for each person
  group_by(person, total) %>% 
  summarise(n_rolls = n()) %>% 
  ungroup() %>% 
  # Get probabilities of each total value for each person
  group_by(person) %>% 
  mutate(prob_total = n_rolls / sum(n_rolls)) %>% 
  ungroup()

# Compare probabilities of each person
probabilities %>% 
  select(-n_rolls) %>% 
  spread(person, prob_total) %>% 
  # Get the probability that Colin is less than each total
  arrange(total) %>% 
  mutate(Colin_lt = lag(cumsum(Colin))) %>% 
  replace_na(list(Colin = 0, 
                  Colin_lt = 0,
                  Peter = 0)) %>% 
  # Find probability that Peter rolls each number & Colin rolls something less
  mutate(joint_prob = Peter * Colin_lt) %>% 
  summarise(prob = sum(joint_prob)) %>% 
  pull(prob) %>% 
  round(7)

# 0.5731441 - CORRECT!

