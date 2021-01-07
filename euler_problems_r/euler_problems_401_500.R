# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')

# Problem 493 - Under The Rainbow -----------------------------------------

# 70 coloured balls are placed in an urn, 10 for each of the seven rainbow 
# colours.
# 
# What is the expected number of distinct colours in 20 randomly picked balls?
#   
# Give your answer with nine digits after the decimal point (a.bcdefghij).

# Need to calculate probability of each distinct count to get expected value
# E(n_col) = 1*P(1) + 2*P(2) + ... + 7*P(7)

options(digits = 12)

n <- 70
k <- 20

# Get the total combinations of (n choose k), which will be the denominator of
# each of the following probabilities
n_choose_k <- factorial(n) / (factorial(k) * factorial(n - k))

probs <- data.frame(n_colors = 2:7) %>% 
  mutate(n_draws = factorial(n_colors * 10) / 
           (factorial(20) * factorial(n_colors * 10 - 20)),
         n_prev_draws = if_else(n_colors == 2, 0, 
                                lag(cumsum(n_draws))),
         n_remaining_draws = n_draws - n_prev_draws,
         probability = n_remaining_draws / sum(n_remaining_draws))

probs %>% 
  mutate(product = n_colors * probability) %>% 
  summarise(e_val = sum(product)) %>% 
  pull(e_val)




