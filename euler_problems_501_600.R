# Project Euler
# projecteuler.net
# Becca Kuss

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

source('~/git/project_euler_rkuss/euler_functions.R')


# Problem 504 - Square on the Inside --------------------------------------

# Let ABCD be a quadrilateral whose vertices are lattice points lying on the 
# coordinate axes as follows:
#   
# A(a, 0), B(0, b), C(−c, 0), D(0, −d), where 1 ≤ a, b, c, d ≤ m and a, b, c, 
# d, m are integers.
# 
# It can be shown that for m = 4 there are exactly 256 valid ways to construct 
# ABCD. Of these 256 quadrilaterals, 42 of them strictly contain a square number 
# of lattice points.
# 
# How many quadrilaterals ABCD strictly contain a square number of lattice 
# points for m = 100?

m = 4

quad_combos <- data.frame(a = 1:m) %>% 
  merge(data.frame(b = 1:m)) %>% 
  merge(data.frame(c = 1:m)) %>% 
  merge(data.frame(d = 1:m)) %>% 
  add_rownames("combo")


