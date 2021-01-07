# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')

# Problem 701 - Random connected area -------------------------------------

# Consider a rectangle made up of W x H square cells each with area 1.
# Each cell is independently coloured black with probability 0.5 otherwise 
# white. Black cells sharing an edge are assumed to be connected.
# Consider the maximum area of connected cells.
# 
# Define E(W,H) to be the expected value of this maximum area. For example, 
# E(2,2) = 1.875, as illustrated below.
# 
# You are also given E(4,4) = 5.76487732, rounded to 8 decimal places.
# 
# Find E(7,7), rounded to 8 decimal places.

w <- 2
h <- 2

grid <- data.frame(row = 1:h) %>% 
  merge(data.frame(col = 1:w))



# Problem 740 - Secret Santa ----------------------------------------------

# Secret Santa is a process that allows n people to give each other presents, so 
# that each person gives a single present and receives a single present. At the 
# beginning each of the n people write their name on a slip of paper and put the 
# slip into a hat. Each person takes a random slip from the hat. If the slip has 
# their name they draw another random slip from the hat and then put the slip 
# with their name back into the hat. At the end everyone buys a Christmas 
# present for the person whose name is on the slip they are holding. This 
# process will fail if the last person draws their own name.
# 
# In this variation each of the n people gives and receives two presents. At the 
# beginning each of the n people writes their name on two slips of paper and 
# puts the slips into a hat (there will be 2n slips of paper in the hat). As 
# before each person takes a random slip from the hat that does not contain 
# their name. Then they do the same process again so that they end up with two 
# slips neither of which contain their name. The process will fail if the last 
# person gets at least one slip with their own name.
# 
# Define q(n) to be the probability of this happening. You are given 
# q(3) = 0.3611111111 and q(5) = 0.2476095994 both rounded to 10 decimal places.
# 
# Find q(100) rounded to 10 decimal places.

# Theory:
# q(3) = p(p1 doesn't pick both p3) & 
#           p(p2 doesn't pick both p3 | p1 doesn't pick both p3)

# q(5) = p(p1 doesn't pick both p5) & 
#           p(p2 doesn't pick both p5 | p1 doesn't pick both p5) & 
#           p(p3 doesn't pick both p5 | p1&p2 don't pick both p5) & 
#           p(p4 doesn't pick both p5 | p1&p2&p3 don't pick both p5)


n <- 3

# p(p1 doesn't pick both pn) = p(p1 pick other) & p(p1 pick other | pick other)
first_prob_num <- 2*n - 2 - 2 # subtract 2 for p1, 2 for pn
first_prob_den <- 2*n - 2 # subtract 2 for p1
second_prob_num <- 2*n - 2 - 2 - 1 # subtract 2 for p1, 2 for pn, 1 for first draw
second_prob_den <- 2*n - 2 - 1 # subtract 2 for p1, 1 for first draw

first_prob <- (first_prob_num / first_prob_den)
second_prob <- (second_prob_num / second_prob_den)

p1_prob_success <- first_prob * second_prob
# ^^ this is wrong because p1 doesn't have to pick both non-p3, there can be
# at most one p3 slip
