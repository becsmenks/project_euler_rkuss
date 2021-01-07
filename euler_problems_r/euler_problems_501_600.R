# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')


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

# Problem 587- Concave triangle -------------------------------------------

# A square is drawn around a circle as shown in the diagram below on the left.
# We shall call the blue shaded region the L-section.
# A line is drawn from the bottom left of the square to the top right as shown 
# in the diagram on the right.
# We shall call the orange shaded region a concave triangle.
# 
# It should be clear that the concave triangle occupies exactly half of the 
# L-section.
# 
# Two circles are placed next to each other horizontally, a rectangle is drawn 
# around both circles, and a line is drawn from the bottom left to the top right 
# as shown in the diagram below.
# 
# This time the concave triangle occupies approximately 36.46% of the L-section.
# 
# If n circles are placed next to each other horizontally, a rectangle is drawn 
# around the n circles, and a line is drawn from the bottom left to the top 
# right, then it can be shown that the least value of n for which the concave 
# triangle occupies less than 10% of the L-section is n = 15.
# 
# What is the least value of n for which the concave triangle occupies less than 
# 0.1% of the L-section?

r <- 1
n_circles <- 2
n<- 2

# Start by calculating the area of the L-section based on the circle's radius
area_circle <- pi * r^2
area_square <- (2 * r)^2
area_l <- (area_square - area_circle) / 4

# Calculate the x,y coordinates where the diagonal line intersects the circle
y <- (((2*n_circles*r) + (2*r)) - sqrt((((2*n_circles*r) - (2*r))^2) - (4*((n^2) + 1)*(r^2)))) /
  (2 * ((n^2) + 1))
x <- y * n_circles

x^2 + y^2 = r^2
yn+rn-r = x

y^2 + (yn+rn-r)^2 = r^2
y^2(1+n^2) + 2(yn*(rn-r)) + (rn+r)^2 -r^2 = 0
y = (-2*(n*(r*n-r)) - sqrt((2*(n*(r*n-r))^2) - 4*(1+n^2)*(r*n+r)^2 - r^2)) / (2*(1+n^2))

circle_data <- data.frame(x = seq(0, 2, 0.01)) %>% 
  mutate(y = sqrt(r^2 - (x-r)^2) + r)

