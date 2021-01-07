# Project Euler
# projecteuler.net
# Becca Kuss

# Problem 16 - Power digit sum --------------------------------------------

# 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
#
# What is the sum of the digits of the number 2^1000?

two_raised = 2**1000

sum_digits(two_raised)

# 1366 - CORRECT!

# Problem 20 - Factorial digit sum ----------------------------------------

# n! means n × (n − 1) × ... × 3 × 2 × 1
#
# For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
# and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
#
# Find the sum of the digits in the number 100!

n_factorial = math.factorial(100)

sum_digits(n_factorial)

# 648 - CORRECT!