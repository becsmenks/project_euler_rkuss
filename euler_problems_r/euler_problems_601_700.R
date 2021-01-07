# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')

# Problem 684 - Inverse Digit Sum -----------------------------------------

# Define s(n) to be the smallest number that has a digit sum of n. For example 
# s(10) = 19.
#
# Let S(k) = SUM(n=1..k) s(n). You are given S(20) = 1074.
# 
# Further let fi be the Fibonacci sequence defined by f0 = 0, f1 = 1 and 
# fi = fi-2 + fi-1 for all i >= 2.
# 
# Find SUM(i=2..90) S(fi). Give your answer modulo 1000000007.


# Start by finding some digit sums
digit_sums_test <- data.frame()
for (n in 1:100) {
  
  # Find the smallest number with digit sum of n
  still_looking <- TRUE
  t <- 0
  while (still_looking & t <= 100 * n) {
    t <- t + 1
    still_looking <- sum_digits(t) != n
  }
  smllst_num <- if_else(!still_looking, t, NA_real_)
  
  # Create data frame
  digit_sums_test <- data.frame(n = n,
                                sn = smllst_num) %>% 
    bind_rows(digit_sums_test)
}

# Needed to cumulate remainders
remainder_lookup <- data.frame(remainder = 1:9) %>% 
  mutate(remainder_cum = cumsum(remainder),
         remainder = if_else(remainder == 9, as.integer(0), remainder))

# Based on the above, it looks like the pattern is to have as many 9's as is
# needed, prepended with the remainder, so we can write a closed form
digit_sums <- data.frame(n = 1:100) %>% 
  mutate(nines_needed = n %/% 9,
         remainder = n %% 9,
         sn = (remainder * 10^nines_needed) + (10^nines_needed - 1)) %>% 
  left_join(remainder_lookup, by = "remainder") %>% 
  mutate(Sn1 = cumsum(sn), # true, but we can write a closed form
         Sn2 = (remainder_cum * 10^nines_needed) + (9 * remainder)) # needs work

# Get Fibonacci sequence at least up to i = 90
fib_seq <- generate_fib_seq(1e20)

fib_seq_df <- data.frame(i = (1:length(fib_seq)) + 1,
                         fi = fib_seq)


# Problem 694 - Cube-full Divisors ----------------------------------------

# A positive integer n is considered cube-full, if for every prime p that 
# divides n, so does p^3. Note that 1 is considered cube-full.
# 
# Let s(n) be the function that counts the number of cube-full divisors of n. 
# For example, 1, 8 and 16 are the three cube-full divisors of 16. Therefore, 
# s(16) = 3.
# 
# Let S(n) represent the summatory function of s(n), that is 
# S(n) = SUM(i=1..n) s(i)
# 
# You are given S(16) = 19, S(100) = 126 and S(1000) = 13344.
# 
# Find S(10^18).
