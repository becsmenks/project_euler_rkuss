# Functions for Solving Project Euler Problems
# projecteuler.net
# Becca Kuss

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(lpSolve)

# Sieve of Eratosthenes
# (Too slow)
sieve_of_eratosthenes <- function(n) {
  
  # Get all prime numbers up to n
  
  # let A be an array of Boolean values, indexed by integers 2 to n,
  # initially all set to true.
  a <- data.frame(ind = 2:n,
                  check = TRUE)
  
  # for i = 2, 3, 4, ..., not exceeding √n do
  for (i in 2:sqrt(n)) {
    
    # if A[i] is true
    progress <- a %>% filter(ind == i) %>% pull(check)
    if (progress) {
      
      # for j = i2, i2+i, i2+2i, i2+3i, ..., not exceeding n do
      j <- i^2
      x <- 1
      while (j <= n) {
        
        # A[j] := false
        a$check[a$ind == j] <- FALSE
        j <- i^2 + (x * i)
        x <- x + 1
        
      }
    }
  }
  
  a %>%
    filter(check) %>%
    pull(ind)
  
}


# Sum digits
# (Too slow, and sometimes doesn't work, maybe due to precision?)
sum_digits <- function(n) {
  
  # Initialize everything
  remaining_digits <- n 
  all_digits <- c()
  
  # Pick off each digit
  while (remaining_digits > 0) {
    
    # Pull out the right-most digit
    rightmost_digit <- remaining_digits - floor(remaining_digits / 10) * 10
    
    # Save out each digit
    all_digits <- c(all_digits, rightmost_digit)
    
    # Update the remaining digits
    remaining_digits <- floor(remaining_digits / 10)
  }
  
  sum(all_digits)
}

# Merge n times
merge_n_times <- function(vector, n = 2, col = "col") {
  
  # Initialize the df
  df <- data.frame(vector)
  names(df) <- paste0(col, "1")
  
  for (i in 2:n) {
    # Merge in same dataframe with new column header
    df_mrg <- data.frame(vector)
    names(df_mrg) <- paste0(col, as.character(i))
    
    df <- merge(df, df_mrg)
  }
  df
}

# Generate Fibonacci sequence up to X
generate_fib_seq <- function(x) {
  fn_minus1 <- 1
  fn <- 1
  fib_seq <- c()
  while (fn < x) {
    
    # Create the sequence
    fib_seq <- c(fib_seq, fn)
    
    # Increment to the next fn
    fn_plus1 <- fn + fn_minus1
    
    # Reset values
    fn_minus1 <- fn
    fn <- fn_plus1
    
  }
  fib_seq
}
