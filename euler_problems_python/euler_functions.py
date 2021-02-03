# Functions for Solving Project Euler Problems
# projecteuler.net
# Becca Kuss

import math
import os
import pandas as pd
import numpy as np
import itertools
import decimal

def sum_digits(n):
    # Initialize everything
    all_digits_sum = 0

    # Pick off each digit
    while n > 0:
        # Pull out the right-most digit and add to total
        all_digits_sum = all_digits_sum + (n % 10)

        # Update the remaining digits
        n = n // 10

    return all_digits_sum

def count_digits(n):
    # Initialize everything
    n_digits = 0

    # Pick off each digit
    while n > 0:
        # Peel off the last digit
        n = n // 10
        # Count that digit
        n_digits = n_digits + 1

    return n_digits

# Sieve of Eratosthenes
# (Too slow)
def sieve_of_eratosthenes(n):
    # Get all prime numbers up to n

    # let A be an array of Boolean values, indexed by integers 2 to n,
    # initially all set to true.
    a1 = [False] * 2
    a2 = [True] * int(n - 1)
    a = a1 + a2

    # for i = 2, 3, 4, ..., not exceeding √n do
    for i in range(2, math.floor(math.sqrt(n))):

        # print(i)
        if a[i]:

            # for j = i2, i2+i, i2+2i, i2+3i, ..., not exceeding n do
            j = i ** 2
            x = 1
            while j <= n:

                a[j] = False
                j = (i ** 2) + (x * i)
                x = x + 1

    res = [i for i, val in enumerate(a) if val]
    return res


# Check Primality
def is_prime(n, primes):
    # Wikipedia:
    # The simplest primality test is trial division: given an input number, n,
    # check whether it is evenly divisible by any prime number between 2 and √n
    # (i.e. that the division leaves no remainder). If so, then n is composite.
    # Otherwise, it is prime.

    if n == 1:
        out = False
    else:
        out = len([num for num in primes if num <= math.sqrt(n) and n % num == 0]) == 0

    return out
