# Functions for Solving Project Euler Problems
# projecteuler.net
# Becca Kuss

import math
import os
import pandas as pd
import numpy as np

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

