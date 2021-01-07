# Project Euler
# projecteuler.net
# Becca Kuss

# Problem 119 - Digit power sum ----------------------------------------

# The number 512 is interesting because it is equal to the sum of its digits
# raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a
# number with this property is 614656 = 28^4.
#
# We shall define an to be the nth term of this sequence and insist that a
# number must contain at least two digits to have a sum.
#
# You are given that a2 = 512 and a10 = 614656.
#
# Find a30.

i = 614656
n = 0
while n < 30 & i < 100:
    # Start with the digit sum
    i_sum = sum_digits(i)

    # Check the powers of the digit sum
    j = 1
    p = i_sum
    while p < i:
        p = i_sum ** j
        j = j + 1

    if p == i:
        n = n + 1
        print(n)
