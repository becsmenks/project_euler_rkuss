# Project Euler
# projecteuler.net
# Becca Kuss


# Problem 700 - Eulercoin --------------------------------------

# Leonhard Euler was born on 15 April 1707.
#
# Consider the sequence 1504170715041707n mod 4503599627370517.
#
# An element of this sequence is defined to be an Eulercoin if it is
# strictly smaller than all previously found Eulercoins.
#
# For example, the first term is 1504170715041707 which is the first
# Eulercoin. The second term is 3008341430083414 which is greater
# than 1504170715041707 so is not an Eulercoin. However, the third
# term is 8912517754604 which is small enough to be a new Eulercoin.
#
# The sum of the first 2 Eulercoins is therefore 1513083232796311.
#
# Find the sum of all Eulercoins.

euler_coins = [1504170715041707 % 4503599627370517]
min_coin = min(euler_coins)
n = 2
while min_coin > 1:
    # Calculate the next term in the sequence
    term_n = (1504170715041707 * n) % 4503599627370517

    # Save it if it's an eulercoin
    if term_n < min_coin:
        print(term_n)
        euler_coins = euler_coins + [term_n]
        min_coin = term_n

    n += 1

sum(euler_coins)