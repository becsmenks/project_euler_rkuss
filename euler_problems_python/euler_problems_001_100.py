# Project Euler
# projecteuler.net
# Becca Kuss

# Problem 12 - Highly divisible triangular number -------------------------

# The sequence of triangle numbers is generated by adding the natural numbers.
# So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
# ten terms would be:
#
#   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
#
# Let us list the factors of the first seven triangle numbers:
#
# 1: 1
# 3: 1,3
# 6: 1,2,3,6
# 10: 1,2,5,10
# 15: 1,3,5,15
# 21: 1,3,7,21
# 28: 1,2,4,7,14,28
#
# We can see that 28 is the first triangle number to have over five divisors.
#
# What is the value of the first triangle number to have over five hundred
# divisors?

i = 50000
n_div = 0
while (n_div < 500) & (i < 55000):

    # Get the next triangular number and its list of possible divisors
    tn = sum(list(range(1, i+1)))
    poss_div = list(range(1, math.ceil((tn+1)/2)))

    # Check number of divisors
    n_div_tn = 0
    for d in poss_div:
        if tn % d == 0:
            n_div_tn = n_div_tn + 1

    print(n_div_tn)

    # Update max number of divisors
    n_div = n_div_tn
    i = i + 1




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


# Problem 22 - Names scores -----------------------------------------------

# Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
# containing over five-thousand first names, begin by sorting it into
# alphabetical order. Then working out the alphabetical value for each name,
# multiply this value by its alphabetical position in the list to obtain a name
# score.
#
# For example, when the list is sorted into alphabetical order, COLIN, which is
# worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
# would obtain a score of 938 × 53 = 49714.
#
# What is the total of all the name scores in the file?

# Read in names data
names = pd.melt(pd.read_csv("~/git/project_euler_rkuss/euler_data/p022_names.txt",
                            header = None, keep_default_na = False), value_name = 'name')

# Add column with alphabetical rank
names['alpha_rank'] = names['name'].rank()

# Separate letters into columns
names[['let_0', 'let_1', 'let_2',
       'let_3', 'let_4', 'let_5',
       'let_6', 'let_7', 'let_8',
       'let_9', 'let_10', 'let_11',
       'let_12']] = names['name'].str.split(pat = "\s*", expand = True)

# Gather letters into single column
names_long = pd.melt(names, id_vars = ['variable', 'name', 'alpha_rank'],
                     var_name = 'position',
                     value_name = 'letter')

# Drop missing or blank values
names_clean = names_long.loc[names_long['letter'] != ''].dropna()

# Create lookup of letter scores
letter_scores = pd.DataFrame(data = {'letter': ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                                                'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'],
                                     'letter_score': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                                                      15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]})

# Join letter scores to names
names_joined = names_clean.set_index('letter').join(letter_scores.set_index('letter'))

# Sum the total letter scores for each name
names_scores = names_joined.groupby(['name', 'alpha_rank'])[['letter_score']].sum().\
    reset_index(level = ['alpha_rank'])

# Calculate the final name score for each name
names_scores['name_score'] = names_scores['alpha_rank'] * names_scores['letter_score']

# Sum together all the name scores
names_scores['name_score'].sum()

# 871198282 - CORRECT!

# Problem 35 - Circular primes --------------------------------------------

# The number, 197, is called a circular prime because all rotations of the
# digits: 197, 971, and 719, are themselves prime.
#
# There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
# 73, 79, and 97.
#
# How many circular primes are there below one million?

x = 1000000

primes_lt_x = sieve_of_eratosthenes(x)

circular_primes = []
for p in primes_lt_x:
    print(p)

    # If it's less than 10, it's circular so skip ahead
    if p < 10:
        circular_primes = circular_primes + [p]
        continue

    # If there's any 2 in p, it won't be circular since some combination will end in 2
    if '2' in str(p) or '4' in str(p) or '5' in str(p) or '6' in str(p) or '8' in str(p) or '0' in str(p):
        continue

    # Start by parsing out the digits of p
    p_part = p
    p_digits = []
    while p_part > 0:
        d = p_part % 10
        p_digits = p_digits + [d]
        p_part = (p_part - d) / 10

    # Now check all the remaining permutations
    p_perms = list(set(itertools.permutations(p_digits)))
    p_perms_primality = []
    for x in p_perms:
        v = 0
        for j in range(0, len(x)):
            scale = 10 ** (len(x) - j - 1)
            v = v + (x[j] * scale)
        p_perms_primality = p_perms_primality + [is_prime(v, primes_lt_x)]

    if all(p_perms_primality):
        circular_primes = circular_primes + [p]

len(circular_primes)

# Problem 37 - Truncatable primes -----------------------------------------

# The number 3797 has an interesting property. Being prime itself, it is
# possible to continuously remove digits from left to right, and remain prime at
# each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
# 3797, 379, 37, and 3.
#
# Find the sum of the only eleven primes that are both truncatable from left to
# right and right to left.
#
# NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

x = 1000000

primes_lt_x = sieve_of_eratosthenes(x)

i = 4
trunc_pr_cnt = 0
trunc_pr = []
while trunc_pr_cnt < 11 and i < len(primes_lt_x):
    # Get the next prime
    p = primes_lt_x[i]
    still_prime = True

    #print(p)

    # Pick off each digit from the right
    n = p
    d_cnt = 0
    while n > 0:
        # Pick off the rightmost digit
        n = n // 10
        still_prime = is_prime(n, primes_lt_x) & still_prime

        # Also keep count of digits so we know where to start from the left
        d_cnt += 1

    # Pick off each digit from the left
    n = p
    while d_cnt > 1 & still_prime:
        # Pick off the leftmost digit
        n = n - (n // (10 ** (d_cnt - 1))) * (10 ** (d_cnt - 1))
        still_prime = is_prime(n, primes_lt_x) & still_prime

        # Increment backward number of digits
        d_cnt -= 1

    # Save prime if always truncatable
    if still_prime:
        trunc_pr_cnt += 1
        trunc_pr = trunc_pr + [p]

    # Increment i
    i += 1

trunc_pr

# Problem 41 - Pandigital prime -------------------------------------------

# We shall say that an n-digit number is pandigital if it makes use of all the
# digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
# also prime.
#
# What is the largest n-digit pandigital prime that exists?

n_digit_primes = sieve_of_eratosthenes(1000000000)

# Problem 46 - Goldbach's other conjecture ------------------------------

# It was proposed by Christian Goldbach that every odd composite number can be
# written as the sum of a prime and twice a square.
#
# 9 = 7 + 2×1^2
# 15 = 7 + 2×2^2
# 21 = 3 + 2×3^2
# 25 = 7 + 2×3^2
# 27 = 19 + 2×2^2
# 33 = 31 + 2×1^2
#
# It turns out that the conjecture was false.
#
# What is the smallest odd composite that cannot be written as the sum of a
# prime and twice a square?

primes = sieve_of_eratosthenes(1000000)

n = 1
keep_going = True
while keep_going:
    n = n + 2

    # If n is prime, skip it since we only care about odd composites
    if is_prime(n, primes):
        continue

    # Try every prime and see if the difference is square
    primes_to_test = [pr for pr in primes if pr < n]
    try_again = True
    j = 0
    while try_again and j < len(primes_to_test):
        # Calculate the number that would be squared to check if it's an integer
        check = math.sqrt((n - primes_to_test[j]) / 2)

        if check == math.floor(check):
            try_again = False
        else:
            j = j + 1

    # If after checking all possible primes, you didn't find one that works, we
    # have our answer
    if try_again:
        keep_going = False

# 5777 - CORRECT!

# Problem 48 - Self powers -----------------------------------------------

# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
#
# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

total = 0
for i in range(1,1000):
    total = total + (i ** i)

# 9110846700 - CORRECT!

# Problem 51 - Prime digit replacements -----------------------------------

# By replacing the 1st digit of the 2-digit number *3, it turns out that six of
# the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
#
# By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
# number is the first example having seven primes among the ten generated
# numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
# 56993. Consequently 56003, being the first member of this family, is the
# smallest prime with this property.
#
# Find the smallest prime which, by replacing part of the number (not
# necessarily adjacent digits) with the same digit, is part of an eight prime
# value family.

primes = sieve_of_eratosthenes(1000000)

looking_for_fam_size = 8
global_smallest_fam_member = 999999999999999
for p in primes:
    print(p)

    # Get the number of digits in the prime number
    str_p = list(str(p))
    num_dig_p = len(str_p)

    # Try replacing up to all of the digits
    for replace_n in range(1, num_dig_p + 1):
        # Initialize the indices you want to replace and get all iterations
        replace_inds_iter = set(itertools.permutations(([1] * replace_n) + ([0] * (num_dig_p - replace_n))))

        for replace_inds in replace_inds_iter:
            # Replace the desired indices with each digit 0-9, counting if prime
            prime_fam_size = 0
            smallest_fam_member = p
            for new_digit in range(0, 10):
                new_number = []
                # Need to check every digit in list of digits for whether it should be replaced or not
                for i in range(0, num_dig_p):
                    if replace_inds[i] == 1:
                        new_number = new_number + [str(new_digit)]
                    else:
                        new_number = new_number + [str_p[i]]

                # Check if it's prime, if so, add it to the family
                new_number_num = int(''.join(new_number))
                if is_prime(new_number_num, primes):
                    prime_fam_size += 1
                    smallest_fam_member = min(smallest_fam_member, new_number_num)

            # If the family size is eight, update the global smallest family member
            if prime_fam_size == looking_for_fam_size:
                print("FOUND ONE!")
                global_smallest_fam_member = min(global_smallest_fam_member, smallest_fam_member)









# Problem 52 - Permuted multiples ----------------------------------------

# It can be seen that the number, 125874, and its double, 251748, contain
# exactly the same digits, but in a different order.
#
# Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
# contain the same digits.

x = 0
go_on = True
while go_on:
    x = x + 1
    print(x)

    x_digits = sorted(list(str(x)))

    if x_digits == sorted(list(str(2 * x))):
        if x_digits == sorted(list(str(3 * x))):
            if x_digits == sorted(list(str(4 * x))):
                if x_digits == sorted(list(str(5 * x))):
                    if x_digits == sorted(list(str(6 * x))):
                        go_on = False

x

# 142857 - CORRECT!

# Problem 53 - Combinatoric selections ------------------------------------

# There are exactly ten ways of selecting three from five, 12345:
#   123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
#
# In combinatorics, we use the notation, n choose r.
#
# It is not until n = 23, that a value exceeds one-million:
#
# How many, not necessarily distinct, values of n choose r for 1 <= n <= 100,
# are greater than one-million?

count = 0
for n in range(1, 101):
    print(n)
    r = 1
    while r <= n:
        # Calculate n choose r
        n_choose_r = math.factorial(n) / (math.factorial(r) * math.factorial(n-r))

        # Check if value is greater than 1,000,000
        if n_choose_r > 1000000:
            count = count + 1

        # Increment r
        r = r + 1

count

# 4075 - CORRECT!

# Problem 56 - Powerful digit sum -----------------------------------------

# A googol (10^100) is a massive number: one followed by one-hundred zeros;
# 100^100 is almost unimaginably large: one followed by two-hundred zeros.
# Despite their size, the sum of the digits in each number is only 1.
#
# Considering natural numbers of the form, a^b, where a, b < 100, what is
# the maximum digital sum?

max_sum = 1
for a in range(1,100):
    print(a)
    for b in range(1,100):
        sum_ab = sum_digits(a ** b)
        if sum_ab > max_sum:
            max_sum = sum_ab

max_sum

# 972 - CORRECT!

# Problem 57 - Square root convergents ------------------------------------

# It is possible to show that the square root of two can be expressed as an
# infinite continued fraction.
#
# By expanding this for the first four iterations, we get:
#
# The next three expansions are...
#
# but the eighth expansion is the first example where the number of digits in the
# numerator exceeds the number of digits in the denominator.
#
# In the first one-thousand expansions, how many fractions contain a numerator
# with more digits than the denominator?

total = 0
for it in range(2,1001):
    print(it)

    # Initialize numerator and denominator for first iteration
    i = it
    numerat = 1
    denomin = 2

    # For each iteration, add 2 and take reciprocal
    while i > 1:
        # Take the reciprocal of 2 + the old fraction
        denomin_new = numerat + (2 * denomin)
        numerat_new = denomin

        # Update the numerator and denominator
        denomin = denomin_new
        numerat = numerat_new

        # Increment i
        i = i - 1

    # Add 1
    numerat = numerat + denomin

    # Count the digits and determine if
    if count_digits(numerat) > count_digits(denomin):
        total = total + 1

total

# 153 - CORRECT!

# Problem 63 - Powerful digit counts ----------------------------------

# The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the
# 9-digit number, 134217728=8^9, is a ninth power.
#
# How many n-digit positive integers exist which are also an nth power?




# Problem 65 - Convergents of e ---------------------------------------

# The square root of 2 can be written as an infinite continued fraction.
#
# The infinite continued fraction can be written, [1;(2)], where the (2)
# indicates that 2 repeats ad infinitum. In a similar way, the square
# root of 23 is [4;(1,3,1,8)].
#
# It turns out that the sequence of partial values of continued fractions
# for square roots provide the best rational approximations. Let us
# consider the convergents for the square root of 2.
#
# Hence the sequence of the first ten convergents for are:
#
# What is most surprising is that the important mathematical constant,
# e = [2;1,2,1,1,4,1,1,6,1,...,1,2k,1,...].
#
# The first ten terms in the sequence of convergents for e are:
#
# The sum of digits in the numerator of the 10th convergent is
#   1 + 4 + 5 + 7 = 17.
#
# Find the sum of digits in the numerator of the 100th convergent of the
# continued fraction for e.

# Start by creating the sequence of denominators
d = []
for j in range(1, 101):
    if (j + 1) % 3 == 0:
        d = d + [int(2 * ((j + 1) / 3))]
    else:
        d = d + [1]

# Set the convergent you are looking for
it = 100

# Initialize numerator and denominator for first iteration
i = it
numerat = 1
denomin = d[i - 2]

# For each iteration, add the previous denominator and take reciprocal
while i > 2:
    # Take the reciprocal of previous denominator + the old fraction
    denomin_new = numerat + (d[i - 3] * denomin)
    numerat_new = denomin

    # Update the numerator and denominator
    denomin = denomin_new
    numerat = numerat_new

    # Increment i
    i = i - 1

# Add 2
numerat = numerat + (2 * denomin)

# Sum the digits of the numerator
sum_digits(numerat)

# 272 - CORRECT!

# Problem 92 - Square digit chains --------------------------------------

# A number chain is created by continuously adding the square of the
# digits in a number to form a new number until it has been seen before.
#
# For example,
#   44 → 32 → 13 → 10 → 1 → 1
#   85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
#
# Therefore any chain that arrives at 1 or 89 will become stuck in an
# endless loop. What is most amazing is that EVERY starting number will
# eventually arrive at 1 or 89.
#
# How many starting numbers below ten million will arrive at 89?

n_89 = 0
for n in range(1, 10000001):
    print(n)

    while n != 1 and n != 89:

        square_digits_sum = 0
        # Pick off each digit
        while n > 0:
            # Pull out the right-most digit and add square to total
            square_digits_sum = square_digits_sum + ((n % 10) ** 2)

            # Update the remaining digits
            n //= 10
        n = square_digits_sum

    # Count it if it ended in 89
    if n == 89:
        n_89 += 1

# Problem 97 - Large non-Mersenne prime ------------------------------------

# The first known prime found to exceed one million digits was discovered in
# 1999, and is a Mersenne prime of the form 2^6972593−1; it contains exactly
# 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p−1,
# have been found which contain more digits.
#
# However, in 2004 there was found a massive non-Mersenne prime which contains
# 2,357,207 digits: 28433×2^7830457+1.
#
# Find the last ten digits of this prime number.

n = (28433 * (2 ** 7830457)) + 1
i = 0
last_ten = 0
while i < 10:
    # Pull out the right-most digit and add to total
    last_ten = last_ten + ((n % 10) * (10 ** i))

    # Update the remaining digits
    n = n // 10

    # Count the digit
    i += 1

last_ten

# Problem 99 - Largest exponential ---------------------------------

# Comparing two numbers written in index form like 2^11 and 3^7 is not
# difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
#
# However, confirming that 632382^518061 > 519432^525806 would be much more
# difficult, as both numbers contain over three million digits.
#
# Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K
# text file containing one thousand lines with a base/exponent pair on
# each line, determine which line number has the greatest numerical value.
#
# NOTE: The first two lines in the file represent the numbers in the
# example given above.

(2 ** 11) < (3 ** 7)

(632382 ** 518061) > (519432 ** 525806)

# Read in base/exponent pairs
pairs = pd.read_csv("~/git/project_euler_rkuss/euler_data/p099_base_exp.txt",
                    header=None, keep_default_na=False, names=["base", "exp"])

greatest_val_line_num = 0
greatest_val = 0
for i in range(1, len(pairs.index)):
    print(i)
    val = pairs["base"][i] ** pairs["exp"][i]
    if val > greatest_val:
        print("NEW WINNER")
        greatest_val = val
        greatest_val_line_num = i

f = open("../../git/project_euler_rkuss/euler_data/p099_base_exp.txt", "r")
x_max = '0,0'
i = 0
for x in f:
  print(x)
  i += 1
  if (int(x.split(",")[0]) ** int(x.split(",")[1])) > (int(x_max.split(",")[0]) ** int(x_max.split(",")[1])):
      x_max = x
      i_max = i



