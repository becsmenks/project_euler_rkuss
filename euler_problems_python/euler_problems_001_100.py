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

i = 11990
n_div = 0
while (n_div < 500) & (i < 12000):

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

n = 9
keep_going = True
while keep_going and n < 100:
    print(n)
    n = n + 2
    if n in primes:
        continue
    j = 1
    while keep_going and j < math.sqrt(n):
        print(j)
        c = n - 2 * (j ** 2)
        if c > 0:
            keep_going = is_prime(c, primes)
        else:
            keep_going = True
        print(keep_going)
        j = j + 1

# Problem 48 - Self powers -----------------------------------------------

# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
#
# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

total = 0
for i in range(1,1000):
    total = total + (i ** i)

# 9110846700 - CORRECT!