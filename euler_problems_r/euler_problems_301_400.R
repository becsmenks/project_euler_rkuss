# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')


# Problem 335 - Gathering the beans ---------------------------------------


# Whenever Peter feels bored, he places some bowls, containing one bean each, in 
# a circle. After this, he takes all the beans out of a certain bowl and drops 
# them one by one in the bowls going clockwise. He repeats this, starting from 
# the bowl he dropped the last bean in, until the initial situation appears 
# again. For example with 5 bowls he acts as follows:
#   
# So with 5 bowls it takes Peter 15 moves to return to the initial situation.
# 
# Let M(x) represent the number of moves required to return to the initial 
# situation, starting with x bowls. Thus, M(5) = 15. It can also be verified 
# that M(100) = 10920.
# 
# Find SUM(k=0..10^18) M(2^k + 1). Give your answer modulo 7^9.

# Start by playing some small games of mancala
mancala_soln <- data.frame()
for (x in 5:50) {
  # Initialize starting condition, one bean in each bowl
  bowls <- data.frame(bowl = 0:(x - 1),
                      n_beans = 1)
  
  # Initialize control variables
  back_to_start <- FALSE
  bowl_in_play <- 0
  n_plays <- 0
  
  # Play mancala
  while (!back_to_start & n_plays < 2000) {
    
    # Pick up all the beans from the bowl in play
    beans_in_play <- bowls %>% 
      filter(bowl == bowl_in_play) %>% 
      pull(n_beans)
    
    # Find which bowls you'll drop the beans into
    subsequent_bowls <- (bowl_in_play + 1):(bowl_in_play + beans_in_play) %% x
    
    # Drop the beans into all subsequent bowls
    bowls <- bowls %>% 
      mutate(n_beans = if_else(bowl == bowl_in_play, 0, n_beans),
             n_beans = if_else(bowl %in% subsequent_bowls, n_beans + 1, n_beans))
    
    # Check if you're back to the initial conditions
    back_to_start <- all(bowls$n_beans == 1)
    
    # Reset the bowl in play
    bowl_in_play <- (bowl_in_play + beans_in_play) %% x
    
    # Increment the play counter
    n_plays <- n_plays + 1
    
  }
  
  mancala_soln <- bind_rows(mancala_soln,
                            data.frame(x = x,
                                       mx = n_plays,
                                       success = back_to_start))
}

# Now try and come up with closed form solution
mancala_soln_test <- mancala_soln %>% 
  filter(success) %>% 
  select(-success) %>% 
  mutate(ratio = mx / x,
         ratio_round = floor(ratio / 10)) %>% 
  group_by(ratio_round) %>% 
  mutate(n_ratio = n())




# Problem 345 - Matrix Sum ------------------------------------------------

# We define the Matrix Sum of a matrix as the maximum possible sum of matrix 
# elements such that none of the selected elements share the same row or column.
# 
# For example, the Matrix Sum of the matrix below equals 3315 ( = 863 + 383 + 
# 343 + 959 + 767):
#   
#   7  53 183 439 863
# 497 383 563  79 973
# 287  63 343 169 583
# 627 343 773 959 943
# 767 473 103 699 303
# 
# Find the Matrix Sum of:

mat <- matrix(c(
    7,  53, 183, 439, 863,
  497, 383, 563,  79, 973,
  287,  63, 343, 169, 583,
  627, 343, 773, 959, 943,
  767, 473, 103, 699, 303
),
nrow = 5,
byrow = T)

mat <- matrix(c(
  7,  53, 183, 439, 863, 497, 383, 563,  79, 973, 287,  63, 343, 169, 583,
  627, 343, 773, 959, 943, 767, 473, 103, 699, 303, 957, 703, 583, 639, 913,
  447, 283, 463,  29,  23, 487, 463, 993, 119, 883, 327, 493, 423, 159, 743,
  217, 623,   3, 399, 853, 407, 103, 983,  89, 463, 290, 516, 212, 462, 350,
  960, 376, 682, 962, 300, 780, 486, 502, 912, 800, 250, 346, 172, 812, 350,
  870, 456, 192, 162, 593, 473, 915,  45, 989, 873, 823, 965, 425, 329, 803,
  973, 965, 905, 919, 133, 673, 665, 235, 509, 613, 673, 815, 165, 992, 326,
  322, 148, 972, 962, 286, 255, 941, 541, 265, 323, 925, 281, 601,  95, 973,
  445, 721,  11, 525, 473,  65, 511, 164, 138, 672,  18, 428, 154, 448, 848,
  414, 456, 310, 312, 798, 104, 566, 520, 302, 248, 694, 976, 430, 392, 198,
  184, 829, 373, 181, 631, 101, 969, 613, 840, 740, 778, 458, 284, 760, 390,
  821, 461, 843, 513,  17, 901, 711, 993, 293, 157, 274,  94, 192, 156, 574,
  34, 124,   4, 878 ,450, 476, 712, 914, 838, 669, 875, 299, 823, 329, 699,
  815, 559, 813, 459, 522, 788, 168, 586, 966, 232, 308, 833, 251, 631, 107,
  813, 883, 451, 509, 615,  77, 281, 613, 459, 205, 380, 274, 302,  35 ,805
),
nrow = 15,
byrow = T)

# Set up data frame with decision variables and their values
mat_df <- data.frame(mat) %>% 
  tibble::rownames_to_column("row") %>% 
  gather(col, value, starts_with("X")) %>% 
  mutate(col = str_remove(col, "X"),
         d_var = paste0(row, "-", col),
         row = as.numeric(row),
         col = as.numeric(col)) %>% 
  arrange(d_var) 

# Create objective function coefficients
obj_coeff <- mat_df %>% 
  pull(value)

# Constraint 1 - sum of all rows is exactly 1
constr1_df <- mat_df %>% 
  mutate(constr_id = row,
         constr_val = 1,
         dir = "=",
         rhs = 1) %>% 
  select(d_var, dir, rhs, constr_id, constr_val) %>% 
  spread(d_var, constr_val) %>% 
  replace(is.na(.), 0)

# Constraint 2 - sum of all columns is exactly 1
constr2_df <- mat_df %>% 
  mutate(constr_id = col,
         constr_val = 1,
         dir = "=",
         rhs = 1) %>% 
  select(d_var, dir, rhs, constr_id, constr_val) %>% 
  spread(d_var, constr_val) %>% 
  replace(is.na(.), 0)

# Combine constraint data frames
constr_df <- bind_rows(constr1_df,
                       constr2_df)

# Create constraint matrix, direction, and rhs
constr_mat <- constr_df %>% 
  select(all_of(mat_df$d_var)) %>% 
  as.matrix()

constr_dir <- constr_df %>% 
  pull(dir)

constr_rhs <- constr_df %>% 
  pull(rhs)

# Solve the linear program
soln <- lp(direction = "max",
           objective.in = obj_coeff,
           const.mat = constr_mat,
           const.dir = constr_dir,
           const.rhs = constr_rhs,
           all.bin = T)

soln$objval

# 13938 - CORRECT!






