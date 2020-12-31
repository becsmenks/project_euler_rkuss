# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_functions.R')


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




