# Project Euler
# projecteuler.net
# Becca Kuss

source('~/git/project_euler_rkuss/euler_problems_r/euler_functions.R')

# Problem 205 - Dice Game -------------------------------------------------

# Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 
# 4. Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 
# 5, 6.
# 
# Peter and Colin roll their dice and compare totals: the highest total wins. 
# The result is a draw if the totals are equal.
# 
# What is the probability that Pyramidal Pete beats Cubic Colin? Give your 
# answer rounded to seven decimal places in the form 0.abcdefg


pyramid_die <- 1:4
cubic_die <- 1:6

# Get all combinations for Peter
peters_possibilities <- data.frame(person = "Peter") %>% 
  merge(data.frame(die1 = pyramid_die)) %>% 
  merge(data.frame(die2 = pyramid_die)) %>% 
  merge(data.frame(die3 = pyramid_die)) %>% 
  merge(data.frame(die4 = pyramid_die)) %>% 
  merge(data.frame(die5 = pyramid_die)) %>% 
  merge(data.frame(die6 = pyramid_die)) %>% 
  merge(data.frame(die7 = pyramid_die)) %>% 
  merge(data.frame(die8 = pyramid_die)) %>% 
  merge(data.frame(die9 = pyramid_die)) %>% 
  tibble::rownames_to_column("roll") %>% 
  gather(die, value, starts_with ("die")) 

# Get all combinations for Colin
colins_possibilities <- data.frame(person = "Colin") %>% 
  merge(data.frame(die1 = cubic_die)) %>% 
  merge(data.frame(die2 = cubic_die)) %>% 
  merge(data.frame(die3 = cubic_die)) %>% 
  merge(data.frame(die4 = cubic_die)) %>% 
  merge(data.frame(die5 = cubic_die)) %>% 
  merge(data.frame(die6 = cubic_die)) %>% 
  tibble::rownames_to_column("roll") %>% 
  gather(die, value, starts_with ("die")) 

# Aggregate to probabilities of each score
probabilities <- peters_possibilities %>% 
  bind_rows(colins_possibilities) %>%
  # Get total for each
  group_by(person, roll) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  # Get counts of each total for each person
  group_by(person, total) %>% 
  summarise(n_rolls = n()) %>% 
  ungroup() %>% 
  # Get probabilities of each total value for each person
  group_by(person) %>% 
  mutate(prob_total = n_rolls / sum(n_rolls)) %>% 
  ungroup()

# Compare probabilities of each person
probabilities %>% 
  select(-n_rolls) %>% 
  spread(person, prob_total) %>% 
  # Get the probability that Colin is less than each total
  arrange(total) %>% 
  mutate(Colin_lt = lag(cumsum(Colin))) %>% 
  replace_na(list(Colin = 0, 
                  Colin_lt = 0,
                  Peter = 0)) %>% 
  # Find probability that Peter rolls each number & Colin rolls something less
  mutate(joint_prob = Peter * Colin_lt) %>% 
  summarise(prob = sum(joint_prob)) %>% 
  pull(prob) %>% 
  round(7)

# 0.5731441 - CORRECT!


# Problem 262 - Mountain Range --------------------------------------------


# The following equation represents the continuous topography of a mountainous 
# region, giving the elevation h at any point (x,y):
#   
# A mosquito intends to fly from A(200,200) to B(1400,1400), without leaving the 
# area given by 0 ≤ x, y ≤ 1600.
# 
# Because of the intervening mountains, it first rises straight up to a point 
# A', having elevation f. Then, while remaining at the same elevation f, it 
# flies around any obstacles until it arrives at a point B' directly above B.
# 
# First, determine fmin which is the minimum constant elevation allowing such a 
# trip from A to B, while remaining in the specified area.
# Then, find the length of the shortest path between A' and B', while flying at 
# that constant elevation fmin.
# 
# Give that length as your answer, rounded to three decimal places.
# 
# Note: For convenience, the elevation function shown above is repeated below, 
# in a form suitable for most programming languages:
#   h=( 5000-0.005*(x*x+y*y+x*y)+12.5*(x+y) ) * 
#     exp( -abs(0.000001*(x*x+y*y)-0.0015*(x+y)+0.7) )


# Create a data frame of heights
mountain_region <- data.frame(x = -200:1800) %>% 
  merge(data.frame(y = -200:1800)) %>% 
  mutate(h = (5000 - 0.005 * (x*x+y*y+x*y) + 12.5*(x+y)) * 
           exp(-abs(0.000001*(x*x+y*y) - 0.0015*(x+y) + 0.7)))

# Create a data frame of labeled points A and B
a_and_b <- data.frame(point = c('A', 'B'),
                      x = c(200, 1400),
                      y = c(200, 1400)) %>% 
  left_join(mountain_region, by = c('x', 'y')) %>% 
  mutate(label = paste0(point, " (", scales::comma(round(h)), " ft)"))

# Plot it to see what it looks like
mountain_region %>% 
  filter(x %% 10 == 0, y %% 10 == 0) %>% 
  rename(`Elevation (ft)` = h) %>% 
  ggplot() +
  aes(x = x, y = y, color = `Elevation (ft)`) +
  geom_point() +
  # Add dashed lines to bound the region the mosquito can't leave
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = 1600, linetype = 'dashed') +
  # Add points A and B
  geom_point(data = a_and_b, 
             aes(x = x, y = y), 
             color = 'black') +
  geom_text(data = a_and_b, 
            aes(x = x, y = y, label = label), 
            color = 'black',
            hjust = 1.1, vjust = 1,
            size = 3.5) +
  scale_color_continuous(labels = scales::comma) +
  xlab("") + ylab("") +
  ggtitle("A mosquito flying from A to B, \nwithout leaving the bounded area") +
  theme_bw()
# Mikey thinks it's not a very interesting mountain range...more like crater 
# lake

