# Empirical Bayes with MLB batting averages 
## Basic workflow for data analysis:
## 1. Wrangling batting data 
## 2. Estimate prior distribution 
## 3. Compute Bayes-adjusted averages
## 4. Visualize 

# install.packages(c("tidyverse", "Lahman", "fitdistrplus"))
library(tidyverse)
library(Lahman)

# Data
career_stats <- Batting %>% 
  filter(AB > 0) %>% 
  anti_join(Pitching, by = "playerID") %>% # filters anyone who appears in the Lahman::Pitching data
  group_by(playerID) %>% 
  summarise(H = sum(H), 
            AB = sum(AB),
            n = sum(AB)) %>% 
  mutate(average = H / AB)

career_stats

career <- People %>% 
  tibble::as_tibble() %>% # coerce to tibble
  select(playerID, nameFirst, nameLast) %>% 
  unite(name, nameFirst, nameLast, sep = " ", na.rm = TRUE) %>% # combine firstName and lastName with a space, call this column "name"
  inner_join(career_stats, by = "playerID") %>% # join the summary object above to career object. Now we have names and summary stats
  select(-playerID)

career

# empirical Bayes estimation 
career_filtered <- career %>% 
  filter(AB >= 500) 

beta_fit <- fitdistrplus::fitdist(
  data = career_filtered$average,
  distr = "beta",
  method = "mle",
  start = list(shape1 = 1, shape2 = 10)
)

alpha0 <- beta_fit$estimate["shape1"]
beta0 <- beta_fit$estimate["shape2"]

# plot it 
beta_curve <- tibble(
  average = seq(
    from = min(career_filtered$average),
    to = max(career_filtered$average),
    length.out = 500
  ),
  density = dbeta(
    average,
    shape1 = alpha0, 
    shape2 = beta0
  )
)

career_filtered %>% 
  ggplot(aes(x = average)) +
  geom_histogram(aes(y = after_stat(density)), # put observed average on density scale, not raw count 
                 bins = 30) +
  geom_line(
    data = beta_curve,
    aes(x = average, y = density), 
    linewidth = 1, 
    color = "#0072B2" # colorblind-friendly blue; comes from the Okabe-Ito palette (we will use these later)
  ) +
  labs(
    title = "Career batting averages with fitted Beta distribution",
    subtitle = "Minimum 500 ABs",
    x = "career batting average",
    y = "density"
  )

ggsave("beta_distr.png", # defaults to most recent plot
       path = "output", # since "output" is in my project directory i can save it there directly
       width = 8, 
       height = 5) 

# continuing empirical Bayes, use fitted prior to estimate adjusted average 
career_eb <- career %>% 
  mutate(
    prior_mean = alpha0 / (alpha0 + beta0),
    alpha_post = alpha0 + H,
    beta_post = beta0 + AB - H, # AB - H = failures (an at bat without a hit)
    eb_average = alpha_post / (alpha_post + beta_post) # combines overall prior estimate and each player's own record
  )
  
# If we look at all players and only players with at least 500 ABs, we can see
# how empirical Bayes pulls estimates toward the prior mean! 
career_eb %>% 
  select(name, H, AB, average, eb_average) %>% 
  arrange(AB) %>% 
  head()
  
career_eb %>% 
  filter(AB >= 500) %>% 
  select(name, H, AB, average, eb_average) %>% 
  arrange(AB) %>% 
  head()

# plot the shrinkage 
career_eb %>% 
  mutate(adjustment = eb_average - average,
         direction = if_else(adjustment >= 0, "Adjusted upward", "Adjusted downward")) %>% 
  ggplot(aes(x = AB, y = adjustment, color = direction)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Adjusted upward" = "#0072B2",
      "Adjusted downward" ="#D55E00"
    )
  ) +
  labs(
    title = "Empirical Bayes adjustment as sample size increases",
    subtitle = "Variation decreases as sample size increases",
    x = "At bats, log scale",
    y = "Empirical Bayes average minus career average",
    color = NULL
  ) +
    theme_minimal()

ggsave("eb_shrinkage.png", 
       path = "output",
       width = 8, 
       height = 5) 

## let's bump it up to 10 to see this pattern if we drop the AB = 1 cases
career_eb %>% 
  filter(AB >= 10) %>% 
  mutate(adjustment = eb_average - average,
         direction = if_else(adjustment >= 0, "Adjusted upward", "Adjusted downward")) %>% 
  ggplot(aes(x = AB, y = adjustment, color = direction)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Adjusted upward" = "#0072B2",
      "Adjusted downward" ="#D55E00"
    )
  ) +
  labs(
    title = "Empirical Bayes adjustment as sample size increases",
    subtitle = "Variation decreases as sample size increases",
    x = "At bats, log scale",
    y = "Empirical Bayes average minus career average",
    color = NULL
  ) +
  theme_minimal()
  
ggsave("eb_shrinkage_10.png", 
       path = "output",
       width = 8, 
       height = 5) 