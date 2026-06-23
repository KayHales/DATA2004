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
            AB = sum(AB)) %>% 
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

# Now we will work on understanding version control using Github by expanding on this project. 
# When we push this script, you can see that I added these changes. This helps you (and collaborators and people who find your repo)
# keep track of everything! 

# To continue this example, let's think about our problem. Above we used empirical Bayes 
# to improve our estimation of each player's batting average. However, if we learn more about baseball 
# we learn something important: When players are better, they are given more chances to bat. 

# In our above code, we gave all players the same prior. However, now we will use a method called 
# a beta-binomal regression (don't worry about this, you don't need to understand this).
# This allows us to model this same problem, but now all batters have their own prior. 
library(tidyverse)
library(Lahman)

# Let's start by looking at an example to demonstrate this. 
career %>% 
  filter(AB >= 20) %>% 
  ggplot(aes(AB, average)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  theme_minimal() 

# There is a linear relationship! Yes, batters with low ABs have a higher variance. 
# However, as we can see, as ABs increase so does batting average. Managers give more 
# ABs to better hitters! 

pitchers <- Pitching %>% 
  group_by(playerID) %>% 
  summarise(gamesPitched = sum(G)) %>% 
  filter(gamesPitched > 3)

# We're using the career, career_stats, and career_filtered we used above 

# Estimate hyperparameters
m <- fitdistrplus::fitdist(
  data = career_filtered$average,
  distr = "beta",
  method = "mle",
  start = list(shape1 = 1, shape2 = 10
))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]
prior_mu <- alpha0 / (alpha0 + beta0)

# Create new career_eb  
career_eb <- career %>% 
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>% 
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0) %>% 
  arrange(desc(eb_estimate))

# So let's try to resolve this problem by modeling it out 
# install.packages("gamlss")
library(gamlss)

fit <- gamlss(cbind(H, AB - H) ~ log(AB), 
              data = career_eb, 
              family = BB(mu.link = "identity"))

mu <- fitted(fit, parameter = "mu")
sigma <- fitted(fit, parameter = "sigma")

head(mu)
head(sigma)

# Now we can calculate our parameters for each player rathern than pooling them all together! 
career_eb_wAB <- career_eb %>% 
  select(name, H, AB, original_eb = eb_estimate) %>% 
  mutate(
    mu = mu, 
    alpha0 = mu / sigma,
    beta0 = (1 - mu) / sigma,
    alpha1 = alpha0 + H,
    beta1 = beta0 + AB - H,
    new_eb = alpha1 / (alpha1 + beta1)
  )

career_eb_wAB %>% 
  ggplot(aes(original_eb, new_eb, color = AB)) +
  geom_point() + 
  geom_abline(color = "#D55E00") +
  scale_color_continuous(trans = "log", breaks = 10 ^ (0:4)) +
  labs(
    title = "Does this change our estimates?",
    x = "Original",
    y = "New"
  )

# Finally, let's compare the estimation methods
career_eb_wAB %>%
  filter(AB >= 10) %>%
  mutate(raw = H / AB) %>%
  gather(type, value, raw, original_eb, new_eb) %>%
  mutate(mu = ifelse(type == "original_eb", prior_mu,
                     ifelse(type == "new_eb", mu, NA))) %>%
  ggplot(aes(AB, value)) +
  geom_point() +
  geom_line(aes(y = mu), color = "#009E73") +
  scale_x_log10() +
  facet_wrap(~type) 