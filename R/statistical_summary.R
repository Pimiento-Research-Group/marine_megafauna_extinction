library(here)
library(tidyverse)


# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

# stage data
data(stages, package = "divDyn")

# missing data ------------------------------------------------------------

dat_clean %>% 
  group_by(group) %>% 
  summarise(across(c(guild, vertical, habitat), 
                   ~ (sum(is.na(.))/nrow(dat_clean))*100)) 
  
dat_clean %>% 
  filter(group == "Non-avian reptiles") %>% 
  summarise(across(c(guild, vertical, habitat), 
                   ~ (sum(is.na(.))/nrow(dat_clean %>% 
                                           filter(group == "Non-avian reptiles")))*100)) 



# group identity ----------------------------------------------------------


dat_clean %>% 
  count(group) %>% 
  mutate(prop = (n/nrow(dat_clean))*100)


dat_clean %>% 
  count(taxonomic_rank) %>% 
  mutate(prop = (n/nrow(dat_clean))*100)


# over time ---------------------------------------------------------------

dat_clean %>% 
  count(early_era) %>% 
  mutate(prop = (n/nrow(dat_clean))*100)


dat_clean %>% 
  count(early_period) %>% 
  mutate(prop = (n/nrow(dat_clean))*100)



# missing at random -------------------------------------------------------

# Create a binary variable indicating missing data
dat_mar <- dat_clean %>% 
  select(guild, vertical, habitat) %>% 
  mutate(across(everything(), as.character))
  
dat_mar$missing <- ifelse(rowSums(is.na(dat_mar)) > 0, 1, 0)

# Perform logistic regression
model <- glm(missing ~ guild + vertical + habitat, 
             data = dat_mar,
             family = "binomial")

# calculate p-values
coefficients <- summary(model)$coefficients[, "Estimate"]
standard_errors <- summary(model)$coefficients[, "Std. Error"]
z_values <- coefficients / standard_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Compare the p-values with the significance level (e.g., 0.05)
significance_level <- 0.05
is_mar <- all(p_values > significance_level)


# ranges ------------------------------------------------------------------

dat_clean %>% 
  left_join(stages %>% 
              group_by(series) %>% 
              summarise(age_fad = mean(mid)) %>% 
              rename(early_epoch = series)) %>% 
  left_join(stages %>% 
              group_by(series) %>% 
              summarise(age_lad = mean(mid)) %>% 
              rename(late_epoch = series)) %>% 
  mutate(age_range = age_fad - age_lad) %>% 
  # summarise(mean(age_range)) %>%
  arrange(desc(age_range)) %>% 
  # 1% = 706*0.025
  slice_head(n = 17) %>% 
  # summarise(min(age_range))
  count(group)
