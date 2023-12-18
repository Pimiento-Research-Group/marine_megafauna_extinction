library(here)
library(tidyverse)


# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))


# missing data ------------------------------------------------------------

dat_clean %>% 
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
