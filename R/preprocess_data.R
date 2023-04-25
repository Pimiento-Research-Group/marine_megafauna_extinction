library(googlesheets4)
library(here)
library(tidyverse)



# read data ---------------------------------------------------------------


# read in data as a list
dat_raw <- 1:5 %>% 
  map(.f = ~ read_sheet("https://docs.google.com/spreadsheets/d/1YgEGDw0m5iaIJcpJufv7AZ0Rnw355Lxbeq4M4HHX7Z4/edit?usp=sharing",
                        sheet = .x)) %>% 
  map(~ mutate(.x, Max_size_m = as.integer(Max_size_m))) %>% 
  bind_rows()


# read in stage data
data(stages, package = "divDyn")


# clean up ----------------------------------------------------------------

# reformat 
dat_clean <- dat_raw %>% 
  rename_with(tolower) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(group = fct_relevel(group, c("Invert", "Fish", "Chondrichthyes", "Reptile", "Bird", "Mammal"))) %>%
  mutate(early_era = fct_relevel(early_era,
                                 c("Paleozoic", "Mesozoic", "Cenozoic"))) %>%
  mutate(late_era = fct_relevel(late_era,
                                c("Paleozoic", "Mesozoic", "Cenozoic"))) %>%
  mutate(late_period = fct_relevel(late_period,
                                   c("Cambrian", "Ordovician", "Silurian", "Devonian",
                                     "Carboniferous", "Permian", "Triassic", "Jurassic",
                                     "Cretaceous", "Paleogene", "Neogene", "Quaternary"))) %>%
  mutate(guild = fct_relevel(guild, c("Herbivore","Micropredator","Macropredator"))) %>%
  mutate(vertical = fct_relevel(vertical, c("Benthic","Benthopelagic","Pelagic"))) %>%
  mutate(habitat = fct_relevel(habitat, c("Coastal", "Coastal/Oceanic","Oceanic"))) %>%
  filter(max_size_m >= 1) %>% # the students had reasons to include some <1m taxa; only 8 taxa where below 1m
  mutate(size_cat = cut(max_size_m, 
                        breaks = c(0, 5, 10, 15, 21),
                        labels = c("<5m", "5-10m", "10-15m", ">15m"))) %>% 
  separate_wider_delim(taxa,
                       delim = " ",
                       names = c("genus", "species"),
                       cols_remove = FALSE, 
                       too_few = "align_start")


  


# add ages by stages ------------------------------------------------------


# get weird stage entries
late_stage_lookup <- dat_clean %>% 
  left_join(stages %>% 
              select(late_stage = stage, 
                     age_late_stage = mid)) %>% 
  filter(is.na(age_late_stage)) %>% 
  distinct(late_stage) %>% 
  drop_na() %>% 
  add_column(corrected_late_stage = c("Eifelian", "Upper Miocene", 
                                 "Lower Miocene", "Middle Miocene", 
                                 "Pliocene", "Upper Miocene", 
                                 "Pleistocene", "Chattian", 
                                 "Middle Miocene", "Hauterivian", 
                                 "Selandian-Thanetian", "Lower Miocene", 
                                 "Selandian-Thanetian", "Pliocene", 
                                 "Pliocene", "Pleistocene"))

# same for early stages
early_stage_lookup <- dat_clean %>%
  left_join(stages %>% 
              select(early_stage = stage, 
                     age_early_stage = mid)) %>% 
  filter(is.na(age_early_stage)) %>% 
  distinct(early_stage) %>% 
  drop_na() %>% 
  left_join(late_stage_lookup %>% 
              rename(early_stage = late_stage, 
                     corrected_stage = corrected_late_stage)) %>% 
  left_join(tibble(early_stage = c("Eilfelian", "Early Eocene", 
                                   "Early Miocene", "Late Miocene"), 
                   corrected_stage2 = c("Eifelian", "Ypresian", 
                                       "Lower Miocene", "Upper Miocene"))) %>% 
  mutate(corrected_stage = if_else(is.na(corrected_stage), 
                                   corrected_stage2, 
                                   corrected_stage)) %>% 
  select(-corrected_stage2, corrected_early_stage = corrected_stage)

# add ages
dat_clean_ages <- dat_clean %>% 
  left_join(late_stage_lookup) %>% 
  left_join(early_stage_lookup) %>% 
  mutate(late_stage = if_else(!is.na(corrected_late_stage), 
                              corrected_late_stage, 
                              late_stage)) %>% 
  left_join(stages %>% 
              select(late_stage = stage, 
                     age_late_stage = mid)) %>% 
  mutate(early_stage = if_else(!is.na(corrected_early_stage), 
                              corrected_early_stage, 
                              early_stage)) %>% 
  left_join(stages %>% 
              select(early_stage = stage, 
                     age_early_stage = mid)) 


# save data ---------------------------------------------------------------

# as rds file
dat_clean_ages %>% 
  write_rds(here("data", 
                 "input", 
                 "megafauna_clean.rds"))

