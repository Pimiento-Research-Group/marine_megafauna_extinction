library(googlesheets4)
library(here)
library(tidyverse)



# read data ---------------------------------------------------------------


# read in data as a list
dat_raw <- 1:5 %>% 
  map(.f = ~ read_sheet("https://docs.google.com/spreadsheets/d/1YgEGDw0m5iaIJcpJufv7AZ0Rnw355Lxbeq4M4HHX7Z4/edit?usp=sharing",
                        sheet = .x)) %>% 
  map(~ mutate(.x, Max_size_m = map(Max_size_m, as.integer)) %>% 
        unnest(Max_size_m)) %>% 
  bind_rows()


# read in stage data
data(stages, 
     package = "divDyn")


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


  


# add ages by series ------------------------------------------------------


# get weird stage entries
late_stage_lookup <- dat_clean %>% 
  left_join(stages %>% 
              group_by(series) %>% 
              summarise(late_age = max(top)) %>% 
              rename(late_epoch = series)) %>% 
  filter(is.na(late_age)) %>% 
  distinct(late_epoch) %>% 
  drop_na() %>% 
  add_column(corrected_late_epoch = c("Maolingian", 
                                      "Lower Cretaceous", 
                                      "Lower Triassic"))


# same for early stages
early_stage_lookup <- dat_clean %>% 
  left_join(stages %>% 
              group_by(series) %>% 
              summarise(early_age = min(bottom)) %>% 
              rename(early_epoch = series)) %>% 
  filter(is.na(early_age)) %>% 
  distinct(early_epoch) %>% 
  drop_na() %>% 
  add_column(corrected_early_epoch = c("Maolingian", 
                                      "Lower Triassic")) 


# add ages
dat_clean_ages <- dat_clean %>% 
  left_join(late_stage_lookup) %>% 
  left_join(early_stage_lookup) %>% 
  mutate(late_epoch = if_else(!is.na(corrected_late_epoch), 
                              corrected_late_epoch, 
                              late_epoch)) %>% 
  left_join(stages %>% 
              group_by(late_epoch = series) %>% 
              summarise(age_late_epoch = max(top))) %>% 
  mutate(early_epoch = if_else(!is.na(corrected_early_epoch), 
                              corrected_early_epoch, 
                              early_epoch)) %>% 
  left_join(stages %>% 
              group_by(early_epoch = series) %>% 
              summarise(age_early_epoch = min(bottom)))


# save data ---------------------------------------------------------------

# as rds file
dat_clean_ages %>% 
  write_rds(here("data", 
                 "input", 
                 "megafauna_clean.rds"))

