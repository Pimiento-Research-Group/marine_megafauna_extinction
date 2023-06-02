library(here)
library(Compadre)
library(tidyverse)


# load data ---------------------------------------------------------------


# megafauna data
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

# get taxon names
spec_names <- dat_clean %>% 
  # select only species
  filter(taxonomic_rank == "species") %>% 
  mutate(taxa = as.character(taxa)) %>% 
  pull(taxa)


# stage information
data("stages", 
     package = "divDyn")

# load the raw pbdb download
pbdb_data_raw <- read_rds(here("data",
                               "output",
                               "pbdb_data_size.rds"))

# remove those where no entries where found, based on length of returned entries
pbdb_data <- pbdb_data_raw[map(pbdb_data_raw, ncol) > 2] %>% 
  # same for where we get a message for no found entries
  .[map(.,
        ~ .x %>%
          select(1) %>%
          pull()) != "THIS REQUEST RETURNED NO RECORDS"] %>% 
  # select only relevant columns
  map(~ select(.x, accepted_name, genus,  
               max_ma, min_ma)) %>% 
  # combine lists into one dataframe
  bind_rows()


# bin to stages -----------------------------------------------------------


# clean up for binning
dat_stages <- stages %>% 
  as_tibble() %>% 
  select(stg, bottom, top)

# first for pbdb data
dat_pbdb_binned <- pbdb_data %>%
  # bin to stages
  mutate(bin_max = 95 - cut(max_ma, breaks = dat_stages$bottom,
                            include.lowest = TRUE,
                            labels = FALSE),
         bin_min = 96 - cut(min_ma, breaks = dat_stages$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  # remove species with too much uncertainty
  drop_na(bin_max, bin_min) %>% 
  filter(bin_max == bin_min) %>% 
  # # remove singletons
  distinct(accepted_name, genus, bin = bin_max) %>%
  # group_by(accepted_name) %>% 
  # mutate(n_occ = n()) %>% 
  # ungroup %>% 
  # filter(n_occ > 1) %>% 
  # add group id
  mutate(group_id = if_else(accepted_name %in% spec_names, 
                            "megafauna", 
                            "baseline")) %>% 
  # add group
  mutate(genus = word(accepted_name, 1)) %>% 
  left_join(dat_clean %>%
              filter(taxonomic_rank == "species") %>%
              distinct(group, genus) %>%
              mutate_if(is.factor, as.character)) %>% 
  drop_na(group) %>% 
  select(-c(genus))




# create absence presence matrix ---------------------------------------------------

dat_abs_pres <- dat_pbdb_binned %>%
  expand_grid(bin_t = seq(min(bin), max(bin))) %>% 
  mutate(pres_abs = if_else(bin == bin_t, 
                            1, 0)) %>% 
  select(-c(bin, group)) %>% 
  pivot_wider(names_from = bin_t, values_from = pres_abs, 
              values_fn = mean) %>% 
  mutate(across(-c(accepted_name, group_id), 
                ~ if_else(.x >0, 1, 0))) %>% 
  group_by(group_id) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(data = map(data, 
                    ~ .x %>% 
                      column_to_rownames(var = "accepted_name") %>% 
                      as.matrix()))
  
# get interval durations
int_dur <- dat_pbdb_binned %>% 
  summarise(min(bin), max(bin)) %>% 
  {{stages$dur[.[[1, 1]]:.[[1, 2]]]}}


# fit models --------------------------------------------------------------

# time averaged model
dat_mod <- dat_abs_pres %>% 
  mutate(mod_setup = map(.x = data,
                         ~ make_BayesCMR(.x, int_dur)), 
         mod_av = map(mod_setup, 
                      MCMC_CMR))
dat_mod %>% 
  mutate(samp_inx = map(mod_av, summary))

plot(dat_mod$mod_av[[1]])
plot(dat_mod$mod_av[[2]])

# extract samples
dat_samples <- dat_mod %>% 
  mutate(samp_inx = map(mod_av, pluck, "Chain"),  
         samp_inx = map(samp_inx, 
                        ~.x[, 3]), 
         samp_inx = map(samp_inx,
                        exp)) %>% 
  unnest(samp_inx) %>% 
  select(group_id, samp_inx)

# visualise
dat_samples %>% 
  write_rds(here("data", 
                 "output",
                 "cmr_data.rds"))
