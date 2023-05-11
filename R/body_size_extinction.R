library(here)
library(tidyverse)
library(divDyn)
library(brms)
library(tidybayes)

# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

# stage information
data("stages")

# table with keys to link stage information
data("keys")



# reformat data -----------------------------------------------------------

dat_clean_sp <- dat_clean %>% 
  # select only species
  filter(taxonomic_rank == "species") %>% 
  # that can be resolved to stage level 
  drop_na(age_early_stage, age_late_stage)


# get taxon names
tax_names <- dat_clean_sp %>% 
  mutate(genus = as.character(genus)) %>% 
  pull(genus)


# pbdb_download -----------------------------------------------------------

# set up function
get_pbdb_url <- function(taxon){
  params <- paste(
    # select group
    paste("base_name=",taxon, sep = ""),
    # only return occurrences identified to at least genus
    # level and lump multiple occurrences from same collection into a single occurrence
    "idreso=lump_genus",
    # only return extinct genera
    "extant=no",
    # classext=taxonomic information, with taxon numbers;
    # ident=individual components of the taxonomic identification
    "show=classext,ident",
    sep="&")
  
  # get url
  uri <- paste("https://paleobiodb.org/data1.2/occs/list.tsv?", params, sep="")
  
  uri
}

# get urls
url_list <- get_pbdb_url(tax_names)


# download data on genus level
pbdb_data_raw <- map(url_list, ~read_tsv(file = .x,
                                         quote = "",
                                         show_col_types = FALSE), 
                     .progress = TRUE)

# remove those where no entries where found, based on length of returned entries
pbdb_data <- pbdb_data_raw[map(pbdb_data_raw, ncol) > 2] %>% 
  # same for where we get a message for no found entries
  .[map(.,
        ~ .x %>%
          select(1) %>%
          pull()) != "THIS REQUEST RETURNED NO RECORDS"] %>% 
  # only lumped genus level
  map(~ filter(.x, 
               accepted_rank %in% c('genus', 'species'))) %>% 
  # select only relevant columns
  map(~ select(.x, accepted_name, accepted_rank, genus,  
               max_ma, min_ma)) %>% 
  # combine lists into one dataframe
  bind_rows()

# save the raw download
pbdb_data_raw %>% 
  write_rds(here("data", 
                 "output", 
                 "pbdb_data_size.rds"), 
            compress = "gz")

# bin to stages -----------------------------------------------------------


# clean up for binning
dat_stages <- stages %>% 
  as_tibble() %>% 
  select(stg, bottom, top)

# first for pbdb data
dat_pbdb_binned <- pbdb_data %>% 
  # bin fad and lad to stages
  mutate(bin_ori = 95 - cut(max_ma, breaks = dat_stages$bottom,
                          include.lowest = TRUE,
                          labels = FALSE),
         bin_ext = 96 - cut(min_ma, breaks = dat_stages$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_ori, bin_ext) %>% 
  group_by(genus) %>% 
  summarise(bin_ori = min(bin_ori), 
            bin_ext = max(bin_ext)) %>% 
  ungroup() %>% 
  # fill in duration bins
  mutate(bin_occ = map2(.x = bin_ori, 
                        .y = bin_ext,
                        .f = ~ seq(.x, .y, by = 1))) %>% 
  select(genus, bin_occ, bin_ext) %>% 
  unnest(bin_occ) %>% 
  # create extinction signal
  group_by(genus) %>% 
  mutate(ext_signal = if_else(bin_occ == bin_ext, 1, 0)) %>% 
  ungroup() %>% 
  add_column(group_id = "baseline")

# same for megafauna data
dat_megafauna_binned <- dat_clean_sp %>% 
  mutate(taxa = as.character(taxa)) %>% 
  select(taxa,
         age_early_stage,
         age_late_stage) %>% 
  # bin fad and lad to stages
  mutate(bin_ori = 95 - cut(age_early_stage, breaks = dat_stages$bottom,
                            include.lowest = TRUE,
                            labels = FALSE),
         bin_ext = 96 - cut(age_late_stage, breaks = dat_stages$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_ori, bin_ext) %>% 
  # fill in duration bins
  mutate(bin_occ = map2(.x = bin_ori, 
                        .y = bin_ext,
                        .f = ~ seq(.x, .y, by = 1))) %>% 
  select(taxa, bin_occ, bin_ext) %>% 
  unnest(bin_occ) %>% 
  # create extinction signal
  group_by(taxa) %>% 
  mutate(ext_signal = if_else(bin_occ == bin_ext, 1, 0)) %>% 
  ungroup() %>% 
  add_column(group_id = "megafauna")



# combine and merge -------------------------------------------------------


# merge
dat_merged <- dat_pbdb_binned %>%
  # clean up unresolved genus names
  filter(genus %in% (dat_clean_sp %>%
                       distinct(genus) %>%
                       pull(genus))) %>% 
  rename(taxa = genus) %>% 
  # add group_id
  left_join(dat_clean_sp %>% 
              distinct(group, taxa = genus) %>% 
              mutate_if(is.factor, as.character)) %>% 
  # bind with megafauna data
  bind_rows(dat_megafauna_binned %>% 
              left_join(dat_clean_sp %>%
                          select(group, taxa, max_size_m))) 

# quick look
for (i in unique(dat_merged$group)) {
  
  print(i)

  dat_merged %>% 
    filter(group == i) %>%
    glm(data = .,
        formula = ext_signal ~ group_id,
        family = "binomial") %>% 
    summary() %>% 
    print()
  
}




# model comparison --------------------------------------------------------


# fit hierarchical mixed effect model
mod_1 <- brm(formula = ext_signal ~ group_id + (group_id | group),
             family = bernoulli,
             data = dat_merged,
             seed = 1511,
             control = list(adapt_delta = 0.95),
             chains = 4,
             cores = 4,
             threads = threading(4),
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr")

# save model 
mod_1 %>% 
  write_rds(here("models", 
                 "risk_change_model.rds"), 
            compress = "gz")


# set up dataframe for plotting
plot_comp <- tibble(group_id = unique(dat_merged$group_id)) %>%
  expand_grid(group = unique(dat_merged$group)) %>% 
  add_epred_draws(mod_1, 
                  ndraws = 1000) %>% 
  group_by(group_id) %>% 
  median_qi(.epred) %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(group_id, .epred, 
             colour = group)) +
  geom_linerange(aes(ymin = .lower, 
                      ymax = .upper), 
                  position = position_dodge(width = 0.3), 
                 linewidth = 1) +
  geom_point(aes(fill = group), 
             colour = "grey20", 
             position = position_dodge(width = 0.3), 
             shape = 21, 
             size = 4) +
  scale_y_continuous(name = "Extinction Risk [%]", 
                     breaks = seq(0, 1, by = 0.2), 
                     labels = seq(0, 1, by = 0.2)*100, 
                     limit = c(0, 1)) + 
  scale_x_discrete(name = NULL, 
                   labels = c("Baseline", "Megafauna")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), 
         fill = guide_legend(nrow = 2, byrow = TRUE, 
                             override.aes = list(size = 2))) +
  scale_color_brewer(type = "qual", 
                     palette = 2) +
  scale_fill_brewer(type = "qual", 
                     palette = 2) +
  labs(colour = NULL, 
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = c(0.3, 0.85), 
        legend.background = element_rect(colour = "grey30", 
                                         linewidth = 0.3))


# save plot
ggsave(plot_comp, filename = here("figures",
                                  "baseline_vs_megafauna.png"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white", device = ragg::agg_png)


# model temporal ----------------------------------------------------------


# fit hierarchical mixed effect model
mod_2 <- brm(formula = ext_signal ~ group_id:bin_occ + (group_id:bin_occ | group),
             family = bernoulli,
             data = dat_merged,
             seed = 1511,
             control = list(adapt_delta = 0.95),
             chains = 4,
             cores = 4,
             threads = threading(4),
             iter = 5000,
             warmup = 1000,
             backend = "cmdstanr")

# save model 
mod_2 %>% 
  write_rds(here("models", 
                 "logit_model.rds"), 
            compress = "gz")


# set up grid
dat_logit <- dat_merged %>%
  distinct(group_id, group, bin_occ) %>% 
  # add draws from the posterior
  add_linpred_draws(mod_2,
                    ndraws = 1000) %>% 
  ungroup() %>% 
  select(group_id, group, bin_occ, .linpred) %>% 
  # reformat
  pivot_wider(names_from = group_id, 
              values_from = .linpred, 
              values_fn = list) %>% 
  mutate(megafauna_draws = map_dbl(megafauna, length)) %>% 
  filter(megafauna_draws != 0) %>%
  # calculate contrasts
  mutate(logit = map2(.x = baseline, 
                      .y = megafauna, 
                      ~ .x - .y), 
         # summarise
         logit_smr = map(logit, median_qi)) %>% 
  select(group, bin_occ, logit_smr, logit) %>% 
  unnest(logit) %>% 
  group_by(group, bin_occ) %>% 
  mutate(.draw = 1:n(), 
         mean_logit = mean(logit)) %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal")))  

# visualize
plot_logit <- dat_logit %>%
  ggplot(aes(bin_occ, logit)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = group, 
                group = paste(group, .draw)), 
            alpha = 0.01) +
  geom_line(aes(y = mean_logit, 
                group = group), 
            linewidth = 1.2, 
            colour = "white", 
            alpha = 0.6) +
  geom_line(aes(y = mean_logit, 
                colour = group), 
            linewidth = 0.8) +
  annotate(geom = "curve",
           x = 9, xend = 9,
           y = 0.1, yend = 1.3,
           curvature = 0,
           colour = "grey70",
           arrow = arrow(length = unit(.2,"cm"))) +
  annotate(geom = "label", 
           x = 9, y = 2, label = "Higher risk\nfor baseline", 
           colour = "grey30", 
           size = 8/.pt, 
           label.size = 0) +
  annotate(geom = "curve",
           x = 9, xend = 9,
           y = -0.1, yend = -1.3,
           curvature = 0,
           colour = "grey70",
           arrow = arrow(length = unit(.2,"cm"))) +
  annotate(geom = "label", 
           x = 9, y = -2, label = "Higher risk\nfor megafauna", 
           colour = "grey30", 
           size = 8/.pt, 
           label.size = 0) +
  scale_x_continuous(breaks = c(4, 51, 81, 95), 
                     labels =  round(stages$mid[c(4, 51, 81, 95)], 0), 
                     name = "Age [myr]", 
                     limits = c(4, 95)) +
  scale_color_brewer(type = "qual", 
                     palette = 2) +
  labs(colour = NULL, 
       y = "Extinction Selectivity [logit]") +
  theme_minimal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1), 
                               nrow = 1)) +
  ylim(c(-6.2, 2)) +
  theme(legend.position = "top")


# save plot
ggsave(plot_logit, filename = here("figures",
                                   "logit_through_time.png"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white", device = ragg::agg_png)



