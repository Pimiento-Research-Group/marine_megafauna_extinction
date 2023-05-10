library(here)
library(tidyverse)
library(divDyn)


# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

# stage information
data("stages")

# table with keys to link stage information
data("keys")
  

# get tax names
tax_names <- dat_clean %>% 
  distinct(taxa) %>% 
  pull(taxa)


# get pbdb data -----------------------------------------------------------

# # set up function
# get_pbdb_url <- function(taxon){
#   params <- paste(
#     # select group
#     paste("base_name=",taxon, sep = ""),
#     # only return occurrences identified to at least genus
#     # level and lump multiple occurrences from same collection into a single occurrence
#     # "idreso=lump_genus",
#     # only return extinct genera
#     "extant=no",
#     # classext=taxonomic information, with taxon numbers;
#     # ident=individual components of the taxonomic identification
#     # coords=location of coordination
#     "show=classext,ident,coords",
#     # use scotese atlas for plate reconstructions
#     # "pgm=scotese",
#     sep="&")
# 
#   # get url
#   uri <- paste("https://paleobiodb.org/data1.2/occs/list.tsv?", params, sep="")
# 
#   uri
# }
# 
# # get urls, adding "%20" instead of white space in the species names
# # resolves the API problem
# url_list <- get_pbdb_url(str_replace(tax_names, " ", "%20"))
# 
# # download data on genus level
# pbdb_data_raw <- map(url_list, ~read_tsv(file = .x,
#                                      quote = "",
#                                      show_col_types = FALSE),
#                      .progress = TRUE)
# 
# # save download
# write_rds(pbdb_data_raw, here("data",
#                               "output",
#                               "pbdb_data_raw.rds"),
#           compress = "gz")

# read in download
pbdb_data_raw <- read_rds(here("data",
                               "output",
                               "pbdb_data_raw.rds"))

 
# remove those where no entries where found, based on length of returned entries
pbdb_data <- pbdb_data_raw[map(pbdb_data_raw, ncol) > 2]

# same for where we get a message for no found entries
pbdb_data <- pbdb_data[map(pbdb_data, 
                           ~ .x %>% 
                             select(1) %>% 
                             pull()) != "THIS REQUEST RETURNED NO RECORDS"]


# set up character with weird symbols in the pbdb list that need to be removed
symb_list <- c("\\?", "sp\\.", "cf\\.", "n\\.", 
               "aff", "gen\\.", "informal", "J sensu Ristedt", 
               "A sensu Ristedt", "B sensu Ristedt", 
               "K sensu Ristedt", "H sensu Ristedt", 
               "ex gr\\.", "\\.", " sp\\.", 
               "spp\\.", '\"', "[:digit:]", " \\s*\\([^\\)]+\\)") %>% 
  paste0(., collapse = "|")

# clean data
pbdb_data <- pbdb_data %>% 
  map(~ .x %>% 
        # get rid of occ that are not genus or species
        filter(accepted_rank %in% c('genus', 'species')) %>%
        # remove weird symbols
        mutate(accepted_name_clean = str_remove_all(accepted_name, symb_list),
               # remove white space and start and end and squish double white
               # spaces in the middle
               accepted_name_clean = str_squish(accepted_name_clean))) 


# identify synonyms -------------------------------------------------------

# if synonyms get lumped to the name that we have in our database, it's fine, 
# however the other way around is a problem and we need to find these cases

# get the used reasons for updating the entered name
reason_list <- pbdb_data %>% 
  map(~ unique(.x$difference)) %>% 
  unlist() %>% 
  unique()

# filter out those cases where the name was changed because of some kind of
# synonym
synon_names <- pbdb_data %>% 
  map_df(~ .x %>% 
        filter(difference %in% reason_list[-c(1,3)]) %>%
        select(identified_name, 
               accepted_name_clean)) %>% 
  distinct(identified_name, 
           accepted_name_clean) %>% 
  # remove weird symbols
  mutate(identified_name_clean = str_remove_all(identified_name, symb_list), 
         # remove white space and start and end and squish double white
         # spaces in the middle
         identified_name_clean = str_squish(identified_name_clean)) %>% 
  # remove the cases where it was simply assigned to genus level
  filter(accepted_name_clean != word(identified_name_clean, 1)) %>% 
  select(-identified_name)


# get vector with accepted names
pbdbd_names <- pbdb_data %>% 
  map(~ unique(.x$accepted_name_clean)) %>% 
  unlist() 
  
# add the synonyms 
pbdbd_names <- c(pbdbd_names, pull(synon_names,
                                   identified_name_clean))



# compare occurrences -----------------------------------------------------


# calculate how many megafauna taxa have occurrences if we do it on genus level
# genus level
genus_level <- word(tax_names, 1)[word(tax_names, 1) %in% word(pbdbd_names, 1)] %>% 
  length() # 474 genera have at least one occurrence in pbdb

# what's the percentage
genus_level/ length(tax_names) # approximately 76% / length(tax_names) # approximately 75%


# how many genus only, i.e. removing those taxa that could be resolved to species level
genus_level_only <- tax_names[str_count(tax_names, "\\S+")==1][tax_names[str_count(tax_names, "\\S+")==1] %in% pbdbd_names] %>% 
  length() # 68 genera

# percentage for genus only
genus_level_only / length(tax_names[str_count(tax_names, "\\S+")==1]) # approximately 62 percent


# same for species level
species_level <- tax_names[str_count(tax_names, "\\S+")==2][tax_names[str_count(tax_names, "\\S+")==2] %in% pbdbd_names] %>% 
  length() # 333 species have at least one occurrence in pbdb

# percentage
species_level / length(tax_names[str_count(tax_names, "\\S+")==2]) # approximately 62%



# merge dataset -----------------------------------------------------------


# make sure that datasets have same column types for merging 
dat_pbdb <- pbdb_data %>% 
  map(~ .x %>% 
        mutate(family_no = as.numeric(family_no), 
               order_no = as.numeric(order_no), 
               class_no = as.numeric(class_no), 
               genus = as.character(genus), 
               primary_reso = as.character(primary_reso), 
               subgenus_name = as.character(subgenus_name), 
               species_reso = as.character(species_reso), 
               subgenus_reso = as.character(subgenus_reso))) %>% 
  # create one dataframe from the list of pbdb occurrences
  bind_rows()



# occurrences per taxon ---------------------------------------------------


# calculate occurrences of the accepted names for species 
# first, identify all synonyms of the taxa in the database, and assign the 
# accepted name to these
tax_names_clean <- tax_names %>% 
  as_tibble_col(column_name = "taxon") %>% 
  mutate(identified_name_clean = if_else(taxon %in% synon_names$identified_name_clean, 
                               taxon, NA_character_)) %>% 
  left_join(synon_names) %>% 
  distinct() %>% 
  mutate(taxon_clean = if_else(is.na(accepted_name_clean), 
                               taxon, 
                               accepted_name_clean)) 

# calculate the number of occurrences
nr_occ <- map_int(tax_names_clean$taxon_clean,
        ~ filter(dat_pbdb,
                 accepted_name_clean == .x) %>%
          nrow())  

dat_occ <- tibble(taxon = tax_names_clean$taxon_clean,
                  occurrences = nr_occ, 
                  synonym = tax_names_clean$identified_name_clean) 

dat_occ %>% 
  drop_na(synonym)

# how many species have more than one occurrence
dat_occ %>% 
  filter(str_count(taxon, "\\S+")==2) %>% 
  filter(occurrences > 1) %>% 
  nrow() /  dat_occ %>% filter(str_count(taxon, "\\S+")==2) %>% nrow() 
# 150 out of 493

# how many true genera have more than one occurrence
dat_occ %>% 
  filter(str_count(taxon, "\\S+")==1) %>% 
  filter(occurrences > 1) %>% 
  nrow() /  dat_occ %>% filter(str_count(taxon, "\\S+")==1) %>% nrow()
# 55 out of 108

# visualize it for species level
dat_occ %>% 
  filter(str_count(taxon, "\\S+")==2) %>% 
  ggplot(aes(occurrences)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(breaks = c(0, 5, 10, 20, 50, 100)) +
  labs(x = "Total occurrences", 
       y = "Number of species") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

  

# maybe it's better to use a log-scale here
dat_occ %>% 
  filter(str_count(taxon, "\\S+")==2) %>% 
  mutate(log_occ = log1p(occurrences)) %>% 
  ggplot(aes(log_occ)) +
  geom_histogram(binwidth = 0.1) +
  scale_y_continuous(breaks = c(0, 5, 10, 20, 50, 100)) +
  scale_x_continuous(breaks = c(0, seq(log1p(1), log1p(300), 
                                       by = log1p(1))), 
                     labels = c(0, expm1(seq(log1p(1), log1p(300), 
                                       by = log1p(1))))) +
  labs(x = "Total occurrences", 
       y = "Number of species") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# FAD and LAD -------------------------------------------------------------

# for each megafauna species, get the FAD and the LAD based on PBDB data
dat_pbdb_fad_lad <- dat_pbdb %>% 
  filter(accepted_name_clean %in% tax_names_clean$taxon_clean) %>% 
  # if late interval is empty,
  # it means that early interval is sufficient to assign age estimates
  mutate(late_interval = if_else(is.na(late_interval), 
                            early_interval, 
                            late_interval)) %>% 
  # get bin numbers based on look-up table
  mutate(early_bin = categorize(early_interval, keys$stgInt), 
         late_bin = categorize(late_interval, keys$stgInt), 
         across(c(early_bin, late_bin), as.numeric)) %>% 
  # get age estimates based on bin names
  # for early age
  full_join(stages %>% 
              as_tibble() %>% 
              select(early_age = mid,
                     early_bin = stg)) %>% 
  # for late age
  full_join(stages %>% 
              as_tibble() %>% 
              select(late_age = mid,
                     late_bin = stg)) %>% 
  group_by(accepted_name_clean) %>% 
  summarise(fad = max(late_age), 
            lad = min(early_age)) %>% 
  rename(taxon = accepted_name_clean, 
         lad_pbdb = lad, 
         fad_pbdb = fad) %>% 
  drop_na(lad_pbdb, fad_pbdb)


# repeat but based on entries in the megafauna database
dat_megafauna_fad_lad <- dat_clean %>% 
  # get correct names
  full_join(tax_names_clean %>% 
              select(taxa = taxon, 
                     taxon_clean)) %>% 
  select(taxon = taxon_clean,
         lad = age_late_stage, 
         fad = age_early_stage) %>% 
  drop_na(fad, lad)


# combine and compare
dat_age_diff <- full_join(dat_pbdb_fad_lad, 
                          dat_megafauna_fad_lad) %>% 
  mutate(LAD = lad_pbdb - lad, 
         FAD = fad_pbdb - fad) %>% 
  select(taxon, LAD, FAD) 

# how many taxa are having exactly the same estimates
dat_age_diff %>% 
  filter(LAD == 0 & FAD == 0)
# 195, actually not that bad

# general distribution
dat_age_diff %>% 
  pivot_longer(cols = c(LAD, FAD)) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = 0.5, 
               colour = "grey80") +
  labs(x = "Age difference in myr", 
       y = NULL, 
       subtitle = "Difference between the age estimates for the\nFAD and LAD of the PBDB data and the megafauna data") +
  scale_y_continuous(breaks = NULL) +
  scale_fill_discrete(name = "Difference") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_line(colour = "grey95"))



# check pbdb extensions ---------------------------------------------------

# check where the FAD from the PBDB is older 
# than the FAD from the megafauna database
dat_age_diff %>% 
  filter(FAD > 0) 
# this is the case for 19 taxa

# check where the LAD from the PBDB is younger 
# than the FAD from the megafauna database
dat_age_diff %>% 
  filter(LAD < 0) 
# this is the case for 27 taxa
