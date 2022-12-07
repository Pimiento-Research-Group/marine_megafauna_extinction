library(here)
library(tidyverse)


# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_raw <- list.files(path = here("data",
                                  "input"),
                      pattern = "*.csv",
                      full.names = TRUE) %>% 
  map_df(~read_csv(.x, 
                   show_col_types = FALSE))



# data processing ---------------------------------------------------------

# some simple cleaning steps
dat_clean <- dat_raw %>% 
  drop_na(Max_size_m) %>% 
  filter(Max_size_m >= 1) %>% 
  filter(Taxonomic_rank != "family")

# get tax names
tax_names <- dat_clean %>% 
  distinct(Taxa) %>% 
  pull(Taxa)




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
#     "show=classext,ident",
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
#                                      show_col_types = FALSE))
# 
# # save download
# write_rds(pbdb_data_raw, here("data",
#                               "output",
#                               "pbdb_data_raw.rds"))

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
word(tax_names, 1)[word(tax_names, 1) %in% word(pbdbd_names, 1)] %>% 
  length() # 346 genera have at least one occurrence in pbdb

# what's the percentage
word(tax_names, 1)[word(tax_names, 1) %in% word(pbdbd_names, 1)] %>% 
  length() / length(tax_names) # approximately 75%


# same for species level
tax_names[str_count(tax_names, "\\S+")==2][tax_names[str_count(tax_names, "\\S+")==2] %in% pbdbd_names] %>% 
  length() # 217 species have at least one occurrence in pbdb

tax_names[str_count(tax_names, "\\S+")==2][tax_names[str_count(tax_names, "\\S+")==2] %in% pbdbd_names] %>% 
  length() / length(tax_names[str_count(tax_names, "\\S+")==2]) # approximately 60%


