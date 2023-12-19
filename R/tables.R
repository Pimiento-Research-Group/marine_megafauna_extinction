library(here)
library(tidyverse)
library(flextable)
library(officer)


# load data ---------------------------------------------------------------

dat_lad <- read_rds(here("data", 
                         "output", 
                         "fad_and_lad_per_system.rds"))


# create tables -----------------------------------------------------------

dat_tbl <- dat_lad %>%
  select(group, early_period, nr_FAD, nr_LAD) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  # set column names
  compose(j = 1, part = "header", 
          value = as_paragraph("Taxon")) %>%
  compose(j = 2, part = "header", 
          value = as_paragraph("Period")) %>%
  compose(j = 3, part = "header", 
          value = as_paragraph("#FADs")) %>% 
  compose(j = 4, part = "header", 
          value = as_paragraph("#LADs")) %>% 
  merge_v(j = c("group")) %>% 
  fix_border_issues()

# create word document ----------------------------------------------------

# open docx-file and add flextable
my_doc <- read_docx() %>% 
  body_add_flextable(dat_tbl) %>% 
  body_add_break() 

# convert to word file/ add input to empty docx
print(my_doc, target = here("data",
                            "output", 
                            "tables.docx"))

