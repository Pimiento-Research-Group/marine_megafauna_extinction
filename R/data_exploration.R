library(here)
library(tidyverse)

age_colors <- c("#A3BC99", "#6CC0DB", "#F2F91E")

# read data ---------------------------------------------------------------

dat_clean_ages <- read_rds(here("data",
                                "input",
                                "megafauna_clean.rds"))



# exploration -------------------------------------------------------------


# how many taxa do we have per group?
dat_clean_ages %>%
  group_by(group) %>%
  summarise(taxa_count = n_distinct(taxa),
            taxa_prop = (n_distinct(taxa)/nrow(.))*100)


# per era 
dat_clean_ages %>%
  group_by(late_era) %>%
  summarise(taxa = n_distinct(taxa))


# per taxonomic rank 
dat_clean_ages %>%
  group_by(taxonomic_rank ) %>%
  summarise(taxa= n_distinct(taxa))


# count taxa per group (and Era)
dat_clean_ages %>%
  ggplot(aes(group)) +
  geom_bar(aes(fill = late_era)) +
  scale_color_manual(values = age_colors, 
                     name = "Era") +
  scale_fill_manual(values = age_colors, 
                    name = "Era") +
  ggtitle("Marine megafaunal taxa") +
  theme(legend.position = c(0.8, 0.8))


# count taxa per clade 
dat_clean_ages %>%
  count(clade) %>% 
  left_join(dat_clean_ages %>% 
              distinct(group, clade)) %>% 
  mutate(clade = fct_reorder(clade, n)) %>% 
  ggplot(aes(n, clade)) +
  geom_col(aes(fill = group)) +
  labs(fill = "Group", 
       x = "Count", 
       y = "Clade") +
  ggtitle("Marine megafaunal taxa") +
  theme(legend.position = c(0.8, 0.3))

# count taxa per Era
dat_clean_ages %>% 
  ggplot(aes(late_era)) +
  geom_bar(aes(fill = late_era)) +
  scale_color_manual(values = age_colors)+
  scale_fill_manual(values = age_colors) + 
  theme(legend.position = "none") +
  ggtitle("Marine megafaunal taxa")
