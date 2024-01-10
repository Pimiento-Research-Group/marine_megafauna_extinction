library(here)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds")) %>%
  # modify
  mutate(across(c(vertical, habitat, guild),
                as.character)) %>% 
  replace_na(list(guild = "Missing",
                  vertical = "Missing",
                  habitat = "Missing")) 






# visualise ---------------------------------------------------------------


plot_1 <- dat_clean %>% 
  mutate(guild = factor(guild, 
                        levels = c("Macropredator", 
                                   "Micropredator", 
                                   "Herbivore", 
                                   "Missing"))) %>% 
  ggplot(aes(guild, 
             fill = group)) +
  geom_bar() +
  facet_wrap(~early_era) +
  labs(y = "Taxon count", 
       x = NULL, 
       title = "Guild") +
  scale_fill_manual(values = c("#1e728eff",
                                 "#ffbc3cff",
                                 "darkorange", 
                                 "coral3",
                                 "#5d7a64ff",
                                 "#ad6d8aff",
                                 "#6d3f2fff",
                                 "#f9938eff"),
                      name = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 20, 
                                   hjust = 0.5,
                                   vjust = 0.6), 
        legend.position = "none", 
        plot.title = element_text(size = 12),
        strip.background = element_rect(fill = "grey95", 
                                        colour = "grey95"))


plot_2 <- dat_clean %>%
  mutate(vertical = factor(vertical,
                           levels = c("Benthic",
                                      "Benthopelagic",
                                      "Pelagic",
                                      "Missing"))) %>%
  ggplot(aes(vertical, 
             fill = group)) +
  geom_bar() +
  facet_wrap(~early_era) +
  labs(y = "Taxon count", 
       x = NULL, 
       title = "Vertical position") +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 20, 
                                   hjust = 0.5,
                                   vjust = 0.6), 
        legend.position = "none", 
        plot.title = element_text(size = 12),
        strip.background = element_rect(fill = "grey95", 
                                        colour = "grey95"))

plot_3 <- dat_clean %>%
  mutate(habitat = factor(habitat,
                          levels = c("Coastal",
                                     "Coastal/Oceanic",
                                     "Oceanic",
                                     "Missing"))) %>%
  ggplot(aes(habitat, 
             fill = group)) +
  geom_bar() +
  facet_wrap(~early_era) +
  labs(y = "Taxon count", 
       x = NULL, 
       title = "Habitat") +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 20, 
                                   hjust = 0.5,
                                   vjust = 0.6), 
        legend.position = "bottom", 
        plot.title = element_text(size = 12),
        strip.background = element_rect(fill = "grey95", 
                                        colour = "grey95"), 
        legend.key.size = unit(3, "mm"))


# save plots --------------------------------------------------------------

# patch together
plot_eco <- plot_1 / 
  plot_2 /
  plot_3 +
  plot_annotation(tag_levels = "A")

# save plot
ggsave(plot_eco, 
       filename = here("figures",
                       "figure_5.pdf"), 
       width = 183, height = 200,
       units = "mm", 
       bg = "white") 


