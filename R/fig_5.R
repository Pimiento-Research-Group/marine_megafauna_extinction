library(here)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds")) %>% 
  mutate(across(c(guild, 
                  vertical, 
                  habitat), 
                ~ as.character(.x) %>% 
                  str_to_sentence())) %>% 
  mutate(guild = str_replace_all(guild, 
                                 "Macropredators", 
                                 "Macropredator"), 
         guild = str_replace_all(guild, 
                                 "Generalist", 
                                 "Macropredator"),
         vertical = str_replace_all(vertical,
                                    "Deep-diver",
                                    "Pelagic"), 
         vertical = str_replace_all(vertical,
                                    "Nearshore",
                                    "Benthopelagic")) 

data(stages, package = "divDyn")




# visualise ---------------------------------------------------------------


plot_eco <- dat_clean %>%
  count(early_era, vertical, habitat, guild) %>% 
  mutate(across(c(vertical, habitat, guild),
                as.character)) %>% 
  replace_na(list(guild = "Missing", 
                  vertical = "Missing", 
                  habitat = "Missing")) %>% 
  mutate(across(c(vertical, habitat, guild), 
                ~ fct_relevel(.x, "Missing",
                              after = Inf))) %>% 
  ggplot(aes(vertical, habitat, 
             shape = guild, 
             size = n, 
             colour = early_era)) +
  geom_point() +
  scale_shape_manual(values = c(0, 1, 2, 4)) +
  scale_colour_manual(values = rev(c(colorspace::darken("#fcea10", 0.1),
                                     "#5dc5ea", "#a9c6a9"))) +
  labs(x = NULL, 
       y = NULL, 
       size = NULL, 
       shape = NULL) +
  guides(colour = "none",
         size = guide_legend(nrow = 1,
                             override.aes = list(shape = 3))) +
  scale_size_continuous(name = "#Taxa", 
                        range = c(1, 9), 
                        breaks = c(1, 15, 30)) +
  facet_wrap(~ early_era) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", 
        legend.box="vertical", 
        legend.margin = margin(),
        legend.title = element_text(size = 10), 
        axis.text.x = element_text(angle = 35,
                                   vjust = 0.98,
                                   hjust = 0.85), 
        axis.text.y = element_text(angle = 35,
                                   vjust = 1,
                                   hjust = 0.7),
        strip.background = element_rect(linewidth = 1), 
        panel.grid.major = element_line(colour = "grey95"))

# save plots --------------------------------------------------------------


# save plot
ggsave(plot_eco, 
       filename = here("figures",
                       "figure_5.pdf"), 
       width = 183, height = 90,
       units = "mm", 
       bg = "white") 


