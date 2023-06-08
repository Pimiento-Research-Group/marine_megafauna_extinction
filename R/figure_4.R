library(here)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))



# visualise ---------------------------------------------------------------


plot_1 <- dat_clean %>% 
  group_by(clade) %>% 
  summarise(max_size = max(max_size_m)) %>% 
  left_join(dat_clean %>% 
              count(clade)) %>% 
  left_join(dat_clean %>% 
              distinct(group, clade)) %>% 
  left_join(dat_clean %>% 
              group_by(clade) %>% 
              filter(max_size_m == max(max_size_m)) %>% 
              ungroup() %>% 
              select(clade, late_era) %>% 
              distinct()) %>% 
  mutate(clade = fct_reorder(clade, interaction(n, group), 
                             .fun = sort), 
         group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(n, clade)) +
  # geom_segment(aes(xend = 0,
  #                  yend = clade),
  #              colour = "grey70") +
  geom_point(aes(size = max_size, 
                 fill = group, 
                 colour = late_era), 
             shape = 21, 
             stroke = 1) +
  scale_fill_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  labs(y = NULL, 
       x = "Taxon count", 
       size = "Maximum size [m]", 
       fill = NULL, 
       colour = NULL) +
  guides(fill = guide_legend(override.aes = list(stroke = 0, 
                                                 size = 3.5)), 
         colour = guide_legend(override.aes = list(size = 3))) +
  scale_size_continuous(breaks = c(1, 10, 20)) +
  scale_color_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  theme_classic(base_size = 12)
  
  


# save --------------------------------------------------------------------

# save plot
ggsave(plot_1, filename = here("figures",
                               "figure_4.png"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white", device = ragg::agg_png)  
