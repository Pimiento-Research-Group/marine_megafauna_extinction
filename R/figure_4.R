library(here)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))



# visualise ---------------------------------------------------------------


dat_plot_1 <- dat_clean %>% 
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
                                   "Mammal")))

# visualise
plot_1 <- dat_plot_1 %>%
  ggplot(aes(n, clade)) +
  geom_segment(aes(xend = 0,
                   yend = clade),
               colour = "grey70") +
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
  scale_size_continuous(breaks = c(1, 10, 20), 
                        range = c(1, 6)) +
  scale_color_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  theme_classic(base_size = 12)
  
  

# occurrence per era ------------------------------------------------------

plot_2 <- dat_clean %>%
  group_by(early_era, clade) %>% 
  summarise(max_size = max(max_size_m)) %>%
  ungroup() %>% 
  complete(clade, early_era, 
           fill = list(max_size = 0)) %>% 
  left_join(dat_clean %>% 
              distinct(group, clade)) %>% 
  mutate(clade = factor(clade, 
                        levels = levels(dat_plot_1$clade), 
                        ordered = TRUE), 
         group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>%
  ggplot(aes(early_era, clade,
             size = max_size, 
             fill = group, 
             colour = early_era)) +
  geom_point(shape = 21, 
             alpha = 0.8, 
             stroke = 1) +
  scale_colour_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  scale_fill_brewer(type = "qual",
                    palette = 2,
                    name = NULL) +
  scale_size_continuous(range = c(-1, 6)) +
  labs(x = "Occurrence", 
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid= element_blank(), 
        axis.text = element_blank(),
        legend.position = "none")


# save --------------------------------------------------------------------


# patch together
plot_final <- plot_1 +
  plot_2 +
  plot_layout(widths = c(6, 1), 
              guides = "collect") +
  plot_annotation(tag_levels = "A") 

  
  

# save plot
ggsave(plot_final, filename = here("figures",
                               "figure_4.png"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white", device = ragg::agg_png)  
