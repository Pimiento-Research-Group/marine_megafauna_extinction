plot_eco <- dat_clean %>% 
  group_by(group, early_era) %>% 
  count(vertical) %>% 
  drop_na()  %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(x = vertical, 
             y = n)) +
  coord_polar(theta = 'x') +
  geom_point(aes(fill = group, 
                 group = 1), 
             shape = 21, 
             colour = "grey30", 
             size = 3, 
             alpha = 0.8) +
  scale_fill_brewer(type = "qual",
                    palette = 2,
                    name = NULL) +
  facet_grid(early_era ~ group) +
  labs(y = NULL, 
       x = NULL) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none", 
        panel.grid.major = element_line(), 
        axis.line = element_blank())

# save plot
ggsave(plot_eco, filename = here("figures",
                                   "figure_eco.png"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white", device = ragg::agg_png) 
