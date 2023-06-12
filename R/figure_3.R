library(here)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

data(stages, package = "divDyn")



# ecology per group -------------------------------------------------------

plot_1 <- dat_clean %>%
  count(group, vertical, habitat, guild) %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal")), 
         across(c(vertical, habitat, guild),
                as.character)) %>% 
  replace_na(list(vertical = "Missing", 
                  habitat = "Missing", 
                  guild = "Missing")) %>% 
  mutate(across(c(vertical, habitat, guild), 
                ~ fct_relevel(.x, 
                              "Missing", 
                              after = 3)), 
         guild = fct_relevel(guild, 
                             "Micropredator", 
                             after = 1)) %>% 
  ggplot(aes(vertical, habitat, 
             shape = guild, 
             size = n, 
             colour = group)) +
  geom_point() +
  scale_shape_manual(values = c(0, 1, 2, 4)) +
  scale_size_continuous(range = c(1, 9), 
                        breaks = c(1, 10, 20, 30)) +
  scale_colour_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  scale_y_discrete(expand = expansion(add = c(1, 1))) +
  labs(x = NULL, 
       y = NULL, 
       size = NULL, 
       shape = NULL) +
  guides(colour = "none",
         size = guide_legend(
           override.aes = list(shape = 3))) +
  facet_wrap(~ group) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", 
        legend.spacing.x = unit(1, 'mm'), 
        axis.text.x = element_text(angle = 18,
                                   vjust = 0.98,
                                   hjust = 0.85), 
        strip.background = element_rect(linewidth = 1), 
        panel.grid.major = element_line(colour = "grey95"))



# and through time --------------------------------------------------------


plot_2 <- dat_clean %>%
  count(early_era, vertical, habitat, guild) %>% 
  mutate(across(c(vertical, habitat, guild),
         as.character)) %>% 
  replace_na(list(vertical = "Missing", 
                  habitat = "Missing", 
                  guild = "Missing")) %>% 
  mutate(across(c(vertical, habitat, guild), 
                ~ fct_relevel(.x, 
                              "Missing", 
                              after = 3)), 
         guild = fct_relevel(guild, 
                             "Micropredator", 
                             after = 1)) %>% 
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
  guides(colour = "none") +
  facet_wrap(~ early_era) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 18,
                                   vjust = 0.98,
                                   hjust = 0.85), 
        strip.background = element_rect(linewidth = 1), 
        panel.grid.major = element_line(colour = "grey95"))



# # per group through time --------------------------------------------------
# 
# dat_clean %>% 
#   group_by(early_era, group) %>% 
#   count(vertical) %>% 
#   drop_na()  %>% 
#   mutate(group = factor(group, 
#                         levels = c("Invert", 
#                                    "Fish", 
#                                    "Chondrichthyes", 
#                                    "Reptile", 
#                                    "Bird", 
#                                    "Mammal"))) %>% 
#   ggplot(aes(vertical, n, 
#              fill = group, 
#              group = early_era)) +
#   # geom_point() +
#   # geom_line() +
#   geom_point(position = position_dodge(width = 0.4), 
#              shape = 21, 
#              size = 3, 
#              stroke = 1, 
#              alpha = 0.8) +
#   scale_fill_brewer(type = "qual",
#                     palette = 2,
#                     name = NULL) +
#   scale_color_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
#   facet_wrap(~early_era) +
#   theme_classic() 
# 
# 



# save final plot ---------------------------------------------------------


# patch together
plot_final <- plot_1 / plot_2 +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(tag_levels = "A")

# save plot
ggsave(plot_final, filename = here("figures",
                                    "figure_3.png"), 
       width = 183, height = 180,
       units = "mm", 
       bg = "white", device = ragg::agg_png)  


dat_clean %>%
  count(group, vertical, habitat, guild) %>% 
  # drop_na() %>% 
  filter(group == "Reptile")
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(vertical, habitat, 
             shape = guild, 
             size = n, 
             colour = group)) +
  geom_point() +
  scale_shape_manual(values = c(0, 1, 2)) +
  scale_size_continuous(range = c(1, 9), 
                        breaks = c(1, 10, 20, 30)) +
  scale_colour_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  labs(x = NULL, 
       y = NULL, 
       size = NULL, 
       shape = NULL) +
  guides(colour = "none",
         size = guide_legend(
           override.aes = list(shape = 21))) +
  facet_wrap(~ interaction(group, early_era)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", 
        legend.spacing.x = unit(1, 'mm'), 
        axis.text.x = element_text(angle = 18,
                                   vjust = 0.98,
                                   hjust = 0.85), 
        strip.background = element_rect(linewidth = 1), 
        panel.grid.major = element_line(colour = "grey95"))
