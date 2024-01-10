library(here)
library(tidyverse)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds")) 



# size per trait ----------------------------------------------------------

# visualise
plot_1 <- dat_clean %>%
  pivot_longer(cols = c(vertical, habitat, guild), 
               names_to = "eco_trait", 
               values_to = "eco_val") %>% 
  mutate(
    eco_val = factor(eco_val,
                          levels = c("Macropredator", "Micropredator", "Herbivore", 
                                     "Benthic", "Benthopelagic", "Pelagic",
                                     "Coastal", "Coastal/Oceanic", "Oceanic")),
         age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>%
  drop_na(eco_val) %>%
  ggplot(aes(age_mid, log(max_size_m), 
             fill = group)) +
  geom_vline(xintercept = c(252, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_point(shape = 21, 
             position = position_jitter(width = 10, 
                                        seed = 123),
             colour = "grey30",
             alpha = 0.3, 
             size = 3, 
             key_glyph = "rect") +
  coord_geo(xlim = c(0, 510), 
            dat = tibble(name = c("Cenozoic", "Mesozoic", "Paleozoic"), 
                         max_age = c(66, 251.9, 538.8), 
                         min_age = c(0, 66, 251.9), 
                         abbr = c("C", "M", "P")),
            pos = list("b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(8/.pt),
            lab_color = "grey20", 
            color = "grey80", 
            abbrv = list(TRUE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.5)) +
  scale_x_reverse() +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  scale_y_continuous(breaks = log(c(1, 2, 5, 10, 20)),
                     labels = c(1, 2, 5, 10, 20), 
                     limits = log(c(0.8, 30))) +
  guides(fill = guide_legend(nrow = 2,
                             byrow = TRUE,
                             override.aes = list(alpha = 1))) +
  labs(y = "Maximum body size [m]", 
       x = "Age [myr]") +
  facet_wrap(~eco_val) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top",
        legend.key.size = unit(1, "mm"))


# percentages -------------------------------------------------------------

# visualise
plot_2 <- dat_clean  %>%
  pivot_longer(cols = c(vertical, habitat, guild),
               names_to = "eco_trait",
               values_to = "eco_val") %>% 
  drop_na(eco_val) %>% 
  count(eco_trait, eco_val, group) %>% 
  group_by(eco_trait) %>% 
  mutate(n_perc = (n/sum(n))*100, 
         eco_trait = factor(eco_trait, 
                            levels = c("guild", 
                                       "vertical", 
                                       "habitat")),
         eco_val = factor(eco_val,
                          levels = c("Macropredator", "Micropredator", "Herbivore",
                                     "Benthic", "Benthopelagic", "Pelagic",
                                     "Coastal", "Coastal / Oceanic", "Oceanic"))) %>% 
  ungroup() %>% 
  ggplot(aes(n_perc, 
             eco_val, 
             fill = group)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~eco_trait, 
             scales = "free") +
  labs(y = NULL, 
       x = NULL) +
  theme_classic(base_size = 12) +
  coord_flip() +
  theme(legend.position = "none", 
        legend.key.size = unit(3, "mm"),
        strip.text = element_blank(), 
        strip.background = element_blank(), 
        axis.text.x = element_text(angle = 18,
                                   vjust = 0.9, 
                                   hjust = 0.7))



# through time ------------------------------------------------------------


plot_3 <- dat_clean %>%
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

# patch together
plot_eco <- plot_2/ 
  plot_1 / 
  plot_layout(heights = c(1,5)) +
  plot_annotation(tag_levels = "A") 

# save plot
ggsave(plot_eco, 
       filename = here("figures",
                       "figure_7.pdf"), 
       width = 183, height = 180,
       units = "mm", 
       bg = "white") 


