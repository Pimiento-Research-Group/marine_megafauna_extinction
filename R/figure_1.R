library(here)
library(tidyverse)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

data(stages, package = "divDyn")

# overall distribution ----------------------------------------------------


plot_1 <- dat_clean %>%
  mutate(log_max = log(max_size_m)) %>% 
  count(group, log_max) %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(log_max, n, 
             colour = group)) +
  geom_line() +
  geom_point(alpha = 0.3) +
  labs(y = "Taxa count", 
       x = "Maximum body size [m]") +
  scale_colour_brewer(type = "qual", 
                    palette = 2, 
                    name = NULL) +
  scale_x_continuous(breaks = log(c(1, 2, 5, 10, 20)), 
                     labels = c(1, 2, 5, 10, 20)) +
  theme_classic(base_size = 12) +
  guides(colour = guide_legend(nrow = 2,
                               byrow = TRUE)) +
  theme(legend.position = c(0.6, 0.8))



# over time ---------------------------------------------------------------


plot_2 <- dat_clean %>%
  mutate(log_max = log(max_size_m), 
         group = factor(group,
                        levels = c("Invert",
                                   "Fish",
                                   "Chondrichthyes",
                                   "Reptile",
                                   "Bird",
                                   "Mammal"))) %>% 
  # plot at the mid points of epochs
  mutate(age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>% 
  ggplot(aes(x = age_mid, 
             y = log_max, 
             fill = group)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  stat_smooth(aes(colour = group), 
              geom = "line", 
              method = "lm", 
              se = FALSE, 
              alpha = 0.6, 
              linewidth = 0.5) +
  geom_point(aes(fill = group), 
             position = position_jitter(height = 0.1,
                                        seed = 123),
             alpha = 0.4,
             size = 2.5,
             shape = 21, 
             colour = "grey30") +
  scale_fill_brewer(type = "qual",
                    palette = 2,
                    name = NULL) +
  scale_colour_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  scale_y_continuous(breaks = log(c(1, 2, 5, 10, 20)),
                     labels = c(1, 2, 5, 10, 20)) +
  labs(y = "Maximum body size [m]", 
       x = "Age [myr]") +
  coord_geo(xlim = c(0, 510), 
            dat = list("periods", "eras"),
            pos = list("b", "b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(7/.pt, 10/.pt),
            lab_color = "grey20", 
            color = "grey20", 
            abbrv = list(TRUE, FALSE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.4, 0.5)) +
  scale_x_reverse() +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") 


# ranges ------------------------------------------------------------------

# visualise
plot_3 <- dat_clean %>%
  mutate(taxa = fct_reorder(taxa, age_early_epoch)) %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal"))) %>% 
  ggplot(aes(y = taxa, 
             colour = group)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_linerange(aes(xmin = age_early_epoch, 
                     xmax = age_late_epoch), 
                 position = position_dodge2(width = 1), 
                 linewidth = 1, 
                 alpha = 0.5) +
  labs(y = "Taxon ranges", 
       x = "Age [myr]") +
  scale_colour_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  scale_y_discrete(limits = rev, 
                   expand = expansion(mult = c(0.2, 0))) +
  coord_geo(xlim = c(0, 520), 
            dat = list("periods", "eras"),
            pos = list("b", "b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(7/.pt, 10/.pt),
            lab_color = "grey20", 
            color = "grey20", 
            abbrv = list(TRUE, FALSE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.4, 0.5)) +
  scale_x_reverse() +
  theme_classic(base_size = 12) +
  guides(colour = guide_legend(nrow = 2,
                               byrow = TRUE)) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())



# fads and lads over time -------------------------------------------------

plot_4 <- dat_clean %>%
  mutate(early_epoch = factor(early_epoch, levels = stages %>% 
                                distinct(series) %>% 
                                pull)) %>% 
  count(early_epoch, name = "FAD") %>% 
  mutate(FAD = FAD/sum(FAD) * 100) %>% 
  full_join(dat_clean %>% 
              mutate(late_epoch = factor(late_epoch, levels = stages %>% 
                                           distinct(series) %>% 
                                           pull)) %>% 
              count(late_epoch, name = "LAD") %>% 
              rename(early_epoch = late_epoch)) %>% 
  drop_na() %>% 
  mutate(LAD = LAD/sum(LAD) * 100) %>% 
  rename(epoch = early_epoch) %>% 
  # add age
  left_join(stages %>% 
              group_by(epoch = series) %>% 
              summarise(mid_age = min(top))) %>% 
  pivot_longer(cols = c(LAD, FAD)) %>% 
  ggplot(aes(mid_age, value, 
             fill = name)) + 
  geom_col(position = position_dodge(), 
           alpha = 0.5, 
           width = 10) +
  scale_fill_manual(values = c("#4A7971",
                               "#CB8387"),
                    name = NULL) +
  scale_x_reverse() +
  scale_y_continuous(breaks = c(0, 10, 20), 
                     labels = paste0(c(0, 10, 20), "%"), 
                     position = "right") +
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 8) +
  labs(y = NULL, 
       x = NULL) +
  theme(legend.position = c(0.97, 1.5), 
        legend.key.size = unit(2, "mm"), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(),
        legend.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA))



# patch together ----------------------------------------------------------

plot_first <- plot_1 /
  plot_2 / 
  (plot_3 +
     inset_element(plot_4, 0.055, 0, 1, 0.3, 
                   ignore_tag = TRUE)) +
  plot_annotation(tag_levels = "A")

# save plot
ggsave(plot_first, filename = here("figures",
                                  "figure_1.png"), 
       width = 183, height = 200,
       units = "mm", 
       bg = "white", device = ragg::agg_png)     


