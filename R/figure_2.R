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



# ranges ------------------------------------------------------------------

# visualise
plot_1 <- dat_clean %>%
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
                   expand = expansion(mult = c(0.08, 0))) +
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
  theme(legend.position = c(0.3, 0.8), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())



# fads and lads over time -------------------------------------------------

plot_2 <- dat_clean %>%
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
              summarise(mid_age = mean(mid))) %>% 
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
  theme(legend.position = c(0.05, 0.6), 
        legend.key.size = unit(2, "mm"), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA))


# patch together ----------------------------------------------------------


plot_second <- plot_1 +
  inset_element(plot_2, 0.05, 0, 1, 0.3)
  
# save plot
ggsave(plot_second, filename = here("figures",
                                   "figure_2.png"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white", device = ragg::agg_png)     
