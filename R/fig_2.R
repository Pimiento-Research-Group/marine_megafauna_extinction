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
  scale_color_manual(values = c("#1e728eff",
                                "#ffbc3cff",
                                "#FF8454", 
                                "coral3",
                                "#5d7a64ff",
                                "#ad6d8aff",
                                "#6d3f2fff",
                                "#f9938eff"),
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
                               byrow = TRUE, 
                               override.aes = list(alpha = 0.9))) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.key.size = unit(3, "mm"), 
        legend.text = element_text(size = 10, 
                                   colour = "grey30"))



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
  geom_col(position = position_dodge(width = 5), 
           # alpha = 0.5, 
           width = 7) +
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




# taxa counts -------------------------------------------------------------


# count taxa per group (and Era)
plot_3 <- dat_clean %>%
  count(group, late_era) %>% 
  ggplot(aes(n, group)) +
  geom_segment(aes(xend = 0,
                   yend = group, 
                   colour = group),
               position = position_dodge2(width = 0.9), 
               alpha = 0.5, 
               linewidth = 0.4) +
  geom_point(aes(colour = group, 
                 fill = late_era), 
             size = 5, 
             shape = 21,
             position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  scale_colour_brewer(type = "qual",
                      palette = 2,
                      name = NULL, 
                      guide = "none") +
  scale_y_discrete(expand = expansion(add = c(1, 1))) +
  guides(fill = guide_legend(override.aes = list(size = 3, 
                                                 stroke = 0))) +
  labs(y = NULL, 
       x = "Taxon count", 
       fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 8, 
                                   margin = margin(r = 0, unit = "pt")))



# alternative for b -------------------------------------------------------

plot_4 <- dat_clean %>%
  ggplot(aes(group)) +
  geom_bar(aes(fill = late_era), 
           # position = position_dodge(), 
           width = 0.8) +
  scale_color_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  scale_fill_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  labs(y = "Taxon count", 
       x = NULL, 
       fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top", 
        legend.key.size = unit(2, "mm"), 
        axis.text.x = element_text(size = 10, 
                                   angle = 15, 
                                   hjust = 0.5,
                                   vjust = 0.6))


# ranges per group --------------------------------------------------------

plot_5 <- dat_clean %>%
  group_by(group) %>%
  summarise(age_early_epoch = max(age_early_epoch), 
            age_late_epoch = min(age_late_epoch)) %>% 
  mutate(age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>% 
  ggplot(aes(y = group, 
             colour = group)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_linerange(aes(xmin = age_early_epoch, 
                     xmax = age_late_epoch)) +
  geom_label(aes(x = age_mid, 
                 label = group), 
             position = position_nudge(y = c(0.6, rep(0, 3), 
                                             0.6, rep(0, 3)), 
                                       x = c(0, -260, 80, 80, 0, -180, -60, -60)),
             label.size = 0, 
             label.padding = unit(0.1, "lines"), 
             size = 8/.pt) +
  labs(y = "Group ranges", 
       x = "Age [myr]") +
  scale_colour_manual(values = c("#1e728eff",
                                 "#ffbc3cff",
                                 "darkorange", 
                                 "coral3",
                                 "#5d7a64ff",
                                 "#ad6d8aff",
                                 "#6d3f2fff",
                                 "#f9938eff"),
                      name = NULL) +
  scale_y_discrete(limits = rev, 
                   expand = expansion(mult = c(0.12, 0.16))) +
  scale_x_reverse(limits = c(520, 0)) + 
  theme_classic(base_size = 12) +
  guides(colour = guide_legend(nrow = 2,
                               byrow = TRUE, 
                               override.aes = list(alpha = 0.9))) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.key.size = unit(3, "mm"), 
        legend.text = element_text(size = 10, 
                                   colour = "grey30"))

# patch together ----------------------------------------------------------


# plot_final <- plot_1 +
#   inset_element(plot_2, 0.065, 0, 1, 0.3, 
#                 ignore_tag = TRUE) +
#   plot_3 +
#   plot_annotation(tag_levels = "A")  +
#   plot_layout(heights = c(2, 1))
  

plot_final <- (plot_4 /
                 plot_5 +
  plot_1 +
  inset_element(plot_2, 0.065, 0, 1, 0.3, 
                ignore_tag = TRUE)) /
  plot_annotation(tag_levels = "A")  +
  plot_layout(heights = c(1, 2, 2))


# save plot
ggsave(plot_final, filename = here("figures",
                                   "figure_2.pdf"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white")     
