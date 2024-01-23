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
  geom_vline(xintercept = c(252, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_linerange(aes(xmin = age_early_epoch, 
                     xmax = age_late_epoch), 
                 position = position_dodge2(width = 1), 
                 linewidth = 0.22, 
                 alpha = 1) +
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

dat_clean %>%
  mutate(early_period = factor(early_period, levels = stages %>% 
                                distinct(system) %>% 
                                pull)) %>% 
  count(group, early_period, name = "nr_FAD") %>% 
  mutate(FAD_prop = nr_FAD/sum(nr_FAD) * 100) %>% 
  full_join(dat_clean %>% 
              mutate(late_period = factor(late_period, levels = stages %>% 
                                           distinct(system) %>% 
                                           pull)) %>% 
              count(group, late_period, name = "nr_LAD") %>% 
              rename(early_period = late_period)) %>% 
  drop_na() %>% 
  mutate(LAD_prop = nr_LAD/sum(nr_LAD) * 100) %>% 
  write_rds(here("data", 
                 "output", 
                 "fad_and_lad_per_system.rds"))

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




# # taxa counts -------------------------------------------------------------
# 
# 
# # count taxa per group (and Era)
# plot_3 <- dat_clean %>%
#   count(group, late_era) %>% 
#   ggplot(aes(n, group)) +
#   geom_segment(aes(xend = 0,
#                    yend = group, 
#                    colour = group),
#                position = position_dodge2(width = 0.9), 
#                alpha = 0.5, 
#                linewidth = 0.4) +
#   geom_point(aes(colour = group, 
#                  fill = late_era), 
#              size = 5, 
#              shape = 21,
#              position = position_dodge(width = 0.9)) +
#   scale_fill_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
#   scale_colour_brewer(type = "qual",
#                       palette = 2,
#                       name = NULL, 
#                       guide = "none") +
#   scale_y_discrete(expand = expansion(add = c(1, 1))) +
#   guides(fill = guide_legend(override.aes = list(size = 3, 
#                                                  stroke = 0))) +
#   labs(y = NULL, 
#        x = "Taxon count", 
#        fill = NULL) +
#   theme_classic(base_size = 12) +
#   theme(legend.position = "top", 
#         legend.text = element_text(size = 8, 
#                                    margin = margin(r = 0, unit = "pt")))



# alternative for b -------------------------------------------------------

plot_sepkoski <- dat_clean %>%
  # add stage
  left_join(stages %>% 
              group_by(early_epoch = series) %>% 
              summarise(early_stg = min(stg))) %>% 
  left_join(stages %>% 
              group_by(late_epoch = series) %>% 
              summarise(late_stg = max(stg))) %>% 
  mutate(stg_occ = map2(early_stg, 
                        late_stg, 
                        ~ seq(.x, .y))) %>% 
  unnest(stg_occ) %>% 
  count(group, stg_occ) %>% 
  # add age
  left_join(stages %>% 
              select(stg_occ = stg, mid)) %>% 
  ggplot(aes(mid, n, 
             fill = group, 
             colour = group)) +
  geom_ribbon(aes(ymin = 0, 
                  ymax = n), 
              alpha = 0.4)  +
  scale_colour_manual(values = c("#1e728eff",
                                 "#ffbc3cff",
                                 "darkorange", 
                                 "coral3",
                                 "#5d7a64ff",
                                 "#ad6d8aff",
                                 "#6d3f2fff",
                                 "#f9938eff"),
                      name = NULL) +
  scale_fill_manual(values = c("#1e728eff",
                                 "#ffbc3cff",
                                 "darkorange", 
                                 "coral3",
                                 "#5d7a64ff",
                                 "#ad6d8aff",
                                 "#6d3f2fff",
                                 "#f9938eff"),
                      name = NULL) +
  labs(x = "Agy [myr]", 
       y = "Taxon count") +
  scale_x_reverse(limits = c(520, 0)) + 
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

  

plot_4 <- dat_clean %>%
  ggplot(aes(group)) +
  geom_bar(aes(fill = late_era), 
           # position = position_dodge(), 
           width = 0.8) +
  scale_color_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  scale_fill_manual(values = rev(c("#fcea10","#5dc5ea", "#a9c6a9"))) +
  guides(fill = guide_legend(nrow = 1,
                             byrow = TRUE)) +
  labs(y = "Taxon count", 
       x = NULL, 
       fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = c(0.28, 0.8), 
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
  geom_vline(xintercept = c(252, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_linerange(aes(xmin = age_early_epoch, 
                     xmax = age_late_epoch)) +
  geom_label(aes(x = age_mid, 
                 label = group), 
             position = position_nudge(y = c(0.7, rep(0, 3), 
                                             0.7, rep(0, 3)), 
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
                   expand = expansion(mult = c(1, 0.16))) +
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
                 inset_element(plot_2, 0.065, 0, 1, 0.3, 
                               ignore_tag = TRUE)) /
  plot_1 +
  plot_annotation(tag_levels = "A")  +
  plot_layout(heights = c(2, 5, 2))


# save plot
ggsave(plot_final, filename = here("figures",
                                   "figure_3.svg"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white")     


# save plot
ggsave(plot_sepkoski, filename = here("figures",
                                   "figure_sepkoski.pdf"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white")  
