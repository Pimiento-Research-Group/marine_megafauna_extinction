library(here)
library(tidyverse)
library(patchwork)
library(ggdist)

# read data ---------------------------------------------------------------

# read cleaned occurrences file
dat_occ <- read_rds(here("data",
                         "output",
                         "pbdb_data_clean.rds"))


# read in sampling rate results from crm analysis
dat_crm <- read_rds(here("data",
                         "output",
                         "cmr_data.rds"))
 
# cleaned pbdb data
dat_pbdb <- read_rds(here("data",
                          "output",
                          "pbdb_data_clean.rds"))
                     
# occurrences ---------------------------------------------------------------


# count taxa
dat_hist <- dat_occ %>%
  filter(occurrences >= 1) %>% 
  group_by(group) %>%
  arrange(log_occ) %>% 
  distinct() %>% 
  mutate(taxa = factor(taxa, levels = taxa)) 


plot_1 <- dat_hist %>%
  ggplot(aes(y = taxa, x = log_occ)) +
  geom_bar(aes(fill = group), stat = "identity") +
  geom_text(aes(label = group, 
                colour = group), 
            position = position_nudge(y = c(6, 12, 14, 14, -20, -20, 14, 14), 
                                      x = c(log(1.7), log(1.6), rep(-log(1.2), 5),-log(2))),
            data = dat_hist %>%
              filter(log_occ == max(log_occ)), 
            size = 10/.pt) +
  labs(x = "# Occurrences", y = NULL) +
  scale_x_continuous(breaks = c(0, seq(log1p(1), log1p(300),
                                       by = log1p(1))),
                     labels = c(0, expm1(seq(
                       log1p(1), log1p(300),
                       by = log1p(1)
                     )))) +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  scale_colour_manual(values = c("#1e728eff",
                                 "#ffbc3cff",
                                 "darkorange", 
                                 "coral3",
                                 "#5d7a64ff",
                                 "#ad6d8aff",
                                 "#6d3f2fff",
                                 "#f9938eff"),
                      name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_cartesian(expand = FALSE) +
  theme_classic(base_size = 12) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "none", 
        legend.key.size = unit(3, "mm"))
  
  

  

# sampling rate -----------------------------------------------------------


plot_2 <- dat_crm %>%
  mutate(group_id = str_to_title(group_id)) %>% 
  ggplot(aes(y = group_id, x = samp_inx)) +
  stat_halfeye(point_colour = "grey20", 
               shape = 21, 
               point_fill = "white", 
               interval_colour = "grey20") +
  theme_minimal(base_size = 12) +
  labs(y = NULL, 
       x = "Sampling completeness") +
  scale_x_continuous(breaks = c(0, 0.05, 0.1), 
                     limits = c(0, 0.13)) +
  theme(panel.grid = element_blank())



# pbdb representation -----------------------------------------------------

plot_pbdb <- dat_pbdb %>%
  mutate(is_pres = if_else(occurrences == 0, "abs", "pres")) %>% 
  group_by(group) %>% 
  count(is_pres) %>% 
  pivot_wider(names_from = is_pres, 
              values_from = n) %>% 
  replace_na(list(abs = 0)) %>% 
  mutate(pres_perc = (pres/(abs + pres))*100) %>% 
  ggplot(aes(group, pres_perc, 
             fill = group)) +
  geom_col(alpha = 0.8) +
  theme_classic(base_size = 12) +
  labs(y = NULL, 
       x = NULL) +
  annotate("text", 
           label = "Representation in PBDB", 
           x = 6, 
           y = 92, 
           size = 11/.pt) +
  scale_y_continuous(breaks = c(0, 50, 100), 
                     labels = function(x) paste0(x, "%"), 
                     expand = expansion(add = 1)) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "darkorange",
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                      name = NULL) +
  theme(panel.grid = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        plot.title = element_text(hjust = 1, 
                                  size = 12))


# patch together ----------------------------------------------------------


# patch together
plot_final <- (plot_1 +
  inset_element(plot_pbdb, 0.4, 0.35, 1, 0.71)) /
  plot_2 +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(tag_levels = "A")

# and save
ggsave(plot_final,
       filename = here("figures",
                       "figure_2.pdf"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white")
