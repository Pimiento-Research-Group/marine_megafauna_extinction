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
 
# occurrences ---------------------------------------------------------------


# count taxa
dat_hist <- dat_occ %>%
  filter(occurrences >= 1) %>% 
  group_by(group) %>%
  arrange(log_occ) %>% 
  distinct() %>% 
  mutate(group = factor(group, 
                        levels = c("Invert", 
                                   "Fish", 
                                   "Chondrichthyes", 
                                   "Reptile", 
                                   "Bird", 
                                   "Mammal")), 
         taxa = factor(taxa, levels = taxa)) 


plot_1 <- dat_hist %>%
  ggplot(aes(y = taxa, x = log_occ)) +
  geom_bar(aes(fill = group), stat = "identity") +
  geom_text(aes(label = group, 
                colour = group), 
            position = position_nudge(y = c(12, -12, rep(12, 4)), 
                                      x = c(-log(1.2), 
                                            0,
                                            rep(-log(1.2), 3),
                                            -log(1.7))),
            data = dat_hist %>%
              filter(log_occ == max(log_occ))) +
  labs(x = "# Occurrences", y = paste0("Taxa (n = ", 
                                       dat_occ %>%
                                         filter(occurrences > 0) %>%
                                         nrow(), ")")) +
  scale_x_continuous(breaks = c(0, seq(log1p(1), log1p(300),
                                       by = log1p(1))),
                     labels = c(0, expm1(seq(
                       log1p(1), log1p(300),
                       by = log1p(1)
                     )))) +
  scale_fill_brewer(type = "qual",
                      palette = 2,
                      name = NULL) +
  scale_colour_brewer(type = "qual",
                      palette = 2,
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



# patch together ----------------------------------------------------------


# patch together
plot_final <- plot_1 /
  plot_2 +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(tag_levels = "A")

# and save
ggsave(plot_final, filename = here("figures",
                                   "figure_sampling.png"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white", device = ragg::agg_png)
