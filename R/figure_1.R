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
  geom_point() +
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
  ggplot(aes(xmin = age_early_epoch, 
             xmax = age_late_epoch, 
             y = log_max, 
             fill = group)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  geom_linerange(aes(colour = group), 
                 position = position_dodge2(width = 0.143), 
                 alpha = 0.8) +
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


# patch together ----------------------------------------------------------


plot_first <- plot_1 /
  plot_2 / 
  plot_annotation(tag_levels = "A")

# save plot
ggsave(plot_first, filename = here("figures",
                                  "figure_1.png"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white", device = ragg::agg_png)     


