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

# calculate trend lines
dat_clean %>%
  mutate(age_mid = -((age_early_epoch - age_late_epoch)/2 + age_late_epoch), 
         log_max = log(max_size_m)) %>% 
  group_by(group) %>%
  nest() %>%
  mutate(mod_res = map(data, ~lm(log_max ~ age_mid, .x)),
         mean_trend = map_dbl(mod_res,
                              ~ coefficients(.x) %>%
                                pluck(2) %>% 
                                exp() %>% 
                                {(.-1)*100}),
         lower_ci = map_dbl(mod_res, 
                            ~ confint(.x) %>% 
                              .[2, 1] %>% 
                              exp() %>% 
                              {(.-1)*100}), 
         upper_ci = map_dbl(mod_res,
                            ~ confint(.x) %>%
                              .[2, 2] %>% 
                              exp() %>% 
                              {(.-1)*100}), 
         p_value = map_dbl(mod_res, 
                           ~ summary(.x) %>% 
                             pluck(coefficients) %>% 
                             .[2,4])) 


# visualise it
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
  stat_smooth(aes(group = 1), 
              geom = "line", 
              method = "lm", 
              se = FALSE, 
              colour = "grey30", 
              linewidth = 1) +
  stat_smooth(aes(colour = group), 
              geom = "line", 
              method = "lm", 
              se = FALSE, 
              alpha = 0.9, 
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




# patch together ----------------------------------------------------------

plot_first <- plot_1 /
  plot_2 +
  plot_annotation(tag_levels = "A")

# save plot
ggsave(plot_first, filename = here("figures",
                                  "figure_1.png"), 
       width = 183, height = 150,
       units = "mm", 
       bg = "white", device = ragg::agg_png)     


