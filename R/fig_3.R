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
  ggplot(aes(log_max, n, 
             colour = group)) +
  geom_line(linewidth = 0.7) +
  geom_point(alpha = 0.3) +
  labs(y = "Taxa count", 
       x = "Maximum body size [m]") +
  scale_color_manual(values = c("#1e728eff",
                                "#ffbc3cff",
                                "#FF8454", 
                                "coral3",
                                "#5d7a64ff",
                                "#ad6d8aff",
                                "#6d3f2fff",
                                "#f9938eff"),
                     name = NULL) +
  scale_x_continuous(breaks = log(c(1, 2, 5, 10, 20)), 
                     labels = c(1, 2, 5, 10, 20)) +
  theme_classic(base_size = 12) +
  guides(colour = guide_legend(nrow = 2,
                               byrow = TRUE, 
                               override.aes = list(alpha = 1))) +
  theme(legend.position = c(0.6, 0.8), 
        legend.text = element_text(size = 9, 
                                   colour = "grey10",
                                   margin = margin(r = 0, unit = "pt")))



# over time ---------------------------------------------------------------

# calculate trend lines
dat_trend <- dat_clean %>%
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
                             .[2,4])) %>% 
  select(-c(data, mod_res)) %>% 
  mutate(p_star = case_when(p_value > 0.05 ~ "ns", 
                            between(p_value, 0.05, 0.01) ~ "*", 
                            between(p_value, 0.001, 0.01) ~ "**",
                            p_value < 0.001 ~ "***"))

# overall trend p-value
dat_clean %>%
  mutate(age_mid = -((age_early_epoch - age_late_epoch)/2 + age_late_epoch), 
         log_max = log(max_size_m)) %>% 
  lm(log_max ~ age_mid, .) %>% 
  # # overall trend
  # coefficients() %>%
  # pluck(2) %>% 
  # exp() %>% 
  # {(.-1)*100}
  # confidence interval
  # confint() %>% 
  # .[2, ] %>% 
  # exp() %>% 
  # {(.-1)*100}
  summary() %>%
  pluck(coefficients) %>% 
  .[2,4] 
  



# visualise it
plot_2 <- dat_clean %>%
  mutate(log_max = log(max_size_m)) %>% 
  # plot at the mid points of epochs
  mutate(age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>% 
  ggplot(aes(x = age_mid, 
             y = log_max)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  stat_smooth(aes(group = 1),
              geom = "line",
              method = "lm",
              se = FALSE,
              colour = "grey10",
              linewidth = 1) +
  geom_point(aes(fill = group), 
             position = position_jitter(height = 0.1,
                                        seed = 123),
             alpha = 0.6,
             size = 2.5,
             shape = 21, 
             colour = "grey30") +
  scale_fill_manual(values = c("#1e728eff",
                               "#ffbc3cff",
                               "#FF8454", 
                               "coral3",
                               "#5d7a64ff",
                               "#ad6d8aff",
                               "#6d3f2fff",
                               "#f9938eff"),
                    name = NULL) +
  scale_y_continuous(breaks = log(c(1, 2, 5, 10, 20)),
                     labels = c(1, 2, 5, 10, 20)) +
  labs(y = "Maximum body size [m]", 
       x = "Age [myr]") +
  # coord_geo(xlim = c(0, 510), 
  #           dat = list("periods", "eras"),
  #           pos = list("b", "b"),
  #           alpha = 0.2, 
  #           height = unit(0.8, "line"), 
  #           size = list(7/.pt, 10/.pt),
  #           lab_color = "grey20", 
  #           color = "grey20", 
  #           abbrv = list(TRUE, FALSE), 
  #           fill = "white",
  #           expand = TRUE, 
  #           lwd = list(0.4, 0.5)) +
  scale_x_reverse() +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") 


# trend lines
plot_3 <- dat_clean %>%
  mutate(log_max = log(max_size_m)) %>% 
  # plot at the mid points of epochs
  mutate(age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>%
  ggplot(aes(x = age_mid, 
             y = log_max)) +
  geom_vline(xintercept = c(443, 365, 252, 
                            210, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  stat_smooth(aes(colour = group),
            geom = "line",
            method = "lm",
            se = FALSE,
            alpha = 0.9,
            linewidth = 0.8) +
  annotate("text",
           label = dat_trend %>%
             filter(p_star != "ns") %>%
             pull(p_star),
           x = c(30, -10, -10),
           y = c(1.14, 1.44, 1.78),
           colour = c("#1e728eff",
                      "#5d7a64ff",
                      "#ad6d8aff"),
           size = 10/.pt) +
  scale_color_manual(values = c("#1e728eff",
                                "#ffbc3cff",
                                "#FF8454",
                                "coral3",
                                "#5d7a64ff",
                                "#ad6d8aff",
                                "#6d3f2fff",
                                "#f9938eff"),
                     name = NULL) +
  # scale_y_continuous(breaks = log(c(1, 2, 5, 10, 20)),
  #                    labels = c(1, 2, 5, 10, 20)) +
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
  plot_3 +
  plot_annotation(tag_levels = "A", 
                  tag_suffix = "  ")

# save plot
ggsave(plot_first, 
       filename = here("figures",
                       "figure_3.pdf"), 
       width = 183, height = 180,
       units = "mm", 
       bg = "white")     


