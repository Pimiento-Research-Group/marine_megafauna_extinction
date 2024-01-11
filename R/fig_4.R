library(here)
library(tidyverse)
library(patchwork)
library(deeptime)
library(ggridges)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))

data(stages, package = "divDyn")

# overall distribution ----------------------------------------------------

colour_vec <- c("darkorange",
                "#6d3f2fff",
                "#ffbc3cff", 
                "#1e728eff",
                "coral3",
                "#5d7a64ff",
                "#f9938eff",
                "#ad6d8aff")

plot_1 <- dat_clean %>%
mutate(log_max = log(max_size_m), 
       group = fct_reorder(group, log_max)) %>% 
  ggplot(aes(x = log_max, 
             y = group, 
             fill = group, 
             colour = group, 
             height = ..density..)) + 
  geom_density_ridges(stat = "density",
                      bounds = c(0, Inf), 
                      alpha = 0.6, 
                      position = position_nudge(y = -0.3), 
                      size = 0.8, 
                      rel_min_height = 0.01) +
  labs(y = NULL, 
       x = "Maximum body size [m]", 
       title = "A") +
  scale_fill_manual(values = colour_vec,
                    name = NULL) +
  scale_colour_manual(values = colour_vec,
                      name = NULL) +
  scale_x_continuous(breaks = log(c(1, 2, 5, 10, 20)), 
                     labels = c(1, 2, 5, 10, 20)) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none", 
        axis.text.y = element_text(colour = colour_vec, 
                                   face = "bold"), 
        axis.ticks.y = element_blank())



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


# overall trend 
ov_trend <- dat_clean %>%
  mutate(age_mid = -((age_early_epoch - age_late_epoch)/2 + age_late_epoch), 
         log_max = log(max_size_m)) %>% 
  lm(log_max ~ age_mid, .)  
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
# p-value
ov_trend %>% 
  summary() %>%
  pluck(coefficients) %>% 
  .[2,4] 
  
tibble(group = "Overall", 
       mean_trend = ov_trend %>% 
         coefficients() %>%
         pluck(2) %>%
         exp() %>%
         {(.-1)*100}, 
       lower_ci = ov_trend %>% 
         confint() %>%
         .[2, ] %>%
         exp() %>%
         {(.-1)*100} %>% 
         .[1], 
       upper_ci = ov_trend %>% 
         confint() %>%
         .[2, ] %>%
         exp() %>%
         {(.-1)*100} %>% 
         .[2], 
       p_value = ov_trend %>%
         summary() %>%
         pluck(coefficients) %>%
         .[2, 4],
       p_star = "***") %>% 
  bind_rows(dat_trend) %>% 
  mutate(across(c(mean_trend, 
                  lower_ci, 
                  upper_ci), ~round(.x *10, 1))) %>% 
  mutate(mean_trend = paste0(round(mean_trend, 1), "%"), 
         CI = paste0("[", 
                     round(lower_ci, 1), 
                     "%, ",
                     round(upper_ci, 1), 
                     "%]")) %>% 
  select(group, mean_trend, p_star, CI) %>% 
  write_csv(here("data", 
                 "output", 
                 "size_trend.csv"))


# visualise it
plot_2 <- dat_clean %>%
  mutate(log_max = log(max_size_m)) %>% 
  # plot at the mid points of epochs
  mutate(age_mid = (age_early_epoch - age_late_epoch)/2 + age_late_epoch) %>% 
  ggplot(aes(x = age_mid, 
             y = log_max)) +
  geom_vline(xintercept = c(252, 66), 
             colour = "grey70", 
             linetype = "dashed") +
  stat_smooth(aes(group = 1),
              geom = "line",
              method = "lm",
              se = FALSE,
              colour = "grey10",
              linewidth = 1) +
  geom_point(aes(fill = group), 
             position = position_jitter(width = 10,
                                        seed = 123),
             alpha = 0.6,
             size = 2.5,
             shape = 21, 
             colour = "grey30") +
  annotate("text",
           x = 498,
           y = 0.9,
           label = "1.8%***",
           colour = "grey10",
           size = 10/.pt) +
  labs(y = "Maximum body size [m]", 
       x = "Age [myr]", 
       title = "B") +
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
                     labels = c(1, 2, 5, 10, 20)) +
  coord_cartesian(xlim = c(510, 0)) +
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
  geom_vline(xintercept = c(252, 66), 
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
           x = c(31, 3, 5),
           y = c(1.1, 1.62, 1.9),
           colour = c("#1e728eff",
                      "#5d7a64ff",
                      "#ad6d8aff"),
           size = 8/.pt) +
  annotate("text",
           label = dat_trend %>%
             filter(p_star != "ns") %>%
             pull(mean_trend) %>% 
             {.*10} %>% 
             round(1) %>% 
             paste0(., "%"),
           x = c(48, 20, 22),
           y = c(1.1, 1.62, 1.9),
           colour = c("#1e728eff",
                      "#5d7a64ff",
                      "#ad6d8aff"),
           size = 8/.pt) +
  scale_color_manual(values = c("#1e728eff",
                                "#ffbc3cff",
                                "#FF8454",
                                "coral3",
                                "#5d7a64ff",
                                "#ad6d8aff",
                                "#6d3f2fff",
                                "#f9938eff"),
                     name = NULL) +
  scale_y_continuous(breaks = c(0.2, 1, 1.8), 
                     labels = c("  0", "  1", "  2")) +
  labs(y = "Mean maximum body size [m]", 
       x = "Age [myr]", 
       title = "C") +
  scale_x_reverse() +
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
  theme_classic(base_size = 12) +
  theme(legend.position = "none") 

# patch together ----------------------------------------------------------

plot_first <- wrap_elements(full = plot_1) +
  wrap_elements(full = plot_2) + 
  wrap_elements(full = plot_3) + 
  plot_layout(ncol = 1) 

# save plot
ggsave(plot_first, 
       filename = here("figures",
                       "figure_4.pdf"), 
       width = 183, height = 180,
       units = "mm", 
       bg = "white")     


