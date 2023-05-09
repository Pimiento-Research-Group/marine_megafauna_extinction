library(here)
library(chronosphere)
library(sf)
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

# read cleaned data file
dat_clean <- read_rds(here("data",
                           "input",
                           "megafauna_clean.rds"))




# pbdb occurrences --------------------------------------------------------


# read in pbdb occurrences
pbdb_data_raw <- read_rds(here("data",
                               "output",
                               "pbdb_data_raw.rds"))


# remove those where no entries where found, based on length of returned entries
pbdb_data <- pbdb_data_raw[map(pbdb_data_raw, ncol) > 2]

# same for where we get a message for no found entries
pbdb_data <- pbdb_data[map(pbdb_data, 
                           ~ .x %>% 
                             select(1) %>% 
                             pull()) != "THIS REQUEST RETURNED NO RECORDS"]


# set up character with weird symbols in the pbdb list that need to be removed
symb_list <- c("\\?", "sp\\.", "cf\\.", "n\\.", 
               "aff", "gen\\.", "informal", "J sensu Ristedt", 
               "A sensu Ristedt", "B sensu Ristedt", 
               "K sensu Ristedt", "H sensu Ristedt", 
               "ex gr\\.", "\\.", " sp\\.", 
               "spp\\.", '\"', "[:digit:]", " \\s*\\([^\\)]+\\)") %>% 
  paste0(., collapse = "|")

# clean data
pbdb_data <- pbdb_data %>% 
  map(~ .x %>% 
        # get rid of occ that are not genus or species
        filter(accepted_rank %in% c('genus', 'species')) %>%
        # remove weird symbols
        mutate(accepted_name_clean = str_remove_all(accepted_name, symb_list),
               # remove white space and start and end and squish double white
               # spaces in the middle
               accepted_name_clean = str_squish(accepted_name_clean))) 


# merge
dat_pbdb <- pbdb_data %>% 
  # select only relevant columns
  map(~ select(.x, accepted_name, accepted_rank, genus,  
               max_ma, min_ma, 
               lng, lat)) %>% 
  # make sure that datasets have same column types for merging 
  map(~ mutate(.x,
               genus = as.character(genus))) %>% 
  # create one dataframe from the list of pbdb occurrences
  bind_rows() 


# # gplates -----------------------------------------------------------------
# 
# # download gplates continental plate model
# 
# # see what resolution and version we have
# datasets(dat = "paleomap") %>% View
# 
# # get the data
# dat_maps <- fetch("paleomap", 
#                   var = "paleocoastlines")
# 
# 
# # assign to nearest map ---------------------------------------------------
# 
# # see what resolution the map is
# map_res <- layers(dat_maps) %>% 
#   str_extract(".+?(?=_)") %>% 
#   str_extract("(\\d)+") %>% 
#   unique() %>% 
#   as.numeric()
# 
# 
# # bin pbdb data
# dat_pbdb_binned <- dat_pbdb %>%
#   # bin fad and lad to stages
#   mutate(bin_low = cut(max_ma,
#                        breaks = map_res-2.5,
#                        labels = map_res[1:length(map_res)-1]),
#          bin_high = cut(min_ma, 
#                         breaks = map_res-2.5,
#                         labels = map_res[1:length(map_res)-1]))  %>% 
#   filter(bin_low == bin_high) %>% 
#   select(everything(), bin = bin_low, 
#          -c(max_ma, min_ma, bin_high)) %>% 
#   mutate(bin = as.numeric(as.character(bin))) %>% 
#   # assign taxon groups 
#   left_join(dat_clean %>% 
#               distinct(group, genus)) %>% 
#   drop_na(group)
# 
# 
# 
# # visualise ---------------------------------------------------------------
# 
# # set up list for plot
# plot_list <- list()
# 
# 
# # loop through maps and bins
# for (i in unique(dat_pbdb_binned$bin) %>% sort()) {
#   
#   layer_var <- paste0("X", 
#                       i, 
#                       "Ma_CM_v7")
#   
#   plot_list <- dat_maps[[layer_var]] %>% 
#     as("sf") %>% 
#     rownames_to_column("id") %>% 
#     ggplot() +
#     geom_sf(colour = "white", 
#             fill = "grey50") + 
#     theme_minimal() +
#     geom_point(aes(geometry = geometry,
#                    fill = group), 
#                colour = "grey10",
#                shape = 21,
#                size = 3,
#                stroke = 0.3,
#                alpha = 0.6,
#                stat = "sf_coordinates",
#                data = dat_pbdb_binned %>%
#                  drop_na(paleolng, paleolat) %>%
#                  filter(bin == i) %>% 
#                  st_as_sf(coords = c("paleolng", "paleolat"),
#                           crs = "WGS84")) +
#     labs(y = NULL, 
#          x = NULL, 
#          fill = NULL) +
#     theme(legend.position = "bottom") 
#   
# }
# 
# plot_list <- unique(dat_pbdb_binned$bin) %>% 
#   sort() %>% 
#   map(~ dat_maps[[paste0("X", 
#                           .x, 
#                           "Ma_CM_v7")]] %>%
#          as("sf") %>% 
#          rownames_to_column("id") %>% 
#          ggplot() +
#          geom_sf(colour = "white", 
#                  fill = "grey50") + 
#          theme_minimal() +
#          geom_point(aes(geometry = geometry,
#                         fill = group_col), 
#                     colour = "grey10",
#                     position = position_jitter(width = 2,
#                                                height = 2,
#                                                seed = 123),
#                     shape = 21,
#                     size = 3,
#                     stroke = 0.3,
#                     alpha = 0.6,
#                     stat = "sf_coordinates",
#                     data = dat_pbdb_binned %>%
#                       mutate(group_col = case_when(
#                         group == "Invert" ~ "red",
#                         group == "Fish" ~ "yellow",
#                         group == "Chondrichthyes" ~ "orange",
#                         group == "Reptile" ~ "green",
#                         group == "Bird" ~ "blue",
#                         group == "Mammal" ~ "brown"
#                       )) %>% 
#                       drop_na(paleolng, paleolat) %>%
#                       filter(bin == .x) %>% 
#                       st_as_sf(coords = c("paleolng", "paleolat"),
#                                crs = st_crs("WGS84"))) +
#          labs(y = NULL, 
#               x = NULL, 
#               fill = NULL, title = paste(.x, "myr")) +
#          theme(legend.position = "bottom") +
#          coord_sf(ylim = c(-90, 90), 
#                   datum = st_crs("WGS84")) +
#         scale_fill_identity(guide = "legend", 
#                             labels = c("Invert", "Fish", "Chondrichthyes", 
#                                        "Reptile", "Bird", "Mammal"), 
#                             breaks = c("red", "yellow", "orange", 
#                                        "green", "blue", "brown"))
#        
#   )
# 
# # save plots
# for(i in 1:length(plot_list)){
#   ggsave(plot = plot_list[[i]], 
#          file = here("figures",
#                      "maps",
#                      paste("file",i,".png",sep="")), 
#          bg = "white")
# }
# 
# # create gif
# list.files(here("figures",
#                 "maps"),
#            pattern = "*png",
#            full.names = TRUE) %>% 
#   as_tibble() %>% 
#   mutate(ord_id = str_sub(value, -6, -1), 
#          ord_id = str_extract(ord_id, "(\\d)+"), 
#          ord_id = as.numeric(ord_id)) %>% 
#   arrange(ord_id) %>% 
#   pull(value) %>% 
#   gifski::gifski(.,
#                  gif_file = here("figures",
#                                  "animation.gif"), width = 800, height = 600, delay = 1)
# 

# world map ---------------------------------------------------------------

# world map outlines
world <- map_data("world")

# plot world map of samples
# plot_map <-
dat_world <- dat_pbdb %>% 
  # bin to eras
  mutate(era_max = cut(max_ma,
                       breaks = c(538.8, # beginning of Cambrium
                                  251.9, # end of Permian
                                  66.04, # end of Cretaceous
                                  0), 
                       labels = c("Cenozoic",
                                  "Mesozoic", 
                                  "Paleozoic")), 
         era_min = cut(min_ma,
                       breaks = c(538.8, # beginning of Cambrium
                                  251.9, # end of Permian
                                  66.04, # end of Cretaceous
                                  0), 
                       labels = c("Cenozoic",
                                  "Mesozoic", 
                                  "Paleozoic"))) %>% 
  filter(era_max == era_min) %>% 
  mutate(era = factor(era_min, 
                      levels = c("Paleozoic", 
                                 "Mesozoic", 
                                 "Cenozoic"))) %>% 
  # add taxonomic groups
  left_join(dat_clean %>% 
              distinct(group, genus))  %>% 
  drop_na(group)

# create map coloured by era
plot_world <- dat_world %>%
  ggplot() +
  geom_map(aes(map_id = region), 
           data = world, map = world, fill = "grey70") + 
  geom_point(aes(x = lng, y = lat,
                 fill = era),
             na.rm = TRUE,
             shape = 21,
             colour = "grey20",
             stroke = 1,
             size = 2, 
             alpha = 0.4) +
  scale_x_continuous(name = '', limits = c(-220, 180), expand = c(0,0), 
                     labels = NULL, breaks = seq(-180, 180, by = 45)) +
  scale_y_continuous(name = '', limits = c(-120, 90),   expand = c(0,0), 
                     labels = NULL) +
  scale_fill_manual(name = "Era", 
                    values = c("#A3BC99", "#6CC0DB", "#F2F91E")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_void() + 
  theme(legend.position = 'none', 
        axis.ticks = element_blank())

# sampling along latitude
dens_lat <- dat_world %>% 
  ggplot(aes(y = lat,
             colour = era)) +
  geom_density(linewidth = 1.2) + 
  scale_colour_manual(name = "Era",
                      values = c("#A3BC99", "#6CC0DB", "#F2F91E")) +
  theme_void() +
  theme(legend.position = "none")


# sampling along longitude
dens_lng <- dat_world %>% 
  ggplot(aes(x = lng,
             colour = era)) +
  geom_density(linewidth = 1.2) + 
  scale_colour_manual(name = "Era",
                      values = c("#A3BC99", "#6CC0DB", "#F2F91E")) +
  theme_void() +
  theme(legend.position = "none")

# patch together
plot_era <- plot_world +
  inset_element(dens_lng, 0.15, 0, 1.1, 0.17) +
  inset_element(dens_lat, 0, 0, 0.17, 1.05)

# save 
ggsave(
  plot = plot_era,
  file = here("figures",
              "spatial_sampling.png"),
  bg = "white"
)


