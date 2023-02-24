library(tidyverse)
library(sf)
library(fpr)

source('scripts/functions.R')


# !!!! requires xref_fhap object in tables.R script

# process the fhap data and present in tables.  This is just for tracking how we did it
# fhap_raw <- readxl::read_excel(
#   'data/FHAP.xlsx',
#   skip = 2,
#   col_names = F)
#
# col_names <- fhap_raw %>%
#   wkb_col_names()
#
#
# fhap_prep <- fhap_raw %>%
#   slice(3:nrow(.)) %>%
#   set_names(nm = col_names) %>%
#   # pad our numbers below 3 digits to ease the join to the waypoints
#   mutate(location_waypoint = stringr::str_pad(location_waypoint, 3, pad = "0")) %>%
#   # this is off now but we used it to get started on mapping the types
#   mutate(across(matches(c('_m', '_percent', 'cm', 'fractions')), as.numeric))
#
# # set the types - this is how we started but now we have a tibble
# # get the column types as a tibble - might need to do this in two steps
# types_tib <- tibble(type_readxl = sapply(fhap_prep, class)) %>%
#   mutate(type_readxl = case_when(type_readxl == 'character' ~ 'text',
#                           T ~ type_readxl))
# # join to make xref object
# xref <- bind_cols(
#   names(fhap_prep) %>% enframe(name = NULL, value = 'spdsht'),
#   types_tib
# ) %>%
#   mutate(report = stringr::str_replace_all(spdsht, '_', ' '),
#          report = stringr::str_to_title(report),
#          report = stringr::str_replace_all(report, 'M ', '(m)'),
#          # just replace M if it is at the end of the string
#          report = stringr::str_replace_all(report, 'M$', '(m)'),
#          report = stringr::str_replace_all(report, 'Location', ''),
#          report = stringr::str_replace_all(report, 'General', '')) %>%
#   select(spdsht, report, type_readxl) %>%
#   mutate(id_join = NA_integer_,
#          id_side = NA_integer_)

fhap_raw <- readxl::read_excel(
  'data/FHAP.xlsx',
  skip = 3,
  # need to leave the col names in at first so we select all the data
  col_names = T,
  col_types = xref_fhap %>% dplyr::pull(type_readxl)
) %>%
  purrr::set_names(nm = xref_fhap %>% pull(spdsht))

# qa the habitat unit length
qa_hu_len <- left_join(
  fhap_raw %>%
    group_by(location_reach_number) %>%
    summarise(len = max(location_distance_m)),

  fhap_raw %>%
  group_by(location_reach_number) %>%
  summarise(qa_len = sum(habitat_unit_length_m)),

  by = 'location_reach_number'
)


# import the gps and join to the fhap

stub_from <- '~/Library/CloudStorage/OneDrive-Personal/'
f <- 'Projects/repo/fish_passage_elk_2022_reporting/data/gps/AI/elk_2022_field_al.GPX'

wp_al <- sf::st_read(paste0(stub_from, f),
                     layer = 'waypoints',
                     quiet = T) %>%
  janitor::clean_names() %>%
  # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
  # mutate(name = as.numeric(name),
  #        name = case_when(name > 1000 ~ round(name/10, 0),
  #                         T ~ name)) %>%
  # dplyr::select(name_old = name, everything())  %>%
  # mutate(source = 'AI',
  #        name = paste0(name_old, '_', source, '_', lubridate::year(time))) %>%
  sf::st_transform(crs = 26911) %>%
  poisspatial::ps_sfc_to_coords(X = 'utm_easting', Y = 'utm_northing') %>%
  select(
    name,
    # name_old,
    # source,
    ele,
    time,
    utm_easting,
    utm_northing)

# join to the data and make it a spatial file
fhap <- left_join(
  fhap_prep,
  wp_al,
  by = c('location_waypoint' = 'name')
)


# map it quickly to see that it is good to go
fhap_sf <- fhap %>%
  sf::st_as_sf(coords = c('utm_easting', 'utm_northing'), crs = 26911, remove = F)
coll <- "whse_basemapping.fwa_stream_networks_sp"
stream <- fwapgr::fwa_query_collection(coll, filter = list(gnis_name = ("Weigert Creek")))
ggplot2::ggplot() +
  ggplot2::geom_sf(data = stream, lwd = 0.15) +
  ggplot2::geom_sf(data = fhap_sf, lwd = 0.15, fill = "steelblue", alpha = 0.5)

# burn out to file
fhap %>% readr::write_csv('data/fhap_clean.csv')
fpr::fpr_make_geopackage(dat = fhap, utm_zone = 10)
