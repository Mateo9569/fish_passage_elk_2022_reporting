library(tidyverse)
library(sf)
library(fpr)
library(readwritesqlite)

source('scripts/functions.R')
# !!!! requires xref_fhap object in functions.R script

# process the fhap data and present in tables.  This is just for tracking how we did it
# fhap_hu_raw <- readxl::read_excel(
#   'data/FHAP.xlsx',
#   skip = 2,
#   col_names = F)
#
# col_names <- fhap_hu_raw %>%
#   wkb_col_names()
#
#
# fhap_prep <- fhap_hu_raw %>%
# # slice(3:nrow(.)) %>%
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

# ##-----get spatial info----------------------------
# import the gps and join to the fhap. Maybe should have left this big file out of the repo but didn't
gps_loc <- 'data/elk_2022_field_al.GPX'
gps <- sf::st_read(gps_loc,
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
    # ele,
    # time,
    easting = utm_easting,
    northing = utm_northing)

##---------------- Import fhap_habitat_units-----------------
fhap_hu_raw <- readxl::read_excel(
  'data/FHAP.xlsx',
  sheet = 'hab_units',
  skip = 1,
  # need to leave the col names in at first so we select all the data
  col_names = T,
  col_types = xref_fhap_hu %>% dplyr::pull(type_readxl)
) %>%
  purrr::set_names(nm = xref_fhap_hu %>% pull(spdsht)) %>%
  mutate(location_waypoint = stringr::str_pad(location_waypoint, 3, pad = "0"))

# qa the habitat unit length
qa_hu_len <- left_join(
  fhap_hu_raw %>%
    group_by(location_site) %>%
    summarise(len = max(location_distance_m)),

  fhap_hu_raw %>%
  filter(habitat_unit_cat == 1) %>%
  group_by(location_site) %>%
  summarise(qa_len = sum(habitat_unit_length_m)),

  by = 'location_site'
)


# join to the data and make it a spatial file
fhap_hu <- left_join(
  fhap_hu_raw,
  gps,
  by = c('location_waypoint' = 'name')
)
# map it quickly to see that it is good to go
fhap_sf <- fhap_hu %>%
  sf::st_as_sf(coords = c('easting', 'northing'), crs = 26911, remove = F)
coll <- "whse_basemapping.fwa_stream_networks_sp"
stream <- fwapgr::fwa_query_collection(coll, filter = list(gnis_name = ("Weigert Creek")))
ggplot2::ggplot() +
  ggplot2::geom_sf(data = stream, lwd = 0.15) +
  ggplot2::geom_sf(data = fhap_sf, lwd = 0.15, fill = "steelblue", alpha = 0.5)

### --------------- burn out to file-----
fhap_hu %>% readr::write_csv('data/fhap_hu_clean.csv')
fpr::fpr_make_geopackage(dat = fhap_hu, utm_zone = 11, x = 'easting', y = 'northing')

# sf::st_delete(dsn = "data/fishpass_mapping/fishpass_mapping.gpkg", layer = 'fhap')
sf::st_layers("data/fishpass_mapping/fishpass_mapping.gpkg")

#---------------- Import fhap_habitat_sites-----
fhap_site <- readxl::read_excel(
  'data/FHAP.xlsx',
  sheet = 'site',
  skip = 1,
  # need to leave the col names in at first so we select all the data
  col_names = T,
  col_types = xref_fhap_site %>% dplyr::pull(type_readxl)
) %>%
  purrr::set_names(nm = xref_fhap_site %>% pull(spdsht)) %>%
  mutate(location_waypoint_start = stringr::str_pad(location_waypoint_start, 3, pad = "0"),
         location_waypoint_end = stringr::str_pad(location_waypoint_end, 3, pad = "0"))



# # this is how we joined to the gps data by start utm and utm
# fhap_site_loc <- left_join(
#   left_join(
#     fhap_site_raw, #called the original fhap_site object above "_raw"
#     gps,
#     by = c('location_waypoint_start' = 'name')
#   ),
#   gps %>% purrr::set_names(nm = paste0(names(.), '_end')),
#   by = c('location_waypoint_end' = 'name_end')
# ) %>%
#   mutate(easting = round(easting, 0),
#          northing = round(northing, 0),
#          easting_end = round(easting_end, 0),
#          northing_end = round(northing_end, 0))
#
# # burn to file for copy and paste into excel
# fhap_site_loc %>%
#   write_csv('data/inputs_extracted/fhap_locations.csv')

# make it a spatial file
fhap_site_sf <- fhap_site %>%
  sf::st_as_sf(coords = c('location_utm_easting', 'location_utm_northing'), crs = 26911, remove = F)

### --------------- burn out to file-----
fpr::fpr_make_geopackage(dat = fhap_site,
                         utm_zone = 11,
                         x = "location_utm_easting",
                         y = "location_utm_northing")

# sf::st_delete(dsn = "data/fishpass_mapping/fishpass_mapping.gpkg", layer = 'fhap')
sf::st_layers("data/fishpass_mapping/fishpass_mapping.gpkg")
# map it quickly to see that it is good to go

# burn it all into the sqlite
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
rws_drop_table("fhap_hu", conn = conn) ##now drop the table so you can replace it if the columns are different
rws_drop_table("fhap_site", conn = conn) ##now drop the table so you can replace it if the columns are different
rws_write(fhap_hu, exists = F, delete = TRUE,
          conn = conn, x_name = "fhap_hu")
rws_write(fhap_site, exists = F, delete = TRUE,
          conn = conn, x_name = "fhap_site")
rws_list_tables(conn)
rws_disconnect(conn)




