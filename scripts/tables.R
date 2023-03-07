# this file imports our data and builds the tables we need for our reporting

source('scripts/packages.R')
source('scripts/functions.R')
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)

bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn) %>%
   mutate(downstream_route_measure = as.integer(downstream_route_measure)) %>%
   mutate(wct_network_km = round(wct_network_km,2))
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn)
# photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)
# # fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
fhap_site <- readwritesqlite::rws_read_table("fhap_site", conn = conn)
fhap_hu <- readwritesqlite::rws_read_table("fhap_hu", conn = conn)
hydrometrics <- readwritesqlite::rws_read_table("hydrometrics", conn = conn)
rws_disconnect(conn)
#
# ##build the dams table
# tab_dams_raw <- bcfishpass %>%
#   filter(aggregated_crossings_id == 1100000129 |
#            aggregated_crossings_id == 1100002016 |
#            aggregated_crossings_id == 197542) %>%
#   select(id = aggregated_crossings_id, stream = gnis_stream_name,utm_zone, utm_easting, utm_northing, dbm_mof_50k_grid) %>%
#   mutate(barrier_ind = case_when(
#     id == 1100000129 ~ 'F',
#     T ~ 'T'),
#     Notes = case_when(
#       id == 1100000129 ~ 'Remnant dam not located in main channel.',
#       id == 1100002016 ~ 'Large dam (15m  high at 55% grade) located in main channel. No fish ladder.',
#       id == 197542 ~ 'Two small dams (30cm and 40cm high) located just upstream (7m and 20m) of Dicken Road. Likely easily passable by adult WCT but barrier to fry and small juveniles. If culvert replaced these could potentially be fixed at the same time.'
#     )
#   )
#

#load priorities
habitat_confirmations_priorities <- readr::read_csv(
  file = "./data/habitat_confirmations_priorities.csv") %>%
  #filter(!alias_local_name %like% 'ef') %>% ##ditch the ef sites
  mutate(site = as.numeric(site),
         upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1)) %>%
    rename(local_name = alias_local_name) #did this to stay consistent for later

####--------------habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(col_filter_na = T, row_empty_remove = T)

hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) %>%   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to filter them out


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site))
  #plyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

# make object for ef sites, can use this to burn to fishpass mapping gpckg
hab_fiss_site <- hab_site %>%
  filter((alias_local_name %like% '_ef')) %>%
  mutate(ef = str_extract(alias_local_name, 'ef.')) %>%
  relocate(ef, .after = location)

hab_fish_collect_map_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(local_name, species, .keep_all = T) %>% ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  filter(alias_local_name %like% 'ef') ##changed this from ef1


# test to see what we get at each site
test <- hab_fish_collect_map_prep %>%
  distinct(local_name, species)



##join the tables together
hab_fish_collect_map_prep2 <- right_join(
  # distinct to get rid of lots of sites
  select(hab_loc2, reference_number, alias_local_name, utm_zone:utm_northing) %>% distinct(alias_local_name, .keep_all = T),
  select(hab_fish_collect_map_prep %>% distinct(local_name, species, .keep_all = T), local_name, species),
  by = c('alias_local_name' = 'local_name')
)


##add the species code
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught')

# this is the table to burn to geojson for mapping
# we are just going to keep 1 site for upstream and downstream because more detail won't show well on the map anyway
# purpose is to show which fish are there vs. show all the sites and what was caught at each. TMI

hab_fish_collect_map_prep3 <- left_join(
  hab_fish_collect_map_prep2 %>%
    mutate(species = as.factor(species)),  ##just needed to do this b/c there actually are no fish.

  select(hab_fish_codes, common_name, species_code),
  by = c('species' = 'common_name')
)
  # this time we ditch the nfc because we don't want it to look like sites are non-fish bearing.  Its a multipass thing
  # WATCH THIS IN THE FUTURE
  # filter(species_code != 'NFC')

# need to make an array for mapping the hab_fish_collect files
# this gives a list column vs an array.  prob easier to use postgres and postgis to make the array
hab_fish_collect <- left_join(
  hab_fish_collect_map_prep3 %>%
    select(alias_local_name:utm_northing) %>%
    distinct(),

  hab_fish_collect_map_prep3 %>%
    select(-species, -reference_number, -utm_zone:-utm_northing) %>%
    pivot_wider(names_from = 'alias_local_name', values_from = "species_code") %>%
    pivot_longer(cols = contains('_ef')) %>%
    rename(alias_local_name = name,
           species_code = value),

  by = 'alias_local_name'
) %>%
  rowwise() %>%
  mutate(species_code = toString(species_code),
         species_code = stringr::str_replace_all(species_code, ',', ''))


rm(hab_fish_collect_map_prep, hab_fish_collect_map_prep2)

hab_fish_collect_prep1 <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_name:-site_number)

hab_features <- left_join(
  habitat_confirmations %>%
    purrr::pluck("step_4_stream_site_data") %>%
    select(reference_number,local_name, feature_type:utm_northing) %>%
    filter(!is.na(feature_type)),

  fpr_xref_obstacles,

  by = c('feature_type' = 'spreadsheet_feature_type')
)


## fish densities ----------------------------------------------------------
hab_fish_indiv_prep <- habitat_confirmations %>%
  purrr::pluck("step_3_individual_fish_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_names:-site_number)

hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_indiv_prep3 <- left_join(
  hab_fish_indiv_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::select(reference_number,
                alias_local_name,
                site_number,
                sampling_method,
                method_number,
                haul_number_pass_number,
                species_code,
                length_mm,
                weight_g) ##added method #

hab_fish_collect_info <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  # select(-gazetted_name:-site_number) %>%
  dplyr::distinct(reference_number, sampling_method, method_number, haul_number_pass_number, .keep_all = T)

# join the indiv fish data to existing site info
hab_fish_indiv <- full_join(
  select(hab_fish_indiv_prep3,
         reference_number,
         sampling_method,
         method_number,
         haul_number_pass_number,
         species_code,
         length_mm,
         weight_g),
  select(hab_fish_collect_info,
         reference_number,
         local_name,
         temperature_c:model, ##added date_in:time_out
         comments
  ),
  by = c(
    "reference_number",
    # 'alias_local_name' = 'local_name',
    "sampling_method",
    "method_number",
    "haul_number_pass_number")
) %>%
  mutate(species_code = as.character(species_code)) %>%
  mutate(species_code = case_when(
    is.na(species_code) ~ 'NFC',
    T ~ species_code)
  ) %>%
  mutate(species_code = as.factor(species_code)) %>%
  mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
    length_mm <= 65 ~ 'fry',
    length_mm > 65 & length_mm <= 110 ~ 'parr',
    length_mm > 110 & length_mm <= 140 ~ 'juvenile',
    length_mm > 140 ~ 'adult',
    T ~ NA_character_
  ),
  life_stage = case_when(
    species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
    T ~ life_stage
  ))%>%
  mutate(life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, '_', location))

# make a summary table for fish sampling data

tab_fish_summary <- hab_fish_indiv %>%
  group_by(site_id,
           ef,
           sampling_method,
           species_code) %>% ##added sampling method!
  summarise(count_fish = n()) %>%
  arrange(site_id, species_code, ef)

# this will be joined to the abundance estimates and the confidence intervals
fish_abund_prep <- hab_fish_indiv %>%
  group_by(local_name,
           site_id,
           ef,
           sampling_method,
           haul_number_pass_number,
           species_code,
           life_stage,
           ef_seconds) %>% ##added sampling method!
  filter(sampling_method == 'electrofishing') %>%
  summarise(catch = n()) %>%
  arrange(site_id, species_code, ef) %>%
  # ungroup() %>%
  mutate(catch = case_when(
    species_code == 'NFC' ~ 0L,
    T ~ catch),
    # effort = catch/ef_seconds,
    id = paste0(local_name, '_', species_code, '_', life_stage)) %>%
  ungroup() %>%
  arrange(id)

# join the total number of passes to each event so that we know if it is a higher number than the pass of the catch
fish_abund_prep2 <- left_join(
  fish_abund_prep,

  fish_abund_prep %>%
    group_by(local_name) %>%
    summarise(pass_total = max(haul_number_pass_number)),
  by = 'local_name'
)

# make a dat to indicate if the nfc in the set for each species
fish_nfc_tag<- fish_abund_prep2 %>%
  mutate(nfc_pass = case_when(
    # species_code != 'NFC' &
    haul_number_pass_number == pass_total ~ F,
    T ~ T),
    nfc_pass = case_when(
      species_code == 'NFC' ~ T,
      T ~ nfc_pass)
  ) %>%
  select(local_name, species_code, life_stage, haul_number_pass_number, pass_total, nfc_pass) %>%
  arrange(desc(haul_number_pass_number)) %>%
  # filter(nfc_pass == T) %>%
  distinct(local_name, species_code, life_stage, .keep_all = T) %>%
  select(-haul_number_pass_number, -pass_total)

# dat to show sites  for those that have a pass where no fish of those species were captured
# nfc_pass tag used to indicate that this is an abundance estimate
# fish_nfc_tag <- left_join(
#   fish_abund_prep2,
#
#   fish_nfc_prep,
#   by = c('local_name','species_code', 'life_stage', 'haul_number_pass_number', 'pass_total')
# ) %>%
#   tidyr::fill(nfc_pass, .direction = 'up')

# filter(!is.na(nfc_pass)) %>%

# mutate(nfc_pass = case_when(
#   species_code != 'NFC' ~ 'TRUE',
#   T ~ NA_character_))


# calculate abundance for each site regardless of whether a nfc_pass occurred.
fish_abund_prep3 <- left_join(
  fish_abund_prep2 %>%
    group_by(local_name, species_code, life_stage) %>%
    summarise(catch = sum(catch)),

  fish_nfc_tag,

  by = c('local_name', 'species_code', 'life_stage')
)


# add back the size of the sites so we can do a density
fish_abund <- left_join(
  fish_abund_prep3,

  hab_fish_collect_info %>%
    select(local_name,
           # sampling_method,
           # haul_number_pass_number,
           ef_seconds:enclosure) %>%
    distinct(local_name, ef_length_m, .keep_all = T),

  by = c('local_name')
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1),
         density_100m2 = round(catch/area_m2 * 100,1)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)


### density results -----------------------------------------------------------
# need to summarize just the sites
tab_fish_sites_sum <- left_join(
  fish_abund_prep2 %>%
    select(local_name, pass_total) %>%
    distinct(),


  hab_fish_collect_info %>%
    select(local_name,
           ef_length_m:enclosure) %>%
    distinct(),

  by = 'local_name'
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(
  fish_abund_nfc_prep,
  fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_abund_prep4,
  fish_nfc_tag
)

####------------priorities phase 1------
##uses habitat value to initially screen but then refines based on what are likely not barriers to most most the time
# phase1_priorities <- pscis_all %>%
#   filter(!source %ilike% 'phase2') %>% ##we don't want the phase 1 action
#   select(aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) %>%
#   mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
#                                      habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
#                                      habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
#                                      T ~ NA_character_)) %>%
#   mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
#                                      T ~ priority_phase1)) %>%
#   mutate(priority_phase1 = case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
#                                      T ~ priority_phase1)) %>%
#   # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
#   #                                    T ~ priority_phase1)) %>%
#   dplyr::rename(utm_easting = easting, utm_northing = northing)
#
#
# ##turn spreadsheet into list of data frames - why was this pscis_crossing_id?
# pscis_phase1_for_tables <- pscis_all %>%
#   filter(source %ilike% 'phase1') %>%
#   arrange(pscis_crossing_id)
#
# pscis_split <- pscis_phase1_for_tables  %>% #pscis_phase1_reassessments
#   # sf::st_drop_geometry() %>%
#   # mutate_if(is.numeric, as.character) %>% ##added this to try to get the outlet drop to not disapear
#   # tibble::rownames_to_column() %>%
#   dplyr::group_split(pscis_crossing_id) %>%
#   purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)  ##changed to my_crossing_id
#
# ##make result summary tables for each of the crossings
# tab_summary <- pscis_split %>%
#   purrr::map(fpr_make_tab_summary)
#
# tab_summary_comments <- pscis_split %>%
#   purrr::map(fpr_make_tab_summary_comments)
#
# ##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
# tab_photo_url <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>%
#   basename() %>%
#   as_tibble() %>%
#   mutate(value = as.integer(value)) %>%  ##need this to sort
#   dplyr::arrange(value)  %>%
#   mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) %>%
#   filter(value %in% pscis_phase1_for_tables$my_crossing_reference)  %>% ##we don't want all the photos - just the phase 1 photos for this use case!!!
#   left_join(., xref_pscis_my_crossing_modelled, by = c('value' = 'external_crossing_reference'))  %>% ##we need to add the pscis id so that we can sort the same
#   arrange(stream_crossing_id) %>%
#   select(-value) %>%
#   # pull(photo)
#   dplyr::group_split(stream_crossing_id)
#   # purrr::set_names(nm = . %>% bind_rows() %>% arrange(value) %>% pull(stream_crossing_id)) %>%
#   # bind_rows()
#   # arrange(stream_crossing_id) %>%
#   # dplyr::group_split(value)
#
#
# ##these are the reassessments!!!!!
# ##built from funciton in functions.R file
# tabs_phase1 <- mapply(fpr_print_tab_summary_all, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)
#
# ##built from funciton in functions.R file
# tabs_phase1_pdf <- mapply(fpr_print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)
#

##not needed this round
# tab_plan_raw <- readr::read_csv(file = 'data/extracted_inputs/planning_results.csv', guess_max = 1500)
#
# tab_plan_sf <- tab_plan_raw %>%
#   filter(!is.na(my_text)) %>%
#   arrange(stream_crossing_id, modelled_crossing_id) %>%
#   st_as_sf(crs = 3005, coords = c("long", "lat")) %>%
#   st_transform(crs = 4326) %>%
#   mutate(my_priority = case_when(my_priority == 'mod' ~ 'moderate',
#                                  T ~ my_priority)) %>%
#   # dplyr::mutate(image_view_url = case_when(is.na(image_view_url) ~ NA_character_,
#   #                                          T ~ paste0('<a href =', image_view_url,'>', 'PSCIS Image link', '</a>'))) %>%
#   dplyr::mutate(image_view_url = case_when(is.na(image_view_url) ~ NA_character_,
#                                            T ~ paste0('<a href =', image_view_url,' target="_blank">PSCIS Image</a>'))) %>%
#   dplyr::mutate(model_link = paste0('<a href =', 'sum/bcfp/', aggregated_crossings_id, '.html ', 'target="_blank">Model Data</a>')) %>%
#   select(
#          Priority = my_priority,
#          `PSCIS ID` = stream_crossing_id,
#          `Modelled ID` = modelled_crossing_id,
#          `Species` = observedspp_upstr,
#          `Order` = stream_order,
#          `Upstream habitat (km)` = salmon_network_km,
#          `Channel width` = downstream_channel_width,
#          `Habitat value` = habitat_value_code,
#          `Image link` = image_view_url,
#          `Model link` = model_link,
#          Comments = my_text)

##add the priorities to the site data
# hab_site_priorities <- left_join(
#   select(habitat_confirmations_priorities, reference_number, alias_local_name, priority),
#   select(hab_site, reference_number, alias_local_name, site, utm_zone:utm_northing),
#   by = 'reference_number'
# ) %>%
#   filter(!alias_local_name %like% '_ds') %>%
#   select(-alias_local_name)
# filter(!is.na(priority))  ##this is how we did it before.  changed it to get a start on it



# When we need to update our column names according to the new output from bcfishpass.crossings...
# bcfishpass_names_updated_prep <- names(bcfishpass) %>%
#   tibble::as_tibble() %>%
#   rename(bcfishpass = value)
#
# # join to the comments
# bcfishpass_names_updated <- left_join(
#   bcfishpass_names_updated_prep,
#   bcfishpass_column_comments,
#   by = c('bcfishpass' = 'column_name')
# )

# -----------bcfishpass modelling table setup for reporting------
#this is how we line up our new column names and put things in order for reporting on the fish habitat modeling
# we need to update this sometimes.  When we do we update 02_prep_reporting/0160-load-bcfishpass-data.R,
# get the data from  rename the xref_bcfishpass_names tribble to xref_bcfishpass_names_old  and go through the following procedure
# xref_bcfishpass_names_old <- xref_bcfishpass_names


# ## join the new with the old so you can kable(xref_bcfishpass_names_prep) then run in Rmd chunk and copy paste tribble yo
# xref_bcfishpass_names_prep <- left_join(
#   bcfishpass_names_updated,
#   select(xref_bcfishpass_names_old, -column_comment),
#   by = c('bcfishpass')
# ) %>%
#     mutate(report = stringr::str_replace_all(bcfishpass, '_', ' ') %>%
#              stringr::str_to_title() %>%
#              stringr::str_replace_all('Km', '(km)') %>%
#              stringr::str_replace_all('Ha', '(ha)') %>%
#              stringr::str_replace_all('Lakereservoir', 'Lake Reservoir') %>%
#              stringr::str_replace_all('Co ', 'CO ') %>%
#              stringr::str_replace_all('Ch', 'CH ') %>%
#              stringr::str_replace_all('St ', 'ST ') %>%
#              stringr::str_replace_all('Sk ', 'SK ') %>%
#              stringr::str_replace_all('Wct ', 'WCT ') %>%
#              stringr::str_replace_all('Pscis', 'PSCIS') %>%
#              stringr::str_replace_all('Spawningrearing', 'Spawning Rearing') %>%
#              stringr::str_replace_all('Betweenbarriers', 'Between Barriers') %>%
#              stringr::str_replace_all('Belowupstrbarriers', 'Below Barriers')) %>%
#   select(bcfishpass, report, id_join, id_side, column_comment)
    # select(bcfishpass, report, id_join, id_side)

xref_bcfishpass_names <- tibble::tribble(
                                                                         ~bcfishpass,                                                 ~report, ~id_join, ~id_side,                                                                                                                                                                                                                                          ~column_comment,
                                                           "aggregated_crossings_id",                               "Aggregated Crossings Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "stream_crossing_id",                                    "Stream Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                            "dam_id",                                                "Dam Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "user_barrier_anthropogenic_id",                         "User Barrier Anthropogenic Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "modelled_crossing_id",                                  "Modelled Crossing Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossing_source",                                       "Crossing Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_feature_type",                                 "Crossing Feature Type",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "pscis_status",                                          "PSCIS Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "crossing_type_code",                                    "Crossing Type Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "crossing_subtype_code",                                 "Crossing Subtype Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                     "modelled_crossing_type_source",                         "Modelled Crossing Type Source",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                    "barrier_status",                                        "Barrier Status",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "pscis_road_name",                                       "PSCIS Road Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_stream_name",                                     "PSCIS Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "pscis_assessment_comment",                              "PSCIS Assessment Comment",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                             "pscis_assessment_date",                                 "PSCIS Assessment Date",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "pscis_final_score",                                     "PSCIS Final Score",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                  "transport_line_structured_name_1",                      "Transport Line Structured Name 1",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                   "transport_line_type_description",                       "Transport Line Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                "transport_line_surface_description",                    "Transport Line Surface Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                               "ften_forest_file_id",                                   "Ften Forest File Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "ften_file_type_description",                            "Ften File Type Description",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                "ften_client_number",                                    "Ften Client Number",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "ften_client_name",                                      "Ften Client Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                       "ften_life_cycle_status_code",                           "Ften Life Cycle Status Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_track_name",                                       "Rail Track Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "rail_owner_name",                                       "Rail Owner Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                        "rail_operator_english_name",                            "Rail Operator English Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "ogc_proponent",                                         "Ogc Proponent",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "dam_name",                                              "Dam Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                         "dam_owner",                                             "Dam Owner",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                          "utm_zone",                                              "Utm Zone",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                       "utm_easting",                                           "Utm Easting",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "utm_northing",                                          "Utm Northing",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "dbm_mof_50k_grid",                                      "Dbm Mof 50k Grid",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                 "linear_feature_id",                                     "Linear Feature Id",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "blue_line_key",                                         "Blue Line Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                     "watershed_key",                                         "Watershed Key",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                          "downstream_route_measure",                              "Downstream Route Measure",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                      "wscode_ltree",                                          "Wscode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "localcode_ltree",                                       "Localcode Ltree",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                              "watershed_group_code",                                  "Watershed Group Code",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                  "gnis_stream_name",                                      "Gnis Stream Name",       NA,       NA,                                                                                                                                                                                                                                                       NA,
                                                                   "crossings_dnstr",                                       "Crossings Dnstr",       NA,       NA,                                                                                                                                          "List of the aggregated_crossings_id values of crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_dnstr",                          "Barriers Anthropogenic Dnstr",       NA,       NA,                                                                                                                                  "List of the aggregated_crossings_id values of barrier crossings downstream of the given crossing, in order downstream",
                                                      "barriers_anthropogenic_upstr",                          "Barriers Anthropogenic Upstr",       NA,       NA,                                                                                                                                                         "List of the aggregated_crossings_id values of barrier crossings upstream of the given crossing",
                                                "barriers_anthropogenic_dnstr_count",                    "Barriers Anthropogenic Dnstr Count",       NA,       NA,                                                                                                                                                                                      "A count of the barrier crossings downstream of the given crossing",
                                                "barriers_anthropogenic_upstr_count",                    "Barriers Anthropogenic Upstr Count",       NA,       NA,                                                                                                                                                                                        "A count of the barrier crossings upstream of the given crossing",
                                                                      "stream_order",                                          "Stream Order",       NA,       NA,                                                                                                                                                                                                                           "Order of FWA stream at point",
                                                                  "stream_magnitude",                                      "Stream Magnitude",       NA,       NA,                                                                                                                                                                                                                       "Magnitude of FWA stream at point",
                                                                          "gradient",                                              "Gradient",       NA,       NA,                                                                                                                                                                                                                                  "Stream slope at point",
                                                             "access_model_ch_co_sk",                                 "Access Model CH CO Sk",       NA,       NA,                                                                                                                                                                                                             "Modelled accessibility to Salmon (15% max)",
                                                                   "access_model_st",                                       "Access Model St",       NA,       NA,                                                                                                                                                                                                          "Modelled accessibility to Steelhead (20% max)",
                                                                  "access_model_wct",                                      "Access Model Wct",       NA,       NA,                                                                                                                                                  "Modelled accessibility to West Slope Cutthroat Trout (20% max or downstream of known WCT observation)",
                                                                 "observedspp_dnstr",                                     "Observedspp Dnstr",       NA,       NA,                                                                                                                                                                           "Fish species observed downstream of point (on the same stream/blue_line_key)",
                                                                 "observedspp_upstr",                                     "Observedspp Upstr",       NA,       NA,                                                                                                                                                                                                       "Fish species observed anywhere upstream of point",
                                                                "watershed_upstr_ha",                                  "Watershed Upstr (ha)",       NA,       NA,                                                                                                                       "Total watershed area upstream of point (approximate, does not include area of the fundamental watershed in which the point lies)",
                                                                  "total_network_km",                                    "Total Network (km)",       NA,       NA,                                                                                                                                                                                                       "Total length of stream network upstream of point",
                                                                   "total_stream_km",                                     "Total Stream (km)",       NA,       NA,                                                                                                                                                "Total length of streams and rivers upstream of point (does not include network connectors in lakes etc)",
                                                            "total_lakereservoir_ha",                             "Total Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                                                                      "Total area lakes and reservoirs upstream of point",
                                                                  "total_wetland_ha",                                    "Total Wetland (ha)",       NA,       NA,                                                                                                                                                                                                                  "Total area wetlands upstream of point",
                                                 "total_slopeclass03_waterbodies_km",                   "Total Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                            "Total length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass03_km",                               "Total Slopeclass03 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 0-3%",
                                                             "total_slopeclass05_km",                               "Total Slopeclass05 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 3-5%",
                                                             "total_slopeclass08_km",                               "Total Slopeclass08 (km)",       NA,       NA,                                                                                                                                                                        "Total length of stream potentially accessible upstream of point with slope 5-8%",
                                                             "total_slopeclass15_km",                               "Total Slopeclass15 (km)",       NA,       NA,                                                                                                                                                                       "Total length of stream potentially accessible upstream of point with slope 8-15%",
                                                             "total_slopeclass22_km",                               "Total Slopeclass22 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 15-22%",
                                                             "total_slopeclass30_km",                               "Total Slopeclass30 (km)",       NA,       NA,                                                                                                                                                                      "Total length of stream potentially accessible upstream of point with slope 22-30%",
                                               "total_belowupstrbarriers_network_km",                     "Total Below Barriers Network (km)",       NA,       NA,                                                                                                                                     "Total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                "total_belowupstrbarriers_stream_km",                      "Total Below Barriers Stream (km)",       NA,       NA,                                                                              "Total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                         "total_belowupstrbarriers_lakereservoir_ha",              "Total Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                                    "Total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                               "total_belowupstrbarriers_wetland_ha",                     "Total Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                                                "Total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                              "total_belowupstrbarriers_slopeclass03_waterbodies_km",    "Total Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                "Total length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass03_km",                "Total Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                          "total_belowupstrbarriers_slopeclass05_km",                "Total Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                          "total_belowupstrbarriers_slopeclass08_km",                "Total Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                            "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                          "total_belowupstrbarriers_slopeclass15_km",                "Total Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                           "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                          "total_belowupstrbarriers_slopeclass22_km",                "Total Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                          "total_belowupstrbarriers_slopeclass30_km",                "Total Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                                          "Total length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                               "ch_co_sk_network_km",                                 "CH CO SK Network (km)",       NA,       NA,                                                                                                                                             "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point",
                                                                "ch_co_sk_stream_km",                                  "CH CO SK Stream (km)",       NA,       NA,                                                                                      "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                         "ch_co_sk_lakereservoir_ha",                          "CH CO SK Lake Reservoir (ha)",       NA,       NA,                                                                                                                                            "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point",
                                                               "ch_co_sk_wetland_ha",                                 "CH CO SK Wetland (ha)",       NA,       NA,                                                                                                                                                        "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point",
                                              "ch_co_sk_slopeclass03_waterbodies_km",                "CH CO SK Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass03_km",                            "CH CO SK Slopeclass03 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                          "ch_co_sk_slopeclass05_km",                            "CH CO SK Slopeclass05 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                          "ch_co_sk_slopeclass08_km",                            "CH CO SK Slopeclass08 (km)",       NA,       NA,                                                                                                                                           "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                          "ch_co_sk_slopeclass15_km",                            "CH CO SK Slopeclass15 (km)",       NA,       NA,                                                                                                                                          "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                          "ch_co_sk_slopeclass22_km",                            "CH CO SK Slopeclass22 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                          "ch_co_sk_slopeclass30_km",                            "CH CO SK Slopeclass30 (km)",       NA,       NA,                                                                                                                                         "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point with slope 22-30%",
                                            "ch_co_sk_belowupstrbarriers_network_km",                  "CH CO SK Below Barriers Network (km)",       NA,       NA,                                                                                                  "Chinook/Coho/Sockeye salmon model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                             "ch_co_sk_belowupstrbarriers_stream_km",                   "CH CO SK Below Barriers Stream (km)",       NA,       NA,                                           "Chinook/Coho/Sockeye salmon model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                      "ch_co_sk_belowupstrbarriers_lakereservoir_ha",           "CH CO SK Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                 "Chinook/Coho/Sockeye salmon model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                            "ch_co_sk_belowupstrbarriers_wetland_ha",                  "CH CO SK Below Barriers Wetland (ha)",       NA,       NA,                                                                                                             "Chinook/Coho/Sockeye salmon model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                           "ch_co_sk_belowupstrbarriers_slopeclass03_waterbodies_km", "CH CO SK Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                   "Chinook/Coho/Sockeye salmon model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass03_km",             "CH CO SK Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass05_km",             "CH CO SK Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass08_km",             "CH CO SK Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                               "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass15_km",             "CH CO SK Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                              "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass22_km",             "CH CO SK Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                       "ch_co_sk_belowupstrbarriers_slopeclass30_km",             "CH CO SK Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                             "Chinook/Coho/Sockeye salmon model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                     "st_network_km",                                       "ST Network (km)",       NA,       NA,                                                                                                                                                               "Steelhead model, total length of stream network potentially accessible upstream of point",
                                                                      "st_stream_km",                                        "ST Stream (km)",       NA,       NA,                                                                                                        "Steelhead model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                               "st_lakereservoir_ha",                                "ST Lake Reservoir (ha)",       NA,       NA,                                                                                                                                                              "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                     "st_wetland_ha",                                       "ST Wetland (ha)",       NA,       NA,                                                                                                                                                                          "Steelhead model, total area wetlands potentially accessible upstream of point",
                                                    "st_slopeclass03_waterbodies_km",                      "ST Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                                                                 "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass03_km",                                  "ST Slopeclass03 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                                "st_slopeclass05_km",                                  "ST Slopeclass05 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                                "st_slopeclass08_km",                                  "ST Slopeclass08 (km)",       NA,       NA,                                                                                                                                                             "Steelhead model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                                "st_slopeclass15_km",                                  "ST Slopeclass15 (km)",       NA,       NA,                                                                                                                                                            "Steelhead model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                                "st_slopeclass22_km",                                  "ST Slopeclass22 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                                "st_slopeclass30_km",                                  "ST Slopeclass30 (km)",       NA,       NA,                                                                                                                                                           "Steelhead model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                  "st_belowupstrbarriers_network_km",                        "ST Below Barriers Network (km)",       NA,       NA,                                                                                                                    "Steelhead model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                   "st_belowupstrbarriers_stream_km",                         "ST Below Barriers Stream (km)",       NA,       NA,                                                             "Steelhead model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                            "st_belowupstrbarriers_lakereservoir_ha",                 "ST Below Barriers Lake Reservoir (ha)",       NA,       NA,                                                                                                                   "Steelhead model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                  "st_belowupstrbarriers_wetland_ha",                        "ST Below Barriers Wetland (ha)",       NA,       NA,                                                                                                                               "Steelhead model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                 "st_belowupstrbarriers_slopeclass03_waterbodies_km",       "ST Below Barriers Slopeclass03 Waterbodies (km)",       NA,       NA,                                                                                     "Steelhead model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass03_km",                   "ST Below Barriers Slopeclass03 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                             "st_belowupstrbarriers_slopeclass05_km",                   "ST Below Barriers Slopeclass05 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                             "st_belowupstrbarriers_slopeclass08_km",                   "ST Below Barriers Slopeclass08 (km)",       NA,       NA,                                                                                                                 "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                             "st_belowupstrbarriers_slopeclass15_km",                   "ST Below Barriers Slopeclass15 (km)",       NA,       NA,                                                                                                                "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                             "st_belowupstrbarriers_slopeclass22_km",                   "ST Below Barriers Slopeclass22 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                             "st_belowupstrbarriers_slopeclass30_km",                   "ST Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                                               "Steelhead model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "wct_network_km",                                      "WCT Network (km)",     117L,       1L,                                                                                                                                                "Westslope Cuthroat Trout model, total length of stream network potentially accessible upstream of point",
                                                                     "wct_stream_km",                                       "WCT Stream (km)",     115L,       1L,                                                                                         "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point (does not include network connectors in lakes etc)",
                                                              "wct_lakereservoir_ha",                               "WCT Lake Reservoir (ha)",     120L,       1L,                                                                                                                                               "Westslope Cuthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point",
                                                                    "wct_wetland_ha",                                      "WCT Wetland (ha)",     125L,       1L,                                                                                                                                                           "Westslope Cuthroat Trout model, total area wetlands potentially accessible upstream of point",
                                                   "wct_slopeclass03_waterbodies_km",                     "WCT Slopeclass03 Waterbodies (km)",     130L,       1L,                                                                                                                 "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass03_km",                                 "WCT Slopeclass03 (km)",     140L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 0-3%",
                                                               "wct_slopeclass05_km",                                 "WCT Slopeclass05 (km)",     150L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 3-5%",
                                                               "wct_slopeclass08_km",                                 "WCT Slopeclass08 (km)",     160L,       1L,                                                                                                                                             "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 5-8%",
                                                               "wct_slopeclass15_km",                                 "WCT Slopeclass15 (km)",     170L,       1L,                                                                                                                                            "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 8-15%",
                                                               "wct_slopeclass22_km",                                 "WCT Slopeclass22 (km)",     180L,       1L,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 15-22%",
                                                               "wct_slopeclass30_km",                                 "WCT Slopeclass30 (km)",       NA,       NA,                                                                                                                                           "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point with slope 22-30%",
                                                 "wct_belowupstrbarriers_network_km",                       "WCT Below Barriers Network (km)",     117L,       2L,                                                                                                    "Westslope Cutthroat Trout model, total length of stream network potentially accessible upstream of point and below any additional upstream barriers",
                                                  "wct_belowupstrbarriers_stream_km",                        "WCT Below Barriers Stream (km)",     115L,       2L,                                              "Westslope Cuthroat Trout model, total length of streams and rivers potentially accessible upstream of point and below any additional upstream barriers (does not include network connectors in lakes etc)",
                                           "wct_belowupstrbarriers_lakereservoir_ha",                "WCT Below Barriers Lake Reservoir (ha)",     120L,       2L,                                                                                                   "Westslope Cutthroat Trout model, total area lakes and reservoirs potentially accessible upstream of point and below any additional upstream barriers",
                                                 "wct_belowupstrbarriers_wetland_ha",                       "WCT Below Barriers Wetland (ha)",     125L,       2L,                                                                                                               "Westslope Cutthroat Trout model, total area wetlands potentially accessible upstream of point and below any additional upstream barriers",
                                "wct_belowupstrbarriers_slopeclass03_waterbodies_km",      "WCT Below Barriers Slopeclass03 Waterbodies (km)",     130L,       2L,                                                                     "Westslope Cutthroat Trout model, length of stream connectors (in waterbodies) potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass03_km",                  "WCT Below Barriers Slopeclass03 (km)",     140L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 0-3%",
                                            "wct_belowupstrbarriers_slopeclass05_km",                  "WCT Below Barriers Slopeclass05 (km)",     150L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 3-5%",
                                            "wct_belowupstrbarriers_slopeclass08_km",                  "WCT Below Barriers Slopeclass08 (km)",     160L,       2L,                                                                                                 "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 5-8%",
                                            "wct_belowupstrbarriers_slopeclass15_km",                  "WCT Below Barriers Slopeclass15 (km)",     170L,       2L,                                                                                                "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 8-15%",
                                            "wct_belowupstrbarriers_slopeclass22_km",                  "WCT Below Barriers Slopeclass22 (km)",     180L,       2L,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 15-22%",
                                            "wct_belowupstrbarriers_slopeclass30_km",                  "WCT Below Barriers Slopeclass30 (km)",       NA,       NA,                                                                                               "Westslope Cutthroat Trout model, length of stream potentially accessible upstream of point and below any additional upstream barriers, with slope 22-30%",
                                                                    "ch_spawning_km",                                      "CH Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Chinook spawning habitat",
                                                                     "ch_rearing_km",                                       "CH Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Chinook rearing habitat",
                                                 "ch_spawning_belowupstrbarriers_km",                       "CH Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook spawning habitat",
                                                  "ch_rearing_belowupstrbarriers_km",                        "CH Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Chinook rearing habitat",
                                                                    "co_spawning_km",                                      "CO Spawning (km)",       NA,       NA,                                                                                                                                                                         "Length of stream upstream of point modelled as potential Coho spawning habitat",
                                                                     "co_rearing_km",                                       "CO Rearing (km)",       NA,       NA,                                                                                                                                                                          "Length of stream upstream of point modelled as potential Coho rearing habitat",
                                                                     "co_rearing_ha",                                       "CO Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of wetlands upstream of point modelled as potential Coho rearing habitat",
                                                 "co_spawning_belowupstrbarriers_km",                       "CO Spawning Below Barriers (km)",       NA,       NA,                                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho spawning habitat",
                                                  "co_rearing_belowupstrbarriers_km",                        "CO Rearing Below Barriers (km)",       NA,       NA,                                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                  "co_rearing_belowupstrbarriers_ha",                        "CO Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of wetlands upstream of point and below any additional upstream barriers, modelled as potential Coho rearing habitat",
                                                                    "sk_spawning_km",                                      "SK Spawning (km)",       NA,       NA,                                                                                                                                                                      "Length of stream upstream of point modelled as potential Sockeye spawning habitat",
                                                                     "sk_rearing_km",                                       "SK Rearing (km)",       NA,       NA,                                                                                                                                                                       "Length of stream upstream of point modelled as potential Sockeye rearing habitat",
                                                                     "sk_rearing_ha",                                       "SK Rearing (ha)",       NA,       NA,                                                                                                                                                                          "Area of lakes upstream of point modelled as potential Sockeye rearing habitat",
                                                 "sk_spawning_belowupstrbarriers_km",                       "SK Spawning Below Barriers (km)",       NA,       NA,                                                                                                                          "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye spawning habitat",
                                                  "sk_rearing_belowupstrbarriers_km",                        "SK Rearing Below Barriers (km)",       NA,       NA,                                                                                                                           "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                  "sk_rearing_belowupstrbarriers_ha",                        "SK Rearing Below Barriers (ha)",       NA,       NA,                                                                                                                              "Area of lakes upstream of point and below any additional upstream barriers, modelled as potential Sockeye rearing habitat",
                                                                    "st_spawning_km",                                      "ST Spawning (km)",       NA,       NA,                                                                                                                                                                    "Length of stream upstream of point modelled as potential Steelhead spawning habitat",
                                                                     "st_rearing_km",                                       "ST Rearing (km)",       NA,       NA,                                                                                                                                                                     "Length of stream upstream of point modelled as potential Steelhead rearing habitat",
                                                 "st_spawning_belowupstrbarriers_km",                       "ST Spawning Below Barriers (km)",       NA,       NA,                                                                                                                        "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead spawning habitat",
                                                  "st_rearing_belowupstrbarriers_km",                        "ST Rearing Below Barriers (km)",       NA,       NA,                                                                                                                         "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Steelhead rearing habitat",
                                                                   "wct_spawning_km",                                     "WCT Spawning (km)",     090L,       1L,                                                                                                                                                          "Length of stream upstream of point modelled as potential Westslope Cutthroat spawning habitat",
                                                                    "wct_rearing_km",                                      "WCT Rearing (km)",     100L,       1L,                                                                                                                                                           "Length of stream upstream of point modelled as potential Westslope Cutthroat rearing habitat",
                                                "wct_spawning_belowupstrbarriers_km",                      "WCT Spawning Below Barriers (km)",     090L,       2L,                                                                                                              "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat spawning habitat",
                                                 "wct_rearing_belowupstrbarriers_km",                       "WCT Rearing Below Barriers (km)",     100L,       2L,                                                                                                               "Length of stream upstream of point and below any additional upstream barriers, modelled as potential Westslope Cutthroat rearing habitat",
                                                                   "all_spawning_km",                                     "All Spawning (km)",       NA,       NA,                                                                                                                                                        "Length of stream upstream of point modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                "all_spawning_belowupstrbarriers_km",                      "All Spawning Below Barriers (km)",       NA,       NA,                                                                                                                                                         "Length of stream upstream of point modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                                    "all_rearing_km",                                      "All Rearing (km)",       NA,       NA,                                                                                                            "Length of stream upstream of point and below any additional upstream barriers, modelled as potential spawning habitat (all CH,CO,SK,ST,WCT)",
                                                 "all_rearing_belowupstrbarriers_km",                       "All Rearing Below Barriers (km)",       NA,       NA,                                                                                                             "Length of stream upstream of point and below any additional upstream barriers, modelled as potential rearing habitat (all CH,CO,SK,ST,WCT)",
                                                            "all_spawningrearing_km",                             "All Spawning Rearing (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                         "all_spawningrearing_belowupstrbarriers_km",              "All Spawning Rearing Below Barriers (km)",       NA,       NA,                                                                                                                                                                                           "Length of all spawning and rearing habitat upstream of point",
                                                    "wct_betweenbarriers_network_km",                     "WCT Between Barriers Network (km)",       NA,       NA,                                                                                                            "Westslope Cutthroat Trout model, total length of potentially accessible stream network between crossing and all in-stream adjacent barriers",
                                                   "wct_spawning_betweenbarriers_km",                    "WCT Spawning Between Barriers (km)",       NA,       NA,                                                                                                                                 "Westslope Cutthroat Trout model, total length of spawning habitat between crossing and all in-stream adjacent barriers",
                                                    "wct_rearing_betweenbarriers_km",                     "WCT Rearing Between Barriers (km)",       NA,       NA,                                                                                                                                  "Westslope Cutthroat Trout model, total length of rearing habitat between crossing and all in-stream adjacent barriers",
                                            "wct_spawningrearing_betweenbarriers_km",            "WCT Spawning Rearing Between Barriers (km)",       NA,       NA,                                                                                                                     "Westslope Cutthroat Trout model, total length of spawning and rearing habitat between crossing and all in-stream adjacent barriers",
                                                   "all_spawningrearing_per_barrier",                      "All Spawning Rearing Per Barrier",       NA,       NA, "If the given barrier and all barriers downstream were remediated, the amount of connected spawning/rearing habitat that would be added, per barrier. (ie the sum of all_spawningrearing_belowupstrbarriers_km for all barriers, divided by n barriers)"
                           )

####-----------overview table------------

# tab_overview_prep1 <- pscis_phase2 %>%
#   select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, habitat_value)
#
# tab_overview_prep2 <- habitat_confirmations_priorities %>%
#   filter(location == 'us') %>%
#   select(site, species_codes, upstream_habitat_length_m, priority, comments) %>%
#   mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))
#
# tab_overview <- left_join(
#   tab_overview_prep1,
#   tab_overview_prep2,
#   by = c('pscis_crossing_id' = 'site')
# ) %>%
#   mutate(utm = paste0(round(easting,0), ' ', round(northing,0))) %>%
#   select(`PSCIS ID` = pscis_crossing_id,
#          Stream = stream_name,
#          Road = road_name,
#          Tenure = road_tenure,
#          `UTM (11U)` = utm,
#          `Fish Species` = species_codes,
#          `Habitat Gain (km)` = upstream_habitat_length_km,
#          `Habitat Value` = habitat_value,
#          Priority = priority,
#          Comments = comments )
# # mutate(test = paste0('[', Site, ']', '(Appendix 1 - Site Assessment Data and Photos)'))##hmm.. thought this worked
# # %>%
# #   replace(., is.na(.), "-")
#
#
# rm(tab_overview_prep1, tab_overview_prep2)
#
# ####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site %>%
    select(alias_local_name,
           site,
           avg_channel_width_m,
           avg_wetted_width_m,
           average_residual_pool_depth_m,
           average_gradient_percent,
           total_cover),

  habitat_confirmations_priorities %>%
    select(local_name,
           length_surveyed,
           hab_value),

  by = c('alias_local_name' = 'local_name')
) %>%
  arrange(alias_local_name) %>%
  select(Site = alias_local_name,
         `Length Surveyed (m)` = length_surveyed,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value) %>%
  # filter out non ef sites for now, can undo later if needed
  filter(Site != "197534_us4") %>%
  filter(Site != "197534_us5")

# # -------------------------map tables
#
# ##we need an sf object with details for the interactive map
# ##prep the location data
# hab_loc_prep <- left_join(
#   hab_loc %>%
#     tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
#     filter(!alias_local_name %ilike% 'ef' &
#              alias_local_name %ilike% 'us') %>%
#     mutate(site = as.integer(site)),
#   select(filter(habitat_confirmations_priorities, location == 'us'),
#          site, priority, comments),
#   by = 'site'
# )
#
#
# ##need to populate the coordinates before this will work
# ####please note that the photos are only in those files ecause they are referenced in other parts
# ##of the document
# tab_hab_map <- left_join(
#   tab_cost_est_phase2 %>% filter(source %like% 'phase2'),
#   hab_loc_prep %>% select(site, priority, utm_easting, utm_northing, comments),
#   by = c('pscis_crossing_id' = 'site')
# )   %>%
#   sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
#                crs = 26911, remove = F) %>%
#   sf::st_transform(crs = 4326) %>%
#   ##changed this to docs .html from fig .png
#   # mutate(data_link = paste0('<a href =',
#   #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/docs/sum/', pscis_crossing_id,
#   #                           '.html', '>', 'data link', '</a>')) %>%
#   mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
#   mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/master/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
#                              'target="_blank">Culvert Photos</a>')) %>%
#   mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# # mutate(photo_link = paste0('<a href =', 'data/photos/', pscis_crossing_id,
# #                           '/crossing_all.JPG', '>', 'Photos', '>New Tab</a>'))
# # mutate(data_link = paste0('[data](fig/sum/', pscis_crossing_id, '.png)')) %>%
# # mutate(photo_link = paste0('<a href =',
# #                            'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', pscis_crossing_id,
# #                            '/crossing_all.JPG', '>', 'photo link', '</a>'))
#
#
# #--------------need to review if this is necessary
# tab_map_prep <- left_join(
#   pscis_all %>%
#     sf::st_as_sf(coords = c("easting", "northing"),
#                  crs = 26911, remove = F) %>% ##don't forget to put it in the right crs buds
#     sf::st_transform(crs = 4326), ##convert to match the bcfishpass format,
#   phase1_priorities %>% select(-utm_zone:utm_northing, -my_crossing_reference, priority_phase1, -habitat_value, -barrier_result), # %>% st_drop_geometry()
#   by = 'pscis_crossing_id'
# )
#
# # mutate(data_link = paste0('<a href =', 'sum/', pscis_crossing_id,
# #                           '.html', '>', 'Data', '>New Tab</a>'))
#
# tab_map <- tab_map_prep %>%
#   # mutate(pscis_crossing_id = as.character(pscis_crossing_id),
#   #        my_crossing_reference = as.character(my_crossing_reference)) %>%
#   # mutate(ID = case_when(
#   #   !is.na(pscis_crossing_id) ~ pscis_crossing_id,
#   #   T ~ paste0('*', my_crossing_reference
#   #   ))) %>%
#   # sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
#   #              crs = 26911, remove = F) %>%
#   # sf::st_transform(crs = 4326) %>%
#   mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
#                                      T ~ priority_phase1)) %>%
#   mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) %>%
#   mutate(photo_link = paste0('<a href =', 'data/photos/', amalgamated_crossing_id, '/crossing_all.JPG ',
#                              'target="_blank">Culvert Photos</a>')) %>%
#   mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>'))
# # mutate(data_link = paste0('<a href =',
# #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/fig/sum/', pscis_crossing_id,
# #                           '.png', '>', 'data link', '</a>')) %>%
# # dplyr::mutate(photo_link = paste0('<a href =',
# #                                   'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/data/photos/', amalgamated_crossing_id,
# #                                   '/crossing_all.JPG', '>', 'photo link', '</a>'))
#
#
#
#
#
# ##clean up the objects
# rm(hab_site_prep,
#    # hab_fish_indiv_prep,
#    # hab_fish_indiv_prep2,
#    hab_fish_collect_prep2,
#    hab_loc2)

# ----------FHAP---------------------------
# make summary table for site area by reach, site, hu
fhap_hu_sum_rsh <- fhap_hu %>%
  mutate(area_hu = habitat_unit_length_m * mean_width_wetted_m) %>%
  group_by(location_site, location_reach_number, habitat_unit_type) %>%
  summarise(area = sum(area_hu))

# make summary table for area by reach, hu
fhap_hu_sum_rh <- fhap_hu %>%
  mutate(area_hu = habitat_unit_length_m * mean_width_wetted_m) %>%
  group_by(location_reach_number, habitat_unit_type) %>%
  summarise(area = sum(area_hu))

# make summary table for area by reach and site
fhap_hu_sum_rs <- fhap_hu %>%
  mutate(area_hu = habitat_unit_length_m * mean_width_wetted_m) %>%
  group_by(location_site, location_reach_number) %>%
  summarise(area_total = sum(area_hu))

# make summary table for area by reach
fhap_hu_sum_r <- fhap_hu %>%
  mutate(area_hu = habitat_unit_length_m * mean_width_wetted_m) %>%
  group_by(location_reach_number) %>%
  summarise(area_total = sum(area_hu))

# get percentage by site
fhap_hu_perc_s <- left_join(
  fhap_hu_sum_rsh,
  fhap_hu_sum_rs,
  by = c('location_site', 'location_reach_number')
) %>%
  mutate(perc = round(area/area_total * 100, 0))

# get percentage by reach
fhap_hu_perc_r <- left_join(
  fhap_hu_sum_rh,
  fhap_hu_sum_r,
  by = c('location_reach_number')
) %>%
  mutate(perc = round(area/area_total * 100, 0))


# get percentage by reach pivoted to show reaches side by side
fhap_hu_perc_rp <- fhap_hu_perc_r %>%
  select(-area, -area_total) %>%
  pivot_wider(names_from = location_reach_number,
              names_prefix = "Reach_",
              values_from = perc) %>%
  arrange(desc(Reach_1))

# make a graph to show the HU results by reach
fhap_p_hu <- fhap_hu_perc_r %>%
  ggplot(aes(x = location_reach_number, y = perc)) +
  geom_bar(stat = "identity")+
  facet_wrap(~habitat_unit_type, scales = "fixed") +
  ggdark::dark_theme_bw()
fhap_p_hu

# make a table summarizing length of size, ave channel width, total area, lwd and lwd/bankfull width
# this needs work bc we need the number of bankful channel widths by dividing total reach lenght by the mean bankfull channel width
fhap_hu_lwal <- fhap_hu %>%
  rowwise() %>%
  mutate(area_hu = location_distance_m * mean_width_wetted_m,
         lwd_fun = sum(across(starts_with("functional")), na.rm = T),
  ) %>%
  filter(habitat_unit_cat == 1) %>%
  group_by(location_site) %>%
  reframe(
    site_length = round(sum(habitat_unit_length_m),0),
    avg_chan_width = round(ave(mean_width_bankfull_m),1),
    area_total_m2 = round(sum(area_hu),0),
    lwd_func = sum(lwd_fun),
    chan_width_per_site = site_length/avg_chan_width,
    lwd_func_bw = round(lwd_func/chan_width_per_site,1)
  ) %>%
  distinct()

# need to summarize percent by site
fhap_hu_perc_s_sum <- fhap_hu_perc_s %>%
  ungroup() %>%
  select(-area, -location_reach_number, -area_total) %>%
  group_by(location_site) %>%
  pivot_wider(names_from = habitat_unit_type,
              values_from = perc)

# overall summary table
fhap_hu_sum <- left_join(
  fhap_hu_lwal %>% select(-chan_width_per_site),
  fhap_hu_perc_s_sum,
  by = 'location_site'
)

# rename column names of fhap_site object for report table
tab_fhap_site <- fhap_site %>%
  purrr::set_names(nm = xref_fhap_site %>% pull(report))

# rename column names of fhap_hu_sum
tab_fhap_hu_sum <- fhap_hu_sum %>%
  purrr::set_names(nm = xref_fhap_hu_sum %>% pull(report))


#------Hydrometrics-----

# import csv
# hydrometrics <- read_csv('data/hydrometrics.csv')
#
# conn <- rws_connect("data/bcfishpass.sqlite")
# rws_list_tables(conn)
# rws_drop_table("hydrometrics", conn = conn)
# rws_write(hydrometrics, exists = F, delete = TRUE,
#            conn = conn, x_name = "hydrometrics")
# rws_disconnect(conn)
