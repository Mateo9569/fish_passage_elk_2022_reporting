


##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}


wkb_col_names <-  function(wkb,
                           slice_from = 1,
                           slice_to = 2,
                           max_col = NULL
){
  a <- wkb %>%
    slice(slice_from:slice_to) %>%
    # rownames_to_column() %>%
    t() %>%
    tibble::as_tibble() %>%
    tidyr::fill(V1, .direction = 'down') %>%
    tidyr::fill(V2, .direction = 'down') %>%
    dplyr::mutate(across(everything(), .fns = ~replace_na(.,'')))
  if(slice_from != slice_to){
    a <- a %>% dplyr::mutate(col_names = paste0(V1, V2))
  }else a <- a %>% dplyr::mutate(col_names = V1)
  # replace_na(list(V2 = "unknown")) %>%
  # (col_names = paste0(V1, V2)) %>%
  b <- a %>% pull(col_names) %>%
    janitor::make_clean_names()
  if(!is.null(max_col)){length(b) <- max_col}
  return(b)
}

xref_fhap_hu <- tibble::tribble(
                                             ~spdsht,                               ~report, ~type_readxl, ~id_join, ~id_side,
                                     "location_site",                                "Site",       "text",       NA,       NA,
                                 "location_waypoint",                            "Waypoint",       "text",       NA,       NA,
                             "location_reach_number",                        "Reach Number",       "text",       NA,       NA,
                               "location_distance_m",                        "Distance (m)",    "numeric",       NA,       NA,
                                 "habitat_unit_type",                   "Habitat Unit Type",       "text",       NA,       NA,
                                  "habitat_unit_cat",                    "Habitat Unit Cat",       "text",       NA,       NA,
                             "habitat_unit_length_m",             "Habitat Unit Length (m)",    "numeric",       NA,       NA,
                     "habitat_unit_gradient_percent",       "Habitat Unit Gradient Percent",    "numeric",       NA,       NA,
                             "mean_depth_bankfull_m",             "Mean Depth Bankfull (m)",    "numeric",       NA,       NA,
                                "mean_depth_water_m",                "Mean Depth Water (m)",    "numeric",       NA,       NA,
                              "mean_depth_water_m_2",               "Mean Depth Water (m)2",    "numeric",       NA,       NA,
                              "mean_depth_water_m_3",               "Mean Depth Water (m)3",    "numeric",       NA,       NA,
                             "mean_width_bankfull_m",             "Mean Width Bankfull (m)",    "numeric",       NA,       NA,
                               "mean_width_wetted_m",               "Mean Width Wetted (m)",    "numeric",       NA,       NA,
                            "pools_only_max_depth_m",            "Pools Only Max Depth (m)",    "numeric",       NA,       NA,
                                "pools_only_crest_m",                "Pools Only Crest (m)",    "numeric",       NA,       NA,
                             "pools_only_residual_m",             "Pools Only Residual (m)",    "numeric",       NA,       NA,
                              "pools_only_pool_type",                "Pools Only Pool Type",       "text",       NA,       NA,
                             "bed_material_type_dom",               "Bed Material Type Dom",    "numeric",       NA,       NA,
                         "bed_material_type_sub_dom",           "Bed Material Type Sub Dom",    "numeric",       NA,       NA,
                 "bed_material_type_spawning_gravel",   "Bed Material Type Spawning Gravel",    "numeric",       NA,       NA,
               "bed_material_type_spawning_gravel_2", "Bed Material Type Spawning Gravel 2",    "numeric",       NA,       NA,
                   "total_lwd_tally_spawning_gravel",     "Total Lwd Tally Spawning Gravel",       "text",       NA,       NA,
                             "functional_lwd10_20cm",               "Functional Lwd10 20cm",    "numeric",       NA,       NA,
                             "functional_lwd20_50cm",               "Functional Lwd20 50cm",    "numeric",       NA,       NA,
                               "functional_lwd_50cm",                 "Functional Lwd 50cm",    "numeric",       NA,       NA,
                                        "cover_type",                          "Cover Type",       "text",       NA,       NA,
                                     "cover_percent",                       "Cover Percent",    "numeric",       NA,       NA,
                                      "cover_type_2",                        "Cover Type 2",       "text",       NA,       NA,
                                   "cover_percent_2",                     "Cover Percent 2",    "numeric",       NA,       NA,
                           "offchannel_habitat_type",             "Offchannel Habitat Type",       "text",       NA,       NA,
                         "offchannel_habitat_access",           "Offchannel Habitat Access",       "text",       NA,       NA,
                       "offchannel_habitat_length_m",       "Offchannel Habitat Length (m)",    "numeric",       NA,       NA,
                   "disturbance_indicators_length_m",   "Disturbance Indicators Length (m)",    "numeric",       NA,       NA,
                 "disturbance_indicators_length_m_2",  "Disturbance Indicators Length (m)2",    "numeric",       NA,       NA,
                 "disturbance_indicators_length_m_3",  "Disturbance Indicators Length (m)3",    "numeric",       NA,       NA,
                          "riparian_vegetation_type",            "Riparian Vegetation Type",       "text",       NA,       NA,
                     "riparian_vegetation_structure",       "Riparian Vegetation Structure",       "text",       NA,       NA,
                "riparian_vegetation_canopy_closure",  "Riparian Vegetation Canopy Closure",       "text",       NA,       NA,
                                    "notes_barriers",                            "Barriers",       "text",       NA,       NA,
                                    "notes_comments",                            "Comments",       "text",       NA,       NA
               )

xref_fhap_site <- tibble::tribble(
                           ~spdsht,                          ~report, ~type_readxl, ~id_join, ~id_side,
                   "location_site",                           "Site",       "text",       NA,       NA,
           "location_reach_number",                   "Reach",       "text",       NA,       NA,
              "location_watershed",                      "Watershed",       "text",       NA,       NA,
              "location_sub_basin",                      "Sub Basin",       "text",       NA,       NA,
         "location_waypoint_start",                 "Waypoint Start",       "text",       NA,       NA,
           "location_waypoint_end",                   "Waypoint End",       "text",       NA,       NA,
            "location_utm_easting",                        "Easting",    "numeric",       NA,       NA,
           "location_utm_northing",                       "Northing",    "numeric",       NA,       NA,
        "location_utm_easting_end",                    "Easting End",    "numeric",       NA,       NA,
       "location_utm_northing_end",                   "Northing End",    "numeric",       NA,       NA,
       "general_survey_date_start",                    "Survey Date",       "date",       NA,       NA,
         "general_survey_date_end",                "Survey Date End",       "date",       NA,       NA,
                 "general_weather",                        "Weather",       "text",       NA,       NA,
              "general_surveyor_1",                     "Surveyor 1",       "text",       NA,       NA,
              "general_surveyor_2",                     "Surveyor 2",       "text",       NA,       NA,
              "general_surveyor_3",                     "Surveyor 3",       "text",       NA,       NA,
          "general_discharge_m3_s",                 "Discharge M3 S",    "numeric",       NA,       NA,
                "general_comments",                       "Comments",       "text",       NA,       NA,
   "subsampling_fractions_riffles",  "Subsampling Fractions Riffles",    "numeric",       NA,       NA,
     "subsampling_fractions_pools",    "Subsampling Fractions Pools",    "numeric",       NA,       NA,
    "subsampling_fractions_glides",   "Subsampling Fractions Glides",    "numeric",       NA,       NA,
  "subsampling_fractions_cascades", "Subsampling Fractions Cascades",    "numeric",       NA,       NA,
     "subsampling_fractions_other",    "Subsampling Fractions Other",    "numeric",       NA,       NA
  )

xref_fhap_hu_sum <- tibble::tribble(
                             ~spdsht,                      ~report,
                     "location_site",                       "Site",
                       "site_length",        "Length Surveyed (m)",
                    "avg_chan_width",  "Average Channel Width (m)",
                     "area_total_m2",            "Total Area (m2)",
                          "lwd_func",       "Total Functional LWD",
                       "lwd_func_bw",     "LWD per Bankfull Width",
                                 "C",                    "Cascade",
                                 "G",                      "Glide",
                                 "P",                       "Pool",
                                 "R",                     "Riffle"
)



