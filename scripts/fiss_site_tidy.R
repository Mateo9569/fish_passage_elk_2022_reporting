source('R/packages.R')

# things to note
# the utms are from the form and not the object - although they should be the same some of the sites where moved so those will be wrong
# later we can pull the coordinates from the


# import a geopackage and rearrange then burn to csv

# relative path to the q project form (assuming we havre same folder structure) -
# use an absolute path if you have to but it is preferable to keep our relative paths the same so we can collab

# name the project directory we are pulling from
dir_project <- 'bcfishpass_elkr_20220904'

# there are tables with no new events and the types are mangels so remove dataframes with only 1 feature
form_names_to_remove <- c('form_fiss_site_20220923', 'form_fiss_site_20221006', 'form_fiss_site_202209100809')

# list all the fiss form names in the file
form_names_l <- list.files(path = paste0('../../gis/mergin/',
                                         dir_project),
                           # ?glob2rx is a funky little unit
                           pattern = glob2rx('*site_2022*.gpkg'),
                           full.names = T
                           ) %>%
  stringr::str_subset('form_fiss_site_20220923', negate = T) %>%
  stringr::str_subset('form_fiss_site_20221006', negate = T) %>%
  stringr::str_subset('form_fiss_site_202209100809', negate = T)

# read all the forms into a list of dataframes using colwise to guess the column types
# if we don't try to guess the col types we have issues later with the bind_rows join
form <- form_names_l %>%
  purrr::map(sf::st_read) %>%
  purrr::map(plyr::colwise(type.convert)) %>%
  # name the data.frames so we can add it later as a "source" column - we use basename to leave the filepath behind
  purrr::set_names(nm = basename(form_names_l)) %>%
  bind_rows(.id = 'source') %>%
  readr::write_csv(paste0('data/inputs_extracted/mergin_backups/form_fiss_site_raw_', format(lubridate::now(), "%Y%m%d")))

# see the names of our form
names(form)

# let's get the names of the input template
# there is lots of work to do to pull out all the information we can use so we will start with one small step at a time
# lets just populate the location and site info pages for now and then move on to the other information later.  It should be ok but might take some gymnastics
form_raw_names_site <- fpr::fpr_import_hab_con(backup = F) %>%
  # pull out just the site info page for now
  pluck(4) %>%
  #see what the types are
#glimpse(form_raw_names_site)

  # only keep the names of the columns
  names()

# location names
form_raw_names_location <- fpr::fpr_import_hab_con(backup = F) %>%
  # pull out just the site info page for now
  pluck(1) %>%
  # only keep the names of the columns
  names()

# we don't want duplicate column names because it messes with them (renames them both) so we need to get rid of dupes
names_dup <- intersect(form_raw_names_site, form_raw_names_location)

# join the names of our two target tables together without (!) the dupes
form_raw_names_sl <- c(form_raw_names_location,
                       form_raw_names_site[!form_raw_names_site %in% names_dup])


# tidy our populated table to PASTE SPECIAL value only!!! to our template. Might need to be in chunks but so be it
# we also want to know who did the assessments and when to be able to sort the input order and populate the habitat_confirmation_priorities spreadsheet so we will add the who and extract the when
# we also want the second comment field and have it appended to the first
# use names(form) to see options
form_site_info_prep <- form %>%
  dplyr::filter(!is.na(date_time_start)) %>%
  dplyr::select(mergin_user,
                date_time_start,
                dplyr::contains('surveyor'),
                dplyr::any_of(form_raw_names_sl),
                comments_2) %>%
  # split out the date and the time - change type of column first
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                survey_date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # arrange by surveyor and time
  dplyr::arrange(mergin_user,
                 date_time_start) %>%
  #give each row and index so we can filter
  tibble::rowid_to_column()

# make the loc form
form_loc <- bind_rows(

  # we need the raw form or we don't have all the right columns
  fpr::fpr_import_hab_con(backup = F) %>%
    # pull out just the site info page for now
    pluck(1) %>%
    # need to convert type for some reason (should be guessed already..)
    mutate(survey_date = lubridate::as_date(survey_date), alias_local_name = as.character(alias_local_name)) %>%
    slice(0),

  form_site_info_prep %>%
    # alias local name is not called the same in both sheets so rename
    rename(alias_local_name = local_name) %>%
    select(rowid,
           dplyr::any_of(form_raw_names_location),
           # add the time to help put the puzzle together after
           time)
)

# make the site form
form_site <- bind_rows(

  # we need the raw form or we don't have all the right columns
  fpr::fpr_import_hab_con(backup = F) %>%
    # pull out just the site info page for now - should use the name but will use index bc short on time
    pluck("step_4_stream_site_data") %>%
    mutate(feature_height_length_method = as.character(feature_height_length_method),
           l_bank_texture_dominant_2 = as.logical(l_bank_texture_dominant_2),
           r_bank_texture_dominant_2 = as.logical(r_bank_texture_dominant_2),
           morphology = as.logical(morphology)) %>%
    slice(0),

  form_site_info_prep %>%
    select(rowid,
           dplyr::any_of(form_raw_names_site),
           # add the time to help put the puzzle together after)
           survey_date,
           time,
           comments_2)
) %>%
  # join the comments
  dplyr::mutate(comments = case_when(
    !is.na(comments_2) ~ paste0(comments, '. ', comments_2, '. ', time),
    T ~ paste0(comments, '. ', time))) %>%
  select(rowid, everything())

# burn to file

form_loc %>%
  readr::write_csv(paste0(
    'data/dff/form_fiss_loc_tidy_',
    format(lubridate::now(), "%Y%m%d"),
    '.csv'),
    na = '')

form_site %>%
  readr::write_csv(paste0(
    'data/dff/form_fiss_site_tidy_',
    format(lubridate::now(), "%Y%m%d"),
    '.csv'),
    na = '')



