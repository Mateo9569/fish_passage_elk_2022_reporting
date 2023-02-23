# Each section should stand on its own and you can navigate around using the control/command + shift + O
source('scripts/packages.R')

##lets use magick to:
## back up original photos ont he d drive
##resize teh photos in our main file
##convert our pngs to jpg
##make a composite image of all the most relevant culvert shots for easy display in leaflet yo
##get rid of the AAE files from the iphone.

##back up your photos onto the D drive.

##create a folder to copy the photos to


##get the name of the folder we are in
bname <- basename(dirname(dirname(getwd())))
name_script_project <- 'fish_passage_skeena_2022_reporting'

##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "al"


#############################################Not Run#############################
# this is the workflow for next time but for now we are not messing with it
##this is a bit different from before. We are going to start with the backed up originals and put them in our
# scripts folder resized and renamed
targetdir = paste0("C:/Users/allan/OneDrive/New_Graph/Current/",
                   bname,
                   '/scripts/',
                   name_script_project,
                   '/data/photos/', filename)
dir.create(targetdir)


##path to the photos
path <- paste0("D:/New_Graph/backups/photos/", bname, "/", filename)

filestocopy <- list.files(path = path,
                          full.names = T)

#copy over the photos in the al folder -- this is done already
file.copy(from=filestocopy, to=targetdir,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)

###########################################NOT RUN TO HERE END###############################################################

# backup kyle---------------------------------------------
##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "kyle"

##here we back everything up to the D drive
targetdir = paste0("D:/New_Graph/backups/photos/", bname, "/")
dir.create(targetdir)

targetdir = paste0("D:/New_Graph/backups/photos/", bname, "/", filename)
dir.create(targetdir)

##path to the photos
path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)

filestocopy <- list.files(path = path,
                          full.names = T)


file.copy(from=filestocopy, to=targetdir,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)

##this scales everything and converts everything to jpg - we had problems with sizes being slightly too large last year
##so we will do manually this year with powertools type thing (image resizer for windows)
# img_resize_convert <- function(img){
#   image <- image_read(img)
#   image_scaled <- image_scale(image,"1440x1080!")
# image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
# }

# backup dallas----------------------------------------------------
##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "dallas"

##here we back everything up to the D drive
targetdir = paste0("D:/New_Graph/backups/photos/", bname, "/")
dir.create(targetdir)

targetdir = paste0("D:/New_Graph/backups/photos/", bname, "/", filename)
dir.create(targetdir)

##path to the photos
path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)

filestocopy <- list.files(path = path,
                          full.names = T)


file.copy(from=filestocopy, to=targetdir,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)




# convert al------------------------------------------------------------
## we  want to convert our png to jpeg in case we want them for something
img_resize_convert <- function(img){
  image <- image_read(img)
  image_scaled <- image_scale(image,"1440x1080!")
  image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(img)), '.JPG'), format = 'jpg')
}

##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "al"
##path to the photos
path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)
filestocopy <- list.files(path = path,
                          full.names = T)
filestoconvert <- grep('.PNG', filestocopy, value=TRUE)
filestoconvert %>%
  purrr::map(img_resize_convert)
############ remove the png files that are now converted to jpg
##identify all the png files in the folder
filesremove <- grep('.PNG', filestocopy, value=TRUE)
file.remove(filesremove)

# convert kyle-------------------------------------------------------------------
##input the name of the file we want to copy.  We should get a list of files next time and then purrr::map the procedure
filename = "kyle"
##path to the photos
path <- paste0("C:/Users/allan/OneDrive/New_Graph/Current/", bname, '/data/photos/', filename)
filestocopy <- list.files(path = path,
                          full.names = T)
filestoconvert <- grep('.PNG', filestocopy, value=TRUE)
filestoconvert %>%
  purrr::map(img_resize_convert)
############ remove the png files that are now converted to jpg
##identify all the png files in the folder
filesremove <- grep('.PNG', filestocopy, value=TRUE)
file.remove(filesremove)



# make folders ------------------------------------------------------------

##get the names of your pscis files
workbooks <-  list.files(path = 'data', pattern = "pscis", all.files = F) %>%
  grep(pattern = '~', invert = T, value = T)


pscis_all <- workbooks %>%
  map_df(fpr_import_pscis)

##create the data folder
dir.create(paste0(getwd(), '/data'))

##create the photos folder
dir.create(paste0(getwd(), '/data/photos'))


folderstocreate <- pscis_all %>%
  filter(!is.na(my_crossing_reference)) %>%
  distinct(my_crossing_reference) %>%
  pull(my_crossing_reference) %>%
  as.character()


folderstocreate %>%
  purrr::map(fpr_photo_folders)

##do the same for our pscis crossings
folderstocreate <- pscis_all %>%
  filter(!is.na(pscis_crossing_id)) %>%
  distinct(pscis_crossing_id) %>%
  pull(pscis_crossing_id) %>%
  as.character()


folderstocreate %>%
  purrr::map(fpr_photo_folders)



## special directories ---------------------------------------------------
folders_special_cases <- c(197662)  ##and some we are hacking in so we don't need to run the whole file

folders_special_cases %>%
  purrr::map(fpr::fpr_photo_folders)



# sort photos to folders --------------------------------------------------
pscis_all <- fpr::fpr_import_pscis_all() %>%
  dplyr::bind_rows()

# ensure you have a surveyor for every crossing
test <- pscis_all %>% filter(is.na(crew_members))


# ensure you have a time for every crossing for all the people you are sorting for
# we only exported fpr_photo_time_prep for this test
test <- fpr::fpr_photo_time_prep() %>%
  filter(
    is.na(date_time_start) &
      (camera_id == 'AI' |
      camera_id == 'KP')
           )


##lets pass it a list of folders to do the sorting on
ls_folders <- c("C:/Users/matwi/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/photos_copy/AI",
                 "C:/Users/matwi/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/photos_copy/MW",
                "C:/Users/matwi/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/photos_copy/mergin"
)


# this should of happened already but it didn't so we need to convert jpeg to JPG to avoid issues later
# find the jpeg files in kyle's folder

# path <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/KP"
# jpegs_jpgs <- list.files(path, full.names = T) %>%
#   grep(pattern = '.*\\.(jpg|jpeg)$',
#         value = T)
#
# jpegs_jpgs %>%
#   purrr::map(fpr::fpr_photo_resize_convert, path = path)
#
# basename(jpegs_jpgs)
#
# # remove the old files
# file.remove(jpegs_jpgs)

# get the photo metadata
photo_meta <- ls_folders %>%
  map(fpr::fpr_photo_sort_metadat) %>%
  purrr::set_names(nm = basename(ls_folders)) %>%
  bind_rows(.id = 'camera_id') %>%
  tibble::rowid_to_column()

# define surveyors
ls_surveyors = c('AI', 'MW', 'mergin')


##we have a few special cases no lets make some conditions.  These are shots of the cards.
# probably not worth doing this again as it is a bit time consuming and it doesn't really matter if the files move.
# photo_ids_dont_copy01 <- c(
#   paste0('KP_IMG_', 0262:0277, '.JPG'),
#   paste0('KP_IMG_', 0492:0515, '.JPG'),
#   paste0('KP_IMG_', 0678:0694, '.JPG'),
#   paste0('KP_IMG_', 0931:0947, '.JPG'),
#   paste0('KP_IMG_', 1171:1180, '.JPG'),
#   paste0('KP_IMG_', 1491:1498, '.JPG'),
#   paste0('KP_IMG_', 2148:2141, '.JPG'),
#   paste0('KP_IMG_', 2703:2714, '.JPG'),
#   paste0('AI_IMG_', 5300:5377, '.JPG'),
#   paste0('AI_IMG_', 5588:5595, '.JPG'),
#   paste0('AI_IMG_', 5680:5691, '.JPG'),
#   paste0('AI_IMG_', 5786:5801, '.JPG'),
#   paste0('AI_IMG_', 5908:5917, '.JPG'),
#   paste0('AI_IMG_', 6134:6152, '.JPG'),
#   paste0('AI_IMG_', 6329:6340, '.JPG'),
#   paste0('AI_IMG_', 6499:6509, '.JPG'),
#   paste0('AI_IMG_', 6539:6640, '.JPG')
# )

photos_to_transfer <- ls_surveyors %>%
  purrr::map(fpr::fpr_photo_sort_plan) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    site = case_when(photo_fullname %in% photo_ids_dont_copy01 ~ NA_real_,
    T ~ site),
    folder_to_path = paste0(getwd(), '/data/photos/', as.character(site), '/', camera_id, '_', photo_basename)  ##we could add the source to the name of the photo if we wanted....
  ) %>%
  filter(
    !sourcefile %ilike% 'not_used' & #not applied in this project
      !is.na(site) ##filter out some photos that shouldn't or don't move as per our ids_dont_copy files
  )


# burn a csv record of the photo assignment
photos_to_transfer %>%
  readr::write_csv(paste0('data/inputs_extracted/photo_transfer_record_', format(now(), "%Y%m%d_%H%M"), '.csv'),
                   na = '')


# # since we have a record we can delete the jpeg and jpgs that were copied over before!
# photos_to_delete <- readr::read_csv('data/inputs_extracted/photos_to_transfer_2022-03-24.csv') %>%
#   filter(folder_to_path %like% '.jpeg|.jpg' ) %>%
#   pull(folder_to_path)
#
# # # remove the old files
# file.remove(photos_to_delete)

# burn photos to files ----------------------------------------------------

# make sure you create the necessary directories
photos_to_transfer %>%
  dplyr::distinct(site) %>%
  pull(site) %>%
  purrr::map(fpr::fpr_photo_folders)

##test a bunch first!!!!!!!
test <- photos_to_transfer %>%
  filter(site == 197662)

# just a test of one folder
file.copy(from = test$sourcefile, to = test$folder_to_path,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)

# we could move them vs. copy but we need to be sure they are backed up first!!!!
# we should script the backup and resizing to an intermediary file then move vs. copy next time
# !!!!!!!!!!!!!this is the command to copy over!
file.copy(from=photos_to_transfer$sourcefile, to=photos_to_transfer$folder_to_path,
                    overwrite = F, recursive = FALSE,
                    copy.mode = TRUE)


##we also can erase the photos we said not to move since they are backed up and
##we want to see any left overs
# photo_folder_targets_delete <- photo_folder_targets %>%
#   filter(
#     is.na(folder_to_id)  ##filter out some photos that shouldn't or don't move
#   )
#
# file.remove(photo_folder_targets_delete$sourcefile)



# rename Lars photo directories -------------------------------------------

# need to rename the directories so they match the site name

path <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/moe_flnr"

folders <- list.dirs(path,
                     recursive = F,
                     full.names = F)

folders_renamed <- folders %>%
  stringr::str_replace('101400022_', '1014000022_') %>% # 101400022 is actually 1014000022
  stringr::str_extract("[^FP_|^?]*$")

# turned off for safety
# file.rename(from = paste0(path, '/', folders),
#             to = paste0(path, '/', folders_renamed)
# )


# flip photos -------------------------------------------------------------

# needed to flip some photos.  couldn't figure out how to run over list.
my_site <- 2021090150
fpr::fpr_photo_flip(str_to_pull = 'KP_TC_00169')

fpr::fpr_photo_flip(site_id = 14000988, rotate = 270, str_to_pull = '5587')
fpr::fpr_photo_flip(site_id = 14000994, rotate = 270, str_to_pull = '00938')
fpr::fpr_photo_flip(site_id = 14000997, rotate = 270, str_to_pull = '5578')
fpr::fpr_photo_flip(site_id = 2021090399, rotate = 270, str_to_pull = '5756')

# QA photo files ----------------------------------------------------------

pscis_all <- fpr_import_pscis_all() %>%
  bind_rows


# here is a little test on how to see the folder that need all photos to be renamed
# if you have a folder that is not in your pscis sheets it will break things.  watch out.
# consider using list of folders in the file
test <- fpr::fpr_photo_qa() %>%
  data.table::rbindlist(fill = T)


do_these_bud <- fpr::fpr_photo_qa()[
  fpr::fpr_photo_qa() %>%
    map(fpr::fpr_dat_w_rows) %>%
    grep(pattern = F)
] %>%
  names(.) %>%
  unique(.)
view(do_these_bud)
# here is the test for missing individual photos
test <- fpr::fpr_photo_qa() %>%
  bind_rows() %>%
  dplyr::filter(if_any(everything(), is.na))



# build photo amalgamation for each site ------------------------------------------------
pscis_all <- fpr::fpr_import_pscis_all() %>%
  bind_rows()

pscis_all %>%
  distinct(site_id) %>%
  arrange(site_id) %>%
  # head() %>% #test
  pull(site_id) %>%
  purrr::map(fpr_photo_amalg_cv)




# make phase2 photo files and copy in photos ------------------------------

# because we did not have the pscis ids our photos for phase 2 sites that had a my_crossing_reference
# need to be copied into new folders

##path to the photos
path <- paste0(getwd(), '/data/photos/')


##use the pscis spreadsheet to make the folders to copy the photos to
# d <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

conn <- rws_connect("data/bcfishpass.sqlite")
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)
rws_disconnect(conn)

pscis_new_sites <- left_join(
  d,
  xref_pscis_my_crossing_modelled,
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  filter(!is.na(external_crossing_reference))

folderstocopy<- pscis_new_sites$external_crossing_reference %>% as.character()

folders_new_names <- pscis_new_sites$pscis_crossing_id %>% as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)

folderstocreate <- paste0(path, folders_new_names)

##create the folders
lapply(folderstocreate, dir.create)


paths_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = T,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}

photo_names_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = F,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}


filestocopy_list <- path_to_photos %>%
  purrr::map(paths_to_copy)

change_file_names <- function(filestocopy, filename_before, filename_after){
  gsub(filestocopy, pattern = filename_before, replacement = filename_after)
}


filestopaste_list <- mapply(change_file_names, filestocopy_list, folderstocopy, folders_new_names)

copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}

mapply(copy_over_photos, filescopy =  filestocopy_list,
       filespaste = filestopaste_list)



