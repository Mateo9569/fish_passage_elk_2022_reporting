##this is for as we work through
preview_chapter('0100-intro.Rmd')
preview_chapter('0200-background.Rmd')
preview_chapter('0300-method.Rmd')
preview_chapter('0400-results.Rmd')
preview_chapter('0600-appendix.Rmd')
preview_chapter('0800-appendix-197559-brule.Rmd')
preview_chapter('0800-appendix-197534-weigert.Rmd')
preview_chapter('0800-appendix-197844-bighorn.Rmd')
preview_chapter('index.Rmd')

#######################################################################################
##change your VErsion #
#######################################################################################

#######################################################################################
##if you have changed your bcfishpass model outputs by saving to sqlite with 0282-extract-bcfishpass2...
##you also need to make new html tables to link to in the leaflet map  use 0355-tables-reporting-html.R
########################################################################################



##this is how we clean up our bib file.  We need to find a way to add together the packages.bib file with the book.bib file first though.
# citr::tidy_bib_file(
#   rmd_file = "Elk-River-Fish-Passage-2020.Rmd",
#   messy_bibliography = 'book.bib',
#   file = 'book_tidy.bib')

##we also need to change all the date header to year in the bib file so that it can be run by our pdf maker
##i did this by hand last time but would be good to automate!!!

##  make the site
# source('R/photos-extract-metadata.R') ##if you added new photos
# source('R/0355-tables-reporting-html.R')  #if you changed the modelling outputs


#################################################################################################
##go to the index.Rmd and change gitbook_on <- TRUE
#################################################################################################

rmarkdown::render_site(output_format = 'bookdown::gitbook',
                       encoding = 'UTF-8')

# pdf version

#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################
##move the phase 1 appendix out of the main directory to a backup file or else the file is too big
# make sure the pdf is not open in your viewer!!!
filename_html <- 'Elk2022'


{
  # file.rename('0600-appendix.Rmd', 'hold/0600-appendix.Rmd')

  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

  ##move the phase 1 appendix back to main directory
 #
  # print to pdf
  pagedown::chrome_print(
    paste0(getwd(), '/', filename_html, '.html'),
    output = paste0(getwd(),'/docs/', filename_html, '.pdf'),
    timeout = 180
  )

  tools::compactPDF(paste0(getwd(), "/docs/", filename_html, ".pdf"),
                    gs_quality = 'screen',
                    # gs_cmd = "C:/Program Files/gs/gs9.56.1/bin/gswin64.exe"
                    gs_cmd = "opt/homebrew/bin/gs")

  # get rid of the html as its too big and not needed
  file.remove(paste0(getwd(), '/', filename_html, '.html'))
  }

