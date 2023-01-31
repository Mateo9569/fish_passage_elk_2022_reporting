


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
