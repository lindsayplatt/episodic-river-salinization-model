
#' @title Extract a file from a zip file
#' @description Use the `zip::unzip()` function to pull a specific file from
#' a zip file and move into a new location.
#' 
#' @param out_file a character string indicating a file path to save the file
#' @param zip_file a character string giving the zip file's path
#' @param file_to_extract character string giving the file(s) stored in the 
#' zip file that will be copied to a new location. 
#' 
#' @return a filepath specifying the name and location of the extracted file
#'
extract_file_from_zip <- function(out_file, zip_file, file_to_extract) {
  
  # Unzip and extract the file(s) of interest
  zipdir <- tempdir()
  zip::unzip(zipfile = zip_file,
             files = file_to_extract,
             exdir = zipdir)
  
  # Copy from the temporary directory into the desired location
  file.copy(from = file.path(zipdir, file_to_extract), 
            to = out_file)
  
  return(out_file)
}

#' @title Download a file from a URL
#' @description Wrapper function for `download.file()` to download
#' a file from a URL, save to a specific location locally, and
#' return the filepath so that `targets` can use the function to 
#' create file targets (those with `format = 'file'`).
#' 
#' @param out_file a character string indicating a file path to save the file
#' @param url_in character string with the full filepath to the 
#' online file (can usually copy the link behind a "download" 
#' button, as long as authentication is not required).
#' 
#' @return a filepath specifying the name and location of the downloaded file
#' 
download_file_from_url <- function(out_file, url_in) {
  download.file(url_in, destfile = out_file)
  return(out_file)
}
