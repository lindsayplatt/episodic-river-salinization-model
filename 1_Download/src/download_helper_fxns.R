
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

#' @title Identify executable path for 7z program
#' @description No matter the operating system, this function
#' looks for the 7z executable file and returns the path. It will 
#' throw an error if it doesn't find one and prompt the user to 
#' install the program. This was built with the help of ChatGPT.
#' 
#' @return a filepath specifying the location of the `7z.exe` file
#' 
find_7z <- function() {
  
  # Define common paths for 7z executable
  possible_paths <- list(
    windows = c("C:/Program Files/7-Zip/7z.exe",
                "C:/Program Files (x86)/7-Zip/7z.exe"),
    mac = c("/usr/local/bin/7z", "/opt/homebrew/bin/7z"),
    linux = c("/usr/bin/7z", "/usr/local/bin/7z")
  )
  
  # Determine the OS
  os <- .Platform$OS.type
  if (os == "windows") {
    for (path in possible_paths$windows) {
      if (file.exists(path)) {
        return(normalizePath(path))
      }
    }
  } else if (os == "unix") {
    # Check if running on Mac or Linux
    if (system("uname", intern = TRUE) == "Darwin") {
      for (path in possible_paths$mac) {
        if (file.exists(path)) {
          return(normalizePath(path))
        }
      }
    } else {
      for (path in possible_paths$linux) {
        if (file.exists(path)) {
          return(normalizePath(path))
        }
      }
    }
  }
  
  # If not found in common paths, try using 'which' command
  which_7z <- system("which 7z", intern = TRUE)
  if (nchar(which_7z) > 0 && file.exists(which_7z)) {
    return(normalizePath(which_7z))
  }
  
  stop("7z executable not found. Please ensure it is installed and accessible in your PATH.")
}
