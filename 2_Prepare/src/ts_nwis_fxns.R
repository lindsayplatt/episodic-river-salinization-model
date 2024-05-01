
#' @title Calculate daily means from instantaneous data
#' @description Using the downloaded NWIS timeseries data, this function will
#' collapse instantaneous records to daily means. It will skip files that are
#' named with 'dv' and return the same file path that was passed in.
#' 
#' @param out_file_dir a character string indicating a file folder to save a feather 
#' file of the daily means
#' @param in_file a character string indicating the file path containing 
#' instantaneous records with at least the columns `site_no`, `dateTime`,
#' `[PARAM]_Inst`, and `[PARAM]_Inst_cd`.
#' @param site_tz_xwalk a tibble with the timezone of each NWIS site so that date
#' times can be converted into days appropriately. Should have the columns `site_no`
#' and `tz_cd`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a feather file containing only daily values for each site; it should have 
#' the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#'
calculate_dv_from_uv <- function(out_file_dir, in_file, site_tz_xwalk, param_colname) {
  
  # All service files (uv + dv) can be passed into this function so that the pipeline 
  # works even if there aren't any UV files to combine (if we tried isolating just the 
  # UV files first it will fail when trying to `map` over an empty target).
  if(grepl('uv', in_file)) {
    
    # If the file is a UV file, collapse 15 min data to daily.
    out_file <- file.path(out_file_dir, gsub('uv', 'uv_to_dv', basename(in_file)))
    data_in <- read_nwis_file(in_file, param_colname)
    
    # Stop now if the data passed in does not represent instantaneous data
    stopifnot(any(grepl('_Inst', names(data_in))))
    
    data_in %>%
      # Rename the data column so that the following code can handle
      # either SC data or Q data. The name will be reinstated at the end.
      rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
      # Create a column representing the day without the time
      # using the appropriate timezone per site
      convert_to_date(site_tz_xwalk) %>%
      group_by(site_no, dateTime) %>%
      summarize(PARAM = mean(PARAM_Inst, na.rm=TRUE),
                PARAM_cd = paste(unique(PARAM_Inst_cd), collapse=';'),
                .groups = 'drop') %>%
      # Replace the `PARAM` placeholder column names with the appropriate
      rename_with(~gsub('PARAM', param_colname, .x)) %>% 
      # Save the data as a file
      write_feather(out_file)
  } else {
    # If this is a DV file, just return the existing file path
    return(in_file)
  }
  
  return(out_file)
}

#' @title Use timezones to convert dateTimes to dates
#' @description Convert the timeseries dateTime stamps into `Date`class rather 
#' than `POSIXct` using the appropriate time zone in order to summarize data
#' by day and use values that are correct for the day.
#' 
#' @param timeseries_data a tibble with at least the columns `site_no` and
#' `dateTime` which are used to join in timezone information and change the date.
#' @param site_tz_xwalk a tibble with the timezone of each NWIS site so that date
#' times can be converted into days appropriately. Should at least have the columns
#' `site_no` and `tz_cd`.
#' 
#' @return a tibble the exact same structure as `timeseries_data` but with values
#' in the `dateTime` column adjusted to dates.
#' 
convert_to_date <- function(timeseries_data, site_tz_xwalk) {
  timeseries_data %>% 
    # Join in the timezone code information as a column for each site
    left_join(select(site_tz_xwalk, site_no, tz_cd), by = 'site_no') %>% 
    # Map the timezone codes from NWIS into `OlsonNames()` to use in R
    mutate(tz_str = case_when(
      tz_cd == "EST" ~ "America/New_York",
      tz_cd == "CST" ~ "America/Chicago",
      tz_cd == "MST" ~ "America/Denver",
      tz_cd == "PST" ~ "America/Los_Angeles",
    )) %>% 
    # For each timezone, adjust the dateTime values and then drop the times. 
    # Only return the dates since combining back into a single column forces
    # dateTimes back into a shared timezone string.
    split(.$tz_str) %>% 
    map(~{
      .x %>% 
        mutate(dateTime_adj = with_tz(dateTime, tzone = unique(tz_str))) %>% 
        mutate(dateTime = as.Date(format(dateTime_adj, '%Y-%m-%d'))) %>% 
        dplyr::select(site_no, dateTime, everything(), 
                      -tz_cd, -tz_str, -dateTime_adj)
    }) %>% 
    # Combine everything and return the order to be by site and date
    bind_rows() %>% 
    arrange(site_no, dateTime)
}

#' @title Combine all downloaded and calculated daily means
#' @description Using the downloaded NWIS daily data and the output from 
#' `calculate_dv_from_uv()`, create a single timeseries dataset of daily means.
#' Note that if the data contains the column `SpecCond` any `-999999` value is 
#' replaced by an NA. 
#' 
#' @param out_file a character string indicating a file path to save a feather 
#' file of all the daily means
#' @param in_files vector of feather filepaths of the data to combines; assumes  
#' these have only the following columns: `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a feather file containing all daily value timeseries data; it should have 
#' the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd`.
#'
combine_all_dv_data <- function(out_file, in_files, param_colname) {
  
  # Map over each of the files and read into memory
  in_files %>% 
    map(read_nwis_file, param_colname = param_colname) %>%
    # Make sure the `dateTime` columns was read in as a `Date` class
    # This happens *after* we already convert uv to dv so this is 
    # just for forcing the correct class, not changing data.
    map(~mutate(.x, dateTime = as.Date(dateTime))) %>% 
    # Combine the list of loaded tables into a single table
    bind_rows() %>% 
    # Arrange so that site's data are together and ordered chronologically
    arrange(site_no, dateTime) %>% {
      if(param_colname == 'SpecCond')
        # For SC data, replace the -999999 code with NAs before counting & filling gaps
        mutate(., SpecCond = na_if(SpecCond, -999999))
      else if(param_colname == 'Flow')
        # Convert Flow from cfs (ft3/s) to m3/s
        mutate(., Flow = Flow/(3.28^3))
      else .
    } %>% 
    # Save the data as a file
    write_feather(out_file)
  
  return(out_file)
}

#' @title Read in an NWIS file and keep only the standard columns
#' @description Some sites return multiple data streams for the same parameter, 
#' usually indicating a different sensor. Sometimes the sensors are just replacements
#' but sometimes they are positioned at different locations (e.g. bottom, top, left bank,
#' right bank, etc). There is nuance to which to choose so this helps retain the 
#' appropriate data streams for each site in a single column.
#' 
#' @param in_file a single feather filepath for the data to be read in. Should have
#' at least the columns `site_no`, `dateTime`, `[PARAM]`, and `[PARAM]_cd` but may
#' have more than that.
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values and value codes. In this workflow, this is likely either
#' `SpecCond` OR `Flow`. No need for `_Inst` here as the code automatically adds that.
#' 
#' @return a table of NWIS timeseries data with only the columns `site_no`, `dateTime`, 
#' `[PARAM]`, and `[PARAM]_cd`
#' 
read_nwis_file <- function(in_file, param_colname) {
  
  # Only accepts columns named `[PARAM]`, `[PARAM]_cd`, `[PARAM]_Inst`, or `[PARAM]_Inst_cd`
  standard_colname_regex <- sprintf('site_no|dateTime|^%s(_Inst)?$|^%s(_Inst)?_cd$', 
                                    param_colname, param_colname)
  
  data_in <- read_feather(in_file)
  
  # If there are more than the expected columns (meaning additional data stream
  # columns for the same site & parameter),choose which ones to use. 
  # For now, just selecting the standard columns and ignoring the other. This
  # will result in more NAs.
  if(!all(grepl(standard_colname_regex, names(data_in)))) {
    # FUTURE IMPROVEMENT: Choose the appropriate sensor column for each site and date
    # Some are dropped entirely because they only have the unique column names,
    # e.g. c("01646500", "02323592", "251003080435500", "251209080350100", 
    #        "251241080385300", "251253080320100")
    
    # E.g. for `01463500` before 1995-09-30, the data is stored in a different column
    # x <- dataRetrieval::readNWISdv(siteNumber = '01463500', startDate = '1968-06-25', parameterCd = '00095') %>%
    #   renameNWISColumns() %>% 
    #   mutate(SC = ifelse(!is.na(SpecCond), SpecCond, `..2.._SpecCond`))
    # plot(x$dateTime, x$SC)
    # in_file <- "1_Download/out_nwis/sc_dv_003.feather"
    
    # For now, just record which files this occurs in.
    message('Found a file with more than `SpecCond` datastream: ', in_file)
  }
  
  data_in %>% 
    # Select only the standard columns to return
    select(matches(standard_colname_regex)) %>% 
    # Filter out the NAs resulting from removing/replacing the non-standard 
    # columns (not including `[PARAM]_cd` NAs)
    filter(if_all(-ends_with('_cd'), ~!is.na(.)))
 
}
