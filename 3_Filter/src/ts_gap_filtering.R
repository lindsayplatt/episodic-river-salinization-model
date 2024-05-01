
#' @title Identify gaps that are too large to be filled
#' @description Count the length of gaps by identifying how many NAs appear
#' multiple days in a row. Filter to only gaps that are above a min number of 
#' days and return a vector of indices that refer to the input vector. Note
#' that this assumes you've only passed in one site's data at a time.
#' 
#' @param data_vector vector of values where some might be NA
#' @param large_gap_days single numeric value indicating the minimum gap (number 
#' of sequential values that are NA) to qualify as "too big to fill"
#' 
#' @return numeric vector of the indices that will not pass the criteria for being 
#' gap-filled; indices correspond to the input vector, `data_vector`
#' 
identify_large_gaps <- function(data_vector, large_gap_days) {
  
  # Count how many NA values appear sequentially
  NA_sequences <- accelerometry::rle2(
    x = is.na(data_vector), 
    indices = TRUE) %>% as_tibble() %>% 
    # value == 0 means "FALSE" to is.na() 
    # value == 1 means "TRUE" to is.na()
    filter(value == 1)
  
  # Filter to only those sequences that are really large
  largeNA_sequences <- NA_sequences %>% filter(length >= large_gap_days)
  
  # If there are really large gaps, return the index of the last large gap
  # If not, return a value of 0, such that any filtering of indices (must be
  # >= the index returned from this function) will return ALL indices
  if(nrow(largeNA_sequences) > 0) {
    # Use the `start` and `stop` fields returned by `rle2` to create
    # a vector of the indices for values that are within a large gap
    large_gap_ids <- apply(largeNA_sequences, 1, 
                           function(x) seq(x[['start']], x[['stop']])) %>% 
      reduce(c)
  } else {
    large_gap_ids <- 0
  }
  
  return(large_gap_ids)
}

#' @title Filter past large gaps in a timeseries
#' @description Identify really large temporal gaps in the timeseries data. No data
#' will be filled within really large gaps (as determined by `identify_large_gaps()`), 
#' so this returns only those data points that occur *after* the last big gap 
#' (prioritizing more recent data rather than older data). 
#' 
#' @param in_file a feather file with the time series data and at least the 
#' columns `site_no`, `dateTime`, and `[PARAM]`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param large_gap_days single numeric value indicating the minimum number of 
#' sequential days that can be NA to qualify as a 'large gap'; passed on 
#' to `identify_large_gaps()`, defaults to 3 years (365*3)
#' 
#' @return tibble with the same columns but potentially fewer rows as anything earlier
#' than the last large gap has been removed.
#' 
filter_beyond_large_ts_gaps <- function(in_file, param_colname, large_gap_days = 1095) {
  
  # Map over each site individually (not a super computationally expensive step,
  # so mapping within the function rather than at the pipeline level)
  read_feather(in_file) %>% 
    split(.$site_no) %>% 
    map(~{
      
      # Start by creating a data frame with all possible days for each site
      ts_data_all_days <- tibble(site_no = unique(.x$site_no),
                                 dateTime = seq(min(.x$dateTime), 
                                                max(.x$dateTime),
                                                by = 'days')) %>% 
        # Join in the real data
        left_join(.x, by = c('site_no', 'dateTime'))
      
      # Create a vector of the row indices that fall into gaps qualifying as "large"
      row_ids_in_large_gap <- identify_large_gaps(ts_data_all_days[[param_colname]], 
                                                  large_gap_days = large_gap_days)
      
      ts_data_all_days %>% 
        # Filter to only the data that is newer than the end of the last large gap
        filter(row_number() > max(row_ids_in_large_gap)) %>% 
        # And remove those values that are NA to get back to the way the data were passed in
        filter(!is.na(!!as.name(param_colname)))
    }) %>% 
    bind_rows()
  
}
