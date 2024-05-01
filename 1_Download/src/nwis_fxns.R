
#' @title Inventory NWIS for sites within a specific state matching certain criteria
#' @description Use the `dataRetrieval` function `whatNWISsites()` to
#' query the USGS National Water Information System (NWIS) and get a list 
#' of sites that have data between the dates for the desired parameter code
#' in the current state. This queries both the daily and instantaneous services.
#' 
#' @param state_cd a two-digit character string with the state abbreviation; the 
#' function only expects one value to be passed in at a time
#' @param param_cd the USGS NWIS 5-digit parameter code passed in as a character string
#' @param start_date either character string or Date with the start of the window  
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param end_date either character string or Date with the end of the window 
#' when sites need to have data; formatted as YYYY-MM-DD
#' 
#' @return a character vector of site numbers
#' 
inventory_nwis_sites_byState <- function(state_cd, param_cd, start_date, end_date) {
  map(c('dv', 'uv'), function(serviceCd) {
    whatNWISsites(stateCd = state_cd, 
                parameterCd = param_cd,
                startDate = start_date,
                endDate = end_date,
                siteType = 'ST',
                service = serviceCd)
    }) %>% bind_rows() %>% 
    # Remove any sites with ST-CA, ST-DCH, ST-TS
    filter(site_tp_cd == 'ST') %>% 
    # Return only the site numbers
    pull(site_no) %>% 
    unique()
}

#' @title Check how much data is available for sites
#' @description Use the `dataRetrieval` function `whatNWISdata()` to
#' query the USGS National Water Information System (NWIS) with a given
#' list of site numbers to check how much data is available for the 
#' given parameter codes; filters to only keep those with data for the 
#' mean statistic codes OR instantaneous records for the parameter.
#' 
#' @param site_numbers a character vector of NWIS site numbers
#' @param param_cd a character string with the USGS NWIS 5-digit parameter code
#' @param start_date either character string or Date with the start of the window  
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param end_date either character string or Date with the end of the window 
#' when sites need to have data; formatted as YYYY-MM-DD
#' 
#' @return a tibble with the columns `site_no` and `param_cd` plus columns
#' with info about the data stream available: `begin_date`, `end_date`,
#' `days_count` (number of days with records), `year_span`, `year_coverage` (
#' number of records per year), `gap_pct` (ratio of `year_coverage` to
#' `year_span` showing how many records were recorded over the full window).
#' 
inventory_nwis_data <- function(site_numbers, param_cd, start_date, end_date) {
  
  # Skip download if there are no site numbers to query
  if(length(site_numbers) == 0) return(tibble())
  
  data_streams <- whatNWISdata(siteNumber = site_numbers, 
                               parameterCd = param_cd,
                               startDate = start_date,
                               endDate = end_date,
                               service = c('dv', 'uv')) 
  
  # Skip remaining filtering/processing if none of the sites in 
  # this query returned any matching data streams (happened 
  # for a query of any Q data matching the SC site `05586300`)
  if(nrow(data_streams) == 0) return(tibble())
  
  # Add new columns summarizing the data streams time coverage
  data_info <- data_streams %>% 
    mutate(year_span = round(as.numeric((end_date - begin_date)/365), 2),
           year_coverage = round(count_nu/365, 2),
           gap_pct = 100 - round((year_coverage / year_span)*100, 0)) 
  
  # Filter to those that have the appropriate data type (either mean dv or uv)
  data_info %>% 
    # Only keep data streams with mean daily values OR instantaneous
    filter(data_type_cd == 'uv' | data_type_cd == 'dv' & stat_cd == '00003') %>%
    # Identify which service should be queried per site in order to get
    # mean daily SC eventually: If a site has mean daily values already available
    # (the row still exists), then use the 'dv' service. If 'dv' was not
    # available or 'dv' with min/max/etc stat codes were the only option (those
    # were filtered out in the previous step), then plan to pull 'uv'.
    group_by(site_no) %>% 
    summarize(query_service = ifelse('dv' %in% unique(data_type_cd), 'dv', 'uv'),
              # Need to retain the stat code so that joining the other columns
              # only joins the info for the mean daily value row
              stat_cd = ifelse(query_service == 'dv', stat_cd, NA)) %>% 
    ungroup() %>% 
    # Add other info about the data stream back in
    left_join(data_info, by = c('site_no', query_service = 'data_type_cd', 'stat_cd')) %>%  
    # Cleanup table to return
    dplyr::select(site_no, query_service, 
                  datastream_name = loc_web_ds, # This is sometimes used to distinguish multiple sensors at one site
                  begin_date, end_date, 
                  days_count = count_nu, 
                  year_span, year_coverage, 
                  gap_pct)
    
}

#' @title Remove sites that don't meet our minimum data requirements
#' @description Filter a table of site numbers with information about
#' the available data streams such that there are minimum years and 
#' dates required. 
#' 
#' @param data_info a table of information about the length of a data record
#' for a given site. Must have at least five columns: `site_no`, `query_service`,
#' `days_count`, `year_coverage`, and `end_date`.
#' @param min_yrs integer value defining the minimum number of years of 
#' data required to retain a site in the output. 
#' @parma min_end_date a `Date` object representing the oldest date for which
#' a site must have data past in order to be retained in the output. For example,
#' if `min_end_date` was June 10, 2018 then any site with a data stream that ends
#' on June 9, 2018 or earlier would be excluded (there must be at least 1 record
#' available *past* the `min_end_date`).
#' 
#' @return a tibble with three columns (`site_no`, `query_service`, and `days_count`) 
#' representing only those data streams that meet the minimum length and timing
#' requirements input to the function.
#' 
filter_to_min_data_standards <- function(data_info, min_yrs, min_end_date) {
  
  # Skip filtering if there isn't any data
  if(nrow(data_info) == 0) return(tibble())
  
  # Using `year_coverage` and not `year_span` for filtering because sometimes
  # sites begin very early, then have a huge gap before resuming. Using
  # the year span alone makes it seem like they have way more data than
  # they really do. For example site number '02458148' has a gap from Feb 
  # 2001 to July 2014 but the begin/end dates make it appear that it has
  # data from 2000 to present when it really doesn't. We will still
  # need to pull data and handle gaps later, but this is at least a preliminary
  # way to check and not waste time pulling data for sites that definitely
  # won't fit our criteria.
  
  data_info %>% 
    filter(year_coverage >= min_yrs) %>% 
    filter(end_date >= min_end_date) %>% 
    # Need `days_count` for splitting queries for download later
    dplyr::select(site_no, query_service, days_count)
}

#' @title Define data download groups
#' @description Use results from `inventory_nwis_data()` to assign a download  
#' group to each site based on max download sizes desired. 
#' This is borrowed from Lauren Koenig's (USGS) brilliant function at 
#' https://github.com/USGS-R/ds-pipelines-targets-example-wqp/blob/main/2_download/src/fetch_wqp_data.R
#' 
#' @param data_info a tibble with at least NWIS site numbers and record counts
#' in columns called `site_no` and `days_count`, respectively
#' @param max_sites the maximum number of sites allowed per group to 
#' optimize download times, defaults to 2000
#' 
#' @return a tibble with the columns `site_no` and `task_num` indicating
#' how to split up the future data download.
#' 
add_download_grp <- function(data_info, max_sites = 2000) {
  data_info %>% 
    split(.$query_service) %>% 
    map(~{
      # Set `max_results` much lower than default for `uv` because 1 day = 1440 records
      max_results <- ifelse(unique(.x$query_service) == 'uv', 15000, 250000)
      .x %>% 
        arrange(desc(days_count), .by_group = TRUE) %>%
        mutate(task_num_by_results = MESS::cumsumbinning(x = days_count, 
                                                         threshold = max_results, 
                                                         maxgroupsize = max_sites), 
               task_num_by_group = cur_group_id()) %>%
        ungroup() %>% 
        # Each group from before (which represents a different data type code 
        # (uv or dv)) will have task numbers that start with "1", so now we create 
        # a new column called `task_num` to create unique task numbers within
        # each group and by splitting those based on max desired download sizes
        group_by(task_num_by_group, task_num_by_results) %>% 
        mutate(task_num = cur_group_id()) %>% 
        ungroup() %>% 
        dplyr::select(site_no, query_service, task_num)
    }) %>% 
    bind_rows()
}

#' @title Download data from NWIS
#' @description Use the `dataRetrieval` function `readNWISdata()` to
#' download records from the USGS National Water Information System (NWIS) 
#' with a given list of site numbers and parameter codes within a certain
#' time window. This assumes you have only passed in one service code, either 
#' `dv` or `uv`. 
#' 
#' @param out_file a character string indicating a file path to save a feather file
#' @param site_numbers a character vector of NWIS site numbers
#' @param param_cd a character string with the USGS NWIS 5-digit parameter code
#' @param start_date either character string or Date with the start of the window  
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param end_date either character string or Date with the end of the window 
#' when sites need to have data; formatted as YYYY-MM-DD
#' @param service_cd either the string `dv` or `uv` is accepted
#' @param timeout_minutes_per_site integer; indicates the maximum time that should be
#' allowed to elapse per site before retrying the data download step. The total time
#' used for the retry is calculated as number of sites * `timeout_minutes_per_site`.
#' Defaults to 5 minutes. 
#' 
#' @return a feather file containing either daily value or instantaneous data 
#' records from NWIS and contains the columns `site_no`, `dateTime`, `[PARAM]`,
#' and `[PARAM]_cd`.
#'
download_nwis_data <- function(out_file, site_numbers, param_cd, start_date, 
                               end_date, service_cd, timeout_minutes_per_site = 5) {
  
  # Require only one service code 
  stopifnot(length(service_cd) == 1)
  
  # Skip download if there are no site numbers to query and
  # save an empty file instead
  if(length(site_numbers) == 0) {
    write_feather(tibble(), out_file)
    return(out_file)
  }
  
  query_params <- list(siteNumber = site_numbers, 
                       parameterCd = param_cd,
                       service = service_cd, 
                       startDate = start_date,
                       endDate = end_date)
  
  # Add the mean statistic code to the query parameters for daily value service
  if(service_cd == 'dv') {
    query_params <- append(query_params, list(statCd = '00003'))
  }
  
  # Borrowed the functions for downloading safely from Lauren Koenig's 
  # example WQP pipeline: https://github.com/DOI-USGS/ds-pipelines-targets-example-wqp/
  # Pull the data, retrying up to the number of times indicated by `max_tries`.
  # For any single attempt, stop and retry if the time elapsed exceeds
  # `timeout_minutes`. Use at least 1 minute so that it doesn't error if 
  # `length(site_numbers) == 0`
  timeout_minutes <- 1 + timeout_minutes_per_site * length(site_numbers)
  
  nwis_data <- pull_data_safely(dataRetrieval::readNWISdata, query_params,
                                timeout_minutes = timeout_minutes,
                                max_tries = 3, 
                                sleep_on_error = 0,
                                verbose = FALSE)
  
  # Throw an error if the request comes back empty
  if(is.data.frame(nwis_data) && nrow(nwis_data) == 0){
    stop(sprintf("\nThe download attempt failed after %s successive attempts", max_tries))
  }
  
  # Rename & select certain columns + save the data as a file 
  nwis_data %>% 
    renameNWISColumns() %>% 
    dplyr::select(site_no, dateTime, contains('SpecCond'), contains('Flow')) %>%
    write_feather(out_file)
  
  return(out_file)
}

#' @title Download metadata from NWIS
#' @description Use the `dataRetrieval` function `readNWISsite()` to download
#' metadata information, including lat/longs and timezone from the USGS 
#' National Water Information System (NWIS) with a given list of site numbers. 
#' 
#' @param site_numbers a vector with NWIS site numbers as character strings
#' 
#' @return a tibble containing the columns `site_no`, `dec_lat_va`, 
#' `dec_long_va`, `datum`, and `tz_cd`
#'
download_nwis_metadata <- function(site_numbers) {
  readNWISsite(site_numbers) %>% 
    dplyr::select(site_no, dec_long_va, dec_lat_va, datum = coord_datum_cd, tz_cd)
}

