
#' @title Combine all static attributes data
#' @description Using `site_no` as the identifier, this function combines 
#' all of the desired static attributes into a single table. If the 
#' attribute was not known for a particular site, there will be an NA in 
#' that column. No filtering is done to remove sites without certain 
#' attributes in this function.
#' 
#' @param ts_data a tibble of daily water quality time series. It needs at least the 
#' columns `site_no`, `dateTime`, and `[PARAM]`
#' @param ts_bf_days a tibble specifying whether a date is a baseflow day for 
#' each site; has the columns `site_no`, `dateTime`, and `is_baseFlow_day`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return a tibble with only the rows that matched site-days from `ts_bf_days`
#' for days with only flow made of baseflow data. Has the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' 
filter_ts_to_baseflow_days <- function(ts_data, ts_bf_days, param_colname) {
  ts_data %>% 
    left_join(ts_bf_days, by = c('site_no', 'dateTime')) %>% 
    filter(is_baseFlow_day) %>% 
    select(site_no, dateTime, !!as.name(param_colname))
}

#' @title Determine if a site can be used to calculate baseflow trends
#' @description A site needs a minimum amount of baseflow data in order to qualify
#' for a trend to be calculated. This function applies criteria to the baseflow-only 
#' data to remove sites that don't qualify to have a trend calculated. A site needs 
#' to have at least 5 different years with at least 1 baseflow day to be kept.
#' 
#' @param ts_data_bf a tibble of daily water quality time series with only those
#' dates considered a "baseflow day". It needs at least the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' @param min_years minimum number of years that need specific conductance on
#' baseflow days per site in order for that site to be used in the baseflow
#' trend analysis. Defaults to 5.
#' 
#' @return a tibble with only the rows that matched site-years from `ts_bf_days`.
#' Has the columns `site_no`, `n_bf_years`, and `site_bf_qualified`. Contains
#' all sites, regardless of whether they passed qualification or not.
#' 
apply_baseflow_trend_criteria <- function(ts_data_bf, min_years = 5) {
  ts_data_bf %>% 
    mutate(year = year(dateTime)) %>% 
    group_by(site_no) %>% 
    # A site only qualifies if it had at least 5 years of baseflow days
    summarize(n_bf_years = length(unique(year)), .groups='keep') %>% 
    ungroup() %>% 
    mutate(site_bf_qualified = n_bf_years >= min_years) %>% 
    select(site_no, n_bf_years, site_bf_qualified)
}

#' @title Filter to sites who met our qualification criteria
#' @description A site needs a minimum amount of data in order for a trend to be 
#' calculated. The function `apply_baseflow_trend_criteria()` applied such 
#' criteria and saved a table that said whether a site qualified or not. This 
#' function filters the actual baseflow time series data to only those sites 
#' which qualified.
#' 
#' @param ts_data_bf a tibble of daily water quality time series with only those
#' dates considered a "baseflow day". It needs at least the columns `site_no`, 
#' `dateTime`, and `[PARAM]`
#' @param site_bf_qualified a tibble with logic for each site from
#' `ts_bf_days`. Contains output from the function `apply_baseflow_trend_criteria()`
#' 
#' @return a tibble of time series data for only sites where enough of the years 
#' had days qualified as baseflow days so that a trend can be calculated. The
#' tibble should have the columns `site_no`, `dateTime`, `SpecCond`.
#' 
filter_ts_to_qualified_site_seasons <- function(ts_data_bf, site_bf_qualified) {
  site_qualified <- site_bf_qualified %>% 
    filter(site_bf_qualified) %>% 
    pull(site_no) %>% 
    unique()
  
  ts_data_bf %>% 
    filter(site_no %in% site_qualified)
}
