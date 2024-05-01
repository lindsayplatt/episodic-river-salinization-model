
#' @title Find sites that meet minimum data quantity-temporal range criteria
#' @description Apply a minimum number of years + a minimum date requirement to
#' filter to lengthier time series that have had data recently.
#' 
#' @param ts_data a tibble of daily water quality time series. It needs at least the 
#' columns `site_no` and `dateTime`
#' @param min_years numeric value indicating the minimum number of years that the
#' minimum and maximum parameter values must span. This assumes that the data has 
#' already been filtered past large gaps (see `filter_beyond_large_ts_gaps()`) 
#' and that any remaining gaps can be filled with the WRTDS method). Defaults to 5.
#' @param min_recent_date Date value indicating the most recent date that the 
#' `ts_data` *MUST* have at least one record beyond. Defaults to `2007-01-01`, 
#' which (at the time of this writing) meant a site must have had at least one 
#' record in the last 15 years.
#' 
#' @return a vector of NWIS site character strings whose `ts_data` met requirements
#' 
identify_temporal_qualifying_sites <- function(ts_data, min_years = 5, min_recent_date = as.Date('2007-01-01')) {
  
  ts_data %>% 
    group_by(site_no) %>% 
    summarize(min_date = min(dateTime),
              max_date = max(dateTime)) %>% 
    mutate(year_span = as.numeric(max_date - min_date)/365) %>% 
    ungroup() %>% 
    # Filter to sites that have the minimum number of years of data (even 
    # if there are some gaps, which will be filled by WRTDS)
    filter(year_span >= min_years) %>% 
    # Filter to sites that have some data more recently than `min_recent_date`
    filter(max_date >= min_recent_date) %>% 
    pull(site_no)
  
}

#' @title Find sites that might be in high agricultural areas
#' @description Identify sites that may be influenced by agriculture because
#' their SC levels could be much higher than other streams, or behave 
#' differently than other streams SC data, unrelated to road salt application. 
#' Sites with over 75% agriculture in their catchments will be flagged here.
#' 
#' @param nhd_ag_attrs a tibble with the columns `site_no`, and `attr_pctAgriculture` 
#' which gives the total percentages of the catchment for each site that is
#' covered with pasture/hay (`CAT_NLCD19_81`) + cultivated crops (`CAT_NLCD19_82`).
#' 
#' @return a vector of NWIS site character strings whose `ts_data` fit into the
#' `agriculture` category and should be removed.
#' 
identify_ag_sites <- function(nhd_ag_attrs) {
  nhd_ag_attrs %>% 
    filter(attr_pctAgriculture > 75) %>% 
    pull(site_no)
}

#' @title Find sites that have outrageously high SC too frequently
#' @description Identify sites that may be influenced by some other source of
#' salt contributing to consistently high SC. Waste water and industrial water
#' can be over `10,000 uS/cm @25degC` and seawater is typically over 
#' `55,000 uS/cm @25degC`. We can use these values to identify sites that might
#' be outside of our typical freshwater scenarios (maybe they are coastal, maybe
#' they are directly downstream of a waste water facility?). As the function is
#' written, any site with over 50% of their SC data above `10,000 uS/cm @25degC`
#' or over 25% of their SC data above `55,000 uS/cm @25degC` will be returned.
#' 
#' @param ts_data a tibble of daily water quality time series. It needs at least the 
#' columns `site_no` and `dateTime`, and `SpecCond`. Note that `SpecCond` is required
#' rather than the generic input option (`param_colname`) to handle other parameters 
#' because this function specifically is designed for specific conductance data.
#' 
#' @return a vector of NWIS site character strings whose `ts_data` fit into the
#' `outlier` category and should be removed.
#' 
identify_highSC_sites <- function(ts_data) {
  ts_data %>% 
    group_by(site_no) %>% 
    summarize(perc50 = quantile(SpecCond, probs = 0.50, na.rm=TRUE),
              perc75 = quantile(SpecCond, probs = 0.75, na.rm=TRUE)) %>% 
    filter(perc75 >= 10000 | perc50 >= 55000) %>% 
    pull(site_no)
}

#' @title Find sites that have zero road salt
#' @description Use the road salt attribute data per site (summarizing road salt
#' applied within the NHD+ catchment, see `aggregate_road_salt_per_poly()`) to keep 
#' only those sites that had some road salt within their catchment.
#' 
#' @param roadSalt_attrs a tibble with the columns `site_no`, and `attr_roadSaltPerSqKm`
#' 
#' @return a vector of NWIS site character strings which do not have road salt 
#' applied within their catchment and should be removed.
#' 
identify_nonsalt_sites <- function(roadSalt_attrs) {
  roadSalt_attrs %>% 
    filter(attr_roadSaltPerSqKm == 0) %>% 
    pull(site_no)
}

#' @title Find sites that are missing any attribute
#' @description Use the attribute table to identify sites that may be missing
#' one. The random forest models will not work with missing attributes, so these
#' sites should not qualify.
#' 
#' @param static_attrs a tibble with the columns `site_no` and any number of
#' columns prefixed with `attr_`
#' 
#' @return a vector of NWIS site character strings which are missing at least one 
#' of the gathered static attributes 
#' 
identify_missing_attr_sites <- function(static_attrs) {
  static_attrs %>% 
    filter(if_any(starts_with('attr_'), is.na)) %>% 
    pull(site_no)
}

#' @title Filter data to sites that met certain criteria
#' @description Using previously identified vectors of NWIS sites, filter the 
#' data to only those of sites that we want to keep and remove any
#' data for sites that we don't want. In the function, we use both those sites
#' in `keep_sites` and those in `remove_sites` because there may be some 
#' cross-listing. For example, some tidal sites have the appropriate length of 
#' record for their SC data but we want to remove from our final data set due 
#' to the tidal influence.
#' 
#' @param site_data a tibble of data with at least the column `site_no`. 
#' @param keep_sites a single character vector of NWIS site numbers that should
#' be kept in the data.
#' @param remove_sites a single character vector of NWIS site numbers that should
#' be removed from the data (e.g. tidal sites, high agriculture sites)
#' 
#' @return a tibble with the same columns as `site_data` but likely fewer rows
#' 
filter_data_to_qualifying_sites <- function(site_data, keep_sites, remove_sites) {

  message('Removing sites that did not meet temporal criteria: ', 
          sum(!unique(site_data$site_no) %in% keep_sites))
  message('Removing additional sites that are ag, tidal, high SpC, nonsalt, 
          missing NHD+ catchments, or missing attributes: ',
          sum(keep_sites %in% remove_sites))

  site_data %>% 
    filter(site_no %in% keep_sites) %>% 
    filter(!site_no %in% remove_sites)
}
