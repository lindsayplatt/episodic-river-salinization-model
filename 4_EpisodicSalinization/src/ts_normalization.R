# Load the znorm function from an earlier phase script
znorm <- function() {}
insertSource("2_Prepare/src/attr_prep_fxns.R", functions="znorm") 

#' @title Apply normalization to each site's time series data
#' @description Normalize the time series data per site (not per site-year) 
#' before passing into the clustering algorithm.
#' 
#' @param ts_data a tibble with at least the columns `site_no` and `[PARAM]`
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' 
#' @return tibble with all the columns from `ts_data` plus `[PARAM]_norm`
#' 
normalize_data_bysite <- function(ts_data, param_colname) {
  ts_data %>% 
    # Temporarily rename the data column so that this can handle any param
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # By site, normalize the data by z-scoring
    group_by(site_no) %>% 
    mutate(PARAM_norm = znorm(PARAM)) %>% 
    ungroup() %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x))
}
