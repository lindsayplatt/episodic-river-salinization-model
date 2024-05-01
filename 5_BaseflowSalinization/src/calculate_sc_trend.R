
#' @title Calculate a trend in SC per site
#' @description For each site in the data, apply different trend algorithms to 
#' categorize that site's SC data as trending 'positive', 'negative', or 'none'.
#' 
#' @param ts_data a tibble containing all daily SC records with at least the columns
#  `site_no`, `dateTime`, and `SpecCond`. Should use data after `3_Filter` phase
#' @param max_pval numeric value indicating the maximum p-value that is allowed
#' to declare a trend significant. Passed on to `extract_mk_trend()`. 
#' 
calculate_sc_trend <- function(ts_data, max_pval = 0.05) {
  ts_data %>% 
    split(.$site_no) %>% 
    map(~{tibble(
      baseflowTrend = apply_MannKendall(.x, max_pval = 0.05)
    )}) %>%
    bind_rows(.id = 'site_no')
}

#' @title Find a trend in SC using a Mann-Kendall test
#' @description Calculate a trend for SC data using `EnvStats::kendallTrendTest()`
#' 
#' @param ts_data a tibble of SC timeseries data for a single site with at least
#' the columns `dateTime` and `SpecCond`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Passed on to `extract_mk_trend()`
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
apply_MannKendall <- function(ts_data, max_pval) {
  
  # To use Mann-Kendall, first transform the data into annual medians
  ts_data_annual <- ts_data %>%
    mutate(year = lubridate::year(dateTime)) %>%
    group_by(year) %>%
    summarize(SpecCond_med = median(SpecCond), .groups="keep") %>%
    ungroup()
  
  # Run the Mann-Kendall test and extract the trend result
  EnvStats::kendallTrendTest(SpecCond_med ~ year,
                             data = ts_data_annual) %>%
    extract_mk_trend(max_pval = max_pval)
  
}

#' @title Extract the trend from Mann-Kendall (MK) output
#' @description Use the output from a `EnvStats::kendallTrendTest()` 
#' to categorize the trend as either `none`, `positive`, or `negative`. 
#' 
#' @param mk_output a model object of the class `htest` from running 
#' `EnvStats::kendallTrendTest()`.
#' @param max_pval numeric value indicating the maximum p-value that is
#' allowed to declare a trend significant. Any model output with a p-value
#' above this value will have return "none" for the trend. Defaults to 0.05.
#' 
#' @return a character string indicating the trend as "none", "positive", or "negative"
#' 
extract_mk_trend <- function(mk_output, max_pval = 0.05) {
  
  # Extract the pvalue
  pval <- mk_output$p.value[['z']]
  
  # Extract the slope 
  slope <- mk_output$estimate[['slope']]
  
  # Return the appropriate trend name based on the
  # Mann-Kendall slope but only if the
  # p-value was below `max_pval`.
  if(pval >= max_pval | slope == 0) {
    trend <- "none"
  } else if(slope < 0) {
    trend <- "negative"
  } else if(slope > 0) {
    trend <- "positive"
  }
  
  return(trend)
}
