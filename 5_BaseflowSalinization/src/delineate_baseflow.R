
#' @title Separate baseflow and quickflow from daily flow
#' @description Using functions from `FlowScreen`, this function takes in a flow
#' time series of daily flow for a number of different sites plus a dataset of
#' static baseflow indices and calculates baseflow.
#' 
#' @param ts_q a tibble of daily discharge time series. It needs at least the 
#' columns `site_no`, `dateTime`, and `Flow`
#' @param site_bfi_data a tibble with the static attributes of baseflow indices
#' (known as BFI) in percentages for each site in the `ts_q` tibble. Expects the
#' columns `site_no` and `attr_baseFlowInd`.
#' 
#' @return a tibble with the columns `site_no`, `dateTime`, `Flow`, `baseFlow`, and
#' `quickFlow`. 
#' 
delineate_baseflow <- function(ts_q, site_bfi_data) {
  
  # Use a custom input for the BFI based on known info for this site.
  bfi <- site_bfi_data %>% 
    filter(site_no == unique(ts_q$site_no)) %>% 
    # Change from percent to fraction for `bf_eckhardt()`
    mutate(bfi = attr_baseFlowInd/100) %>% 
    pull(bfi)
  
  # Use a constant for alpha as suggested by Eckhardt 2012 for perennial stream
  alpha <- 0.970 
  
  # Delineate baseflow from quickflow
  ts_q %>%
    mutate(baseFlow = bf_eckhardt(Flow, alpha, bfi),
           quickFlow = Flow - baseFlow) %>% 
    select(site_no,
           dateTime,
           Flow, 
           baseFlow,
           quickFlow)
  
}

#' @title Return whether or not a day qualifies as a "baseflow day"
#' @description Compare baseflow to total flow and return a T/F for whether a 
#' particular day at a particular site qualifies as a "baseflow day". In this
#' analysis, a "baseflow day" is one that does not occur in winter and where the
#' baseflow makes up 90% of the total flow.
#' 
#' @param ts_bf a tibble of daily flow time series with at least the columns
#' `site_no`, `dateTime`, `Flow` and `baseFlow`.
#' @param min_baseflow_frac the minimum percentage of total flow that baseflow
#' will need to be in order for a day to qualify as a "baseflow day"
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' 
#' @return a tibble with the columns `site_no`, `dateTime`, and `is_baseFlow_day`
#' 
identify_baseflow_days <- function(ts_bf, min_baseflow_frac = 0.90, winter_months = c(12,1,2,3)) {
  
  ts_bf %>% 
    # Determine if the day qualifies as a "non-winter baseflow" day
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    mutate(is_baseFlow_day = baseFlow >= min_baseflow_frac*Flow & !is_winter) %>% 
    select(site_no,
           dateTime,
           is_baseFlow_day)
  
}
