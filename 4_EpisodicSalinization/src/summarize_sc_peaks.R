
#' @title Identify sites that qualify as episodic winter sites
#' @description Using peaks identified with `find_event_peaks()`, this function
#' will summarize peaks per site based on whether or not they occurred during
#' winter. Then, it will add a new field indicating whether the site qualifies
#' as an "episodic winter salting site", meaning that specific conductance
#' peaks in winter are much larger than those outside of winter.
#' 
#' @param ts_peak_data a tibble output from `find_event_peaks()` with at least the 
#' columns `site_no`, `dateTime`, and `peak_flag`.
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param num_peaks_per_year a single numeric value indicating the number of
#' top peaks to use per year in winter and outside of winter in order to 
#' compare winter peaks to non-winter peaks.
#' @param spec_cond_buffer a single numeric value in `uS/cm@25degC` indicating
#' the amount by which median winter peak specific conductance needs to be higher
#' than median non-winter peak specific conductance. 
#' 
summarize_salt_peaks <- function(ts_peak_data, winter_months = c(12,1,2,3), 
                                 num_peaks_per_year, spec_cond_buffer) {
  
  
  ts_peak_data %>% 
    
    # Identify which dates are in winter
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    
    # First keep only years where there is sufficient data in winter (>60 days)
    group_by(site_no, year = year(dateTime)) %>% 
    mutate(winterSum = sum(is_winter)) %>% 
    # This removes 488 site-years (out of 3,302 so ~15% dropped)
    filter(winterSum > 60) %>%
    
    # Now filter to only peaks themselves (which removes other data that may
    # be part of one of the peak events but isn't the peak itself)
    filter(peak_flag) %>% 
    
    # Change `is_winter` to be season and show winter vs not winter
    mutate(season = ifelse(is_winter, 'winter', 'not_winter')) %>% 
    select(-is_winter) %>% 
    
    # Find top peaks per site per year per season 
    group_by(site_no, year, season) %>% 
    arrange(desc(SpecCond)) %>% 
    slice(1:num_peaks_per_year) %>%
    ungroup() %>% 
    
    # Now calculate median SpC per site per season using only those top peaks
    group_by(site_no, season) %>% 
    summarize(medianSpC = mean(SpecCond, na.rm=TRUE), .groups='keep') %>% 
    ungroup() %>% 
    
    # Pivot so that each site has a row and each season has a column 
    # containing the median SpC of the top `num_peaks_per_year`
    pivot_wider(id_cols = c('site_no'), 
                names_from = 'season', 
                values_from = medianSpC) %>% 
    
    # Now determine whether a site is episodic by comparing winter vs non-winter
    # and applying a buffer so that winter is "much higher"
    mutate(is_episodic = winter >= not_winter + spec_cond_buffer)
  
}
