
#' @title Identify sites that qualify as episodic winter sites
#' @description Using peaks identified with `find_event_peaks()`, this function
#' will summarize peaks per site based on whether or not they occurred during
#' winter. Then, it will add a new field indicating whether the site qualifies
#' as an "episodic winter salting site", meaning that specific conductance
#' peaks occur during the winter a minimum percent of the time, the difference
#' between winter and non-winter average specific conductance is greater than 
#' some minimum percent, and the annual maximum specific conductance occurs during
#' the winter a minimum percent of the time.
#' 
#' @param ts_peak_data a tibble output from `find_event_peaks()` with at least the 
#' columns `site_no`, `dateTime`, and `peak_flag`.
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param min_perc_peaks_winter a single numeric value indicating the percentage of 
#' time the peak values should occur in winter to qualify the site as an episodic
#' winter salting site. Given as a fraction; defaults to `0.50` (or 50%).
#' @param min_perc_diff a single numeric value indicating the minimum percent  
#' difference between average winter and non-winter specific conductance to 
#' qualify the site as an episodic winter salting site. Given as a fraction; 
#' defaults to `0.10` (or 10%).
#' @param min_perc_winter_higher a single numeric value indicating the percentage
#' of years that the maximum annual specific conductance for a particular site
#' should occur during the winter in order to qualify as an episodic winter
#' salting site. Given as a fraction; defaults to `0.75` (or 75%).
#' 
summarize_salt_peaks <- function(ts_peak_data, winter_months = c(12,1,2,3), 
                                 min_perc_peaks_winter = 0.50, min_perc_diff = 0.10, 
                                 min_perc_winter_higher = 0.75) {
  
  # Calculate maximum SpC by site, year, and season. Then determine annual 
  # seasonal maximums to use when qualifying a site as an episodic site or
  # not because winter maximums should be greater than non-winter maximums 
  # a majority of the time series to qualify
  salt_sites_max_info <- ts_peak_data %>% 
    # Create a winter flag - if month in (12,1,2,3)
    mutate(year = year(dateTime), 
           is_winter = month(dateTime) %in% winter_months) %>% 
    group_by(site_no, year, is_winter) %>% 
    # Calculate two maximums per year - one for winter, one for not winter
    summarize(maxSpC = max(SpecCond, na.rm=TRUE), .groups = 'keep') %>% 
    # Pivot wider so that the winter and non-winter maximums are each their own column
    mutate(is_winter = ifelse(is_winter, 'winterSpCMax', 'notWinterSpCMax')) %>% 
    pivot_wider(names_from = is_winter, values_from = maxSpC) %>% 
    # Get rid of years that don't have a value for both winter/not winter
    filter(!is.na(winterSpCMax) & !is.na(notWinterSpCMax)) %>% 
    # Summarize the total number of years and the number of years where the
    # winter maximum was higher than the non-winter maximum
    group_by(site_no) %>% 
    summarize(n_years = n(), 
              n_winter_higher = sum(winterSpCMax > notWinterSpCMax), 
              .groups = 'keep') %>% 
    # Calculate the percent that the winter maximum was higher
    mutate(perc_winter_max_higher = n_winter_higher / n_years) %>% 
    select(site_no, perc_winter_max_higher)
  
  salt_sites_info <- ts_peak_data %>% 
    # Create a winter flag - if month in (12,1,2,3)
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    # Tally number of peaks and calculate average SpC for winter/non-winter 
    group_by(site_no, is_winter) %>% 
    summarize(n_peaks = sum(peak_flag, na.rm=T),
              avg_sc = mean(SpecCond, na.rm = T),
              .groups = 'keep') %>% 
    # Calculate the percent of peaks per season
    group_by(site_no) %>% 
    mutate(n_peaks_total = sum(n_peaks, na.rm=TRUE),
           peaks_perc = n_peaks/n_peaks_total) %>% 
    select(site_no, is_winter, peaks_perc, avg_sc) %>% 
    # Munge the data so that there is a column per metric per season
    pivot_longer(cols = -c(site_no, is_winter), names_to = 'metric') %>% 
    mutate(metric_season = sprintf('%s_%s', metric, ifelse(is_winter, 'winterYes', 'winterNo'))) %>% 
    pivot_wider(id_cols = site_no, names_from = metric_season, values_from = value) %>%
    # Calculate the percent difference between winter and non-winter average SpC
    group_by(site_no) %>% 
    mutate(sc_perc_diff = ((avg_sc_winterYes - avg_sc_winterNo) / avg_sc_winterYes)) %>%
    select(site_no, peaks_perc_winterYes, sc_perc_diff)
  
  # Now join these data for each site and determine which sites meet criteria 
  # for being considered 'episodic'. To qualify as winter salting sites for 
  # this analysis, 1) more than `min_perc_peaks_winter` of the SpC peaks need to happen 
  # during winter, 2) the percent difference between winter and non-winter
  # SpC averages should be greater than `min_perc_diff`, and 3) the maximum annual
  # SpC value should occur in winter more than `min_perc_winter_higher` percent
  salt_sites_info %>% 
    # Join in the data about winter maximums
    left_join(salt_sites_max_info, by = 'site_no') %>% 
    mutate(is_salt_site = 
             # More than `min_perc_peaks_winter` percent of global peaks must occur in winter
             peaks_perc_winterYes > min_perc_peaks_winter & 
             # The non-winter and winter mean SpC must be more than `min_perc_diff` percent different
             sc_perc_diff > min_perc_diff &
             # Maximum annual SpC should occur in winter more than `` percent of the years
             perc_winter_max_higher > min_perc_winter_higher)
  
}
