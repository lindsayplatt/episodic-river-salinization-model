
#' @title Join attribute data with site categorization
#' @description Takes all site attributes and adds data about how each site is 
#' classified. Depending on the data passed it, this will show site categories 
#' for 1) episodic or not episodic, 2) baseflow trend positive or negative or 
#' none, 3) or a combination of both (episodic, baseflow positive trend, both,
#' or neither). The output can be passed to a random forest.
#' 
#' @param site_attr_data a tibble with one row per site and any number of columns
#' giving different attributes; needs the columns `site_no` and `attr_[attribute]`
#' @param sites_episodic a character vector giving site numbers that met criteria
#' for being an "episodic site". See targets in `4_EpisodicSalinization`. When this
#' parameter is specified with `site_baseflow_trend_info` being NULL, the random
#' forest will only use whether a site was episodic or not to assess static attributes.
#' @param site_baseflow_trend_info a tibble specifying the baseflow trend found
#' for each site; should have a row per site and the columns `site_no` and
#' `baseflowTrend`. When this is specified with `sites_episodic` being NULL, the
#' random forest will only use the baseflow trends (positive, none, or negative)
#' to assess static attributes.
#' 
#' @returns a tibble with the columns `site_no`, `site_category_fact`, and any 
#' number of columns that give static attributes but are NO LONGER prefixed `attr_`
#' 
prep_attr_randomforest <- function(site_attr_data, sites_episodic = NULL, site_baseflow_trend_info = NULL) {
  
  if(!is.null(sites_episodic) & !is.null(site_baseflow_trend_info)) {
    # Prep attributes for running an RF based on both baseflow salinization trend & site episodic behavior
    
    # Filter to just those sites with positive baseflow trends
    sites_baseflow <- site_baseflow_trend_info %>% filter(baseflowTrend == 'positive') %>% pull(site_no)
    
    # TODO: expand categories later by considering a combination of episodic + trend type (rather than just positive)
    #   Both episodic and positive baseflow trend
    #   Episodic with negative baseflow trend
    #   Episodic without a baseflow trend
    #   Not episodic but with positive baseflow trend
    #   Not episodic and with negative baseflow trend
    #   Neither episodic nor baseflow trend
    
    site_category_data <- site_attr_data %>% 
      mutate(site_category = case_when(
        site_no %in% sites_episodic & site_no %in% sites_baseflow ~ 'Both',
        site_no %in% sites_episodic & !site_no %in% sites_baseflow ~ 'Episodic',
        !site_no %in% sites_episodic & site_no %in% sites_baseflow ~ 'Baseflow',
        TRUE ~ 'Neither'
      ))
    
  } else if(is.null(sites_episodic) & !is.null(site_baseflow_trend_info)) {
    # Prep attributes for running an RF based on only baseflow trends
    site_category_data <- site_baseflow_trend_info %>% 
      left_join(site_attr_data, by = 'site_no') %>% 
      mutate(site_category = baseflowTrend)
  } else if(!is.null(sites_episodic) & is.null(site_baseflow_trend_info)) {
    # Prep attributes for running an RF based solely on episodic sites or not
    site_category_data <- site_attr_data %>% 
      mutate(site_category = case_when(
        site_no %in% sites_episodic ~ 'Episodic',
        !site_no %in% sites_episodic ~ 'Not episodic',
        TRUE ~ 'Neither'
      )) 
  } else {
    stop('Too many inputs are NULL. Need with `sites_episodic`, `site_baseflow_trend_info`, or both')
  }
  
  # Now prepare the rest of the data by removing some attributes that we can't use right now
  site_category_data %>% 
    # Make the site category a factor so that random forest
    # is doing classification, not regression
    mutate(site_category_fact = factor(site_category)) %>% 
    # Remove the columns that are not needed for the RF (except site_no)
    select(site_no, site_category_fact, starts_with('attr')) %>% 
    # Drop the `attr_` prefix so that the results are cleaner
    rename_with(~gsub("attr_", "", .x))
  
}

#' @title Log transform specific attributes for plotting purposes
#' @description Some of the variables will be better suited to visualize on a log
#' scale. Given the faceting steps, it was complex to add a log y scale to specfic
#' factets, so we are first transforming the data and renaming the attributes to 
#' signify that they have been logged. The attributes currently coded to be logged 
#' are `areaCumulativeSqKm` and `medianFlow`. If more should be logged, they will
#' need to be manually added to this function.
#' 
#' @param site_attr_data a tibble with the columns `site_category`, `attribute`,
#' and `attr_val`. Note that the values in `attribute` do not have the prefix `attr_`
#' 
#' @returns a tibble of the same row x column dimensions but with different values
#' and names (prefixed with `log_`) for any attribute that is currently coded to
#' have a log scale within the function.
#' 
log_transform_to_select_attributes <- function(site_attr_data) {
  attributes_to_log <- c('areaCumulativeSqKm', 'medianFlow')
  
  site_attr_data %>% 
    rowwise() %>% 
    mutate(attr_val = ifelse(attribute %in% attributes_to_log, log(attr_val), attr_val),
           attribute = ifelse(attribute %in% attributes_to_log, paste0('log_', attribute), attribute))
}
