
#' @title Create boxplots of attribute distributions by site categories
#' @description This function creates a large figure with a facet per static
#' attribute and a boxplot per site category within each facet. Boxes are colored
#' by site category. Y axes are free to scale to each parameter, but some attributes
#' are specifically plot on a log scale (and any other attributes that need to be 
#' on a log scale would need to be manually added to the `log_transform_to_select_attributes()`
#' function in `6_DefineCharacteristics/src/prep_attr_randomforest.R`). 
#' 
#' @param site_attr_data a tibble with the columns `site_no`, `site_category_fact`, 
#' and any number of columns that give static attributes (not prefixed with `attr_`)
#' 
#' @returns a ggplot object
#' 
visualize_numeric_attrs <- function(site_attr_data) {
  # Plotting attribute distributions per site category
  
  data_to_plot <- site_attr_data %>% 
    dplyr::select(site_category_fact, where(is.numeric)) %>%
    pivot_longer(cols = -site_category_fact, 
                 names_to = 'attribute', 
                 values_to = 'attr_val') %>% 
    # Log scale appropriate attributes for visual purposes
    log_transform_to_select_attributes()
  
  ggplot(data_to_plot, aes(x = site_category_fact, y = attr_val)) +
    geom_boxplot(aes(fill = site_category_fact)) + 
    facet_wrap(vars(attribute), scales = 'free_y') +
    theme_bw() +
    scico::scale_fill_scico_d(begin = 0, end = 0.75) +
    xlab('Site Category') + ylab('Attribute value (various scales and units)') +
    ggtitle('Attributes by site categorization') +
    theme(strip.background = element_rect(fill = 'white', color = 'transparent'))
}

#' @title Create a basemap of specific states
#' @description This function creates a basemap of specific states to be used
#' along with the rest of a `ggplot()` command. The code is only set up to 
#' return a `geom_sf()` object with darkgrey outlines and white fill. This will
#' use the default `usmap` projection and place Alaska and Hawaii in the empty 
#' space west of Texas and south of California. It will not work on US territories.
#' 
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns. 
#' 
#' @returns specifically a `geom_sf()` object that must be used along with a 
#' complete `ggplot()` command to actually visualize 
#' 
add_state_basemap <- function(states_to_include) {
  states_sf <- usmap::us_map(include = states_to_include) %>% 
    st_as_sf(coords = c('x', 'y'), 
             crs = usmap::usmap_crs())
  
  geom_sf(data=states_sf, fill = 'white', color = 'darkgrey')
}

#' @title Create maps showing sites and their category
#' @description This function creates a figure with a facet per site category
#' and shows a map with state outlines and site locations within each facet.
#' 
#' @param site_attributes a tibble with at least the columns `site_no` and
#' `site_category_fact` that will be used to distinguish which category a site 
#' belongs to on a faceted map per site category.
#' @param sites_sf 
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns via `add_state_basemap()`. 
#' 
#' @returns a ggplot object
#' 
visualize_catgory_sites_map <- function(site_attributes, sites_sf, states_to_include) {
  
  category_sites_sf <- sites_sf %>% 
    left_join(site_attributes, by = 'site_no') %>% 
    st_transform(crs=usmap::usmap_crs()) %>% 
    filter(!is.na(site_category_fact)) %>% 
    mutate(site_category = as.character(site_category_fact)) %>% 
    group_by(site_category) %>% 
    mutate(n_category = n()) %>% 
    ungroup() %>% 
    mutate(category_title = sprintf('%s (n = %s)', site_category, n_category))
  
  ggplot() +
    add_state_basemap(states_to_include) +
    geom_sf(data=category_sites_sf, 
            aes(fill = site_category_fact), 
            alpha=0.75, shape=24, size=5) +
    guides(color = 'none') +
    scico::scale_fill_scico_d(begin = 0, end = 0.75) +
    facet_wrap(vars(category_title), ncol=2) +
    theme_void()
  
}

visualize_all_attributes <- function(site_attributes) {
  site_attributes %>% 
    select(starts_with('attr_')) %>% 
    pivot_longer(everything(), names_to = 'attribute') %>% 
    mutate(attribute = gsub('attr_', '', attribute)) %>% 
    rowwise() %>% 
    mutate(value = ifelse(attribute %in% c('medianFlow', 'roadSaltPerSqKm') & value > 0, 
                          log10(value), value)) %>% 
    ggplot(aes(x = attribute, y = value, fill=attribute)) +
    geom_boxplot() +
    # geom_violin(color = NA) +
    facet_wrap(vars(attribute), scales='free') +
    scico::scale_fill_scico_d() +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 14, face='bold'),
          legend.position = 'none')
}
