
#' @title Create a map of sites for a specific category
#' @description Show sites on a map for a single category
#' 
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param category_sites a vector of site numbers to map
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' @param site_color character string indicating what color to give the map markers
#' @param map_title character string for the plot title
#' @param point_title_subtitle_sizes vector of three values giving the point
#' size, the title text size, and the subtitle text size; defaults to `c(2, 14, 12)`
#' 
#' @returns a character string giving the location of the saved figure file
#' 
map_category_sites <- function(sites_sf, category_sites, states_to_include, site_color, 
                               map_title, point_title_subtitle_sizes = c(2, 14, 12)) {
  
  category_sites_sf <- sites_sf %>% 
    filter(site_no %in% category_sites)
  
  ggplot() +
    add_state_basemap(states_to_include) +
    geom_sf(data=category_sites_sf, fill = site_color,
            alpha=0.75, shape=24, size=point_title_subtitle_sizes[1]) +
    guides(color = 'none') +
    ggtitle(map_title, subtitle = sprintf('n = %s', length(category_sites))) +
    theme_void() +
    theme(text = element_text(hjust = 0.5, color = site_color),
          plot.title = element_text(size=point_title_subtitle_sizes[2], face='bold', hjust = 0.5),
          plot.subtitle = element_text(size=point_title_subtitle_sizes[3], hjust = 0.5),
          plot.margin = unit(c(0,0,0,0), unit='line'))
}

#' @title Create a figure with side-by-side maps of sites categorized by episodic or not
#' @description Maps both the episodic and not episodic sites in side-by-side maps
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_episodic_site_map <- function(out_file, sites_sf, all_site_categories, states_to_include) {
  
  episodic_sites <- all_site_categories %>% filter(site_category == 'Episodic') %>% pull(site_no)
  p_episodic <- map_category_sites(sites_sf, episodic_sites, states_to_include, '#0b5394', 'Episodic sites')
  
  notepisodic_sites <- all_site_categories %>% filter(site_category == 'Not episodic') %>% pull(site_no)
  p_notepisodic <- map_category_sites(sites_sf, notepisodic_sites, states_to_include, 'grey30', 'Not episodic sites')
  
  png(out_file, width = 6.5, height = 3.25, units='in', res=500)
  print(cowplot::plot_grid(p_episodic, p_notepisodic, nrow=1, labels=c("(a)", "(b)"), label_size = 10))
  dev.off()
  
  return(out_file)
}

#' @title Create a figure with maps of sites categorized by baseflow trend
#' @description Maps all three of the baseflow trend results in a single figure
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_baseflow_site_map <- function(out_file, sites_sf, site_category_data, states_to_include) {
  
  positive_sites <- site_category_data %>% filter(site_category == 'positive') %>% pull(site_no)
  p_positive <- map_category_sites(sites_sf, positive_sites, states_to_include, '#b45f06', 'Positive baseflow trend sites')
  
  none_sites <- site_category_data %>% filter(site_category == 'none') %>% pull(site_no)
  p_none <- map_category_sites(sites_sf, none_sites, states_to_include, 'grey30', 'No baseflow trend sites',
                               point_title_subtitle_sizes = c(1, 12, 10))
  
  negative_sites <- site_category_data %>% filter(site_category == 'negative') %>% pull(site_no)
  p_negative <- map_category_sites(sites_sf, negative_sites, states_to_include, '#663329', 'Negative baseflow trend sites',
                                   point_title_subtitle_sizes = c(1, 12, 10))
  
  p_nonenegative <- cowplot::plot_grid(p_none, p_negative, ncol=1, labels=c("(b)", "(c)"), label_size = 10)
  png(out_file, width = 6.5, height = 3.25, units='in', res=500)
  print(cowplot::plot_grid(p_positive, p_nonenegative, nrow=1, rel_heights = c(0.5, 1), 
                           labels = c("(a)", ""), label_size = 10))
  dev.off()
  
  return(out_file)
}

#' @title Create a figure with maps of sites categorized with the overlap between baseflow/episodic
#' @description Maps either the sites that are both baseflow and episodic salinization.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_overlap_site_map <- function(out_file, sites_sf, site_category_data, states_to_include) {
  
  both_sites <- site_category_data %>% 
    pivot_wider(names_from = 'model', values_from = 'site_category') %>% 
    filter(episodic == 'Episodic', baseflow == 'positive') %>% 
    pull(site_no)
  p_both <- map_category_sites(sites_sf, both_sites, states_to_include, 'grey20', 
                               'Positive baseflow trend\nand episodic sites')
  
  png(out_file, width = 3.25, height = 3.25, units='in', res=500)
  print(p_both)
  dev.off()
  
  return(out_file)
}
