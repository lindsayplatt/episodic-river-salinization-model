
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
          plot.title = element_text(size=point_title_subtitle_sizes[2], face='bold', hjust = 0.5, vjust = -3),
          plot.subtitle = element_text(size=point_title_subtitle_sizes[3], hjust = 0.5, vjust = -3),
          plot.margin = unit(c(0,0,0,0), unit='line'))
}

#' @title Create a figure with side-by-side figs displaying sites as episodic or not
#' @description Maps both the episodic and not episodic sites in side-by-side maps
#' and includes a figure of the criteria used to determine if a site was episodic.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' @param sites_category_criteria a tibble with one row per site and the criteria
#' used to classify it as well as the class. Expects the columns `site_no`,
#' `not_winter`, `winter`, and `is_episodic`.
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' @param episodic_col character string indicating what color to give episodic data
#' @param not_episodic_col character string indicating what color to give not episodic data
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_episodic_criteria_fig <- function(out_file, sites_sf, all_site_categories, 
                                         sites_category_criteria, states_to_include, 
                                         episodic_col, not_episodic_col) {
  
  # Map of episodic sites
  episodic_sites <- all_site_categories %>% filter(site_category == 'Episodic') %>% pull(site_no)
  p_episodic <- map_category_sites(sites_sf, episodic_sites, states_to_include, 
                                   episodic_col, 'Episodic sites', 
                                   point_title_subtitle_sizes = c(1.5, 10, 8)) +
    theme(plot.title.position = 'plot')
  
  # Map of not episodic sites
  notepisodic_sites <- all_site_categories %>% filter(site_category == 'Not episodic') %>% pull(site_no)
  p_notepisodic <- map_category_sites(sites_sf, notepisodic_sites, states_to_include, 
                                      not_episodic_col, 'Not episodic sites', 
                                      point_title_subtitle_sizes = c(1.5, 10, 8))
  
  # # Combine the two maps so that they are vertical
  # p_maps <- cowplot::plot_grid(p_episodic, p_notepisodic, nrow=2, 
  #                              labels=c("(a)", "(b)"), label_size = 8)
    
  # Criteria of episodic vs not  
  p_criteria <- sites_category_criteria %>% 
    mutate(is_episodic = ifelse(is_episodic, 'Episodic', 'Not episodic')) %>% 
    ggplot(aes(x = not_winter, y = winter, fill = is_episodic)) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = 0.5, size=2, shape = 21) +
    scale_fill_manual(values = c(`Not episodic` = not_episodic_col, 
                                 Episodic = episodic_col),
                      name = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 2),
          plot.margin = unit(c(1.5,0.5,1,1), unit='line')) +
    xlab('Non-Winter Mean Peak SpC') +
    ylab('Winter Mean Peak SpC')
  
  # 1 column width
  png(out_file, width = 8.7, height = 15, units='cm', res=500)

  print(cowplot::plot_grid(p_episodic, p_notepisodic, p_criteria,
                           ncol=1, labels=c("(a)", "(b)", "(c)"), 
                           label_size = 8, rel_heights = c(1,1,1)))
  # print(cowplot::plot_grid(p_maps, p_criteria, nrow=1, labels=c("", "(c)"), 
  #                          label_size = 8, rel_widths = c(0.4, 0.6)))
  dev.off()
  
  return(out_file)
}
