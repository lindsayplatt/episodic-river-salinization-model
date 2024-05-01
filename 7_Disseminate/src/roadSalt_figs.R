
#' @title Create a boxplot of road salt attributes by site model category
#' @description Show how road salt may differ in distribution between the episodic
#' classification, the baseflow classification, and the overall distribution.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param site_attr_data a tibble with one row per site and any number of columns
#' giving different attributes; needs the columns `site_no` and `attr_[attribute]`
#' @param all_site_categories a tibble with one row per site per model to visualize.
#' Expects the columns `site_no`, `model`, `site_category`.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_roadSalt_boxplot <- function(out_file, site_attr_data, all_site_categories) {
  
  # Make a data set that includes all sites data in one 'category'
  all_site_no_category <- site_attr_data %>% 
    mutate(model = 'none', site_category = 'All Sites') %>% 
    select(site_no, model, site_category)
  
  # Combine with road salt and order the models by factor
  all_road_salt_data <- all_site_categories %>% 
    bind_rows(all_site_no_category) %>% 
    left_join(site_attr_data, by = 'site_no') %>% 
    mutate(fill_col = case_when(
      model == 'episodic' ~ '#0b5394',
      model == 'baseflow' ~ '#b45f06',
      model == 'none' ~ 'grey50'))  %>% 
    mutate(site_category_fact = factor(site_category, 
                                       levels = c('All Sites', 'Episodic', 'Not episodic',
                                                  'positive', 'none', 'negative'))) 
  
  count_vals <- function(data) {
    tibble(y = 300000,
           label = sprintf('n = %s', length(data)))
  }
  
  p_boxes <- ggplot(all_road_salt_data, 
         aes(x = site_category_fact,
             y = attr_roadSaltPerSqKm, 
             fill = fill_col)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_identity(guide = 'none') +
    stat_summary(fun.data = count_vals, geom = "text", vjust=-1) +
    xlab('') + ylab(expression("Road salt applied per "~km^2~" (kg)")) +
    theme_bw() +
    theme(text = element_text(size=14),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank())
  
  ggsave(out_file, p_boxes, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}

#' @title Create a map of sites with road salt values categorized as low, high, medium
#' @description Show how road salt is distributed spatially
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param site_attr_data a tibble with one row per site and any number of columns
#' giving different attributes; needs the columns `site_no` and `attr_[attribute]`
#' @param sites_sf a spatial data frame with locations for NWIS sites. Needs
#' at least a `site_no` and `geometry` column.
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_roadSalt_site_map <- function(out_file, site_attr_data, sites_sf, states_to_include) {
  
  # Calculate the 25th, 75th, and max values in the road salt data
  quants_these_sites <- quantile(site_attr_data$attr_roadSaltPerSqKm, probs = c(0.25, 0.75, 1))
  quants_these_sites <- quants_these_sites * c(1,1,1.01) # Ensure that the maximum value is actually included
  
  sites_w_roadSalt <- sites_sf %>% 
    # First filter to only the sites we modeled with
    filter(site_no %in% unique(site_attr_data$site_no)) %>% 
    # Then join road attribute data and categorize road salt
    left_join(select(site_attr_data, site_no, attr_roadSaltPerSqKm), by = 'site_no') %>% 
    mutate(roadSalt_cat = cut(attr_roadSaltPerSqKm, 
                              breaks = c(0,quants_these_sites), 
                              labels = c('Low', 'Medium', 'High'), 
                              right = TRUE))
  
  p_sitemap <- ggplot() +
    add_state_basemap(states_to_include) +
    geom_sf(data=sites_w_roadSalt, aes(fill = roadSalt_cat),
            alpha=0.75, shape=24, size=3) +
    scico::scale_fill_scico_d(palette = 'batlow', 
                              name = expression(atop('Road salt applied',' per'~km^2~'(kg)'))) +
    theme_void() +
    theme(plot.background = element_rect(fill = 'white', color = 'white'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title.position = "left",
          legend.position = "bottom",
          legend.direction = "vertical")
  
  ggsave(out_file, p_sitemap, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}

#' @title Create a map of gridded road salt data
#' @description Show how road salt may differ in distribution between the episodic
#' classification, the baseflow classification, and the overall distribution.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param road_salt_tif filepath to the road salt tif file
#' @param states_to_include a vector of state two-letter abbreviation codes to
#' create the map object using `usmap` package fxns.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_roadSalt_map <- function(out_file, road_salt_tif, states_to_include) {
  
  # Create state polygons to crop raster
  states_sf <- usmap::us_map(include = states_to_include) %>% 
    st_as_sf(coords = c('x', 'y'), 
             crs = usmap::usmap_crs())
  
  # Prepare road salt raster data
  road_salt_rast <- raster::raster(road_salt_tif) %>% 
    # Reproject the road salt raster data
    raster::projectRaster(., crs = usmap::usmap_crs()$input) %>% 
    # Crop to the exact states polygons
    crop(., extent(states_sf)) %>% 
    mask(., states_sf)
  
  # Calculate quantiles for this region of road salt
  salt_quants <- quantile(road_salt_rast, probs = c(0.25, 0.75, 1))
  salt_quants <- salt_quants * c(1,1,1.01) # Ensure that the maximum value is actually included
  
  # Prepare road salt grids for plotting - only those above 0 and in categories
  road_salt_tbl <- road_salt_rast %>% 
    as.data.frame(., xy=TRUE) %>% 
    filter(road_salt_2015 > 0) %>% 
    mutate(roadSalt_cat = cut(road_salt_2015, 
                              breaks = c(0,salt_quants), 
                              labels = c('Low (< 25%ile)', 'Medium', 'High (> 75%ile)'), 
                              right = TRUE))
  
  p_saltmap <- ggplot() +
    add_state_basemap(states_to_include) +
    geom_raster(data = road_salt_tbl, aes(x=x, y=y, fill = roadSalt_cat)) +
    scico::scale_fill_scico_d(palette = 'bilbao', direction = -1, end = 0.75,
                            name = expression(atop('Road salt applied',' per'~km^2~'(kg)'))) +
    theme_void() +
    theme(plot.background = element_rect(fill = 'white', color = 'white'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title.position = "left",
          legend.position = "bottom",
          legend.direction = "vertical")
  
  ggsave(out_file, p_saltmap, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}
