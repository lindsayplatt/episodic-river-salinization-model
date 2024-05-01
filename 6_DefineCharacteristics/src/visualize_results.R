
#' @title Create a 4-panel view of random forest results
#' @description This function creates a 4-panel figure that includes attribute
#' distributions for different categories as boxplots and maps, the importance
#' of different attributes as determined by the random forest model, and the
#' partial dependence plots. These figures can be used to help evaluate the output.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param partial_dep_ggplot ggplot object output from `visualize_partial_dependence()`
#' @param attributes_boxes_ggplot ggplot object output from `visualize_numeric_attrs()`
#' @param rf_importance_ggplot ggplot object output from `visualize_attr_importance()`
#' @param rf_category_map ggplot object output from `visualize_catgory_sites_map()`
#' 
#' @results a character string giving the location of the saved figure file
#' 
create_summary_view <- function(out_file, partial_dep_ggplot, attributes_boxes_ggplot, 
                                rf_importance_ggplot, rf_category_map) {
  
  rf_plots <- list(partial_dep_ggplot, attributes_boxes_ggplot, 
                   rf_importance_ggplot, rf_category_map)
  png(out_file, width = 22, height = 17, res = 100, units = 'in')
  print(cowplot::plot_grid(plotlist = rf_plots, nrow=2, 
                     rel_heights = c(0.60, 0.40), 
                     rel_widths = c(0.60, 0.40)))
  dev.off()
  
  return(out_file)
}

create_site_catchment_map <- function(the_site_no, site_nhd_xwalk, nhd_comids_upstream,
                                      site_attributes, sites_sf,
                                      flowlines_sf, catchments_sf) {
  
  the_comid <- filter(site_nhd_xwalk, site_no == the_site_no) %>% 
    pull(nhd_comid)
  
  upstream_comids <- nhd_comids_upstream %>% 
    filter(nhd_comid == the_comid) %>% 
    pull(nhd_comid_upstream)
  
  flowlines_sf <- flowlines_sf %>% 
    filter(nhd_comid %in% upstream_comids)
  catchment_sf <- p2_nhdplus_catchment_sf %>% 
    filter(nhd_comid %in% upstream_comids)
  
  the_stream_site_sf <- sites_sf %>% 
    filter(site_no %in% the_site_no)
  the_catchment_sf <- catchments_sf %>% 
    filter(nhd_comid %in% the_comid)
  
  the_site_attrs <- site_attributes %>% 
    filter(site_no %in% the_site_no)
  
  fill_category <- case_when(
    the_site_attrs$site_category_fact == 'Baseflow' ~ 'lightcyan2',
    the_site_attrs$site_category_fact == 'Episodic' ~ 'mistyrose2',
    the_site_attrs$site_category_fact == 'Both' ~ 'antiquewhite3',
    the_site_attrs$site_category_fact == 'Neither' ~ 'white')
  
  ggplot() + 
    geom_sf(data = catchment_sf, fill='lightgrey', color = 'white', linewidth=0.5) +
    geom_sf(data = the_catchment_sf, fill = 'lightcoral', color='white') +
    geom_sf(data = flowlines_sf, color = 'cornflowerblue', aes(linewidth = streamorde)) +
    scale_linewidth_continuous(range = c(0.25,4)) +
    geom_sf(data = the_stream_site_sf, shape = 21, color = 'black', 
            bg = 'black', size=4) +
    ggtitle(sprintf('USGS gage %s', the_site_no),
            subtitle = sprintf('Categorization: %s', the_site_attrs$site_category_fact)) +
    theme_void() + 
    theme(plot.background = element_rect(fill = fill_category, color = NA))
  
}

# library(tidyverse)
# library(targets)
# library(sf)
# 
# tar_load(p2_nhdplus_flowlines_ALL_sf)
# tar_load(p2_nhdplus_catchment_sf)
# tar_load(p3_nwis_site_nhd_comid_xwalk)
# tar_load(p6_site_attr)
# tar_load(p1_nwis_sc_sites_sf)
# tar_load(p1_nhdplus_comids_upstream_ALL)
# tar_load(p3_static_attributes)
# 
# # # 1 site's catchment
# # site1 <- p3_nwis_site_nhd_comid_xwalk$site_no[1]
# # map_list <- tar_read(p3_static_attributes) %>% 
# #   filter(site_no == '03072655') %>% 
# #   split(.$site_no) %>%
# #   # head(10) %>%
# #   purrr::map(
# #     ~create_site_catchment_map(
# #       .x$site_no, 
# #       p3_nwis_site_nhd_comid_xwalk, 
# #       p1_nhdplus_comids_upstream_ALL,
# #       p6_site_attr,
# #       p1_nwis_sc_sites_sf,
# #       p2_nhdplus_flowlines_ALL_sf,
# #       p2_nhdplus_catchment_sf) 
# #   )
# # 
# # cowplot::plot_grid(plotlist = map_list)
# # tar_load(p3_static_attributes)
# 
# pdf('all_site_catchment_maps.pdf', width = 25, height = 12.5)
# grp_size <- 12
# n_grps <- ceiling(nrow(p3_static_attributes)/grp_size)
# for(grp in 1:n_grps) {
#   i_start <- ((grp-1)*grp_size)+1
#   i_end <- ifelse(grp == n_grps, nrow(p3_static_attributes), grp*grp_size)
#   sites_in_grp <- p3_static_attributes$site_no[i_start:i_end]
#   
#   map_list <- p3_static_attributes %>% 
#     filter(site_no %in% sites_in_grp) %>% 
#     split(.$site_no) %>% 
#     purrr::map(
#       ~create_site_catchment_map(
#         .x$site_no, 
#         p3_nwis_site_nhd_comid_xwalk, 
#         p1_nhdplus_comids_upstream_ALL,
#         p6_site_attr,
#         p1_nwis_sc_sites_sf,
#         p2_nhdplus_flowlines_ALL_sf,
#         p2_nhdplus_catchment_sf) 
#     )
#   
#   print(cowplot::plot_grid(plotlist = map_list, nrow = 4))
# }
# dev.off()
