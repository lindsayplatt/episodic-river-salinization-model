# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/attribute_correlations.R')
source('7_Disseminate/src/roadSalt_figs.R')
source('7_Disseminate/src/partialDependence_figs.R')
source('7_Disseminate/src/importance_figs.R')
source('7_Disseminate/src/attribute_boxplot_figs.R')
source('7_Disseminate/src/category_map_figs.R')
source('7_Disseminate/src/episodic_detection_figs.R')

p7_targets <- list(
  
  # Create a manual crosswalk between attribute names and display names
  tar_target(p7_attr_name_xwalk, tibble(
    attribute = c("medianFlow", "roadSaltCumulativePerSqKm", "annualPrecip", "baseFlowInd", 
                  "subsurfaceContact", "gwRecharge", "pctOpenWater", "basinSlope", 
                  "pctForested", "pctWetland", "pctAgriculture", "pctDeveloped", 
                  "annualSnow", "winterAirTemp", "depthToWT", "transmissivity"),
    display_name = c("Median Flow (m3/s)", "Watershed Road Salt (kg/km2)", "Precip (mm/yr)", "Baseflow Index",
                     "Subsurface Contact (days)", "GW Recharge (mm/yr)", "Open Water (% area)", "Basin Slope (%)",
                     "Forested (% area)", "Wetland (% area)", "Agriculture (% area)", "Developed (% area)", 
                     "Snow (mm/yr)", "Winter Air Temp (°C)", "Depth to WT (m)", "Transmissivity (m2/day)"))),
  
  # Isolate overall importance and out-of-bag errors
  tar_target(p7_overall_attr_importance_episodic, p5_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_episodic, round(p5_rf_oob*100, digits=1)),
  
  # Create a single dataset that shows only site episodic categorization
  tar_target(p7_site_categories, p5_site_attr %>% 
               mutate(model = 'episodic') %>% 
               select(site_no, site_category = site_category_fact)),
  
  ##### Attribute correlation figure #####
  
  # Saving attribute correlations as a file
  tar_target(p7_attribute_correlations_png, 
             visualize_attr_correlation('7_Disseminate/out/3_attribute_correlations.png',
                                        p5_site_attr_rf,
                                        p7_attr_name_xwalk), 
             format='file'),
  
  ##### Road salt figures #####
  
  # Boxplot of roadsalt showing that sites across all the categories had similar amounts
  tar_target(p7_roadsalt_boxes_png, 
             create_roadSalt_boxplot('7_Disseminate/out/4_roadSalt_boxes.png', p3_static_attributes, p7_site_categories),
             format = 'file'),
  
  # Map of roadsalt per site showing that sites don't completely follow a gradient from south --> north
  tar_target(p7_roadsalt_sitemap_png, 
             create_roadSalt_site_map('7_Disseminate/out/roadSalt_sitemap.png', p3_static_attributes, p1_nwis_sc_sites_sf, p1_conus_state_cds),
             format = 'file'),
  
  # Map of gridded roadsalt 
  tar_target(p7_roadsalt_gridmap_png, 
             create_roadSalt_map('7_Disseminate/out/2.2_roadSalt_gridmap.png', p2_road_salt_rast, p1_conus_state_cds),
             format = 'file'),
  
  ##### Partial dependence plots #####
  
  # Partial dependence plots showing how probability varies by attribute value
  tar_target(p7_partDep_episodic_png, 
             create_partialDependence_miniPlots_figure('7_Disseminate/out/partDep_episodic.png', 
                                                       p5_rf_attr_partdep, p5_site_attr_rf_optimal,
                                                       p7_overall_attr_importance_episodic$attribute,
                                                       p7_attr_name_xwalk, '#0b5394'),
             format = 'file'),
  
  ##### Attribute importance figures #####
  
  tar_target(p7_importance_episodic_png, 
             visualize_attr_importance('7_Disseminate/out/importance_episodic.png', 
                                       p7_overall_attr_importance_episodic,
                                       p7_attr_name_xwalk, scico_palette = 'lapaz'), 
             format='file'),
  
  ##### Boxplots of attributes by model/category #####
  
  tar_target(p7_attr_all_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/3_attributes_boxes_all.png',
                                       mutate(p5_site_attr_rf, site_category_fact = 'ALL'),
                                       # Same order as the table
                                       c('medianFlow', 'basinSlope', 'pctAgriculture',
                                         'pctDeveloped', 'pctForested', 'pctOpenWater',
                                         'pctWetland', 'annualPrecip', 'annualSnow',
                                         'winterAirTemp', 'baseFlowInd', 'gwRecharge',
                                         'subsurfaceContact', 'depthToWT', 
                                         'transmissivity', 'roadSaltCumulativePerSqKm'),
                                       p7_attr_name_xwalk, 
                                       c(ALL='#868b8e'),
                                       legend_position = "none",
                                       attribute_text_size = 9), 
             format='file'),
  
  tar_target(p7_attr_episodic_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_episodic.png',
                                       p5_site_attr_rf_optimal,
                                       p7_overall_attr_importance_episodic$attribute,
                                       p7_attr_name_xwalk, 
                                       c(Episodic='#0b5394', `Not episodic` = 'grey40')), 
             format='file'),
  
  ##### Combine random forest output into a single figure #####
  
  # Note that `cowplot::draw_image()` requires that you have `magick`
  tar_target(p7_rf_results_episodic_png, {
    out_file <- '7_Disseminate/out/3.1_episodic_rf_results.png'
    fig_importance <- cowplot::ggdraw() + cowplot::draw_image(p7_importance_episodic_png)
    fig_partDep <- cowplot::ggdraw() + cowplot::draw_image(p7_partDep_episodic_png)
    fig_boxes <- cowplot::ggdraw() + cowplot::draw_image(p7_attr_episodic_boxplots_png)
    png(out_file, width = 6.5, height = 3.25, units = 'in', res = 500)
    print(cowplot::plot_grid(fig_importance, fig_partDep, fig_boxes, 
                             nrow=1, label_size=10,
                             labels=sprintf('(%s)', letters[1:3])))
    dev.off()
    return(out_file)
  }, format='file'),
  
  ##### Map of sites by category #####
  
  tar_target(p7_all_sitemap_png, {
    out_file <- '7_Disseminate/out/2.2_sitemap_all_qualified.png'
    p_map <- map_category_sites(p1_nwis_sc_sites_sf, p3_static_attributes$site_no, p1_conus_state_cds, 
                                site_color = 'grey30', map_title = 'Qualified sites')
    ggsave(out_file, p_map, width = 3.25, height = 3.25, dpi = 500, bg='white')
    return(out_file)
  }, format='file'),
  
  tar_target(p7_episodic_sitemap_png, 
             create_episodic_site_map('7_Disseminate/out/3.1_episodic_sitemap.png', p1_nwis_sc_sites_sf, 
                                      p7_site_categories, p1_conus_state_cds), 
             format='file'),
  
  ##### Save plots of episodic behavior for each site #####
  
  tar_target(p7_episodic_plotlist, create_episodic_plotlist(p3_ts_sc_qualified, p4_episodic_sites)),
  tar_target(p7_episodic_png, 
             ggsave(filename = sprintf('7_Disseminate/out/SI_episodic_grp%s.png', names(p7_episodic_plotlist)), 
                    plot = p7_episodic_plotlist[[1]], height = 8, width = 10, dpi = 500), 
             format = 'file', pattern = map(p7_episodic_plotlist)),
  
  tar_target(p7_episodic_examples_plot, {
    example_episodic_sites <- c('02042500', '01481500', '04166500') # Richmond, Wilmington, Detroit
    example_not_episodic_sites <- c('01481000', '03183500', '04176500') # NW of Wilmington, SE of Philly, West Virginia, S of Detroit
    p3_ts_sc_qualified %>% 
      filter(site_no %in% c(example_episodic_sites, example_not_episodic_sites)) %>%
      create_episodic_plotlist(example_episodic_sites, nrow=2) 
  }),
  tar_target(p7_episodic_examples_png, {
    out_file <- '7_Disseminate/out/3.1_episodic_ts.png'
    png(out_file, width = 13, height = 6.5, units='in', res=500)
    print(p7_episodic_examples_plot)
    dev.off()
    return(out_file)
  }, format='file')
  
)
