# Targets for creating paper and slidedeck visualizations

source('7_Disseminate/src/attribute_correlations.R')
source('7_Disseminate/src/roadSalt_figs.R')
source('7_Disseminate/src/partialDependence_figs.R')
source('7_Disseminate/src/importance_figs.R')
source('7_Disseminate/src/attribute_boxplot_figs.R')
source('7_Disseminate/src/category_map_figs.R')
source('7_Disseminate/src/episodic_detection_figs.R')
source('7_Disseminate/src/RF_compilation_figs.R')
source('7_Disseminate/src/getWatershedArea.R')

p7_targets <- list(
  
  # Define episodic vs not episodic colors
  tar_target(p7_color_episodic, '#c28e0d'),
  tar_target(p7_color_not_episodic, '#005271'),
  
  # Create a manual crosswalk between attribute names and display names
  tar_target(p7_attr_name_xwalk, tibble(
    attribute = c("medianFlow", "roadSaltCumulativePerSqKm", "annualPrecip", "baseFlowInd", 
                  "subsurfaceContact", "gwRecharge", "pctOpenWater", "basinSlope", 
                  "pctForested", "pctWetland", "pctAgriculture", "pctDeveloped", 
                  "annualSnow", "winterAirTemp", "depthToWT", "transmissivity", 
                  "areaCumulativeSqKm", "streamorder"),
    display_name = c("Median Flow (m3/s)", "Watershed Road Salt (kg/km2)", "Precip (mm/yr)", "Baseflow Index",
                     "Subsurface Contact (days)", "GW Recharge (mm/yr)", "Open Water (% area)", "Basin Slope (%)",
                     "Forested (% area)", "Wetland (% area)", "Agriculture (% area)", "Developed (% area)", 
                     "Snow (mm/yr)", "Winter Air Temp (°C)", "Depth to WT (m)", 
                     "Transmissivity (m2/day)", "Watershed Area (km2)", "Stream Order"))),
  
  # Create a single dataset that shows only site episodic categorization
  tar_target(p7_site_categories, p5_site_attr %>% 
               mutate(model = 'episodic') %>% 
               select(site_no, site_category = site_category_fact)),
  
  # This takes a few minutes, could probably figure out something faster 
  tar_target(p7_huc_flowlines_distinct, 
               bind_rows(p6_huc_flowlines_sf) %>%
               select(-huc2) %>% # to get rid of duplicates between hucs ~ n = 419
               rename(nhd_comid = COMID) %>% 
               distinct()),
  
  ##### Main manuscript figures #####
  
  ###### Figure 1: episodic vs not + qualifying criteria ######
  
  tar_target(p7_episodic_classification_png, 
             create_episodic_criteria_fig('7_Disseminate/out/Fig1_episodic_results.png', 
                                          p1_nwis_sc_sites_sf, p7_site_categories,
                                          p4_ts_sc_peak_summary, p1_conus_state_cds, 
                                          p7_color_episodic, p7_color_not_episodic), 
             format='file'),
  
  ###### Figure 2: select SpC time series ######
  
  tar_target(p7_episodic_examples_plot, {
    example_episodic_sites <- c('02042500', '01481500', '04166500') # Richmond, Wilmington, Detroit
    example_not_episodic_sites <- c('05062130', '03183500', '04176500') # Red River, SE of Philly, West Virginia, S of Detroit
    ts_sc = p3_ts_sc_qualified %>% 
      filter(site_no %in% c(example_episodic_sites, example_not_episodic_sites))
    
    create_episodic_plotlist(ts_sc, sites_episodic = example_episodic_sites, 
                               episodic_col = p7_color_episodic, 
                               not_episodic_col = p7_color_not_episodic, 
                               usenrow=2, addNWISName = TRUE) 
  }),
  tar_target(p7_episodic_examples_png, {
    out_file <- '7_Disseminate/out/Fig2_episodic_ts.png'
    png(out_file, width = 6.5, height = 4.5, units='in', res=500)
    print(p7_episodic_examples_plot)
    dev.off()
    return(out_file)
  }, format='file'),
  
  ###### Figure 3: random forest results ######
  
  # Isolate overall importance and out-of-bag errors
  tar_target(p7_overall_attr_importance_episodic, p5_rf_attr_importance %>% 
               filter(site_category == 'Overall mean') %>% 
               arrange(desc(importance))),
  tar_target(p7_oob_error_episodic, round(p5_rf_oob*100, digits=1)),
  
  # Plot attribute importance as ranked dots
  tar_target(p7_importance_episodic_png, 
             visualize_attr_importance('7_Disseminate/out/importance_episodic.png', 
                                       p7_overall_attr_importance_episodic,
                                       p7_attr_name_xwalk, 
                                       point_seg_col = p7_color_episodic), 
             format='file'),
  
  # Partial dependence plots showing how probability varies by attribute value
  tar_target(p7_partDep_episodic_png, 
             create_partialDependence_miniPlots_figure('7_Disseminate/out/partDep_episodic.png', 
                                                       p5_rf_attr_partdep, p5_site_attr_rf_optimal,
                                                       p7_overall_attr_importance_episodic$attribute,
                                                       p7_attr_name_xwalk, 
                                                       line_color = p7_color_episodic),
             format = 'file'),
  
  # Boxplots of attribute values by classification
  tar_target(p7_attr_episodic_boxplots_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_episodic.png',
                                       p5_site_attr_rf_optimal,
                                       p7_overall_attr_importance_episodic$attribute,
                                       p7_attr_name_xwalk, 
                                       c(Episodic=p7_color_episodic, 
                                         `Not episodic` = p7_color_not_episodic)), 
             format='file'),
  
  # Boxplots of attribute values by classification
  tar_target(p7_attr_episodic_boxplotsALL_png, 
             create_attribute_boxplots('7_Disseminate/out/attributes_boxes_episodicALL.png',
                                       p5_site_attr_rf,
                                       calculate_attr_importance(p5_rf_model_hypertuned) %>% 
                                         filter(site_category == 'Episodic') %>% 
                                         arrange(desc(importance)) %>% pull(attribute),
                                       p7_attr_name_xwalk, 
                                       c(Episodic=p7_color_episodic, 
                                         `Not episodic` = p7_color_not_episodic)), 
             format='file'),
  
  
  # Compilation plot of above three figures
  tar_target(p7_rf_results_episodic_png, compilationPlot(
        out_file = '7_Disseminate/out/Fig3_episodic_rf_results.png',
        rf_model_importance = p7_overall_attr_importance_episodic, 
        attribute_name_xwalk = p7_attr_name_xwalk, 
        episodicColor = p7_color_episodic,
        pdp_data = p5_rf_attr_partdep, 
        real_attribute_values = p5_site_attr_rf_optimal,
        attribute_order = p7_overall_attr_importance_episodic$attribute,
        box_colors = c(Episodic=p7_color_episodic, 
                       `Not episodic` = p7_color_not_episodic)), 
        format='file'),
  
  
  # Note that `cowplot::draw_image()` requires that you have `magick`
  # tar_target(p7_rf_results_episodic_png, {
  #   out_file <- '7_Disseminate/out/Fig3_episodic_rf_results.png'
  #   fig_importance <- cowplot::ggdraw() + cowplot::draw_image(p7_importance_episodic_png)
  #   fig_partDep <- cowplot::ggdraw() + cowplot::draw_image(p7_partDep_episodic_png)
  #   fig_boxes <- cowplot::ggdraw() + cowplot::draw_image(p7_attr_episodic_boxplots_png)
  #   png(out_file, width = 6.5, height = 3.25, units = 'in', res = 500)
  #   print(cowplot::plot_grid(fig_importance, fig_partDep, fig_boxes, 
  #                            nrow=1, label_size=8,
  #                            labels=sprintf('(%s)', letters[1:3])))
  #   dev.off()
  #   return(out_file)
  # }, format='file'),
  
  ###### Figure 4: Prediction maps for the full region #####
  
  # Make a map of predicted classes per defined river outlet
  tar_target(p7_comid_xwalk_grp, 
               p6_state_comids %>% 
               select(-tar_group) %>% 
               distinct() %>% 
               rename(nhd_comid = COMID) %>% 
               inner_join(p6_predicted_comid_streamorder, by = 'nhd_comid') %>% 
               # Remove tiny streams before trying to map!
               filter(streamorder > 1) %>%
               select(region, region_fname, nhd_comid) %>%
               filter(region_fname %in% p6_states) %>% 
               group_by(region) %>% 
               tar_group(),
             iteration = 'group'),
  
  ############################ Print statistics ##########################
  tar_target(p7_stats, {
    file_out <- '7_Disseminate/out/outputStats.txt'
    
    p7_comid_xwalk = p6_state_comids %>% 
      select(-tar_group) %>% 
      distinct() %>% 
      rename(nhd_comid = COMID) %>% 
      filter(region_fname %in% p6_states) %>% 
      inner_join(p6_predicted_comid_streamorder, by = 'nhd_comid') %>% 
      select(region, region_fname, nhd_comid) 
    
    regionJoin = p7_huc_flowlines_distinct %>% 
      st_drop_geometry() %>% 
      right_join(p7_comid_xwalk) %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid')
    
    stateTot = regionJoin %>% 
      group_by(region, region_fname) %>% 
      mutate(stateLength = sum(LENGTHKM, na.rm = T)) %>% 
      group_by(region, region_fname, stateLength, pred_fct) %>% 
      summarise(predLength = sum(LENGTHKM, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(predLengthPer = 100*predLength/stateLength) %>% 
      arrange(pred_fct, desc(predLengthPer))
    
    regionTot = regionJoin %>% 
      mutate(regionLength = sum(LENGTHKM, na.rm = T)) %>% 
      group_by(regionLength, pred_fct) %>% 
      summarise(predLength = sum(LENGTHKM, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(predLengthPer = 100*predLength/regionLength) %>% 
      arrange(pred_fct, desc(predLengthPer))
      
    orderTot = regionJoin %>% 
      group_by(StreamOrde) %>% 
      mutate(orderLength = sum(LENGTHKM, na.rm = T)) %>% 
      group_by(StreamOrde, orderLength, pred_fct) %>% 
      summarise(n = n(), predLength = sum(LENGTHKM, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(predLengthPer = 100*predLength/orderLength) %>% 
      arrange(pred_fct, StreamOrde, desc(predLengthPer))
    
    mediandays = p4_ts_sc_norm %>% group_by(site_no) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      summarise(mediandays = median(n)) %>% 
      pull(mediandays)
    
    minmaxSpc = p4_ts_sc_norm %>% group_by(site_no) %>% 
      summarise(medianSpc = median(SpecCond)) %>% 
      ungroup() %>% 
      summarise(min(medianSpc), max(medianSpc), median(medianSpc))
    
    # p4_ts_sc_norm %>% group_by(site_no) %>% 
    #   summarise(medianSpc = median(SpecCond)) %>% 
    #   left_join(p3_static_attributes %>% select(site_no, attr_streamorder)) %>% 
    #   ggplot() +
    #   geom_point(aes(x = attr_streamorder, y = medianSpc))
    
    sites10 = p4_ts_sc_norm %>% group_by(site_no) %>% 
      summarise(n = n()) %>% 
      filter(n >= 365*10)
    sites10_episodic <- sites10 %>% filter(site_no %in% p4_episodic_sites)
    
    episodicN = p3_static_attributes %>% select(site_no, attr_streamorder) %>% 
      filter(site_no %in% p4_episodic_sites) %>% 
      group_by(attr_streamorder) %>% 
      tally()
    
    #### Output values for manuscript
    sink(file_out)
      cat("DATASET STATS \n")
      cat("Total Episodic sites/Total sites\n")
      print(length(p4_episodic_sites))
      print(nrow(p3_static_attributes))
      cat("\n")
      cat("Unique site days:")
      print(nrow(p4_ts_sc_norm))
      cat("\n")
      cat("Median number of days:")
      print(mediandays)
      cat("\n")
      cat("Sites over 10 years (n, %)\n")
      print(nrow(sites10))
      print(nrow(sites10)/nrow(p3_static_attributes))
      cat("\n")
      cat(sprintf("Episodic sites over 10 years: %s", length(sites10_episodic)))
      cat("\n")
      cat("Number of sites by streamorder\n")
      print(p3_static_attributes %>% group_by(attr_streamorder) %>% tally())
      cat("\n")
      cat("Minimum and Maximum SpC\n")
      print(minmaxSpc)
      cat("\n")
      cat("Number of episodic sites by streamorder\n")
      print(episodicN)
      cat("\n")
      cat("\n")
      
      cat("MODEL STATS \n")
      cat("Model Accuracy:")
      print(p5_rf_accuracy)
      cat("\n")
      cat("Model OOB")
      print(p5_rf_oob)
      cat("\n")
      cat("Model Confusion matrix")
      print(p5_rf_model_optimized$confusion)
      
      cat("Number of stream segments predicted:")
      print(nrow(p6_predict_episodic))
      cat("\n")
      
      cat("\n")
      cat("PREDICTION OUTPUT \n")
      cat("For entire region \n")
      print(regionTot)
      cat("\n")
      cat("For individual states \n")
      print(stateTot, n = 80)
      cat("\n")
      cat("By stream order \n")
      print(orderTot, n = 80)
      cat("\n")
      
      cat("Watershed Areas\n")
      cat("Cuyahoga")
      print(getWatershedArea(lat = 41.49730, long = -81.70296, 
                             useUpstream = p6_huc4_upstream_comids_df, 
                             useWatersheds = p6_huc4_catchment_sf))
      cat("Maummee")
      print(getWatershedArea(lat = 41.68858, long = -83.47631, 
                             useUpstream = p6_huc4_upstream_comids_df, 
                             useWatersheds = p6_huc4_catchment_sf))
      cat("Yahara")
      print(getWatershedArea(lat = 42.809, long = -89.1245,
                             useUpstream = p6_huc7_upstream_comids_df, 
                             useWatersheds = p6_huc7_catchment_sf))
      cat("Housatonic")
      print(getWatershedArea(lat = 41.203129, long = -73.10991, 
                             useUpstream = p6_huc1_upstream_comids_df, 
                             useWatersheds = p6_huc1_catchment_sf))
      cat("\n")
    sink()
    return(file_out)
  }, format='file'),
  
  tar_target(p7_stats_methods, {
    file_out <- '7_Disseminate/out/outputStatsMethods.txt'
    
    final_sites <- p3_static_attributes$site_no
    
    states_with_sites <- dataRetrieval::readNWISsite(final_sites) %>% 
      pull(state_cd) %>% 
      unique() %>% 
      dataRetrieval::stateCdLookup(outputType = 'postal')
    states_without_sites <- p1_conus_state_cds[!p1_conus_state_cds %in% states_with_sites]
    
    sites_missing_nhd_comid_match <- p1_nwis_site_nhd_comid_ALL_xwalk %>% 
      filter(site_no %in% p3_all_downloaded_sites) %>% 
      filter(is.na(nhd_comid)) %>% pull(site_no)
    
    n_downloaded <- p3_all_downloaded_sites %>% length()
    n_after_temporal <- length(p3_ts_sc_temporal_qualified_sites)
    n_tidal <- length(p1_nwis_sc_sites_tidal)
    n_highSpC <- length(p3_ts_sc_highSC_sites)
    n_after_tidal_highSpC <- n_after_temporal - length(unique(c(p1_nwis_sc_sites_tidal, p3_ts_sc_highSC_sites)))
    n_missing_static <- length(p3_attr_missing_sites)
    n_missing_area <- length(p3_nwis_site_with_zero_nhd_area)
    n_missing_comid <- length(sites_missing_nhd_comid_match)
    n_after_nhd_filter <- n_after_tidal_highSpC - length(unique(c(p3_attr_missing_sites, p3_nwis_site_with_zero_nhd_area, 
                                                                  sites_missing_nhd_comid_match)))
    
    site_counts <- tibble(
      step = c('Download', 'After temporal filter', 'After tidal/highSC filter', 'After filtering for NHD+'),
      n = c(n_downloaded, n_after_temporal, n_after_tidal_highSpC, n_after_nhd_filter)
    )
    
    sites_requeried_nhd <- p1_nwis_site_nhd_comid_ALL_xwalk %>% 
      filter(site_no %in% final_sites) %>% 
      filter(with_retry) %>% 
      nrow()
    
    overlapping_comids <- p1_nwis_site_nhd_comid_ALL_xwalk %>% 
      filter(site_no %in% final_sites) %>% 
      group_by(nhd_comid) %>% 
      tally() %>% 
      filter(n > 1) %>% 
      pull(nhd_comid)
    sites_with_overlapping_comids <- p1_nwis_site_nhd_comid_ALL_xwalk %>% 
      filter(site_no %in% final_sites) %>% 
      filter(nhd_comid %in% overlapping_comids) %>% 
      nrow()
    
    median_Q_range <- range(p3_static_attributes$attr_medianFlow)
    
    # Output values for manuscript methods section
    sink(file_out)
      cat("States missing USGS gage sites")
      print(states_without_sites)
      cat("\n")
      cat("Site counts through filtering")
      print(site_counts)
      cat("\n")
      cat("Somtimes the tidal and high SpC sites overlap.")
      cat("\n")
      print(sprintf('Tidal sites: %s', paste(p1_nwis_sc_sites_tidal, collapse = ", ")))
      print(sprintf('High SpC sites: %s', paste(p3_ts_sc_highSC_sites, collapse = ", ")))
      cat("\n")
      cat("Somtimes the NHD+ missing attributes and zero area overlap.")
      cat("\n")
      print(sprintf('Missing COMID sites: %s', paste(sites_missing_nhd_comid_match, collapse = ", ")))
      print(sprintf('Missing attributes sites: %s', paste(p3_attr_missing_sites, collapse = ", ")))
      print(sprintf('Missing catchment area sites: %s', paste(p3_nwis_site_with_zero_nhd_area, collapse = ", ")))
      cat("\n")
      cat(sprintf("Num. sites that were requeried with a larger search radius for COMID: %s", sites_requeried_nhd))
      cat("\n")
      cat(sprintf("Num. sites that share a COMID with at least one other site: %s", sites_with_overlapping_comids))
      cat("\n")
      cat(sprintf("Range of median flow values from National Water Model: %s", 
                  paste(median_Q_range, collapse = ' to ')))
      cat("\n")
      cat("\n")
      cat(sprintf('Number of COMIDs used in the predict step: %s', length(p6_predicted_comid)))
      cat("\n")
    sink()
    return(file_out)
  }),
  
  ############################ Full Map ############################
  tar_target(p7_predict_episodic_mapAll_png, {
    file_out <- '7_Disseminate/out/Fig4_predict_map.png'
   
     region_predict_map <- p7_huc_flowlines_distinct %>% 
      right_join(p7_comid_xwalk_grp, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      filter(pred_fct %in% c('Not episodic', 'Episodic')) %>% 
      ggplot() +
      theme_bw(base_size = 7) +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 7) +
      geom_sf(aes(color = pred_fct), linewidth = 0.3) +
      scale_color_manual(values = c(Episodic = p7_color_episodic,
                                    `Not episodic` = p7_color_not_episodic,
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass') 
    ggsave(file_out, region_predict_map, height = 2.5, units = 'in', dpi = 500)
    return(file_out)
  }, format = 'file'),
  
  # Individual states
  tar_target(p7_predict_episodic_map_png, {
    file_out <- sprintf('7_Disseminate/out/Fig4_predict_map_%s.png', 
                        unique(p7_comid_xwalk_grp$region_fname))
    region_predict_map <- p7_huc_flowlines_distinct %>% 
      right_join(p7_comid_xwalk_grp, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      ggplot() +
      theme_bw(base_size = 9) +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 8) +
      geom_sf(aes(color = pred_fct), linewidth = 0.3) +
      scale_color_manual(values = c(Episodic = p7_color_episodic,
                                    `Not episodic` = p7_color_not_episodic,
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass') +
      ggtitle(sprintf('Predicted class for %s', unique(p7_comid_xwalk_grp$region)))
    ggsave(file_out, region_predict_map, width = 6, height = 4, units = 'in', dpi = 500)
    return(file_out)
  }, pattern = map(p7_comid_xwalk_grp), format = 'file'),
  
  ############################ Watershed Maps ############################
  # Maumee -83.59542, 41.59138 
  # Yahara -89.1245, 42.809
  tar_target(p7_watershedmap_maumeee, {
    # Find starting comid
    outletPoint = sf::st_sfc(sf::st_point(c(-83.47631, 41.68858)),
                             crs = 4326)
    
    startComid =  discover_nhdplus_id(outletPoint)
    usehuc = get_huc(AOI = outletPoint, type = 'huc02') %>% pull(huc2) %>% as.numeric()
    # useUpstream <- get(paste0('p6_huc',usehuc,'_upstream_comids_df')) # argh this doesn't work in targets
    
    # Hardcode for now which sucks 
    upstreamids = p6_huc4_upstream_comids_df %>% filter(nhd_comid == startComid) %>% 
      select(nhd_comid = nhd_comid_upstream)
    
    file_out <- sprintf('7_Disseminate/out/Fig4_predict_map_%s.png', 
                        startComid)

    p.maumee <-p7_huc_flowlines_distinct %>%
        right_join(upstreamids, by = 'nhd_comid') %>% 
        left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
        filter(pred_fct %in% c('Not episodic', 'Episodic')) %>% 
        ggplot() +
        theme_bw(base_size = 7) +
        ggspatial::annotation_map_tile(type = 'cartolight', zoom = 10) +
        geom_sf(aes(color = pred_fct), linewidth = 0.3) +
        scale_color_manual(values = c(Episodic = p7_color_episodic,
                                      `Not episodic` = p7_color_not_episodic,
                                      `Not classified` = 'grey50'),
                           name = 'Predicted\nclass') 
        
    ggsave(file_out, p.maumee, 
           height = 2.5, units = 'in', dpi = 500)
    
    return(p.maumee)
  }),
  tar_target(p7_watershedmap_cuyahoga, {
    # Find starting comid
    outletPoint = sf::st_sfc(sf::st_point(c(-81.70296, 41.49730)),
                             crs = 4326)
    
    startComid =  discover_nhdplus_id(outletPoint)
    usehuc = get_huc(AOI = outletPoint, type = 'huc02') %>% pull(huc2) %>% as.numeric()
    # useUpstream <- get(paste0('p6_huc',usehuc,'_upstream_comids_df')) # argh this doesn't work in targets
    
    # Hardcode for now which sucks 
    upstreamids = p6_huc4_upstream_comids_df %>% filter(nhd_comid == startComid) %>% 
      select(nhd_comid = nhd_comid_upstream)
    
    file_out <- sprintf('7_Disseminate/out/Fig4_predict_map_%s.png', 
                        startComid)
    
    p.cuyahoga <-p7_huc_flowlines_distinct %>%
      right_join(upstreamids, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      filter(pred_fct %in% c('Not episodic', 'Episodic')) %>% 
      ggplot() +
      theme_bw(base_size = 7) +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 11) +
      geom_sf(aes(color = pred_fct), linewidth = 0.3) +
      scale_x_continuous(n.breaks = 4) +
      scale_color_manual(values = c(Episodic = p7_color_episodic,
                                    `Not episodic` = p7_color_not_episodic,
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass') 
    
    ggsave(file_out, p.cuyahoga, 
           height = 2.5, units = 'in', dpi = 500)
    
    return(p.cuyahoga)
  }),
  tar_target(p7_watershedmap_yahara, {
    # Find starting comid
    outletPoint = sf::st_sfc(sf::st_point(c(-89.1245, 42.809)),
                             crs = 4326)
    
    startComid =  discover_nhdplus_id(outletPoint)
    usehuc = get_huc(AOI = outletPoint, type = 'huc02') %>% pull(huc2) %>% as.numeric()
    # useUpstream <- get(paste0('p6_huc',usehuc,'_upstream_comids_df'))
    
    upstreamids = p6_huc7_upstream_comids_df %>% filter(nhd_comid == startComid) %>% 
      select(nhd_comid = nhd_comid_upstream)
    
    file_out <- sprintf('7_Disseminate/out/Fig4_predict_map_%s.png', 
                        startComid)
    
    p.yahara <- p7_huc_flowlines_distinct %>%
      right_join(upstreamids, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      filter(pred_fct %in% c('Not episodic', 'Episodic')) %>% 
      ggplot() +
      theme_bw(base_size = 7) +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 11) +
      geom_sf(aes(color = pred_fct), linewidth = 0.3) +
      scale_x_continuous(n.breaks = 3) +
      scale_color_manual(values = c(Episodic = p7_color_episodic,
                                    `Not episodic` = p7_color_not_episodic,
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass')
      
    ggsave(file_out, p.yahara, 
          height = 2.5, units = 'in', dpi = 500)
    return(p.yahara)
  }),
  tar_target(p7_watershedmap_housatonic, {
    # Find starting comid
    outletPoint = sf::st_sfc(sf::st_point(c(-73.10991, 41.203129)),
                             crs = 4326)
    
    startComid =  discover_nhdplus_id(outletPoint)
    usehuc = get_huc(AOI = outletPoint, type = 'huc02') %>% pull(huc2) %>% as.numeric()
    # useUpstream <- get(paste0('p6_huc',usehuc,'_upstream_comids_df')) # argh this doesn't work in targets
    
    # Hardcode for now which sucks 
    upstreamids = p6_huc1_upstream_comids_df %>% filter(nhd_comid == startComid) %>% 
      select(nhd_comid = nhd_comid_upstream)
    
    file_out <- sprintf('7_Disseminate/out/Fig4_predict_map_%s.png', 
                        startComid)
    
    p.housatonic <- p7_huc_flowlines_distinct %>%
      right_join(upstreamids, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      filter(pred_fct %in% c('Not episodic', 'Episodic')) %>% 
      ggplot() +
      theme_bw(base_size = 7) +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 11) +
      geom_sf(aes(color = pred_fct), linewidth = 0.3) +
      scale_x_continuous(n.breaks = 2) +
      scale_color_manual(values = c(Episodic = p7_color_episodic,
                                    `Not episodic` = p7_color_not_episodic,
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass') 
    
    ggsave(file_out, p.housatonic, 
           height = 2.5, units = 'in', dpi = 500)
    
    return(p.housatonic)
  }),
  
  ############################ Supplemental figures ############################
  
  ###### SpC time series for ALL sites ######
  
  tar_target(p7_episodic_plotlist, create_episodic_plotlist(p3_ts_sc_qualified,
                                                            p4_episodic_sites, 
                                                            p7_color_episodic, 
                                                            p7_color_not_episodic)),
  tar_target(p7_episodic_png, 
             ggsave(filename = sprintf('7_Disseminate/out/SI_episodic_grp%s.png', names(p7_episodic_plotlist)), 
                    plot = p7_episodic_plotlist[[1]] + 
                      theme(axis.text.x = element_text(size = 6, angle = 40, hjust=1),
                            axis.text.y = element_text(size = 6)), 
                    height = 8, width = 6.5, dpi = 500), 
             format = 'file', pattern = map(p7_episodic_plotlist)),

  
  
  ##### Extra figures (not in manuscript) #####
  
  ###### Attribute correlation figure ######
  
  # Saving attribute correlations 
  tar_target(p7_attribute_correlations_png, 
             visualize_attr_correlation('7_Disseminate/out/attribute_correlations.png',
                                        p5_site_attr_rf,
                                        p7_attr_name_xwalk), 
             format='file'),
  
  ###### Road salt figures ######
  
  # Boxplot of roadsalt showing that sites across all the categories had similar amounts
  tar_target(p7_roadsalt_boxes_png, 
             create_roadSalt_boxplot('7_Disseminate/out/roadSalt_boxes.png', p3_static_attributes, p7_site_categories),
             format = 'file'),
  
  # Map of roadsalt per site showing that sites don't completely follow a gradient from south --> north
  tar_target(p7_roadsalt_sitemap_png, 
             create_roadSalt_site_map('7_Disseminate/out/roadSalt_sitemap.png', p3_static_attributes, p1_nwis_sc_sites_sf, p1_conus_state_cds),
             format = 'file'),
  
  # Map of gridded roadsalt 
  tar_target(p7_roadsalt_gridmap_png, 
             create_roadSalt_map('7_Disseminate/out/roadSalt_gridmap.png', p2_road_salt_rast, p1_conus_state_cds),
             format = 'file'),
  
  ###### Boxplots of attributes for all sites combined ######
  
  # tar_target(p7_attr_all_boxplots_png, 
  #            create_attribute_boxplots('7_Disseminate/out/attributes_boxes_all.png',
  #                                      mutate(p5_site_attr_rf, site_category_fact = 'ALL'),
  #                                      # Same order as the table
  #                                      c('medianFlow', 'basinSlope', 'pctAgriculture',
  #                                        'pctDeveloped', 'pctForested', 'pctOpenWater',
  #                                        'pctWetland', 'annualPrecip', 'annualSnow',
  #                                        'winterAirTemp', 'baseFlowInd', 'gwRecharge',
  #                                        'subsurfaceContact', 'depthToWT', 
  #                                        'transmissivity', 'roadSaltCumulativePerSqKm'),
  #                                      p7_attr_name_xwalk, 
  #                                      c(ALL='#868b8e'),
  #                                      legend_position = "none",
  #                                      attribute_text_size = 9), 
  #            format='file'),
  
  ###### Map of all qualified sites ######
  
  tar_target(p7_all_sitemap_png, {
    out_file <- '7_Disseminate/out/sitemap_all_qualified.png'
    p_map <- map_category_sites(p1_nwis_sc_sites_sf, p3_static_attributes$site_no, p1_conus_state_cds, 
                                site_color = 'grey30', map_title = 'Qualified sites')
    ggsave(out_file, p_map, width = 3.25, height = 3.25, dpi = 500, bg='white')
    return(out_file)
  }, format='file')
  
)
