# Targets for using a previously built random forest model to 
# predict whether a stream is episodic or not episodic based
# on its associated attributes 

p6_targets <- list(
  
  ##### Declare COMIDs and their upstream COMIDs #####
  
  ###### First, use states to identify a set of COMIDs ######
  
  # Setup states as an sf polygon
  tar_target(p6_states, 'MN'),
  tar_target(p6_state_sf, usmap::us_map(include = p6_states) %>% 
               st_as_sf(coords = c('x', 'y'), 
                        crs = usmap::usmap_crs()) %>% 
               st_transform(crs = 4326)),
  
  tar_target(p6_state_comids, 
             identify_comids_aoi(p6_state_sf) %>% 
               mutate(region = p6_state_sf$full,
                      region_fname = p6_state_sf$abbr) %>% 
               st_drop_geometry() %>% 
               select(nhd_comid = comid, region, region_fname),
             pattern = map(p6_state_sf)),
  
  ###### Prepare the full network of flowlines for identifying upstream COMIDs ######
  
  # Filter the national flowlines to only those in HUCs that overlap with the states.
  # This is just so that we don't have to traverse a larger network than necessary.
  
  # For each state, identify the HUC-02s that overlap
  # This is so that we can download flowlines that will be 
  # upstream of everything. 
  tar_target(p6_state_huc_sf, get_huc(AOI = p6_state_sf, type = 'huc02')),
  
  # Now for each HUC-02 that intersected with our states, 
  # identify the COMIDs of flowlines within those.
  tar_target(p6_huc_comids, 
             identify_comids_aoi(p6_state_huc_sf) %>% 
               mutate(region = p6_state_huc_sf$name,
                      region_fname = p6_state_huc_sf$huc2) %>% 
               st_drop_geometry() %>% 
               select(nhd_comid = comid, region, region_fname),
             pattern = map(p6_state_huc_sf)),
  
  # Add `toids` for preparing to find upstream COMIDs and then 
  # subset the NHD+ to just the relevant HUCs
  tar_target(p6_huc_flowlines_sf, p1_nhdplus_flowlines_sf %>% 
               hydroloom::add_toids() %>% 
               filter(COMID %in% p6_huc_comids$nhd_comid)),
  
  # Now make a smaller table that is only the network info
  # needed to find the upstream COMIDs later.
  tar_target(p6_huc_flowlines_network_tbl, 
             p6_huc_flowlines_sf %>% 
               st_drop_geometry() %>% 
               select(comid = COMID, toid)),
  
  ##### Get the upstream COMIDs for *each one within* our region of interest #####
  
  # This could be updated in the future to do all the HUC flowlines BUT since the  
  # model was built using sites that fell into one of the states in `p6_states`, 
  # it makes sense that we are only using the model to predict flowlines within
  # the modeled spatial region. 
  
  # Set up groups of 1000 COMIDs per target (avoid too many targets at once)
  tarchetypes::tar_group_size(p6_state_comids_grp, 
                              size = 1000, # Set groups of 1000 to map over NLDI navigation
                              p6_state_comids),
  
  # Identify the full upstream network, treating each COMID as its own outlet
  # This must be done separately for *each* COMID
  tar_target(p6_upstream_comids, {
    
    # TODO: in parallel but be careful that targets for downloading DO NOT
    # run in parallel. Need to try this out still. And hopefully, increase 
    # the clusters to something like 10-12? 
    cl <- makeCluster(2)
    registerDoParallel(cl)
    
    foreach(i=1:nrow(p6_state_comids_grp), .combine=bind_rows) %dopar% 
      identify_upstream_comids_hy(comid_in = p6_state_comids_grp$nhd_comid[i],
                                  flines_network = p6_huc_flowlines_network_tbl)
    
    # Restore the regular state
    registerDoSEQ()
  }, pattern = map(p6_state_comids_grp)),
  
  # TODO: SCALING THE 'UPSTREAM' PART OF THIS IS INSANE.
  # Seemingly need to individually query `navigate_nldi()` for each COMID
  # to get the upstream COMIDs associated with each. Which is insane when 
  # MN alone has 60k+.
  # Surely there is a way to query for more than one at a time ...
  # Currently looking into hydroloom options: 
  #   https://github.com/DOI-USGS/nhdplusTools/issues/354
  #   https://doi-usgs.github.io/hydroloom/reference/sort_network.html
  
  # x<-get_nhdplus(AOI = p6_state_sf[[1]], realization = "outlet")
  # plot(st_geometry(x), col='cornflowerblue')
  # mn_comids <- get_nhdplus(AOI = p6_state_sf[[1]], realization = "flowline")
  # plot(st_geometry(mn_comids), col='cornflowerblue')
  # plot(st_geometry(p6_state_sf[[1]]), border = 'black', lwd=3, add=TRUE)
  # mn_upstream <- mn_comids %>% 
  #   rename(nhd_comid = comid) %>% 
  #   split(.$nhd_comid) %>% 
  #   map(~identify_upstream_comids(comid_in = .x$nhd_comid)) %>% 
  #   bind_rows()
  # mn_upstream_flowlines <- get_nhdplus(comid = unique(mn_upstream$nhd_comid_upstream), realization = "flowline")
  # plot(st_geometry(mn_upstream_flowlines), col='cornflowerblue')
  # plot(st_geometry(p6_state_sf[[1]]), border = 'black', lwd=3, add=TRUE)
  
  ##### Identify all upstream COMIDs #####
  
  # # Set up groups of 1000 COMIDs per target (avoid too many targets)
  # tarchetypes::tar_group_size(p6_region_comids_grp, 
  #                             size = 1000, # Set groups of 1000 to map over NLDI navigation
  #                             p6_region_comids),
  # 
  # # Using the previous downstream COMIDs, identify all that exist upstream
  # tar_target(p6_upstream_comids, 
  #            # Map within each group to do one COMID at a time
  #            p6_region_comids_grp %>% 
  #              split(.$nhd_comid) %>% 
  #              map(~identify_upstream_comids(comid_in = .x$nhd_comid)) %>% 
  #              bind_rows(),
  #            pattern = map(p6_region_comids_grp)),
  
  # SKIPPING THE `enumerated` version for now because I have all the focal
  # COMIDs (those that intersect my state) and am grabbing the appropriate 
  # upstream COMIDs to calculate road salt for those in the previous step. I
  # don't want to do it all again, so we just won't predict for anything other 
  # than the 'focal' COMIDS, so that we don't have to get upstream COMIDs for 
  # the upstream COMIDs ... a brain twister for sure. 
  
  # # Now we need a table that provides the upstream COMIDs for each individual 
  # # one in this watershed in order to calculate cumulative road salt. We won't
  # # add to the downloads because we will look at uniques for downloads since
  # # many will repeat.
  # tarchetypes::tar_group_size(p6_upstream_comids_grp, 
  #                             size = 500, # Set groups of 500 to map over
  #                             p6_upstream_comids),
  # tar_target(p6_upstream_comids_enumerated,{
  #   p6_upstream_comids_grp %>% 
  #     split(.$nhd_comid_upstream) %>%
  #     purrr::map(~identify_upstream_comids(.x$nhd_comid_upstream)) %>% 
  #     bind_rows()
  # }, pattern = map(p6_upstream_comids_grp)),
  
  tar_target(p6_unique_comids, unique(p6_upstream_comids$nhd_comid_upstream)),
  
  ##### Download all attributes for COMIDs #####
  
  # Download NHD+ attributes data for each COMID
  tar_target(p6_nhdplus_attr_vals_tbl, 
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_unique_comids),
             pattern = map(p1_nhdplus_attr_list)),
  
  # Pivot & combine NHD+ attributes
  tar_target(p6_attr_nhd, prepare_nhd_attributes(p6_nhdplus_attr_vals_tbl, NULL)),
  
  # Download catchment polygons to calculate salt
  tarchetypes::tar_group_size(p6_unique_comids_grp, 
                              size = 1000, # Set groups of 1000 to map over
                              # Create unique vector of COMIDs to download catchments only once
                              tibble(nhd_comid = p6_unique_comids)),
  # Download NHD+ catchment polygons by groups of COMIDs
  tar_target(p6_nhdplus_catchments_gpkg,
             download_nhdplus_catchments(out_file = sprintf('6_PredictClass/tmp/nhdplus_catchment_%s.gpkg',
                                                            unique(p6_unique_comids_grp$tar_group)),
                                         comids = p6_unique_comids_grp$nhd_comid),
             pattern = map(p6_unique_comids_grp),
             format = 'file', error = "continue"),
  
  # TODO: CHECK EXTENT OF SALT LAYER!!!!!!!!
  # Extract the catchments as polygons and summarize total salt per catchment
  # This includes any catchments that will only be used for upstream calculations
  tar_target(p6_nhdplus_catchment_sf, extract_nhdplus_geopackage_layer(p6_nhdplus_catchments_gpkg)),
  tar_target(p6_nhdplus_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = p2_road_salt_rast, 
                                                                     polys_sf = p6_nhdplus_catchment_sf)),
  
  # Before summarizing the rest of the data below, the NHD COMID data for what 
  # is included in the COMID to site crosswalk and the upstream COMIDs was 
  # filtered in `3_Filter` to only to COMIDs with nonzero drainage areas. The 
  # catchment area calculations above do not need the filtering because they are 
  # already missing catchments with 0 drainage areas.
  
  # Calculate the total area of each catchment & its upstream catchments
  tar_target(p6_attr_basinArea, calculate_catchment_areas(polys_sf = p6_nhdplus_catchment_sf,
                                                          comid_upstream_tbl = p6_upstream_comids,
                                                          comid_site_xwalk = NULL)),
  
  # Then, map salt for each NHD COMID catchment polygon to sites and calculate cumulative road salt
  tar_target(p6_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_nhdplus_catchment_salt, 
                                                              basin_areas = p6_attr_basinArea,
                                                              comid_site_xwalk = NULL,
                                                              comid_upstream_tbl = p6_upstream_comids)),
  
  # Now keep only the salt attributes of interest in the final model
  tar_target(p6_attr_roadSalt_forModel, p6_attr_roadSalt %>% select(nhd_comid, attr_roadSaltCumulativePerSqKm)),
  
  # Prepare the attributes from Zell and Sanford 2020 which are based on NHD+ COMIDs
  tar_target(p6_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv, 
                                                          p1_sb_transmissivity_csv,
                                                          tibble(nhd_comid = p6_unique_comids),
                                                          returnSite = FALSE)),
  
  # Didn't worry about downloading Flow from NOAA's National Water Model because
  # the downloads were really slow and the final optimized model from May 2024
  # did not include or need Flow. 
  
  # Now combine all attributes into a single table
  tar_target(p6_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                    # p2_attr_flow_nwm, 
                                                    p6_attr_basinArea %>% select(nhd_comid, attr_areaCumulativeSqKm),
                                                    p6_attr_roadSalt_forModel,
                                                    p6_attr_nhd,
                                                    p6_attr_depth2wt_trnmsv) %>% 
               # Drop the `attr_` prefix so that the results are cleaner
               rename_with(~gsub("attr_", "", .x))
  ),
  
  ##### Use the model and attributes to predict episodic/not episodic #####
  
  # Verify the attributes developed in p6_ match what is needed by the optimized model
  tar_target(p6_verify_model_attrs, {
    attrs_model <- row.names(p5_rf_model_optimized$importance)
    stopifnot(all(attrs_model %in% names(p6_attr_all)))
  }), 
  
  # Extract the flowline spatial features from the downloaded geopackages. This 
  # includes ALL COMIDs (even those with 0 drainage areas), but will be filtered later.
  tar_target(p6_nhdplus_flowlines_sf, extract_nhdplus_geopackage_layer(p6_nhdplus_catchments_gpkg, 
                                                                       gpkg_layer = 'NHDFlowline_Network')),
  # TODO: Use `streamorde` column to filter out teeny tiny ones before trying to map!
  
  tar_target(p6_predict_episodic, p6_attr_all %>%
               mutate(pred = as.character(predict(p5_rf_model_optimized, .))) %>%
               replace_na(list(pred = 'Not classified')) %>% 
               mutate(pred_fct = factor(pred, ordered = TRUE, 
                                        levels = c('Episodic', 'Not episodic', 'Not classified'))) %>% 
               select(nhd_comid, pred, pred_fct)),
  
  # Make a map of predicted classes per defined river outlet
  tar_target(p6_comid_xwalk_grp, p6_upstream_comids %>% 
               left_join(p6_region_comids, by = 'nhd_comid') %>% 
               select(region, region_fname, nhd_comid = nhd_comid_upstream) %>% 
               group_by(region) %>% 
               tar_group(),
             iteration = 'group'),
  tar_target(p6_predict_episodic_map_png, {
    file_out <- sprintf('6_PredictClass/out/predict_map_%s.png', 
                        unique(p6_comid_xwalk_grp$region_fname))
    region_predict_map <- p6_nhdplus_flowlines_sf %>%
      right_join(p6_comid_xwalk_grp, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      ggplot() +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 10) +
      geom_sf(aes(color = pred_fct)) +
      scale_color_manual(values = c(Episodic = '#c28e0d',
                                    `Not episodic` = '#005271',
                                    `Not classified` = 'grey50'),
                         name = 'Predicted\nclass') +
      ggtitle(sprintf('Predicted class for %s', unique(p6_comid_xwalk_grp$region)))
    ggsave(file_out, region_predict_map, width = 6.5, height = 6.5, units = 'in', dpi = 500)
    return(file_out)
  }, pattern = map(p6_comid_xwalk_grp), format = 'file')
  
)
