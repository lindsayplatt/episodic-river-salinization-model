# Targets for using a previously built random forest model to 
# predict whether a stream is episodic or not episodic based
# on its associated attributes 

source('6_PredictClass/src/navigate_nhdplus_network.R') 

p6_targets <- list(
  
  ##### Declare COMIDs and their upstream COMIDs #####
  
  ###### First, use states to identify a set of COMIDs ######
  
  # Setup states as an sf polygon
  tar_target(p6_states, c('MN', 'IA', 'IL', 'WI')),
  # tar_target(p6_states, p1_conus_state_cds),
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
  
  # Filter flowlines sf to just those in the states for mapping later
  tar_target(p6_state_flowlines_sf, p1_nhdplus_flowlines_sf %>% 
               rename(nhd_comid = COMID) %>% 
               filter(nhd_comid %in% p6_state_comids$nhd_comid)),
  
  ###### Prepare the full network of flowlines for identifying upstream COMIDs ######
  
  # Filter the national flowlines to only those in HUCs that overlap with the states.
  # This is just so that we don't have to traverse a larger network than necessary.
  
  # For each state, identify the HUC-02s that overlap then combine so we only 
  # have one of each HUC (need to query for HUCs one state at a time).
  # This is so that we can download flowlines that will be 
  # upstream of everything. 
  tar_target(p6_state_huc_sf_all, get_huc(AOI = p6_state_sf, type = 'huc02'),
             pattern = map(p6_state_sf), iteration = 'list'),
  tar_target(p6_state_huc_sf, bind_rows(p6_state_huc_sf_all) %>% distinct()),
  
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
    
    # Run each mapped group in parallel since it doesn't access any 
    # web services (that's not a good practice), but mapping is sequential
    
    # Number of workers is based on the number of cores your computer has
    # so that we don't use up ALL possible cores on your computer
    comp_cores <- detectCores()
    n_cores <- ifelse(comp_cores > 3, comp_cores-2, 2)
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    upstream_tbl <- foreach(i=1:nrow(p6_state_comids_grp), 
                            .combine=bind_rows, 
                            # Need to explicitly pass in packages and functions so that
                            # the parallel workers have them available.
                            .export = c('identify_upstream_comids_hy'),
                            .packages = c('nhdplusTools', 'tidyverse'),
                            .verbose = FALSE) %dopar% {
                              identify_upstream_comids_hy(comid_in = p6_state_comids_grp$nhd_comid[i],
                                                          flines_network = p6_huc_flowlines_network_tbl)
                            }
      
    # Restore the regular state
    registerDoSEQ()
    stopCluster(cl)
    
    return(upstream_tbl)
    
  }, pattern = map(p6_state_comids_grp)),
  
  tar_target(p6_unique_comids, unique(p6_upstream_comids$nhd_comid_upstream)),
  
  ##### Download all attributes for COMIDs #####
  
  # Download NHD+ attributes data for each COMID
  tar_target(p6_nhdplus_attr_vals_tbl, 
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p6_unique_comids),
             pattern = map(p1_nhdplus_attr_list)),
  
  # Pivot & combine NHD+ attributes
  tar_target(p6_attr_nhd, prepare_nhd_attributes(p6_nhdplus_attr_vals_tbl, NULL)),
  
  # Filter NHD+ catchment polygons to just the COMIDs used here
  tar_target(p6_nhdplus_catchment_sf, p1_nhdplus_catchments_sf %>% filter(nhd_comid %in% p6_unique_comids)),
  
  # Now use the catchment polygons to summarize total salt per catchment
  # This includes any catchments that will only be used for upstream calculations
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
  
  tar_target(p6_predict_episodic, p6_attr_all %>%
               mutate(pred = as.character(predict(p5_rf_model_optimized, .))) %>%
               replace_na(list(pred = 'Not classified')) %>% 
               mutate(pred_fct = factor(pred, ordered = TRUE, 
                                        levels = c('Episodic', 'Not episodic', 'Not classified'))) %>% 
               select(nhd_comid, pred, pred_fct)),
  
  # Pull out COMIDs that were modeled as a vector
  tar_target(p6_predicted_comid, p6_attr_all %>% pull(nhd_comid) %>% unique()),
  
  # Find their streamorder so that we can filter to bigger streams for mapping
  tar_target(p6_predicted_comid_streamorder, p6_state_flowlines_sf %>%
               st_drop_geometry() %>% 
               filter(nhd_comid %in% p6_predicted_comid) %>% 
               select(nhd_comid, streamorder = StreamOrde)),
  
  # Make a map of predicted classes per defined river outlet
  tar_target(p6_comid_xwalk_grp, p6_predicted_comid_streamorder %>% 
               left_join(p6_state_comids, by = 'nhd_comid') %>% 
               # Remove tiny streams before trying to map!
               filter(streamorder > 0) %>%
               select(region, region_fname, nhd_comid) %>% 
               group_by(region) %>% 
               tar_group(),
             iteration = 'group'),
  tar_target(p6_predict_episodic_map_png, {
    file_out <- sprintf('6_PredictClass/out/predict_map_%s.png', 
                        unique(p6_comid_xwalk_grp$region_fname))
    region_predict_map <- p6_state_flowlines_sf %>%
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
