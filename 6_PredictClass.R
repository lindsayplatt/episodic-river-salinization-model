# Targets for using a previously built random forest model to 
# predict whether a stream is episodic or not episodic based
# on its associated attributes 

p6_targets <- list(
  
  ##### Declare and identify the downstream-most COMID for a set of watersheds #####
  
  # Setup a table with rivers and the location of their outlets
  tar_target(p6_river_outlet_latlon, 
             tibble(river = c('Yahara River', 'Rock River', 'Maumee River'),
                    river_fname = c('yahara', 'rock', 'maumee'), 
                    lat = c(42.809, 41.481570, 41.692059), 
                    lon = c(-89.1245, -90.613621, -83.469831))),
  
  # Convert the river outlets in an sf object (leave as a list since previous
  # targets experience did not play nicely with mapping over sf objects)
  tar_target(p6_river_outlet_sf_list, 
             st_as_sf(p6_river_outlet_latlon, 
                      coords = c('lon', 'lat'),
                      crs = 4326), 
             pattern = map(p6_river_outlet_latlon),
             iteration = 'list'),
  
  # For each river outlet, identify the COMID
  tar_target(p6_river_outlet_comid, 
             tibble(river = p6_river_outlet_sf_list$river,
                    river_fname = p6_river_outlet_sf_list$river_fname,
                    nhd_comid = discover_nhdplus_id(p6_river_outlet_sf_list)),
             pattern = map(p6_river_outlet_sf_list)),
  
  ###### Identify all COMIDs for a specific watershed #####
  
  # Using the previous downstream COMIDs, identify all that exist upstream
  tar_target(p6_river_upstream_comids, 
             identify_upstream_comids(comid_in = p6_river_outlet_comid$nhd_comid),
             map(p6_river_outlet_comid)),
  
  # Now we need a table that provides the upstream COMIDs for each individual 
  # one in this watershed in order to calculate cumulative road salt. We won't
  # add to the downloads because we will look at uniques for downloads since
  # many will repeat.
  tarchetypes::tar_group_size(p6_river_upstream_comids_grp, 
                              size = 500, # Set groups of 500 to map over
                              p6_river_upstream_comids),
  tar_target(p6_river_upstream_comids_enumerated,{
    p6_river_upstream_comids_grp %>% 
      split(.$nhd_comid_upstream) %>%
      purrr::map(~identify_upstream_comids(.x$nhd_comid_upstream)) %>% 
      bind_rows()
  }, pattern = map(p6_river_upstream_comids_grp)),
  
  tar_target(p6_unique_comids, unique(p6_river_upstream_comids_enumerated$nhd_comid_upstream)),
  
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
                              size = 100, # Set groups of 100 to map over
                              # Create unique vector of COMIDs to download catchments only once
                              tibble(nhd_comid = p6_unique_comids)),
  # Download NHD+ catchment polygons by groups of COMIDs
  tar_target(p6_nhdplus_catchments_gpkg,
             download_nhdplus_catchments(out_file = sprintf('6_PredictClass/tmp/nhdplus_catchment_%s.gpkg',
                                                            unique(p6_unique_comids_grp$tar_group)),
                                         comids = p6_unique_comids_grp$nhd_comid),
             pattern = map(p6_unique_comids_grp),
             format = 'file', error = "continue"),
  
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
  
  # Calculate the total area of each catchment & its upstream catchements
  tar_target(p6_attr_basinArea, calculate_catchment_areas(polys_sf = p6_nhdplus_catchment_sf,
                                                          comid_upstream_tbl = p6_river_upstream_comids_enumerated,
                                                          comid_site_xwalk = NULL)),
  
  # Then, map salt for each NHD COMID catchment polygon to sites and calculate cumulative road salt
  tar_target(p6_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p6_nhdplus_catchment_salt, 
                                                              basin_areas = p6_attr_basinArea,
                                                              comid_site_xwalk = NULL,
                                                              comid_upstream_tbl = p6_river_upstream_comids_enumerated)),
  
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
  
  tar_target(p6_predict_episodic, p6_attr_all %>%
               mutate(pred = predict(p5_rf_model_optimized, .)) %>%
               replace_na(list(pred = 'Not classified')) %>% 
               select(nhd_comid, pred)),
  
  # Make a map of predicted classes per defined river outlet
  tar_target(p6_river_comid_xwalk_grp, p6_river_upstream_comids %>% 
               left_join(p6_river_outlet_comid, by = 'nhd_comid') %>% 
               select(river, river_fname, nhd_comid = nhd_comid_upstream) %>% 
               group_by(river) %>% 
               tar_group(),
             iteration = 'group'),
  tar_target(p6_predict_episodic_map_png, {
    file_out <- sprintf('6_PredictClass/out/predict_map_%s.png', 
                        unique(p6_river_comid_xwalk_grp$river_fname))
    river_predict_map <- p6_nhdplus_flowlines_sf %>%
      right_join(p6_river_comid_xwalk_grp, by = 'nhd_comid') %>% 
      left_join(p6_predict_episodic, by = 'nhd_comid') %>% 
      ggplot() +
      ggspatial::annotation_map_tile(type = 'cartolight', zoom = 10) +
      geom_sf(aes(color = pred)) +
      scale_color_manual(values = c(Episodic = '#c28e0d',
                                    `Not episodic` = '#005271',
                                    `Not classified` = 'grey40'),
                         name = 'Predicted\nclass') +
      ggtitle(sprintf('Predicted class for %s', unique(p6_river_comid_xwalk_grp$river)))
    ggsave(file_out, river_predict_map, width = 6.5, height = 6.5, units = 'in', dpi = 500)
    return(file_out)
  }, pattern = map(p6_river_comid_xwalk_grp), format = 'file')
  
)
