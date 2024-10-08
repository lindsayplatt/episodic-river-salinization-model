# Targets for transforming data to be used in modeling or 
# site selection in this analysis

source('2_Prepare/src/ts_nwis_fxns.R')
source('2_Prepare/src/attr_prep_fxns.R')
source('2_Prepare/src/attr_combine_all.R')
source('2_Prepare/src/extract_nhdplus_geopackage_layer.R')


p2_targets <- list(
  
  ##### TIMESERIES DATA PREP #####
  
  # All are prefixed with `p2_ts_`
  
  ###### TS DATA 1: Calc daily mean SC from instantaneous data ######
  
  # Processing UV files by combining instantaneous data into daily data
  tar_target(p2_ts_sc_all_as_dv_feather, 
             calculate_dv_from_uv(out_file_dir = '2_Prepare/tmp',
                                  in_file = p1_nwis_sc_data_feather,
                                  site_tz_xwalk = p1_nwis_sc_sites_metadata,
                                  param_colname = 'SpecCond'), 
             pattern = map(p1_nwis_sc_data_feather),
             format = 'file'),
  
  ###### TS DATA 2: Combine all daily mean SC data ######
  
  # Combine/format into a single file. Also, it replaces values of -999999. 
  tar_target(p2_ts_sc_dv_feather, 
             combine_all_dv_data(out_file = '2_Prepare/tmp/ts_sc_dv.feather',
                                 in_files = p2_ts_sc_all_as_dv_feather,
                                 param_colname = 'SpecCond'),
             format = 'file'),
  
  # Then this SC data is filtered to only qualifying sites and data in `3_Filter`
  
  ##### STATIC ATTRIBUTES PREP #####
  
  # All are prefixed with `p2_attr_`
  
  ###### ATTR DATA 1: Collapse Q time series to mean Q per site ######
  
  # First, convert instantaneous Q to daily Q
  tar_target(p2_attr_q_all_as_dv_feather,
             calculate_dv_from_uv(out_file_dir = '2_Prepare/tmp/',
                                  in_file = p1_nwis_q_data_feather,
                                  site_tz_xwalk = p1_nwis_sc_sites_metadata,
                                  param_colname = 'Flow'),
             pattern = map(p1_nwis_q_data_feather),
             format = 'file'),
  
  # Then, format and combine all daily Q
  tar_target(p2_attr_q_dv_feather, 
             combine_all_dv_data(out_file = '2_Prepare/tmp/attr_q_dv.feather',
                                 in_files = p2_attr_q_all_as_dv_feather,
                                 param_colname = 'Flow'),
             format = 'file'),
  
  # Then this Q data is filtered to only qualifying sites in `3_Filter`
  
  # Then, calculate static attributes of daily Q value per site
  tar_target(p2_attr_flow_nwis, calculate_q_stats_per_site(p3_attr_q_qualified)),
  
  # Instead of using NWIS streamflow, we will use NWM flow so that
  # we can use our model later to predict to streams without gages
  tar_target(p2_attr_flow_nwm, prep_nwm_flow(p1_nwm_streamflow, p1_nwis_site_nhd_comid_ALL_xwalk)),
  
  ###### ATTR DATA 2: Extract road salt application per site ######
  
  # First, calculate an average across all tifs from 2010-2019
  tar_target(p2_road_salt_rast, average_road_salt_rasters(p1_sb_road_salt_tif)),
  
  # Extract the flowline spatial features from the downloaded geopackages. This 
  # includes ALL COMIDs (even those with 0 drainage areas), but will be filtered later.
  tar_target(p2_nhdplus_flowlines_ALL_sf, extract_nhdplus_geopackage_layer(p1_nhdplus_catchments_gpkg, 
                                                                       gpkg_layer = 'NHDFlowline_Network')),
  
  # Each COMID and site will have a value for salt application for just the
  # individual COMID catchment (`attr_roadSaltPerSqKm`) but also a total including
  # all NHD+ catchments upstream (`attr_roadSaltCumulativePerSqKm`).
  
  # Extract the catchments as polygons and summarize total salt per catchment
  # This includes any catchments that will only be used for upstream calculations
  tar_target(p2_nhdplus_catchment_sf, extract_nhdplus_geopackage_layer(p1_nhdplus_catchments_gpkg)),
  tar_target(p2_nhdplus_catchment_salt, aggregate_road_salt_per_poly(road_salt_rast = NULL, 
                                                                     polys_sf = p2_nhdplus_catchment_sf,
                                                                    rasterTifs = p1_sb_road_salt_tif)),
  
  # Before summarizing the rest of the data below, the NHD COMID data for what 
  # is included in the COMID to site crosswalk and the upstream COMIDs was 
  # filtered in `3_Filter` to only to COMIDs with nonzero drainage areas. The 
  # catchment area calculations above do not need the filtering because they are 
  # already missing catchments with 0 drainage areas.
  
  # Calculate the total area of each catchment & its upstream catchements
  tar_target(p2_attr_basinArea, calculate_catchment_areas(polys_sf = p2_nhdplus_catchment_sf,
                                                          comid_upstream_tbl = p3_nhdplus_comids_upstream,
                                                          comid_site_xwalk = p3_nwis_site_nhd_comid_xwalk)),
  
  # Calculate the total area of each catchment & its upstream catchments, keep all upstream comids
  tar_target(p2_attr_basinArea_upstream, calculate_Upstream_areas(polys_sf = p2_nhdplus_catchment_sf,
                                                                     comid_upstream_tbl = p3_nhdplus_comids_upstream,
                                                                     comid_site_xwalk = p3_nwis_site_nhd_comid_xwalk)),
  
  # Then, map salt for each NHD COMID catchment polygon to sites and calculate cumulative road salt
  tar_target(p2_attr_roadSalt, map_catchment_roadSalt_to_site(road_salt_comid = p2_nhdplus_catchment_salt, 
                                                              basin_areas = p2_attr_basinArea,
                                                              comid_site_xwalk = p3_nwis_site_nhd_comid_xwalk,
                                                              comid_upstream_tbl = p3_nhdplus_comids_upstream)),
  
  # Now keep only the salt attributes of interest in the final model
  tar_target(p2_attr_roadSalt_forModel, p2_attr_roadSalt %>% select(site_no, attr_roadSaltCumulativePerSqKm)),
  
  ###### ATTR DATA 3: Pivot and link NHD+ attributes to sites ######
  
  tar_target(p2_attr_nhd, prepare_nhd_attributes(p1_nhdplus_attr_vals_tbl,
                                                 p3_nwis_site_nhd_comid_xwalk)),
 
  # Isolate the agriculture-specific attribute
  tar_target(p2_ag_attr_nhd, p2_attr_nhd %>% select(site_no, attr_pctAgriculture)),
  
  # Prepare the attributes from Zell and Sanford 2020 which are based on NHD+ COMIDs
  tar_target(p2_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv, 
                                                          p1_sb_transmissivity_csv,
                                                          p3_nwis_site_nhd_comid_xwalk)),
  
  ####### ATTR DATA 4: Get stream order for every comid ##########
  tar_target(p2_streamorder, get_nhdplus(comid = p1_nhdplus_comids) %>% 
                              select(nhd_comid = comid, streamorde) %>% 
                              st_drop_geometry() %>% 
                              left_join(p3_nwis_site_nhd_comid_xwalk) %>% 
                              select(site_no, attr_streamorder = streamorde)),
  
  ###### ATTR DATA 5: Combine all static attributes into one table ######
  
  tar_target(p2_attr_all, combine_static_attributes(joinby = 'site_no',
                                                    # Start with a tibble of all the sites we want 
                                                    # attributes for and join other tables into that.
                                                    tibble(site_no = p3_ts_sc_temporal_qualified_sites),
                                                    p2_attr_flow_nwm,
                                                    p2_streamorder,
                                                    p2_attr_basinArea %>% select(site_no, attr_areaCumulativeSqKm),
                                                    p2_attr_roadSalt_forModel,
                                                    p2_attr_nhd,
                                                    # p2_attr_nhd_upstream,
                                                    p2_attr_depth2wt_trnmsv
                                                    ))
  
)
