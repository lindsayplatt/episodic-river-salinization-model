# Targets for filtering sites and data based on specific citeria

source('3_Filter/src/nhd_catchments.R')
source('3_Filter/src/ts_gap_filtering.R')
source('3_Filter/src/ts_qualification.R')

p3_targets <- list(
  
  ##### NHD+ FILTERING: Filter NHD+ COMIDs with no drainage area #####
  
  # Identify the COMIDs that do not have any drainage area, then remove and use
  # the updated COMID lists in subsequent functions.
  tar_target(p3_nhd_comid_zero_areas, identify_nonexistent_catchments(p2_nhdplus_flowlines_ALL_sf)),
  tar_target(p3_nhdplus_flowlines_sf, p2_nhdplus_flowlines_ALL_sf %>% 
               filter(!nhd_comid %in% p3_nhd_comid_zero_areas)),
  tar_target(p3_nwis_site_nhd_comid_xwalk, p1_nwis_site_nhd_comid_ALL_xwalk %>% 
               filter(!nhd_comid %in% p3_nhd_comid_zero_areas)),
  tar_target(p3_nhdplus_comids_upstream, p1_nhdplus_comids_upstream_ALL %>% 
               filter(!nhd_comid %in% p3_nhd_comid_zero_areas,
                      !nhd_comid_upstream %in% p3_nhd_comid_zero_areas)),
  
  # Identify any sites who correspond to a COMID with no drainage area to remove
  tar_target(p3_nwis_site_with_zero_nhd_area, p1_nwis_site_nhd_comid_ALL_xwalk %>% 
               filter(nhd_comid %in% p3_nhd_comid_zero_areas) %>% 
               pull(site_no)),
  
  ##### TS FILTERING: Filter sites and data as part of processing in `2_Prepare` #####
  
  # Some of these steps are happening *BEFORE* the gap-filling steps in 
  # 2_Prepare because WRTDS is an expensive computational step and we 
  # only want to fill gaps for those sites that pass our other criteria.
  
  ##### Step 1: remove very large gaps in timeseries data #####
  
  # Before running WRTDS, remove any data that is too large of a gap to attempt
  # to fill. Only apply gap-filling to data more recent than the final large gap
  # (currently defined as 2 years of sequential NAs)
  tar_target(p3_ts_sc_winter_qualified,
             filter_winter(p2_ts_sc_dv_feather,
                                         param_colname = 'SpecCond')),
  
  ##### Step 2: identify sites that meet (or don't) certain criteria #####
  
  # Identify the sites with at least 5 years and some of those years occurring 
  # in the last 15 years (will keep them). Note that these need to be identified
  # *after* the removal of the large gaps above.
  tar_target(p3_ts_sc_temporal_qualified_sites, 
             identify_temporal_qualifying_sites(p3_ts_sc_winter_qualified)),
  
  # Identify sites that have suspiciously high SC (will remove them)
  tar_target(p3_ts_sc_highSC_sites, identify_highSC_sites(p3_ts_sc_winter_qualified)),
  
  ##### Step 3: filter data to just those sites that match our requirements #####
  
  # Filter the data to just those sites that match our requirements
  tar_target(p3_ts_sc_qualified, 
             filter_data_to_qualifying_sites(p3_ts_sc_winter_qualified, 
                                             keep_sites = p3_ts_sc_temporal_qualified_sites,
                                             remove_sites = c(p1_nwis_sc_sites_tidal,
                                                              p3_ts_sc_highSC_sites,
                                                              p3_nwis_site_with_zero_nhd_area,
                                                              p3_attr_missing_sites))),
  
  # Do the same for Flow data.
  tar_target(p3_attr_q_qualified, 
             read_feather(p2_attr_q_dv_feather) %>% 
               filter_data_to_qualifying_sites(keep_sites = p3_ts_sc_temporal_qualified_sites,
                                               remove_sites = c(p1_nwis_sc_sites_tidal,
                                                                p3_ts_sc_highSC_sites,
                                                                p3_nwis_site_with_zero_nhd_area,
                                                                p3_attr_missing_sites))),
  
  # The `_qualified` data above go back to `2_Prepare` to continue prepping
  
  ##### ATTR FILTERING #####
  
  # Remove sites from all data if any of the attributes are missing
  tar_target(p3_attr_missing_sites, identify_missing_attr_sites(p2_attr_nhd)),
  
  # Filter the final static attribute table to only those sites that qualified.
  tar_target(p3_static_attributes, 
             filter_data_to_qualifying_sites(p2_attr_all, 
                                             keep_sites = p3_ts_sc_temporal_qualified_sites,
                                             remove_sites = c(p1_nwis_sc_sites_tidal,
                                                              p3_ts_sc_highSC_sites,
                                                              p3_nwis_site_with_zero_nhd_area,
                                                              p3_attr_missing_sites)))
  
)
