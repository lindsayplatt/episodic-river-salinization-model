
# Number of SC sites
p8_targets <- list(

  # NWIS sites that don't have Q
  tar_target(p8_nwis_sc_notqualified, 
             bind_rows(p1_nwis_sc_sites_qualified) %>% 
               filter(!site_no %in% p1_nwis_q_sites_query$site_no)),
  
  ###### NWIS DATA: Prepare to download all the SC data ###### 
  tar_target(p8_nwis_sc_notqualified_download_grps,
             p8_nwis_sc_notqualified %>% 
               add_download_grp() %>% 
               group_by(query_service, task_num) %>% 
               tar_group(),
             iteration = 'group'),
  
  ###### NWIS DATA: Download the SC data and save as files in `8_Validate/out_nwis` ######
  
  tar_target(p8_nwis_sc_data_feather,
             download_nwis_data(
               out_file = sprintf('8_Validate/out_nwis/sc_%s_%03d.feather',
                                  unique(p8_nwis_sc_notqualified_download_grps$query_service),
                                  unique(p8_nwis_sc_notqualified_download_grps$task_num)),
               site_numbers = p8_nwis_sc_notqualified_download_grps$site_no,
               param_cd = p1_nwis_pcode_sc,
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = unique(p8_nwis_sc_notqualified_download_grps$query_service)
             ),
             pattern = map(p8_nwis_sc_notqualified_download_grps),
             format = 'file'), 
  
  ###### NWIS DATA 9: Download site metadata ######
  
  tar_target(p8_nwis_sc_sites_metadata,
             download_nwis_metadata(p8_nwis_sc_notqualified$site_no)),
  
  # Identify which of the sites have tidal influence, which will be used
  # in filtering or grouping later in the pipeline, by querying for sites
  # that may have data for parameter codes that contain `tidally filtered`
  # discharge, velocity, or gage height, indicating tidal influence.
  # Returns empty vector when none of the sites have tidal influence
  tar_target(p8_nwis_sc_sites_tidal, 
             whatNWISdata(siteNumber = p8_nwis_sc_notqualified$site_no,
                          parameterCd = c("72137", "72138", "72139", "72168",
                                          "72169", "72170", "72171")) %>%
               pull(site_no) %>% unique()), 
  
  ##### TIMESERIES DATA PREP #####
  ###### TS DATA 1: Calc daily mean SC from instantaneous data ######
  
  # Processing UV files by combining instantaneous data into daily data
  tar_target(p8_ts_sc_all_as_dv_feather, 
             calculate_dv_from_uv(out_file_dir = '8_Validate/tmp',
                                  in_file = p8_nwis_sc_data_feather,
                                  site_tz_xwalk = p8_nwis_sc_sites_metadata,
                                  param_colname = 'SpecCond'), 
             pattern = map(p8_nwis_sc_data_feather),
             format = 'file'),
  
  ###### TS DATA 2: Combine all daily mean SC data ######
  
  # Combine/format into a single file. Also, it replaces values of -999999. 
  tar_target(p8_ts_sc_dv_feather, 
             combine_all_dv_data(out_file = '8_Validate/tmp/ts_sc_dv.feather',
                                 in_files = p8_ts_sc_all_as_dv_feather,
                                 param_colname = 'SpecCond'),
             format = 'file'), 
  
  ##### TS FILTERING: Filter sites and data as part of processing in `2_Prepare` #####
  
  ##### Step 1: identify sites that meet (or don't) temporal criteria #####
  
  # Identify sites that have at least 3 years 
  tar_target(p8_ts_sc_winter_qualified,
             filter_winter(p8_ts_sc_dv_feather,
                           param_colname = 'SpecCond')),
  
  # Identify the sites that also have some data occurring in the last 15 years (will keep them).
  tar_target(p8_ts_sc_temporal_qualified_sites, 
             identify_temporal_qualifying_sites(p8_ts_sc_winter_qualified)),
  
  ##### Step 2: Identify sites that have suspiciously high SC (will remove them) #####
  
  tar_target(p8_ts_sc_highSC_sites, identify_highSC_sites(p8_ts_sc_winter_qualified)),
  
  ##### Step 3: filter data to just those sites that match our requirements #####
  
  tar_target(p8_sites_unqualified, c(p8_nwis_sc_sites_tidal,
                                     p8_ts_sc_highSC_sites)),
  
  # Qualified sites that were left out of initial model because no Q data
  tar_target(p8_ts_sc_qualified, 
             filter_data_to_qualifying_sites(p8_ts_sc_winter_qualified, 
                                             keep_sites = p8_ts_sc_temporal_qualified_sites,
                                             remove_sites = p8_sites_unqualified)),
  
  # Create site ID to nhd COMID crosswalk 
  tar_group_size(p8_nwis_sc_sites_sf, 
                 fetch_site_locations(p8_nwis_sc_sites_metadata),
                 size = 50),
  
  # Then, query NHDPlus using the site locations to identify
  # the NHD COMID of the closest reach. Two of the COMIDs are 
  # reused for the same sites:
  #   COMID 5866457 is linked to site_no '01104455' and '01104460'
  #   COMID 11079215 is linked to site_no '07381324' and '07381328'
  tar_target(p8_nwis_site_nhd_comid_ALL_xwalk, 
             identify_site_comids(p8_nwis_sc_sites_sf),
             pattern = map(p8_nwis_sc_sites_sf)),
  
  ###### SpC time series for ALL sites ######
  tar_target(p8_ts_sc_qualified_nhd, 
                    p8_ts_sc_qualified %>% 
                    left_join(p8_nwis_site_nhd_comid_ALL_xwalk %>% 
                    select(site_no, nhd_comid))), 
  
  tar_target(p8_predfct,
             p6_predict_episodic %>% inner_join(p8_nwis_site_nhd_comid_ALL_xwalk)),
  
  tar_target(p8_episodic_sites,
             p8_predfct %>% filter(pred == 'Episodic') %>% pull(site_no)),
  
  tar_target(p8_episodic_plotlist, create_episodic_plotlist(p8_ts_sc_qualified,
                                                            p8_episodic_sites,
                                                            p7_color_episodic,
                                                            p7_color_not_episodic)),
  tar_target(p8_episodic_png,
             ggsave(filename = sprintf('8_Validate/out/SI_episodic_grp%s.png', names(p8_episodic_plotlist)),
                    plot = p8_episodic_plotlist[[1]], height = 8, width = 6.5, dpi = 500),
             format = 'file', pattern = map(p8_episodic_plotlist))
)
