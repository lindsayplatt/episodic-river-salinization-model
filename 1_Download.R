# Targets for downloading data used in this analysis

source('1_Download/src/download_helper_fxns.R')
source('1_Download/src/nhdplus_fxns.R')
source('1_Download/src/nwis_fxns.R')
source('1_Download/src/retry_fxns.R')
source('1_Download/src/nwm_fxns.R')
source('1_Download/src/get_flowline_index_HD.R')

p1_targets <- list(
  
  # Define target listing the state abbreviations within the Contiguous United States (CONUS)
  tar_target(p1_conus_state_cds, c("CT", "DE", "IL", "IN", "IA", "KY", "ME", "MD", 
                                   "MA", "MI", "MN", "MO", "NH", "NJ", "NY", "OH", 
                                   "PA", "RI", "VT", "VA", "WV", "WI")), 
  
  ##### NWIS DATA: Download SC and Q {75 MIN} #####
  
  # All are prefixed with `p1_nwis_`
  
  ###### NWIS DATA 0: Set download configs for streamflow (Q) & specific conductivity (SC) ######
  
  tar_target(p1_nwis_start_date, as.Date('1950-01-01')), 
  tar_target(p1_nwis_end_date, as.Date('2024-03-31')), 
  tar_target(p1_nwis_pcode_sc, '00095'), # NWIS specific conductance code
  tar_target(p1_nwis_pcode_q, '00060'), # NWIS streamflow code
  tar_target(p1_nwis_min_years, 3), # Minimum number of years required
  tar_target(p1_nwis_min_end_date, as.Date('2000-01-01')), # Sites must have at least 1 record more recent than this
  
  ###### NWIS DATA 1: Identify sites with continuous SC (by state) ######
  tar_target(p1_nwis_sc_sites_all,
             inventory_nwis_sites_byState(state_cd = p1_conus_state_cds, 
                                          param_cd = p1_nwis_pcode_sc, 
                                          start_date = p1_nwis_start_date,
                                          end_date = p1_nwis_end_date),
             pattern = map(p1_conus_state_cds),
             iteration = 'list'),
  
  ###### NWIS DATA 2: Inventory NWIS & only keep sites with mean daily or instantaneous SC (by state) ######
  tar_target(p1_nwis_sc_data_info, inventory_nwis_data(site_numbers = p1_nwis_sc_sites_all, 
                                                       param_cd = p1_nwis_pcode_sc,
                                                       start_date = p1_nwis_start_date,
                                                       end_date = p1_nwis_end_date),
             pattern = map(p1_nwis_sc_sites_all),
             iteration = 'list'),
  
  ###### NWIS DATA 3: Filter SC sites for `min_yrs` and `min_end_date` (by state) ######
  tar_target(p1_nwis_sc_sites_qualified, 
             filter_to_min_data_standards(p1_nwis_sc_data_info, 
                                          min_yrs = p1_nwis_min_years, 
                                          min_end_date = p1_nwis_min_end_date),
             pattern = map(p1_nwis_sc_data_info),
             iteration = 'list'),
  
  ###### NWIS DATA 4: Identify SC sites that also have mean daily or instantaneous Q (by state) ######
  # The Q data should meet the same minimum data standards as SC. That filtering is combined into
  # a single step below. Notice there is no `iteration = 'list'` because we want the output to be
  # a single table, not a list of tables by state as we have had above.
  tar_target(p1_nwis_q_sites_query,
             inventory_nwis_data(site_numbers = p1_nwis_sc_sites_qualified$site_no, 
                                 param_cd = p1_nwis_pcode_q,
                                 start_date = p1_nwis_start_date,
                                 end_date = p1_nwis_end_date) %>% 
               filter_to_min_data_standards(min_yrs = p1_nwis_min_years, 
                                            min_end_date = p1_nwis_min_end_date),
             pattern = map(p1_nwis_sc_sites_qualified)),
  
  # Using the sites that also have qualified Q, filter the SC site list before querying
  tar_target(p1_nwis_sc_sites_query, p1_nwis_sc_sites_qualified, #%>% 
             # Someone can uncomment this in the future but we are not
             # currently using NWIS Q to filter the sites used in the 
             # remainder of the analysis, though it would be easy to 
             # uncomment this and do that.
             #  filter(site_no %in% p1_nwis_q_sites_query$site_no),
             pattern = map(p1_nwis_sc_sites_qualified)),
  
  ###### NWIS DATA 5: Prepare to download all the SC data ###### 
  
  # Separate download groups by the service and reasonably sized downloads.
  # dv = daily data
  # uv = instantaneous data (downloading only if mean daily values aren't available)
  
  tar_target(p1_nwis_sc_sites_query_download_grps,
             p1_nwis_sc_sites_query %>% 
               add_download_grp() %>% 
               group_by(query_service, task_num) %>% 
               tar_group(),
             iteration = 'group'),
  
  ###### NWIS DATA 6: Download the SC data and save as files in `1_Download/out_nwis` ######
  
  tar_target(p1_nwis_sc_data_feather,
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/sc_%s_%03d.feather',
                                  unique(p1_nwis_sc_sites_query_download_grps$query_service),
                                  unique(p1_nwis_sc_sites_query_download_grps$task_num)),
               site_numbers = p1_nwis_sc_sites_query_download_grps$site_no,
               param_cd = p1_nwis_pcode_sc,
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = unique(p1_nwis_sc_sites_query_download_grps$query_service)
             ),
             pattern = map(p1_nwis_sc_sites_query_download_grps),
             format = 'file'),
  
  ###### NWIS DATA 7: Prepare to download all the Q data ###### 
  
  # Separate download groups by the service and reasonably sized downloads.
  # dv = daily data
  # uv = instantaneous data (downloading only if mean daily values aren't available)
  
  tar_target(p1_nwis_q_sites_query_download_grps,
             p1_nwis_q_sites_query %>% 
               add_download_grp() %>% 
               group_by(query_service, task_num) %>% 
               tar_group(),
             iteration = 'group'),
  
  ###### NWIS DATA 8: Download the Q data and save as files in `1_Download/out_nwis` ######
  
  tar_target(p1_nwis_q_data_feather, 
             download_nwis_data(
               out_file = sprintf('1_Download/out_nwis/q_%s_%03d.feather', 
                                  unique(p1_nwis_q_sites_query_download_grps$query_service),
                                  unique(p1_nwis_q_sites_query_download_grps$task_num)),
               site_numbers = p1_nwis_q_sites_query_download_grps$site_no,
               param_cd = p1_nwis_pcode_q, 
               start_date = p1_nwis_start_date,
               end_date = p1_nwis_end_date,
               service_cd = unique(p1_nwis_q_sites_query_download_grps$query_service)
             ),
             pattern = map(p1_nwis_q_sites_query_download_grps),
             format = 'file'),
  
  ###### NWIS DATA 9: Download site metadata ######
  
  tar_target(p1_nwis_sc_sites_metadata,
             download_nwis_metadata(p1_nwis_sc_sites_query$site_no)),
  
  # Identify which of the sites have tidal influence, which will be used
  # in filtering or grouping later in the pipeline, by querying for sites
  # that may have data for parameter codes that contain `tidally filtered`
  # discharge, velocity, or gage height, indicating tidal influence.
  # Returns empty vector when none of the sites have tidal influence
  tar_target(p1_nwis_sc_sites_tidal, 
             whatNWISdata(siteNumber = p1_nwis_sc_sites_query$site_no,
                          parameterCd = c("72137", "72138", "72139", "72168",
                                          "72169", "72170", "72171")) %>%
               pull(site_no) %>% unique()),
  
  ##### SCIENCEBASE DATASET DOWNLOADS {< 1 MIN} #####
  
  # All are prefixed with `p1_sb_`
  
  # All ScienceBase (SB) datasets can be visited on SB by visiting the URL
  #   `https://www.sciencebase.gov/catalog/item/{ITEM ID}` and input the 
  #   item id for each dataset at the end of the URL.
  
  ###### SB DATA 1: Road salt application rates (Falcone et al., 2018) ######
  
  # Falcone, J. A., Oelsner, G. P., and Bock, A. R. (2018). Estimates of Road
  #   Salt Application across the conterminous United States (1992-2015).
  
  # Downloading two full zipfiles and then extracting gridded road salt 
  # application rate raster files to calculate an average from 2010-2019
  
  # Have to do this in two chunks because there are two different zip files
  tar_target(p1_sb_road_salt_before2015_zip, 
             item_file_download(sb_id = '5b15a50ce4b092d9651e22b9',
                                names = '1992_2015.zip',
                                destinations = '1_Download/tmp/road_salt_1992_2015.zip'),
             format = 'file'),
  tar_target(p1_sb_road_salt_before2015_tif, 
             extract_file_from_zip(out_file = sprintf('1_Download/out/road_salt_%s.tif', 2010:2015), 
                                   zip_file = p1_sb_road_salt_before2015_zip,
                                   file_to_extract = sprintf('%s.tif', 2010:2015)), 
             format = 'file'),
  
  # Now need to get 2016-2019, which are stored in a different zip file
  tar_target(p1_sb_road_salt_after2015_zip, 
             item_file_download(sb_id = '5b15a50ce4b092d9651e22b9',
                                names = '2016_2019.zip',
                                destinations = '1_Download/tmp/road_salt_2016_2019.zip'),
             format = 'file'),
  tar_target(p1_sb_road_salt_after2015_tif, 
             extract_file_from_zip(out_file = sprintf('1_Download/out/road_salt_%s.tif', 2016:2019), 
                                   zip_file = p1_sb_road_salt_after2015_zip,
                                   file_to_extract = sprintf('%s.tif', 2016:2019)), 
             format = 'file'),
  
  # Now combine into a single list of files
  tar_target(p1_sb_road_salt_tif, 
             c(p1_sb_road_salt_before2015_tif, p1_sb_road_salt_after2015_tif),
             format = 'file'),
  
  ###### SB DATA 2: Gridded groundwater attributes (Zell and Sanford, 2020) ######
  # Includes transmissivity and depth to water table
  
  # Zell, W. O., Sanford, W. E. (2020). Calibrated simulation of the long‐term average
  #   surficial groundwater system and derived spatial distributions of its characteristics 
  #   for the contiguous United States. Water Resources Research, 55, e2019WR026724. 
  #   https://doi.org/10.1029/2019WR026724.
  
  tar_target(p1_sb_depth2wt_csv, 
             item_file_download(sb_id = '60be54f6d34e86b9389117f9',
                                names = 'dtw.csv',
                                destinations = '1_Download/out/depth2wt.csv'), 
             format = 'file'),
  
  tar_target(p1_sb_transmissivity_csv, 
             item_file_download(sb_id = '60be54f6d34e86b9389117f9',
                                names = 'trans.csv',
                                destinations = '1_Download/out/transmissivity.csv'), 
             format = 'file'),
  
  ##### NHD+: Identify COMIDs for USGS NWIS sites & download catchment attributes {?? MIN} #####
  
  ###### Link stream sites to COMIDs #####
  
  # First, turn sites into a spatial features object and group into sets of 100 sites
  # in preparation for querying NHDPlus
  tar_group_size(p1_nwis_sc_sites_sf, 
                 fetch_site_locations(p1_nwis_sc_sites_metadata),
                 size = 50),
  
  # Then, query NHDPlus using the site locations to identify
  # the NHD COMID of the closest reach. Two of the COMIDs are 
  # reused for the same sites:
  #   COMID 5866457 is linked to site_no '01104455' and '01104460'
  #   COMID 11079215 is linked to site_no '07381324' and '07381328'
  tar_target(p1_nwis_site_nhd_comid_ALL_xwalk, 
             identify_site_comids(p1_nwis_sc_sites_sf),
             pattern = map(p1_nwis_sc_sites_sf)),
  
  ###### Download desired catchment polygons and static attributes from NHDPlus #####
  
  # Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
  #   NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
  #   for the Conterminous United States (ver. 4.0, August 2023): U.S. Geological Survey 
  #   data release, https://doi.org/10.5066/F7765D7V.
  
  # NHDPlus reach and catchment attributes originating from Wieczorek et al., 2018 but
  # downloaded via functions in the `nhdplusTools` R package.
  tar_target(p1_nhdplus_attr_yml, '1_Download/in/nhdplus_attributesTOT.yml', format='file'),
  tar_target(p1_nhdplus_attr_list, load_nhdplus_attribute_list(p1_nhdplus_attr_yml)),
  
  # Download catchment attributes data for each COMID
  # Needs to include attributes relating to agriculture (CAT_NLCD19_81 is pasture/hay 
  # and CAT_NLCD19_82 is cultivated crops) to be used during `3_Filter`.
  tar_target(p1_nhdplus_attr_vals_tbl, 
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = unique(p1_nwis_site_nhd_comid_ALL_xwalk$nhd_comid)),
             pattern = map(p1_nhdplus_attr_list)),
  
  # Save attributes with their definitions and commit to the repo
  tar_target(p1_nhdplus_attr_definitions, 
             get_nhdplus_attribute_definitions(p1_nhdplus_attr_vals_tbl)),
  
  # Prepare COMIDs to download so that polygons are only downloaded and stored once
  tar_target(p1_nhdplus_comids, na.omit(unique(p1_nwis_site_nhd_comid_ALL_xwalk$nhd_comid))),
  tar_target(p1_nhdplus_comids_upstream_ALL, identify_upstream_comids(p1_nhdplus_comids), map(p1_nhdplus_comids)), # Identify upstream COMIDs
  tarchetypes::tar_group_count(p1_nhdplus_comids_grp, 
                               count = 1000, # Set 1000 groups to map over
                               # Create unique vector of COMIDs to download catchments only once
                               tibble(nhd_comid = unique(c(p1_nhdplus_comids, p1_nhdplus_comids_upstream_ALL$nhd_comid_upstream)))),
  
  # Download NHD+ catchment polygons by groups of COMIDs (should be 1000 total branches with
  # ~590 COMIDs each). This takes slightly over two hours to download over 600k COMID catchments
  tar_target(p1_nhdplus_catchments_gpkg,
             download_nhdplus_catchments(out_file = sprintf('1_Download/out_nhdplus/nhdplus_catchment_%s.gpkg',
                                                            unique(p1_nhdplus_comids_grp$tar_group)),
                                         comids = p1_nhdplus_comids_grp$nhd_comid),
             pattern = map(p1_nhdplus_comids_grp),
             format = 'file', error = "continue"),
  # tar_target(p1_nhdplus_catchments_gpkg, list.files('1_Download/out_nhdplus/',full.names = T))
  
  # Download the full, national NHD+ flowlines and save as a local file
  tar_target(p1_nhdplus_gdb_7z, {
    download_nhdplusv2("./1_Download/tmp/")
    # The NHD+ function doesn't return the filepath, so have to do it manually
    return('1_Download/tmp/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z')
  }, format='file'),
  
  # You will need the 7-Zip program to unzip the file from NHDPlus
  # On Windows, the 7z executable is typically available in `C:/Program Files/7-Zip/`
  # On a Mac, the 7z executable is typically available in `/usr/local/bin/7z`
  tar_target(p1_path_7z, find_7z()), 
  
  # Unzip flowlines GDB *to a new folder* (this part is key)
  tar_target(p1_nhdplus_gdb, {
    zip_file <- p1_nhdplus_gdb_7z
    dest_dir <- '1_Download/out_nhdplus'
    dest_file <- 'NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb'
    system2(p1_path_7z, args = c("x", zip_file, paste0("-o", dest_dir), '-aoa'))
    return(file.path(dest_dir, dest_file))
  }, format = 'file'),
  
  # Load the national flowlines layer as an sf object and drop the vertical attributes
  # Also, remove any flowline with a catchment area of zero. These indicate flowlines that
  # will not have a polygon and therefore, will not work for summarizing road salt.
  # This is kind of a "process" step but there is no point to saving more data than
  # we need to.
  tar_target(p1_nhdplus_flowlines_sf, 
             st_read(p1_nhdplus_gdb, layer = 'NHDFlowline_Network') %>% 
             st_zm()), #filter(AreaSqKM > 0) !Don't filter out zeros 
  
  # Load the national catchments layer as an sf object and rename `FEATUREID` to `nhd_comid`
  tar_target(p1_nhdplus_catchments_sf, 
             st_read(p1_nhdplus_gdb, layer = 'CatchmentSP') %>% 
               rename(nhd_comid = FEATUREID, areasqkm = AreaSqKM)),
  
  ##### Download National Water Model streamflow #####
  
  # NOAA National Water Model CONUS Retrospective Dataset was accessed 
  #   on May 27, 2024 from https://registry.opendata.aws/nwm-archive.
  
  # And calculates the median per COMID before returning data
  # This took ~4 hrs to run 4 groups of ~100 COMIDs
  
  tarchetypes::tar_group_size(p1_nwm_comids_grp,
                               size = 100, # Set groups of 10 to map over NWM download
                               tibble(nhd_comid = p1_nhdplus_comids)),
  tar_target(p1_nwm_streamflow, download_NWMv2_streamflow(p1_nwm_comids_grp$nhd_comid),
             pattern = map(p1_nwm_comids_grp))
  
)
