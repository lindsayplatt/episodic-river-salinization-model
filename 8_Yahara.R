
# 
source('8/attr_prep.R')
# 
# 13293526
# 13296724
# subset_nhdplus(comids = 13296724,  nhdplus_data = "download", flowline_only = FALSE)
# -89.0358, 42.5172, 
# Yahara = -89.1245, 42.809

p8_targets <- list(

  # Find starting comid
  tar_target(p8_startComid, 
             discover_nhdplus_id(sf::st_sfc(sf::st_point(c(-89.1245, 42.809)),
                                                crs = 4326))),
  # Get upstream comids
  tar_target(p8_upstreamComids,
             identify_upstream_comids(comid_in = p8_startComid)),
  
  # get the upstream comids
  tar_target(p8_useComids, p8_upstreamComids$nhd_comid_upstream),

  # Download NHD+ catchment polygons by groups of COMIDs
  tar_target(p8_nhdplus_catchments_gpkg,
             download_nhdplus_catchments(out_file = '1_Download/out_yahara/nhdplus_catchments.gpkg',
                                         comids = p8_useComids),
             format = 'file', error = "continue"),

  
  # Extract the flowline spatial features from the downloaded geopackages. This 
  # includes ALL COMIDs (even those with 0 drainage areas), but will be filtered later.
  tar_target(p8_nhdplus_flowlines_ALL_sf, extract_nhdplus_geopackage_layer(p8_nhdplus_catchments_gpkg, 
                                                                           gpkg_layer = 'NHDFlowline_Network')),
  # Each COMID and site will have a value for salt application for just the
  # individual COMID catchment (`attr_roadSaltPerSqKm`) but also a total including
  # all NHD+ catchments upstream (`attr_roadSaltCumulativePerSqKm`).
  
  # Extract the catchments as polygons and summarize total salt per catchment
  # This includes any catchments that will only be used for upstream calculations
  tar_target(p8_nhdplus_catchment_sf, extract_nhdplus_geopackage_layer(p8_nhdplus_catchments_gpkg)),
  tar_target(p8_nhdplus_catchment_salt, aggregate_road_salt_per_poly(road_salt_tif = p1_sb_road_salt_2015_tif,
                                                                     polys_sf = p8_nhdplus_catchment_sf)),

  # Calculate the total area of each catchment & its upstream catchements
  tar_target(p8_attr_basinArea, p8_nhdplus_catchment_sf %>% 
               st_drop_geometry() %>% 
               select(nhd_comid, attr_area_sqkm = areasqkm)),
  
  # Then, map salt for each NHD COMID catchment polygon to sites and calculate cumulative road salt
  tar_target(p8_attr_roadSalt, p8_nhdplus_catchment_salt %>% left_join(p8_attr_basinArea, by = join_by(nhd_comid)) %>% 
               mutate(attr_roadSaltPerSqKm = road_salt_kgs / attr_area_sqkm)),
  
  # Get upstream vars 
  tar_target(p8_nhdplus_attr_vals_tbl_upstream, 
             download_nhdplus_attributes(attributes = unlist(p1_nhdplus_attr_list),
                                         comids = p8_useComids),
             pattern = map(p1_nhdplus_attr_list)),
  
  
  # TEST OUT TOTAL ATTRIBUTES (instead of local. similar but different)
  # tar_target(p9_nhdplus_attr_yml, '1_Download/in/nhdplus_attributesTOT.yml', format='file'),
  # tar_target(p9_nhdplus_attr_list, load_nhdplus_attribute_list(p9_nhdplus_attr_yml)),
  # tar_target(p9_nhdplus_attr_vals_tbl_upstream, 
  #            download_nhdplus_attributes(attributes = unlist(p9_nhdplus_attr_list),
  #                                        comids = p8_useComids),
  #            pattern = map(p9_nhdplus_attr_list)),
  # tar_target(p9_attr_nhd, prepare_nhd_attributesComidTOT(p9_nhdplus_attr_vals_tbl_upstream,
  #                                                     p8_useComids)),
  
  ###### Pivot and link NHD+ attributes to sites ######
  
  tar_target(p8_attr_nhd, prepare_nhd_attributesComid(p8_nhdplus_attr_vals_tbl_upstream,
                                                      p8_useComids)),
  
  # Prepare the attributes from Zell and Sanford 2020 which are based on NHD+ COMIDs
  tar_target(p8_attr_depth2wt_trnmsv, prepare_sb_gw_attrs(p1_sb_depth2wt_csv, 
                          p1_sb_transmissivity_csv,
                          data.frame(nhd_comid = p8_useComids),
                          returnSite = FALSE)),
  
  ###### Combine all static attributes into one table ######
  tar_target(p8_attr_all, combine_static_attributes(joinby = 'nhd_comid',
                                                    # p2_attr_flow,
                                                    p8_attr_roadSalt,
                                                    p8_attr_nhd,
                                                    p8_attr_depth2wt_trnmsv) %>% 
               rename_with(~gsub("attr_", "", .x))),
  
  # p8_attr_all %>% select(nhd_comid, any_of(row.names(p6b_rf_model_optimized$importance)))

  # Get all upstream comids for the entire watershed
  tar_target(p8_allupstreamComids,
             identify_upstream_comids(comid_in = p8_useComids),
             map(p8_useComids)),
  
  # Join upstream comids to individual comids
  tar_target(p8_joinComids, 
                p8_allupstreamComids %>% 
                left_join(p8_attr_all %>% rename(nhd_comid_upstream = nhd_comid))),
  
  # Create upstream attributes
  tar_target(p8_attr_nhd_upstream, p8_joinComids %>% 
               mutate(across(-c(nhd_comid, nhd_comid_upstream, area_sqkm), ~ . * area_sqkm)) %>% 
               group_by(nhd_comid) %>% 
               summarise_all(sum, na.rm = TRUE) %>% 
               select(-nhd_comid_upstream) %>% 
               ungroup() %>% 
               mutate(across(-c(nhd_comid, area_sqkm), ~ . / area_sqkm)) %>% 
               select(nhd_comid, total_area_sqkm = area_sqkm, everything()) %>% 
               rename_at(vars(-nhd_comid, -total_area_sqkm), function(x) paste0(x,"_upstream")))
)


# tar_load(p8_attr_nhd_upstream)
# tar_load(p6b_rf_model_optimized)
# tar_load(p8_nhdplus_flowlines_ALL_sf)
# 
# a = p8_attr_nhd_upstream %>%
#   rename(roadSaltCumulativePerSqKm = roadSaltPerSqKm_upstream) %>%
#   rename(pctAgriculture = pctAgriculture_upstream) %>%
#   rename(depthToWT = depthToWT_upstream) %>%
#   mutate(pred = predict(p6b_rf_model_optimized, .))
# 
# b = p8_nhdplus_flowlines_ALL_sf %>%
#   left_join(a %>% select(nhd_comid, pred))
# 
# library(ggspatial)
# ggplot(b) +
#   annotation_map_tile(type = 'cartolight', zoom = 10) +
#   geom_sf(aes(color = pred))
