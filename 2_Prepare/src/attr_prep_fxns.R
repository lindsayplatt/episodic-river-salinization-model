
#' @title Calculate static Q metrics for each site
#' @description Using the daily timeseries data for streamflow, calculate a single
#' value per site to serve as a static attribute, including median flow, high and
#' low percentiles, and flow based on specific seasons.
#' 
#' @param data_q a tibble containing all daily flow records with at least the 
#' columns `site_no`, `dateTime`, and `Flow`.
#' 
#' @return a feather file containing only one row for each site; it should have 
#' the columns `site_no` and any number of columns with the naming pattern 
#' `attr_[metric]Flow`.
#'
calculate_q_stats_per_site <- function(data_q) {
  data_q %>% 
    group_by(site_no) %>% 
    summarize(attr_medianFlow = median(Flow, na.rm = TRUE))
}

#' @title Join NWM median streamflow to site numbers
#' @description Simple processing to get median streamflow per NHD COMID to
#' align with the site numbers we are using. Also, add the NHM scaling
#' factor to get NHM flow values into m3/s.
#' 
#' @param nwm_streamflow a tibble of median streamflow with the columns 
#' `nhd_comid` and `attr_medianFlowNWM`
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
#' 
#' @return a tibble with the columns `site_no`, `attr_medianFlow`
#'
prep_nwm_flow <- function(nwm_streamflow, comid_site_xwalk) {
  # This is stored in the `.zattrs` file under https://noaa-nwm-retro-v2-zarr-pds.s3.amazonaws.com/index.html#streamflow/
  nhm_scaling_factor <- 0.009999999776482582
  comid_site_xwalk %>% 
    left_join(nwm_streamflow, by = 'nhd_comid') %>% 
    select(site_no, attr_medianFlow = attr_medianFlowNWM) %>% 
    filter(!is.na(attr_medianFlow)) %>% 
    mutate(attr_medianFlow = round(attr_medianFlow*nhm_scaling_factor, 3))
}

#' @title Z-normalize a set of values
#' @description Find the z-scores for a set of values. This assumes that the
#' values passed in are already grouped appropriately prior to this function
#' being used. Courtesy of https://jmotif.github.io/sax-vsm_site/morea/algorithm/znorm.html
#' 
#' @param ts a vector of numeric values
#' 
#' @return a vector the same length as `ts` but with z-scored values
#' 
znorm <- function(ts){
  ts.mean <- mean(ts)
  ts.dev <- sd(ts)
  (ts - ts.mean)/ts.dev
}

#' @title Get area of catchment and watershed (cumulative catchments)
#' @description Using the column of polygon area per catchment returned by 
#' NHD+ and saved as `areasqkm`, this function calculates the area per
#' individual catchment and the total cumulative area for catchments upstream
#' of the featured COMID (including that COMIDs area). This is used downstream
#' as an attribute but also to calculate road salt application per sq km.
#' 
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` 
#' class. Should have at least the columns `nhd_comid` and `areasqkm`.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA. If
#' this argument is set to `NULL`, so site numbers are mapped and `nhd_comid` is 
#' returned.
#' 
#' @return tibble with four columns `site_no`, `attr_areaSqKm`,
#'  `attr_areaCumulativeSqKm`, `attr_areaRatio`, and `numNACatchments`. The ratio
#'  shows how big this catchment is compared to its full watershed. Values near 
#'  1 indicate that this catchment is either a headwaters catchment -OR- we are
#'  currently missing data for the upstream catchments.
#' 
calculate_catchment_areas <- function(polys_sf, comid_upstream_tbl, comid_site_xwalk) {
  # Using area that is given already - I checked and they are very similar 
  # to calculating the area using `st_area()`.
  
  catchment_area_tbl <- polys_sf %>% 
    st_drop_geometry() %>% 
    select(nhd_comid_upstream = nhd_comid, area_sqkm = areasqkm)
  
  comid_catchment_info <- comid_upstream_tbl %>% 
    left_join(catchment_area_tbl, by = 'nhd_comid_upstream') %>% 
    # Per NHD COMID, calculate the area of *just* that catchment and
    # the area of all upstream catchments. 
    group_by(nhd_comid) %>% 
    summarize(attr_areaSqKm = area_sqkm[nhd_comid_upstream == nhd_comid],
              attr_areaCumulativeSqKm = sum(area_sqkm, na.rm=TRUE),
              attr_areaRatio = attr_areaSqKm / attr_areaCumulativeSqKm,
              numNACatchments = sum(is.na(area_sqkm))) %>% 
    ungroup() 
  
  if(!is.null(comid_site_xwalk)) {
    comid_catchment_info_ready <- comid_catchment_info %>% 
      # Map these attributes from NHD COMIDs to NWIS sites
      right_join(comid_site_xwalk, by = 'nhd_comid') %>%
      select(site_no, attr_areaSqKm, attr_areaCumulativeSqKm, attr_areaRatio, numNACatchments)
  } else {
    comid_catchment_info_ready <- comid_catchment_info %>% 
      select(nhd_comid, attr_areaSqKm, attr_areaCumulativeSqKm, attr_areaRatio, numNACatchments) 
  }
  
  return(comid_catchment_info_ready)
}

#' @title Get area of each catchment and total upstream area, keep all upstream comids
#' @description Using the column of polygon area per catchment returned by 
#' NHD+ and saved as `areasqkm`, this function calculates the area per
#' individual catchment and the total cumulative area for catchments upstream
#' of the featured COMID (including that COMIDs area). This is used downstream
#' as an attribute but also to calculate road salt application per sq km.
#' 
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` 
#' class. Should have at least the columns `nhd_comid` and `areasqkm`.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
#' 
#' @return tibble with four columns `site_no`, `areaSqKm`,
#'  `total_area_sqkm`, `nhd_comid`, and `nhd_comid_upstream`.
#' 
calculate_Upstream_areas <- function(polys_sf, comid_upstream_tbl, comid_site_xwalk) {
  # Using area that is given already - I checked and they are very similar 
  # to calculating the area using `st_area()`.
  catchment_area_tbl <- polys_sf %>% 
    st_drop_geometry() %>% 
    select(nhd_comid_upstream = nhd_comid, area_sqkm = areasqkm)
  
  comid_upstream_tbl %>% 
    left_join(catchment_area_tbl, by = 'nhd_comid_upstream') %>% 
    # Map these attributes from NHD COMIDs to NWIS sites
    right_join(comid_site_xwalk, by = 'nhd_comid') %>%
    group_by(nhd_comid) %>% 
    mutate(total_area_sqkm = sum(area_sqkm)) %>% 
    ungroup() %>% 
    select(site_no, nhd_comid, nhd_comid_upstream, area_sqkm, total_area_sqkm)
}

#' @title Average road salt application rasters
#' @description Read annual road salt application rasters and calculate an average
#' 
#' @param road_salt_tif filepath(s) to the road salt tif files that should be averaged
#' 
#' @returns a single raster of the same dimensions and size as the tifs passed in
#' 
average_road_salt_rasters <- function(road_salt_tifs) {
  raster::stack(road_salt_tifs) %>% 
    mean() %>% 
    setNames('road_salt_avg')
}

#' @title Aggregate road salt application values per polygon
#' @description Extract and sum cell values from the road salt raster file
#' to get a single road salt application rate value for each polygon.
#' 
#' @param road_salt_rast a single raster object of road salt application rates
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` class.
#' @param rasterTifs the file path for the raster input tifs. Only needed if road_salt_raster == NULL 
#' 
#' @returns a tibble with the columns `nhd_comid` and `road_salt_kgs` with the
#' total road salt per COMID catchment polygon
#' 
aggregate_road_salt_per_poly <- function(road_salt_rast, polys_sf, rasterTifs = NULL) {
  
  if(is.null(road_salt_rast)) {
    salt_per_poly = raster::stack(rasterTifs) %>% 
      mean() %>% 
      setNames('road_salt_avg') %>% 
      raster::projectRaster(crs = st_crs(polys_sf)$input) %>%
      exactextractr::exact_extract(polys_sf, 'sum')
      
  } else {
  # Reproject the road salt raster data
  road_salt_rast_proj <- raster::projectRaster(road_salt_rast, 
                                               crs = st_crs(polys_sf)$input)
  # Now calculate the total amount of salt within each polygon. Note that for
  # this function, sum = "the sum of non-NA raster cell values, multiplied by 
  # the fraction of the cell that is covered by the polygon".
  salt_per_poly <- exactextractr::exact_extract(road_salt_rast_proj, polys_sf, 'sum')
  }
  
  # Now add to a table for export
  road_salt_poly <- polys_sf %>% 
    mutate(road_salt_lbs = salt_per_poly,
           # Convert from pounds to kilograms (2.205 lb/kg)
           road_salt_kgs = road_salt_lbs / 2.205) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    dplyr::select(nhd_comid, road_salt_kgs)
  
  return(road_salt_poly)
}

#' @title Get road salt value per site
#' @description Using road salt values per NHD+ COMID catchment, map road salt to
#' NWIS sites based on the crosswalk from COMID to site.
#' 
#' @param road_salt_comid a tibble with at least the columns `nhd_comid` and
#' `road_salt_kgs`. Note that as described in `extract_nhdplus_geopackage_layer()`,
#' not all COMIDs had a catchment polygon that were downloaded (e.g. COMID `4672393`
#' did not have a catchment polygon available) and therefore will not have a salt value.
#' @param basin_areas a tibble with at least the columns `site_no`, `attr_areaSqKm`,
#' and `attr_areaCumulativeSqKm` representing catchment and upstream basin total area.
#' @param comid_site_xwalk a tibble with at least the columns `site_no` and 
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA. If
#' this argument is set to `NULL`, so site numbers are mapped and `nhd_comid` is 
#' returned.
#' @param comid_upstream_tbl a tibble with the columns `nhd_comid` and `nhd_comid_upstream`
#' mapping all upstream comids to each COMID with an NWIS site. It should match
#' the output of `identify_upstream_comids()`.
#' 
#' @return tibble with three columns `site_no`, `attr_roadSaltPerSqKm`,
#'  `attr_roadSaltCumulativePerSqKm`, and `attr_roadSaltRatio` (the ratio of 
#'  salt per sq km applied in the catchment vs all upstream catchments; values
#'  larger than 1 mean more salt/km2 applied in this catchment compared to the
#'  rest of the upstream watershed, and values less than one mean less salt/km2
#'  applied in this catchment compared to its upstream watershed).
#' 
map_catchment_roadSalt_to_site <- function(road_salt_comid, basin_areas, comid_site_xwalk, comid_upstream_tbl) {
  
  # Calculate cumulative road salt for each catchment and all upstream catchments
  comid_roadSalt <- comid_upstream_tbl %>% 
    left_join(road_salt_comid, by = c('nhd_comid_upstream' = 'nhd_comid')) %>% 
    group_by(nhd_comid) %>% 
    summarize(roadSalt = road_salt_kgs[nhd_comid_upstream == nhd_comid],
              roadSaltCumulative = sum(road_salt_kgs, na.rm=TRUE),
              .groups='keep') %>% 
    ungroup()

  join_col <- ifelse(is.null(comid_site_xwalk), 'nhd_comid', 'site_no')
  
  # Use the road salt data per catchment & by upstream catchment
  comid_roadSalt_ready <- comid_roadSalt %>% {
    if(!is.null(comid_site_xwalk)) {
      # Now join in site_no to nhd_comid if xwalk is provided
      right_join(., comid_site_xwalk, by = 'nhd_comid')
    } else .
  } %>% 
    # Now join the catchment areas 
    left_join(basin_areas, by = join_col) %>% 
    # Now calculate road salt per area
    mutate(attr_roadSaltPerSqKm = roadSalt / attr_areaSqKm,
           attr_roadSaltCumulativePerSqKm = roadSaltCumulative / attr_areaCumulativeSqKm,
           attr_roadSaltRatio = roadSalt / roadSaltCumulative) %>% 
    dplyr::select(one_of(join_col), attr_roadSaltPerSqKm, attr_roadSaltCumulativePerSqKm, attr_roadSaltRatio)
  
  return(comid_roadSalt_ready)
}

#' @title Pivot downloaded NHD attributes from long to wide
#' @description The output from `download_nhdplus_attributes()` is in 
#' long format but we need each static attribute to be its own column
#' and to identify the site that matches the NHD COMID.
#' 
#' @param nhd_attribute_table a tibble of attribute values for each COMID with  
#' the columns `nhd_comid`, `nhd_attr_id`, `nhd_attr_val`, and `percent_nodata`
#' @param comid_site_xwalk a tibble with the columns `site_no`, `nhd_comid`, 
#' `with_retry`. If `NULL`, the id column will be `nhd_comid`.
#' 
#' @return tibble with the columns `site_no` and any number of columns containing
#' NHD+ static catchment attributes, most prefixed with `attr_[attribute name]`. Any
#' column that is an NHD+ attribute but not prefixed `attr_` is not included in the 
#' function renaming step. You could add it, or leave it as-is. 
#' 
prepare_nhd_attributes <- function(nhd_attribute_table, comid_site_xwalk) {
  if(is.list(nhd_attribute_table)) {
    nhd_attribute_table = bind_rows(nhd_attribute_table)
  }

  id_col <- ifelse(is.null(comid_site_xwalk), 'nhd_comid', 'site_no')
  
  nhd_attribute_table %>%
    # Pivot the NHD attributes to be columns
    pivot_wider(id_cols = nhd_comid, 
                names_from = nhd_attr_id, 
                values_from = nhd_attr_val) %>% {
      if(!is.null(comid_site_xwalk)) {
        # Join *into* the site number by COMID
        right_join(., comid_site_xwalk, by = 'nhd_comid') 
      } else .
        
    } %>% 
    
    # Operate the following sums/means/etc by row
    rowwise() %>% 
    
    # Combine some of the land-use categories (in % catchment area)
    mutate(
      # Forested = deciduous forest + evergreen forest + mixed forest
      attr_pctForested = sum(c_across(contains("19_41") | contains("19_42") | contains("19_43"))),
      # attr_pctWetland = CAT_NLCD19_90 + CAT_NLCD19_95,
      attr_pctWetland = sum(c_across(contains("19_90") | contains("19_95"))),
      # # Agriculture = pasture/hay + cropland
      attr_pctAgriculture = sum(c_across(contains("19_81") | contains("19_82"))),
      # # Developed = open (<20% impervious) + low (20-49%) + medium (50-79%) + high (80-100%)
      attr_pctDeveloped = sum(c_across(contains("19_21") | contains("19_22") | contains("19_23") | contains("19_24")))) %>%
      
      # attr_pctForested = NLCD19_41 + CAT_NLCD19_42 + CAT_NLCD19_43,
      # # Wetland = woody wetland + herbaceous wetland
      # attr_pctWetland = CAT_NLCD19_90 + CAT_NLCD19_95,
      # # Agriculture = pasture/hay + cropland
      # attr_pctAgriculture = CAT_NLCD19_81 + CAT_NLCD19_82,
      # # Developed = open (<20% impervious) + low (20-49%) + medium (50-79%) + high (80-100%)
      # attr_pctDeveloped = CAT_NLCD19_21 + CAT_NLCD19_22 + CAT_NLCD19_23 + CAT_NLCD19_24) %>% 
    
    # Calculate snow (in mm) from precip total
    # mutate(attr_annualSnow = CAT_PPT7100_ANN*CAT_PRSNOW/100) %>% 
    mutate(attr_annualSnow = sum(c_across(contains("PPT7100_ANN") | contains("PRSNOW")))/100) %>% 
    
    # Calculate average winter air temperature from monthly averages
    # mutate(attr_winterAirTemp = mean(c(CAT_TAV7100_DEC, CAT_TAV7100_JAN,
    #                                       CAT_TAV7100_FEB, CAT_TAV7100_MAR))) %>% 
    mutate(attr_winterAirTemp = mean(c_across(contains("TAV7100_DEC") | 
                                                contains("TAV7100_JAN") |
                                                contains("TAV7100_FEB") |
                                                contains("TAV7100_MAR")))) %>% 
    
    # Rename the columns whose values are used as-is
    rename(any_of(c(
      attr_annualPrecip = 'CAT_PPT7100_ANN', # in mm
      attr_baseFlowInd = 'CAT_BFI', # % total flow
      attr_subsurfaceContact = 'CAT_CONTACT', # days
      attr_gwRecharge = 'CAT_RECHG', # in mm/year
      attr_pctOpenWater = 'CAT_NLCD19_11', # % catchment area
      attr_basinSlope = 'CAT_BASIN_SLOPE', # % rise
      attr_annualPrecip = 'TOT_PPT7100_ANN', # in mm
      attr_baseFlowInd = 'TOT_BFI', # % total flow
      attr_subsurfaceContact = 'TOT_CONTACT', # days
      attr_gwRecharge = 'TOT_RECHG', # in mm/year
      attr_pctOpenWater = 'TOT_NLCD19_11', # % catchment area
      attr_basinSlope = 'TOT_BASIN_SLOPE' # % rise
    ))) %>% 
    
    # Select only the final attributes, which are those prefixed with `attr_`
    # Keep only the site_no and those final NHD attribute columns
    select(one_of(id_col), starts_with('attr_'))
  
}

#' @title Prepare the attributes from Zell and Sanford 2020
#' @description Load in the CSVs from Zell and Sanford 2020 and reformat to be
#' merged with the other static attribute data. Currently includes just the 
#' depth to the water table.
#' 
#' @param depth2wt_csv a string specifying the filepath to the CSV downloaded from
#' ScienceBase with the Zell and Sanford 2020 data release for depth to water table.
#' @param transmissivity_csv a string specifying the filepath to the CSV downloaded from
#' ScienceBase with the Zell and Sanford 2020 data release for transmissivity.
#' @param comid_site_xwalk a tibble with the columns `site_no`, `nhd_comid`, `with_retry`
#' @param returnSite binary. TRUE, matches to site, FALSE, returns comid

#' @return tibble with the columns `site_no`, `attr_zellSanfordDepthToWT`, and
#' `attr_transmissivity.`
#' 
prepare_sb_gw_attrs <- function(depth2wt_csv, transmissivity_csv, comid_site_xwalk, 
                                returnSite = TRUE) {
  
  dtw <- read_csv(depth2wt_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_depthToWT = dtw250) %>% 
    filter(!is.na(attr_depthToWT))
  
  trnmsv <- read_csv(transmissivity_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_transmissivity = trans250)
  
  if (returnSite == TRUE) {
    comid_site_xwalk %>% 
      left_join(dtw, by = 'nhd_comid') %>% 
      left_join(trnmsv, by = 'nhd_comid') %>% 
      select(site_no, starts_with('attr_')) 
  } else {
    comid_site_xwalk %>% 
      left_join(dtw, by = 'nhd_comid') %>% 
      left_join(trnmsv, by = 'nhd_comid') %>% 
      select(nhd_comid, starts_with('attr_')) 
  }
  
}
