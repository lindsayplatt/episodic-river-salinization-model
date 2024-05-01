
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
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
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
  
  comid_upstream_tbl %>% 
    left_join(catchment_area_tbl, by = 'nhd_comid_upstream') %>% 
    # Per NHD COMID, calculate the area of *just* that catchment and
    # the area of all upstream catchments. 
    group_by(nhd_comid) %>% 
    summarize(attr_areaSqKm = area_sqkm[nhd_comid_upstream == nhd_comid],
              attr_areaCumulativeSqKm = sum(area_sqkm, na.rm=TRUE),
              attr_areaRatio = attr_areaSqKm / attr_areaCumulativeSqKm,
              numNACatchments = sum(is.na(area_sqkm))) %>% 
    ungroup() %>% 
    # Map these attributes from NHD COMIDs to NWIS sites
    right_join(comid_site_xwalk, by = 'nhd_comid') %>%
    select(site_no, attr_areaSqKm, attr_areaCumulativeSqKm, attr_areaRatio, numNACatchments)
}

#' @title Aggregate road salt application values per polygon
#' @description Extract and sum cell values from the road salt raster file
#' to get a single road salt application rate value for each polygon.
#' 
#' @param road_salt_tif filepath to the road salt tif file
#' @param polys_sf a spatial data frame with polygons. Needs to be an `sf` class.
#' 
#' @returns a tibble with the columns `nhd_comid` and `road_salt_kgs` with the
#' total road salt per COMID catchment polygon
#' 
aggregate_road_salt_per_poly <- function(road_salt_tif, polys_sf) {
  
  # Load and reproject the road salt raster data
  road_salt_rast <- raster::raster(road_salt_tif)
  road_salt_rast_proj <- raster::projectRaster(road_salt_rast, 
                                               crs = st_crs(polys_sf)$input)
  
  # Now calculate the total amount of salt within each polygon. Note that for
  # this function, sum = "the sum of non-NA raster cell values, multiplied by 
  # the fraction of the cell that is covered by the polygon".
  salt_per_poly <- exactextractr::exact_extract(road_salt_rast_proj, polys_sf, 'sum')
  
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
#' `nhd_comid`. Note that not all sites are mapped to a COMID and may be NA.
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
              .groups='keep')

  comid_site_xwalk %>% 
    # Now join the road salt data per catchment & by upstream catchment
    left_join(comid_roadSalt, by = 'nhd_comid') %>% 
    # Now do the same but for catchment area 
    left_join(basin_areas, by = 'site_no') %>% 
    # Now calculate road salt per area
    mutate(attr_roadSaltPerSqKm = roadSalt / attr_areaSqKm,
           attr_roadSaltCumulativePerSqKm = roadSaltCumulative / attr_areaCumulativeSqKm,
           attr_roadSaltRatio = roadSalt / roadSaltCumulative) %>% 
    dplyr::select(site_no, attr_roadSaltPerSqKm, attr_roadSaltCumulativePerSqKm, attr_roadSaltRatio)
}

#' @title Pivot downloaded NHD attributes from long to wide
#' @description The output from `download_nhdplus_attributes()` is in 
#' long format but we need each static attribute to be its own column
#' and to identify the site that matches the NHD COMID.
#' 
#' @param nhd_attribute_table a tibble of attribute values for each COMID with  
#' the columns `nhd_comid`, `nhd_attr_id`, `nhd_attr_val`, and `percent_nodata`
#' @param comid_site_xwalk a tibble with the columns `site_no`, `nhd_comid`, `with_retry`
#' 
#' @return tibble with the columns `site_no` and any number of columns containing
#' NHD+ static catchment attributes, most prefixed with `attr_[attribute name]`. Any
#' column that is an NHD+ attribute but not prefixed `attr_` is not included in the 
#' function renaming step. You could add it, or leave it as-is. 
#' 
prepare_nhd_attributes <- function(nhd_attribute_table, comid_site_xwalk) {
  
  nhd_attribute_table %>%
    # Pivot the NHD attributes to be columns
    pivot_wider(id_cols = nhd_comid, 
                names_from = nhd_attr_id, 
                values_from = nhd_attr_val) %>% 
    # Join *into* the site number by COMID
    right_join(comid_site_xwalk, by = 'nhd_comid') %>% 
    # Keep only the site_no and NHD attribute columns
    dplyr::select(site_no, everything(), 
                  -nhd_comid, -with_retry) %>% 
    
    # Operate the following sums/means/etc by row
    rowwise() %>% 
    
    # Combine some of the land-use categories (in % catchment area)
    mutate(
      # Forested = deciduous forest + evergreen forest + mixed forest
      attr_pctForested = CAT_NLCD19_41 + CAT_NLCD19_42 + CAT_NLCD19_43,
      # Wetland = woody wetland + herbaceous wetland
      attr_pctWetland = CAT_NLCD19_90 + CAT_NLCD19_95,
      # Agriculture = pasture/hay + cropland
      attr_pctAgriculture = CAT_NLCD19_81 + CAT_NLCD19_82,
      # Developed = open (<20% impervious) + low (20-49%) + medium (50-79%) + high (80-100%)
      attr_pctDeveloped = CAT_NLCD19_21 + CAT_NLCD19_22 + CAT_NLCD19_23 + CAT_NLCD19_24) %>% 
    
    # Calculate snow (in mm) from precip total
    mutate(attr_annualSnow = CAT_PPT7100_ANN*CAT_PRSNOW/100) %>% 
    
    # Calculate average winter air temperature from monthly averages
    mutate(attr_winterAirTemp = mean(c(CAT_TAV7100_DEC, CAT_TAV7100_JAN,
                                          CAT_TAV7100_FEB, CAT_TAV7100_MAR))) %>% 
    
    # Rename the columns whose values are used as-is
    rename(any_of(c(
      attr_annualPrecip = 'CAT_PPT7100_ANN', # in mm
      attr_baseFlowInd = 'CAT_BFI', # % total flow
      attr_subsurfaceContact = 'CAT_CONTACT', # days
      attr_gwRecharge = 'CAT_RECHG', # in mm/year
      attr_pctOpenWater = 'CAT_NLCD19_11', # % catchment area
      attr_basinSlope = 'CAT_BASIN_SLOPE' # % rise
    ))) %>% 
    
    # Select only the final attributes, which are those prefixed with `attr_`
    select(site_no, starts_with('attr_'))
  
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
#' 
#' @return tibble with the columns `site_no`, `attr_zellSanfordDepthToWT`, and
#' `attr_transmissivity.`
#' 
prepare_sb_gw_attrs <- function(depth2wt_csv, transmissivity_csv, comid_site_xwalk) {
  
  dtw <- read_csv(depth2wt_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_depthToWT = dtw250)
  
  trnmsv <- read_csv(transmissivity_csv, show_col_types = FALSE) %>% 
    mutate(nhd_comid = comid) %>% 
    select(nhd_comid, attr_transmissivity = trans250)
  
  comid_site_xwalk %>% 
    left_join(dtw, by = 'nhd_comid') %>% 
    left_join(trnmsv, by = 'nhd_comid') %>% 
    select(site_no, starts_with('attr_'))
}
