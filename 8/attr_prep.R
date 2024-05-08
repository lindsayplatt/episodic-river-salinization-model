#' @title Pivot downloaded NHD attributes from long to wide
#' @description The output from `prepare_nhd_attributesComid()` is in 
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
prepare_nhd_attributesComid <- function(nhd_attribute_table, comids) {
  
  nhd_attribute_table %>%
    # Pivot the NHD attributes to be columns
    pivot_wider(id_cols = nhd_comid, 
                names_from = nhd_attr_id, 
                values_from = nhd_attr_val) %>% 
    # filter by site number COMID
    filter(nhd_comid %in% comids) %>% 
    
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
    select(nhd_comid, starts_with('attr_'))
  
}

#' @title Pivot downloaded NHD attributes from long to wide
#' @description The output from `prepare_nhd_attributesComid()` is in 
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
prepare_nhd_attributesComidTOT <- function(nhd_attribute_table, comids) {
  
  nhd_attribute_table %>%
    # Pivot the NHD attributes to be columns
    pivot_wider(id_cols = nhd_comid, 
                names_from = nhd_attr_id, 
                values_from = nhd_attr_val) %>% 
    # filter by site number COMID
    filter(nhd_comid %in% comids) %>% 
    
    # Operate the following sums/means/etc by row
    rowwise() %>% 
    
    # Combine some of the land-use categories (in % catchment area)
    mutate(
      # Forested = deciduous forest + evergreen forest + mixed forest
      attr_pctForested = TOT_NLCD19_41 + TOT_NLCD19_42 + TOT_NLCD19_43,
      # Wetland = woody wetland + herbaceous wetland
      attr_pctWetland = TOT_NLCD19_90 + TOT_NLCD19_95,
      # Agriculture = pasture/hay + cropland
      attr_pctAgriculture = TOT_NLCD19_81 + TOT_NLCD19_82,
      # Developed = open (<20% impervious) + low (20-49%) + medium (50-79%) + high (80-100%)
      attr_pctDeveloped = TOT_NLCD19_21 + TOT_NLCD19_22 + TOT_NLCD19_23 + TOT_NLCD19_24) %>% 
    
    # Calculate snow (in mm) from precip total
    mutate(attr_annualSnow = TOT_PPT7100_ANN*TOT_PRSNOW/100) %>% 
    
    # Calculate average winter air temperature from monthly averages
    mutate(attr_winterAirTemp = mean(c(TOT_TAV7100_DEC, TOT_TAV7100_JAN,
                                       TOT_TAV7100_FEB, TOT_TAV7100_MAR))) %>% 
    
    # Rename the columns whose values are used as-is
    rename(any_of(c(
      attr_annualPrecip = 'TOT_PPT7100_ANN', # in mm
      attr_baseFlowInd = 'TOT_BFI', # % total flow
      attr_subsurfaceContact = 'TOT_CONTACT', # days
      attr_gwRecharge = 'TOT_RECHG', # in mm/year
      attr_pctOpenWater = 'TOT_NLCD19_11', # % catchment area
      attr_basinSlope = 'TOT_BASIN_SLOPE' # % rise
    ))) %>% 
    
    # Select only the final attributes, which are those prefixed with `attr_`
    select(nhd_comid, starts_with('attr_'))
  
}