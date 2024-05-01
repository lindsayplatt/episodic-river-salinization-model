
#' @title Identify COMIDs with 0 drainage area
#' @description Some of the COMIDs do not have drainage areas and therefore do not have 
#' catchments. We *could* add something during the `identify_upstream_comids()`
#' function in `1_Download` to only return COMIDs with an appropriate drainage
#' area, but we were getting an error there (see issue linked below), so instead
#' we will check at this step and filter out any that have 0 drainage areas. See 
#' https://github.com/DOI-USGS/nhdplusTools/issues/376#issuecomment-1960684357
#' This function identifies those COMIDs with no catchment area using the data
#' stored in the flowlines spatial features downloaded from NHD+
#' 
#' @param nhd_flowlines_sf a spatial object with flowlines and at least the 
#' columns `nhd_comid` and `areasqkm`, expects flowlines output from the function
#' `extract_nhdplus_geopackage_layer()`
#' 
#' @returns a vector of numeric COMIDs that have no drainage area
#' 
identify_nonexistent_catchments <- function(nhd_flowlines_sf) {
  nhd_flowlines_sf %>% 
    filter(areasqkm == 0) %>% 
    pull(nhd_comid)
}
