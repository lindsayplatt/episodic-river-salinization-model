
#' @title Extract specific layers from the downloaded geopackages
#' @description Open and pull a specific layer from the saved NHD+ geopackages. 
#' Noting that sometimes a COMID does not have all available layers. 
#' 
#' @param in_files a character vector of filepaths to the geopackages downloaded
#' using `nhdplusTools::subset_nhdplus()`.
#' @param gpkg_layer a character value of either `CatchmentSP` or `NHDFlowline_Network` 
#' to indicate which of the geopackage layers to extract. Defaults to `CatchmentSP`.
#' @param crs_out the EPSG code indicating which projection to transform the 
#' output spatial features object to before returning
#' 
#' @returns an `sf` data frame with a row for each spatial feature. Should have the
#' same number of rows as `in_files` but sometimes a geopackage will not have the
#' layer and therefore will not have anything to return.
#' 
extract_nhdplus_geopackage_layer <- function(in_files, gpkg_layer = 'CatchmentSP', crs_out = 4326) {
  
  # Identify which geopackages have the layer requested
  gpkg_has_layer <- purrr::map(in_files, ~{
    gpkg_layer %in% st_layers(.x)$name
  }) %>% reduce(c) 
  
  # Print a message about how many are missing this layer
  message(sprintf('Note that %s gpkg files do not have the %s layer', 
                  sum(!gpkg_has_layer), gpkg_layer))
  
  # Extract the layer for geopackages where it exacts and return as a single
  # `sf` table where each spatial feature has been appropriately transformed.
  map(in_files[gpkg_has_layer], ~{
    st_read(.x, gpkg_layer, quiet = TRUE) %>%
      st_transform(crs = crs_out) %>% {
        if(gpkg_layer == 'NHDFlowline_Network') {
          # Now choose which columns we want from the flowlines layer (there are 
          # many but some issues with data type and binding rows later, so only
          # keep the ones that we need to avoid these potential issues).
          select(., comid, lengthkm, streamleve, streamorde, 
                 fromnode, tonode, areasqkm)
        } else {
          .
        }
      }
  }) %>%
    bind_rows() %>% {
      # CatchmentSP uses `featureid` for the comid and `NHDFlowline_Network` uses `comid`
      # Rename to the same thing here 
      if(gpkg_layer == 'CatchmentSP') {
        rename(., nhd_comid = 'featureid')
      } else {
        rename(., nhd_comid = 'comid')
      }
    }
}
