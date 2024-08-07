
#' @title Get area of a watershed given starting coordinates
#' @description Get area of a watershed given starting coordinates 
#' 
#' @param lat latitude coordinate for start of watershed
#' @param long longitude coordinate for start of watershed
#' @param huc huc-02 
#' 
#' @returns area in sq km
#' 
getWatershedArea <- function(lat, long, useUpstream, useWatersheds) {
  
  # Find starting comid
  outletPoint = sf::st_sfc(sf::st_point(c(long, lat)),
                           crs = 4326)
  
  startComid =  discover_nhdplus_id(outletPoint)
  usehuc = get_huc(AOI = outletPoint, type = 'huc02') %>% pull(huc2) %>% as.numeric()
  # useUpstream <- get(paste0('p6_huc',usehuc,'_upstream_comids_df')) # argh this doesn't work in targets
  # useWatersheds <- get(paste0('p6_huc',usehuc,'_catchment_sf')) # argh this doesn't work in targets

  # Hardcode for now which sucks 
  upstreamids = useUpstream %>% filter(nhd_comid == startComid) %>% 
    select(nhd_comid = nhd_comid_upstream)
  
  watershed = useWatersheds %>% filter(nhd_comid %in% pull(upstreamids))
  sum(watershed$areasqkm)
  
}


