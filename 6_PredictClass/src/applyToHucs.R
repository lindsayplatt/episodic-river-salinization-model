#' @title Bind HUC lists 
#' @description List to datafame and write file 
#' 
#' @param huclist a list of upstream comids
#' 
#' @return a tibble of COMIDs with three columns - `nhd_comid` which includes the
#' COMID passed into the function and `nhd_comid_upstream` which are the COMIDs
#' found to be upstream of the value in `nhd_comid`. `huc2`. 
#' 
#' 
bindHucs <- function(huclist) {
  p6_upstream_full = bind_rows(huclist)
  outfile = sprintf('6_PredictClass/out/allupstreamcomids_full_huc_%s.feather',
                    p6_upstream_full$huc2[1])
  write_feather(p6_upstream_full, outfile)
  return(p6_upstream_full)
} 

#' @title Get unique upstreams IDs 
#' @description Extract unique upstream COMIDs for each huc
#' 
#' @param hucdf a dataframe of upstream comids
#' 
#' @return a vector of upstream COMIDs fore each HUC 
#' 
#' 
getUpstream <- function(hucdf) {
  unique(hucdf$nhd_comid_upstream)
} 