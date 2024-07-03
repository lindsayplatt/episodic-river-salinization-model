
#' @title Use a downloaded network to identify upstream COMIDs
#' @description Rather than query the webservice using the `navigate_nldi()`
#' function, employ hydroloom functionality to query a local network of flowlines
#' with `toid` in order to identify the upstream COMIDs for a given COMID as 
#' the outlet. Similar to `identify_upstream_comids()` but doesn't hit a web 
#' service each time it is called. If a COMID is the furthest upstream, only 
#' one row will be returned and the same COMID will be in both columns.
#' 
#' @param comid_in a single character string representing the NHD COMID
#' @param flines_network a data.frame with at least one column giving the 
#' identifier and one called `toid` declaring the downstream flowline. See 
#' `?nhdplusTools::get_sorted()` for more information.
#' 
#' @return a tibble of COMIDs with three columns - `nhd_comid` which includes the
#' COMID passed into the function and `nhd_comid_upstream` which are the COMIDs
#' found to be upstream of the value in `nhd_comid`. Note that values in 
#' `nhd_comid` will repeat as necessary to align with however many 
#' `nhd_comid_upstream` values they have.
#' 
identify_upstream_comids_hy <- function(comid_in, flines_network) {
  # Using `split = TRUE` to get the terminalID
  get_sorted(flines_network, outlets = comid_in, split=TRUE) %>% 
    select(nhd_comid = terminalID, 
           nhd_comid_upstream = comid)
}
