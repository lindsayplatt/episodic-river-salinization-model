
#' @title Download and summarize NWM streamflow
#' @description Download and calculate the median for National Water Model (NWM)
#' streamflow for a specific set of COMIDs. NWM modeled streamflow is output on
#' an hourly scale, so this code tries to minimize the amount of data downloaded
#' from the Amazon S3 bucket by only grabbing some data points per day and only
#' using data from 2010-2018 to calculate the median flow.
#' 
#' @param comids character vector of NHD COMIDs to retrieve attribute values
#' @param n_daily_pts number of hourly data points to keep per day to calculate
#' the median flow. Defaults to 4 (which means one data point per 6 hrs)
#' 
#' @returns a tibble of median streamflow with the columns `nhd_comid` and `attr_medianFlowNWM`
#' 
download_NWMv2_streamflow <- function(comids, n_daily_pts = 4) {
  
  # First, sort the COMIDs because the data returned from the NWM
  # output will always return them in numeric order by COMID/feature_id
  # and we need this order to match for when we create the output table.
  comids <- sort(comids)
  
  # Set start/end time of data available to grab (need the full range in order to 
  # appropriately set the index values). Then filter just to data from 2010-2018
  # because 9 years of hourly flow is enough to give us an appropriate median value.
  start_time <- as.POSIXct("1993-01-01 00:00:00")
  end_time <- as.POSIXct("2018-12-31 23:59:00")
  
  # Constrain data request to every nth hour after 2010
  time_seq <- seq.POSIXt(from = start_time, to = end_time, by = 'hour')
  time_index <- data.frame(time = time_seq) %>%  
    # Add row number
    mutate(index = row_number()) %>% 
    # Filter to only those data after 2010
    filter(year(time) >= 2010) %>% 
    # Filter every nth row (to keep one data point per n hours)
    filter(index %% ceiling(24/n_daily_pts) == 0) %>% 
    pull(index)
  
  # Configure the S3 client
  s3_client <- paws.storage::s3(
    config = list(
      credentials = list(anonymous = TRUE), 
      region = "us-west-2", 
      s3_force_path_style = TRUE)
  )
  
  # Declare S3 Bucket URL
  s3_address_NWMv2 <- "https://s3.amazonaws.com/noaa-nwm-retro-v2-zarr-pds"
  
  # Code to look at features available from NWM v2 output 
  # zarr_overview(s3_address, s3_client = s3_client) # All data
  # zarr_overview(sprintf('%s/feature_id', s3_address_NWMv2), s3_client = s3_client) # Feature IDs (COMIDs)
  # zarr_overview(sprintf('%s/streamflow', s3_address_NWMv2), s3_client = s3_client) # Streamflow
  
  # Read in all feature_id values and identify which ones match the COMIDs requested
  #   Shape: 2729077
  #   Chunk Shape: 2729077
  #   No. of Chunks: 1 (1)
  feature_ids <- read_zarr_array(sprintf('%s/feature_id', s3_address_NWMv2), s3_client = s3_client)
  comid_index <- which(feature_ids %in% comids)
  
  # Sometimes a COMID is not present in the feature_ids and will just get dropped
  # so we need to figure out which are like that so we can fill with an NA
  comids_missing_i <- which(!comids %in% feature_ids)
  
  # Load streamflow for specific time chunk indices across the COMIDs as an xarray
  #   Shape: 227904 x 2729077
  #   Chunk Shape: 672 x 30000
  #   No. of Chunks: 30940 (340 x 91)
  nwm_streamflow <- read_zarr_array(sprintf('%s/streamflow', s3_address_NWMv2), 
                                    s3_client = s3_client, 
                                    index = list(time_index, comid_index))
  
  # Replace `-999900` with NAs (as they are supposed to be)
  nwm_streamflow[nwm_streamflow == -999900] <- NA
  
  # Calculate the medians for each COMID
  nwm_streamflow_median <- matrixStats::colMedians(x = nwm_streamflow, na.rm = TRUE)
  
  # Return these in a nicely formatted table 
  if(length(comids_missing_i) == 0) {
    nwm_streamflow_tbl <- tibble(nhd_comid = comids,
                                 attr_medianFlowNWM = nwm_streamflow_median)
  } else {
    # Fill with an NA for those COMIDs that were missing from the NWM output data
    nwm_streamflow_tbl <- tibble(nhd_comid = c(comids[-comids_missing_i], comids[comids_missing_i]),
                                 attr_medianFlowNWM = c(nwm_streamflow_median, rep(NA, length(comids_missing_i))))
  }
  
  return(nwm_streamflow_tbl)
}
