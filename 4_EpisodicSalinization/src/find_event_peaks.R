
#' @title Identify event peaks in the SpecCond data
#' @description This was borrowed from code by Galen Gorksi for identifying events
#' in discharge data. We are repurposing this concept for Specific Conductance 
#' values to identify winter storm events that move road salt into streams. The
#' original code written by Galen Gorski can be found at:
#' https://github.com/galengorski/EventPicker/blob/master/appCode/event_picker_functions.R
#' 
#' @param ts_data a data.frame with at least a date-time column and parameter column 
#' of values. Gaps can exist because the denominator of slope will be really large for
#' large gaps and data should not be normalized.
#' @param date_colname a character string specifying the date column name
#' @param param_colname a character string indicating the name used in the columns 
#' for the data values. In this workflow, this is likely `SpecCond`.
#' @param sb_pk_thresh a single numeric value specifying the threshold for detecting
#' a peak based on the rising limb of the peak (`sb` = "slope backward").
#' @param sf_pk_thresh a single numeric value specifying the threshold for detecting
#' a peak based on the falling limb of the peak (`sf` = "slope forward").
#' 
#' @returns a data.frame with new columns for whether a value is a peak and which
#' event it falls within. There may be fewer rows since missing or negative values
#' of the parameter were removed.
#' 
find_event_peaks <- function(ts_data, date_colname, param_colname, sb_pk_thresh = 0.000001, sf_pk_thresh = 0){
  
  ts_peaks <- ts_data %>% 
    # Convert to seconds
    mutate(Days := as.numeric(as.Date(!!as.name(date_colname)))) %>%
    # Rename the param_colname column to help code stay clean
    rename_with(~gsub(param_colname, 'PARAM', .x)) %>% 
    # Remove NAs & make sure the plotting variable is greater than zero
    filter(!is.na(PARAM)) %>% 
    # Make sure plotting variable is greater than zero
    # Calculate slope forward and backward
    mutate(slope_bkwd = (PARAM - lag(PARAM)/(Days - lag(Days))),
           slope_fwd = (lead(PARAM)-PARAM)/(lead(Days)-Days)) %>% 
    # Reverse the `PARAM` placeholder column names 
    rename_with(~gsub('PARAM', param_colname, .x)) %>% 
    # Add flag for peaks: if the slope back is greater than some threshold
    # and the slope forward is negative, flag it as a peak
    ##-default sb_pk_thresh 0.000001
    ##-default sf_pk_thresh 0
    mutate(peak_flag = slope_bkwd >= sb_pk_thresh & slope_fwd <= sf_pk_thresh) %>% 
    select(-Days, -slope_bkwd, -slope_fwd)
  
  return(ts_peaks)
}
