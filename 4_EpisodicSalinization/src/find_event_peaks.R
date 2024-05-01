
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
  
  ts_data <- ts_data %>% 
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
    mutate(peak_flag = slope_bkwd > sb_pk_thresh & slope_fwd < sf_pk_thresh) %>% 
    # Prepare dataset to have the `event_flag` added (which denotes which values
    # are a part of a specific "event" aka the rise/fall of a peak)
    mutate(event_flag = NA)
  
  # Keeping the event identification code as-is for now.
  # FUTURE IMPROVEMENT: update these nested for loops for easier readability?
  # TODO: I ACTUALLY DON'T THINK I NEED THE "EVENT" code. Just need the peaks.
  
  ##-Flag the changes in derivatives, events is the row of single site which have events
  events <- which(ts_data$peak_flag)
  ##-if there are no events return ts_data
  if(length(events) > 0){
    
    ##-within single site, make a column that will signal which observations belong to which peak
    ##-this will turn into the rising and falling limbs
    
    for(i in 1:length(events)){
      k <- events[i]
      ##-the while loop goes forward in time until the slope forward is no longer negative
      while(ts_data$slope_fwd[k] <0){
        ##-and labels that with the event number (i) and makes it positive (+)
        ts_data$event_flag[k] <- i
        k <- k +1
        
        ##-if the last row of single sites is an event peak then move on
        if(k == nrow(ts_data)){
          break
        }else{
        }
      }
      
      ##-now step backward in time for the falling limb
      ##-starting with the point directly before the peak
      j <- events[i]-1
      ##-if it's the first two data points in the site don't bother
      if(j == 1|j == 0){
        next
      }else{
        ##-as you step backwards label the days with the event number (i) and make it negative (-)
        ##-this time to indicate it is the rising limb (before the peak)
        while(ts_data$slope_bkwd[j] > 0){
          ts_data$event_flag[j] <- -1*i
          ##-label j-1 as part of the rising limb too because j-1 won't be grouped with the rising limb
          ##-if it has a negative slope on the next step going back
          ts_data$event_flag[j-1] <- -1*i
          j <- j - 1
          ##-if i is 1,2 or 3 in the data frame forget about it
          if(j == 2| j == 1| j == 0){
            break
          }else{
          }
        }
      }
    }
  }
  
  # Edit peak events to only include days where SpC was above the 75th percentile
  ts_peaks <- ts_data %>%
    mutate(peak_flag = peak_flag & event_flag > 0 & SpecCond > quantile(ts_data$SpecCond, 0.75)) %>%
    select(-Days, -slope_bkwd, -slope_fwd)
  
  return(ts_peaks)
}
