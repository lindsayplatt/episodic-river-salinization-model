# Targets for identifying sites that exhibit "baseflow salinization"

source('5_BaseflowSalinization/src/delineate_baseflow.R')
source('5_BaseflowSalinization/src/baseflow_qualification.R')
source('5_BaseflowSalinization/src/calculate_sc_trend.R')

p5_targets <- list(
  
  # Prepare to map over each site
  tar_target(p5_q_grp, p3_attr_q_qualified %>% 
               group_by(site_no) %>% 
               tar_group(),
             iteration = 'group'),
  
  # Calculate how much of daily flow is baseflow vs quickflow using 
  # Rumsey et al 2023 methods: `FlowScreen::bf_eckhardt()` with the
  # constant `a=0.97` as suggested in Eckhardt 2012 for perennial streams
  tar_target(p5_bfi, select(p3_static_attributes, site_no, attr_baseFlowInd)), 
  tar_target(p5_baseflow_sep, 
             delineate_baseflow(p5_q_grp, p5_bfi),
             pattern = map(p5_q_grp)),
  
  # Identify which site-days meet criteria for being a non-winter baseflow day
  # defaults to Dec, Jan, Feb, Mar as "winter" and baseflow >= 90% of total daily flow
  tar_target(p5_baseflow_days, identify_baseflow_days(p5_baseflow_sep)),
  
  # Filter SC data to only non-winter baseflow days
  tar_target(p5_sc_baseflow, filter_ts_to_baseflow_days(p3_ts_sc_qualified, p5_baseflow_days, 'SpecCond')),
  
  # Filter data so that trend is only calculated for site-seasons that meets our criteria 
  # and has enough non-winter baseflow days that justifies a trend calc.
  tar_target(p5_sc_baseflow_qualified_info, apply_baseflow_trend_criteria(p5_sc_baseflow)),
  tar_target(p5_sc_baseflow_qualified, filter_ts_to_qualified_site_seasons(p5_sc_baseflow, p5_sc_baseflow_qualified_info)),
  
  # Calculate SC trends for non-winter baseflow days
  # If a site is not in this target, we were not able to calculate the trend
  tar_target(p5_sc_baseflow_trend, calculate_sc_trend(p5_sc_baseflow_qualified, max_pval = 0.05)),
 
  # Save the trends as a log file to compare future versions
  tar_target(p5_sc_baseflow_trend_csv, {
    file_out <- '5_BaseflowSalinization/log/baseflow_trends.csv'
    write_csv(p5_sc_baseflow_trend, file_out)
    return(file_out)
  }, format = 'file')
  
)
