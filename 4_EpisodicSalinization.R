# Targets for identifying sites that exhibit "episodic salinization"

source('4_EpisodicSalinization/src/find_event_peaks.R')
source('4_EpisodicSalinization/src/summarize_sc_peaks.R')
source('4_EpisodicSalinization/src/ts_normalization.R')

p4_targets <- list(
  
  # Normalize the specific conductance before calculating peaks
  tar_target(p4_ts_sc_norm, normalize_data_bysite(p3_ts_sc_qualified, 'SpecCond')),
  
  # Calculate event peaks for all sites 
  tar_target(p4_ts_sc_peaks, {
    p4_ts_sc_norm %>% 
      split(.$site_no) %>%
      map(~find_event_peaks(ts_data = .x,
                            date_colname = 'dateTime',
                            param_colname = 'SpecCond_norm',
                            sb_pk_thresh = 0.000005,
                            sf_pk_thresh = 0)
      ) %>% bind_rows()
  }),
  
  # Now summarize the peak information and filter to just those sites that meet 
  # our criteria for exhibiting "episodic" patterns in winter.
  tar_target(p4_ts_sc_peak_summary, 
             summarize_salt_peaks(p4_ts_sc_peaks, 
                                  num_peaks_per_year = 3, 
                                  spec_cond_buffer = 200)),
  tar_target(p4_episodic_sites, filter(p4_ts_sc_peak_summary, is_episodic)$site_no)
  
)
