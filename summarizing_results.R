# Get values we list in the results section of the paper

library(targets)
library(tidyverse)

# Number of sites used in model
tar_read(p3_static_attributes) %>% 
  pull(site_no) %>% 
  unique() %>% 
  length()

# Number of site-days of SpC data
tar_read(p3_ts_sc_qualified) %>% 
  nrow()

# Median number of SpC records per site
tar_read(p3_ts_sc_qualified) %>% 
  group_by(site_no) %>% 
  tally() %>% 
  pull(n) %>% 
  median()

# Number of sites with records >= 10 years
tar_read(p3_ts_sc_qualified) %>% 
  group_by(site_no) %>% 
  tally() %>% 
  filter(n > 365*10) %>% 
  pull(site_no) %>% 
  unique() %>% 
  length()

# Number of sites per stream order
tar_read(p3_static_attributes) %>% 
  group_by(attr_streamorder) %>% 
  tally()

# Median specific conductance
medianSpC_tbl <- tar_read(p3_ts_sc_qualified) %>% 
  group_by(site_no) %>% 
  summarize(medianSpC = median(SpecCond)) %>% 
  left_join(select(tar_read(p3_static_attributes), 
                   site_no, attr_streamorder),
            by = 'site_no')

range(medianSpC_tbl$medianSpC)
median(medianSpC_tbl$medianSpC)

# Correlation of SpC and streamorder
plot(medianSpC_tbl$medianSpC, medianSpC_tbl$attr_streamorder)
cor.test(medianSpC_tbl$medianSpC, medianSpC_tbl$attr_streamorder)

# Number of episodic sites
episodic_sites <- tar_read(p4_episodic_sites) 
length(episodic_sites)

# Number of episodic sites per stream order
tar_read(p3_static_attributes) %>% 
  filter(site_no %in% episodic_sites) %>% 
  group_by(attr_streamorder) %>% 
  tally()
