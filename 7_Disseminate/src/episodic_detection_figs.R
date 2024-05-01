
#' @title Make a bunch of mini plots identifying SpC at every site
#' @description Plot SpC values to visualize how we identified whether a site
#' was episodic or not. Creates groups of facet ggplots.
#' 
#' @param ts_sc a tibble of time series data for specific conductance has the  
#' columns `site_no`, `dateTime`, `SpecCond`
#' @param sites_episodic a character vector giving site numbers that met criteria
#' for being an "episodic site". See targets in `4_EpisodicSalinization`. 
#' @param winter_months a numeric vector indicating which months should be 
#' considered "winter"; defaults to `c(12,1,2,3)` or Dec, Jan, Feb, and Mar.
#' @param nrow numeric value indicating the number of rows of facets to plot; defaults to 5
#' 
#' @returns a list of ggplots
#' 
create_episodic_plotlist <- function(ts_sc, sites_episodic, winter_months = c(12,1,2,3), nrow=5) {
  
  # Prepare information for ordering sites and adding horizontal lines to each plot
  site_category <- ts_sc %>% 
    mutate(is_winter = month(dateTime) %in% winter_months) %>% 
    group_by(site_no) %>% 
    summarize(SpC75 = quantile(SpecCond, probs = 0.75, na.rm=TRUE),
              .groups = 'keep') %>% 
    # Order sites based on whether or not they were episodic
    mutate(site_category = ifelse(site_no %in% sites_episodic, 'Episodic', 'Not episodic')) %>% 
    arrange(site_category, site_no) %>% 
    # Setup site number as an ordered factor to control order of facets
    mutate(site_no_ord = factor(site_no, levels = .$site_no)) %>% 
    group_by(site_no_ord) %>% 
    # Prepare each site to be shown in a specific group's set of facets
    mutate(grp_num = ceiling(cur_group_id()/25)) %>% 
    ungroup()
  
  ts_sc_category <- ts_sc %>% 
    mutate(year = year(dateTime),
           is_winter = month(dateTime) %in% winter_months) %>% 
    left_join(site_category, by = 'site_no') %>% 
    # Add a column that gives the category to change the winter color depending
    # on whether or not that site was found to be episodic
    mutate(winter_behavior = ifelse(is_winter & site_category == 'Episodic', 
                                 'Winter date (episodic site)', 
                                 ifelse(is_winter & site_category == 'Not episodic', 
                                        'Winter date (not episodic site)', 
                                        'Non-winter date'))) %>% 
    # Now split SC ts data by group number for plotting
    split(.$grp_num)
  
  # Now split SC ts category data by group number for plotting
  site_category <- site_category %>% split(.$grp_num)
  
  plot_episodic_facets <- function(ts_sc, ts_sc_info) {
    ggplot(data = ts_sc) +
      geom_path(aes(x = dateTime, y = SpecCond, color = winter_behavior, group=year), na.rm = TRUE) +
      scale_color_manual(values = c(`Winter date (episodic site)` = '#0b5394', 
                                    `Winter date (not episodic site)` = 'grey15',
                                    `Non-winter date` = 'grey70')) +
      facet_wrap(vars(site_no_ord), scales='free', nrow=nrow) +
      geom_hline(data = ts_sc_info, aes(yintercept = SpC75), linetype = 'dashed') + 
      ylab('Specific conductance, µS/cm at 25°C') + 
      theme_bw() +
      theme(text = element_text(size=10), 
            axis.text.x = element_text(size=8, angle=20, hjust=1),
            axis.title.y = element_text(vjust = 1.5),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=12),
            legend.position = "bottom",
            strip.background = element_blank(),
            strip.text = element_text(size = 10, face = 'bold'),
            strip.clip = "off",
            plot.margin = unit(c(0.5,0.5,0,0.5), 'lines'))
  }
  
  # Now plot all site's data and save as a list object
  plotlist_out <- map2(ts_sc_category, site_category, plot_episodic_facets)
  return(plotlist_out)
}
