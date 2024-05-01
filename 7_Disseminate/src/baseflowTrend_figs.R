
#' @title Make a bunch of mini plots showing baseflow at every site
#' @description Plot baseflow SpC values along with median annual values and
#' indicate the trend that was calculated (through color) for every site. Creates
#' groups of facet ggplots.
#' 
#' @param ts_sc_bf a tibble of time series data for only days that qualified 
#' as baseflow days so that a trend can be calculated; has the columns 
#' `site_no`, `dateTime`, `SpecCond`.
#' @param trends_sc a tibble with the baseflow trend calculated for each site
#' with the columns `site_no` and `baseflowTrend` ("none", "positive", or "negative")
#' @param nrow numeric value indicating the number of rows of facets to plot; defaults to 5
#' @param dir character value (either `h` or `v`) to pass to `facet_wrap()` to 
#' indicate whether it should fill by row or by column; defaults to `h`.
#' 
#' @returns a list of ggplots
#' 
create_baseflow_trend_plotlist <- function(ts_sc_bf, trends_sc, nrow = 5, dir = "h") {
  # Order trends to prep order of facets
  trends_sc <- trends_sc %>% arrange(desc(baseflowTrend), site_no)
  
  ts_sc_bf_trends <- ts_sc_bf %>% 
    left_join(trends_sc, by = 'site_no') %>% 
    # Setup site number as an ordered factor to control order of facets
    mutate(site_no_ord = factor(site_no, levels = trends_sc$site_no)) %>% 
    group_by(site_no_ord) %>% 
    # Prepare each site to be shown in a specific group's set of facets
    mutate(grp_num = ceiling(cur_group_id()/25)) %>% 
    ungroup()
  
  # Calculate annual median SpC values and retain information needed for plotting
  annual_sc_bf <- ts_sc_bf_trends %>% 
    mutate(year = year(dateTime)) %>% 
    group_by(grp_num, site_no_ord, year, baseflowTrend) %>% 
    summarize(SpecCond = median(SpecCond), .groups="keep") %>%
    ungroup() %>% 
    mutate(dateTime = as.Date(sprintf('%s-06-30', year))) %>% # Add mid-year as the date to plot these
    # Split data by group for plotting
    split(.$grp_num)
  
  # Now split SC baseflow ts data by group number for plotting
  ts_sc_bf_trends <- ts_sc_bf_trends %>% split(.$grp_num)
  
  plot_baseflow_facets <- function(ts_sc_bf, annual_sc_bf) {
    ggplot(ts_sc_bf, aes(x = dateTime, y = SpecCond, color = baseflowTrend)) +
      geom_point(alpha = 0.25, shape=21, size=0.5) +
      geom_point(data = annual_sc_bf, size=1.5) +
      facet_wrap(vars(site_no_ord), scales='free', nrow=nrow, dir=dir) +
      scale_color_manual(values = c(positive = '#b45f06', none = 'grey30', negative = '#663329'),
                         name = '') +
      ylab('Specific conductance, µS/cm at 25°C') + 
      theme_bw() +
      theme(text = element_text(size=10), 
            axis.text.x = element_text(size=8, angle=20, hjust=1),
            axis.title.y = element_text(vjust = 1.5),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            strip.background = element_blank(),
            strip.text = element_text(size = 10, face = 'bold'),
            strip.clip = "off",
            plot.margin = unit(c(0.5,0.5,0,0.5), 'lines'))
  }
  
  # Now plot all site's data and save as a list object
  plotlist_out <- map2(ts_sc_bf_trends, annual_sc_bf, plot_baseflow_facets)
  return(plotlist_out)
}
