
#' @title Create a plot of partial dependence lines
#' @description This function creates a plot with facets for each attribute and 
#' shows partial dependence lines for each site category. It also adds a rug 
#' (little vertical lines at the bottom) to show the actual attribute values that
#' went into the model.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param pdp_data a data.frame of partial dependence returned from `calculate_partial_dependence()` 
#' with the columns `attribute`, `site_category`, `attr_val`, and `attr_partdep`
#' @param real_attribute_values a tibble with the columns `site_category_fact` and
#' any number of columns that give static attributes (not prefixed with `attr_`)
#' @param attribute_order a vector of attributes in the order that the mini plots
#' within the figure should be ordered.
#' @param attribute_name_xwalk tibble with the columns `attribute` and `display_name`
#' @param line_color character string indicating what color to give the line
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_partialDependence_miniPlots_figure <- function(out_file, pdp_data, real_attribute_values, 
                                                      attribute_order, attribute_name_xwalk, line_color) {
  
  display_name_in_order <- tibble(attribute = attribute_order) %>% 
    left_join(attribute_name_xwalk, by = 'attribute') %>% 
    pull(display_name)
  
  attribute_as_factor <- function(attr_vec, attr_order, attr_display) {
    factor(attr_vec, levels = attr_order, ordered = TRUE, 
           labels = attr_display)
  }
  
  # Use the actual values of the attributes to add a rug on the bottom so 
  # we can tell which patterns of dependence for given attributes are 
  # maybe just artifacts of a lack of input data.
  real_attribute_values_to_plot <- real_attribute_values %>% 
    # Reshape the data
    pivot_longer(-site_category_fact, names_to = 'attribute', values_to = 'attr_val') %>% 
    filter(attribute %in% unique(pdp_data$attribute)) %>% 
    # Log scale appropriate attributes for visual purposes
    # log_transform_to_select_attributes() %>%
    rename(site_category = site_category_fact) %>% 
    mutate(attr_fact = attribute_as_factor(attribute, attribute_order, display_name_in_order))
  # TODO: REVISIT LOGGED SCALE
  p_pdp <- pdp_data %>% 
    mutate(attr_fact = attribute_as_factor(attribute, attribute_order, display_name_in_order)) %>% 
    # Log scale appropriate attributes for visual purposes
    # log_transform_to_select_attributes() %>%
    ggplot(aes(x = attr_val)) +
    facet_wrap(vars(attr_fact), scales = 'free_x') +
    scico::scale_color_scico_d(begin = 0, end = 0.75) +
    geom_line(aes(y = attr_partdep), color = line_color, linewidth = 1.5) +
    geom_rug(data=real_attribute_values_to_plot, sides='b') +
    theme_bw() +
    theme(text = element_text(size=12), 
          axis.text = element_text(size=10),
          axis.title.y = element_text(vjust = 1.5),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = 'bold'),
          strip.clip = "off",
          panel.spacing = unit(0.75, "lines"),
          plot.margin = unit(c(0.5,1,0,0.5), 'lines')) +
    ylab('Probability') + xlab('')
  
  ggsave(out_file, p_pdp, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}
