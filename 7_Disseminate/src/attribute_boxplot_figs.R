
#' @title Create boxplots of attribute distributions by site categories
#' @description This function creates a multi-faceted figure with a facet per static
#' attribute and a boxplot per site category within each facet. Boxes are colored
#' by site category. Y axes are free to scale to each parameter. Attributes are
#' ordered by importance as found by the model.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param site_attr_data a tibble with the columns `site_no`, `site_category_fact`, 
#' and any number of columns that give static attributes (not prefixed with `attr_`)
#' @param attribute_order a vector of attributes in the order that the mini plots
#' within the figure should be ordered.
#' @param attribute_name_xwalk tibble with the columns `attribute` and `display_name`
#' @param box_colors a named vector of character strings indicating what color to 
#' give the boxes; names should match the site categories exactly
#' @param legend_position character string indicating where to put the legend, see 
#' options in `?ggplot2::theme`; defaults to `bottom`, use `none` to skip.
#' @param attribute_text_size numeric value indicating the font size to use for the attribute
#' facet labels; defaults to 12.
#' 
#' @returns a character string giving the location of the saved figure file
#' 
create_attribute_boxplots <- function(out_file, site_attr_data, attribute_order, attribute_name_xwalk, 
                                      box_colors, legend_position = 'bottom',
                                      attribute_text_size = 12) {
  
  display_name_in_order <- tibble(attribute = attribute_order) %>% 
    left_join(attribute_name_xwalk, by = 'attribute') %>% 
    pull(display_name)
  
  attribute_as_factor <- function(attr_vec, attr_order, attr_display) {
    factor(attr_vec, levels = attr_order, ordered = TRUE, 
           labels = attr_display)
  }
  
  data_to_plot <- site_attr_data %>% 
    dplyr::select(site_category_fact, where(is.numeric)) %>%
    # Log scale median flow for visual purposes
    mutate(medianFlow = log10(medianFlow)) %>% 
    pivot_longer(cols = -site_category_fact, 
                 names_to = 'attribute', 
                 values_to = 'attr_val') %>% 
    mutate(attr_fact = attribute_as_factor(attribute, attribute_order, display_name_in_order))
  
  p_boxes <- ggplot(data_to_plot, aes(x = site_category_fact, y = attr_val)) +
    geom_boxplot(aes(fill = site_category_fact)) + 
    facet_wrap(vars(attr_fact), scales = 'free_y') +
    theme_bw() +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = box_colors) +
    theme(axis.text = element_text(size=10),
          strip.text = element_text(size = attribute_text_size, face = 'bold'),
          strip.background = element_rect(fill = 'white', color = 'transparent'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.position = legend_position, 
          strip.clip = "off",
          panel.spacing = unit(0.75, "lines"),
          plot.margin = unit(c(0.5,1,0,0.5), 'lines'))
  
  ggsave(out_file, p_boxes, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}
