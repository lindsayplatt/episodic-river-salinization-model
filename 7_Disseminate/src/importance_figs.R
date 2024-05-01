
#' @title Create an plot of attribute importance
#' @description This function generates a plot of ranked attribute importance 
#' using the overall mean importance.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param rf_model_importance a tibble with attribute importance and the columns 
#' `attribute`, `site_category`, `importance`, `importance_sd`, and `attribute_grp`.
#' @param attribute_name_xwalk tibble with the columns `attribute` and `display_name`
#' @param scico_palette character string indicating what `scico` color palette to use
#' 
#' @returns a character string giving the location of the saved figure file
#'
visualize_attr_importance <- function(out_file, rf_model_importance, attribute_name_xwalk, scico_palette) {
  
  attribute_order <- rev(rf_model_importance$attribute)
  display_name_in_order <- tibble(attribute = attribute_order) %>% 
    left_join(attribute_name_xwalk, by = 'attribute') %>% 
    pull(display_name)
  
  attribute_as_factor <- function(attr_vec, attr_order, attr_display) {
    factor(attr_vec, levels = attr_order, ordered = TRUE, 
           labels = attr_display)
  }
  
  data_to_plot <- rf_model_importance %>% 
    mutate(attr_fact = attribute_as_factor(attribute, attribute_order, display_name_in_order))
  
  p_importance <- ggplot(data_to_plot, aes(x = importance, y = attr_fact,
                           color = attribute_grp)) +
    geom_point(size = 4) +
    geom_segment(aes(x = importance - importance_sd,
                     xend = importance + importance_sd,
                     yend = attr_fact), linewidth = 1) +
    scico::scale_color_scico_d(name = 'Attribute group', 
                               palette = scico_palette,
                               begin = 0.25, end = 0.75) +
    theme_bw() +
    theme(text = element_text(color = 'grey30'),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=14, vjust = -1),
          axis.text.y = element_text(size=14),
          axis.text.x = element_text(size=12),
          axis.ticks.y = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(face='bold'),
          panel.grid.major.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold', size = 12),
          plot.margin = unit(c(0.5,0,1,0.5), 'lines')) +
    xlab('Gini index of importance')
  
  ggsave(out_file, p_importance, width = 6.5, height = 6.5, dpi = 500)
  return(out_file)
}
