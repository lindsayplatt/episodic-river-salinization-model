compilationPlot <- function(out_file, rf_model_importance, 
                                      attribute_name_xwalk, episodicColor,
                                      pdp_data, real_attribute_values, 
                                      attribute_order,
                                      box_colors) {
  
  ##################### FIRST PLOT (importance) ##################
  rev.attribute_order <- rev(rf_model_importance$attribute)
  
  display_name_in_order <- tibble(attribute = rev.attribute_order) %>% 
    left_join(attribute_name_xwalk, by = 'attribute') %>%
    mutate(display_name = if_else(display_name == 'Watershed Road Salt (kg/km2)', 
                                  'Salt (kg/km2)', display_name)) %>% 
    pull(display_name)
  
  attribute_as_factor <- function(attr_vec, attr_order, attr_display) {
    factor(attr_vec, levels = attr_order, ordered = TRUE, 
           labels = attr_display)
  }
  
  data_to_plot <- rf_model_importance %>% 
    mutate(attr_fact = attribute_as_factor(attribute, rev.attribute_order, display_name_in_order))
  
  p_importance <- ggplot(data_to_plot, aes(x = importance, y = attr_fact)) +
    geom_point(size = 1, color = episodicColor) +
    geom_segment(aes(x = importance - importance_sd,
                     xend = importance + importance_sd,
                     yend = attr_fact), linewidth = 1,
                 color = episodicColor) +
    scale_x_continuous(breaks = c(0.05, 0.1)) +
    theme_bw(base_size = 9) +
    theme(text = element_text(color = 'grey30'),
          axis.title.y = element_blank(),
          axis.title.x = element_text(vjust = -1, size = 7),
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          axis.ticks.y = element_blank(),
          legend.title = element_text(face='bold'),
          panel.grid.major.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold'),
          plot.margin = unit(c(0.5,0,1,0.5), 'lines')) +
    xlab('Importance index')

  ##################### SECOND PLOT (partial dependence) ################## 
  attribute_order <- rf_model_importance$attribute
  
  display_name_in_order <- tibble(attribute = attribute_order) %>% 
    left_join(attribute_name_xwalk, by = 'attribute') %>%
    mutate(display_name = if_else(display_name == 'Watershed Road Salt (kg/km2)', 
                                  'Salt (kg/km2)', display_name)) %>% 
    pull(display_name)
  
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
    facet_wrap(vars(attr_fact), scales = 'free_x', nrow = 2) +
    scico::scale_color_scico_d(begin = 0, end = 0.75) +
    geom_line(aes(y = attr_partdep), color = episodicColor, linewidth = 1.5) +
    geom_rug(data=real_attribute_values_to_plot, sides='b', length = unit(0.05, "npc"),
             linewidth = 0.1) +
    scale_x_continuous(n.breaks = 4) +
    theme_bw(base_size = 9) +
    theme(#text = element_text(size=12), 
          axis.text.x = element_text(size = 7),
          axis.title.y = element_text(vjust = 1.5),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold'),
          strip.clip = "off",
          panel.spacing = unit(0.75, "lines"),
          plot.margin = unit(c(0.5,1,0,0.5), 'lines')) +
    ylab('Probability') + xlab('')

  
##################### THIRD PLOT (boxplots) ##################

  data_to_plot <- real_attribute_values %>% 
    dplyr::select(site_category_fact, where(is.numeric)) %>% {
      if('medianFlow' %in% names(.)) 
        # Log scale median flow for visual purposes 
        mutate(., medianFlow = log10(medianFlow))
      else .
    } %>%
    pivot_longer(cols = -site_category_fact, 
                 names_to = 'attribute', 
                 values_to = 'attr_val') %>% 
    mutate(attr_fact = attribute_as_factor(attribute, attribute_order, display_name_in_order))
  
  p_boxes <- ggplot(data_to_plot, aes(x = site_category_fact, y = attr_val)) +
    geom_boxplot(aes(fill = site_category_fact)) + 
    facet_wrap(vars(attr_fact), scales = 'free_y', nrow = 2) +
    theme_bw(base_size = 9) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = box_colors) +
    theme(#axis.text = element_text(size=10),
          strip.text = element_text(face = 'bold'),
          strip.background = element_rect(fill = 'white', color = 'transparent'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.position = 'right', 
          strip.clip = "off",
          panel.spacing = unit(0.75, "lines"),
          plot.margin = unit(c(0.5,1,0.5,0.5), 'lines'))
  
layout <- "
AABBB
AABBB
CCCCC
CCCCC
"

  # p = p_importance + p_pdp + p_boxes + 
  #   plot_layout(design = layout) +
  #   plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  #   theme(plot.tag = element_text(size = 8)); p
 
  top_row <- plot_grid(p_importance, p_pdp, labels = c('(a)', '(b)'), label_size =8,
                       rel_widths = c(0.9, 2))
  p = plot_grid(top_row, p_boxes, labels = c('', '(c)'), label_size = 8, ncol = 1)
  
  ggsave(out_file, p, width = 6.5, height = 5, dpi = 500)
  return(out_file)                        
}