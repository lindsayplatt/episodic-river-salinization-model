
#' @title Visualize attribute correlations
#' @description Check attributes for potential autocorrelation to
#' improve the model. This calculates correlation between every attribute
#' used in the model using Spearman Rank Correlation. We must use this output
#' along with our knowledge of the ecological relationships between variables
#' to assess attributes for autocorrelation.
#' 
#' @param out_file a filepath specifying where to save the image output as a PNG
#' @param site_attr_data a tibble with the columns `site_category_fact` and any
#' number of columns that give static attributes (not prefixed with `attr_`)
#' 
#' @results a character string giving the location of the saved figure file
#' 
visualize_attr_correlation <- function(out_file, site_attr_data, attribute_name_xwalk) {
  
  # First, drop units from the display names and squish any spaces
  attribute_name_xwalk <- attribute_name_xwalk %>% 
    mutate(display_name_adj = gsub('(\\/)|\\([^()]*\\)', '', display_name), # Remove units
           display_name_adj = gsub('[[:space:]]', '', display_name_adj))# Remove whitespace
    
  data_to_plot <- site_attr_data %>% 
    select(-site_category_fact) %>%
    # Rename the columns of attributes based on the crosswalk
    rename_with(~attribute_name_xwalk$display_name_adj, 
                attribute_name_xwalk$attribute)
  
  p <- ggcorr(data = data_to_plot, 
              method = c("everything", "spearman"),
              nbreaks = 5,
              name = "Correlation\ncoefficient",
              low = "darkred",
              mid = "white",
              high = "seagreen4",
              label = TRUE,
              label_alpha = TRUE,
              label_round = 2,
              label_size = 3.25, # Size of correlation values
              size = 3.5, # Size of attribute names
              legend.size = 12,
              legend.position = "bottom",
              hjust = 1, 
              nudge_x = 0.3) +
    coord_equal(clip = "off") +
    theme(plot.margin=grid::unit(c(0,0,0,0.75),"cm"))
  
  png(out_file, width = 6.25, height = 6.25, units = 'in',res = 500)
  print(p)
  dev.off()
  
  return(out_file)
}
