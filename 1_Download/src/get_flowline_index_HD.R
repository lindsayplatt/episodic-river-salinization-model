get_flowline_index_HD <- function (flines, points, search_radius = NULL, precision = NA, 
          max_matches = 1) {
  if (is.character(flines) && flines == "download_nhdplusv2") {
    if ((!is.null(nrow(points)) && nrow(points)) == 1 | 
        length(points) == 1) {
      search_radius <- hydroloom:::check_search_radius(search_radius, 
                                                       points)
      req <- sf::st_buffer(points, search_radius)
    }
    else {
      req <- points
    }
    flines <- sf::st_transform(align_nhdplus_names(get_nhdplus(AOI = sf::st_transform(req, 
                                                                                      4326), realization = "flowline")), sf::st_crs(points))
  }
  flines = flines %>% filter(Divergence %in% c(0,1))
  
  flines <- nhdplusTools:::check_names(flines, "get_flowline_index")
  out <- hydroloom:::index_points_to_lines(flines, points, search_radius, 
                               precision, max_matches)
  rename(out, any_of(c(id = "point_id", REACH_meas = "REACHCODE_measure")))
}
