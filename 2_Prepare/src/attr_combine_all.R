
#' @title Combine all static attributes data
#' @description Using `site_no` as the identifier, this function combines 
#' all of the desired static attributes into a single table. If the 
#' attribute was not known for a particular site, there will be an NA in 
#' that column. No filtering is done to remove sites without certain 
#' attributes in this function.
#' 
#' @param ... any number of separate tibbles that contain columns of static
#' attributes and at least the `site_no` column
#' 
#' @return a complete static attributes table with at least the column
#' `site_no` and one row per site. All columns are prefixed with `attr_`
#' and the number of columns will vary based on how many attributes were
#' combined.
#' 
combine_static_attributes <- function(...) {
    purrr::reduce(
      list(...),
      dplyr::left_join, 
      by = 'site_no')
}
