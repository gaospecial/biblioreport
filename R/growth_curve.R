#' Plot growth as a function of time
#'
#' @inheritParams ggplot2::ggplot
#'
#' @return
#' @export
#'
#' @examples
growth_curve <- function(data = NULL, mapping = aes()){
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}
