#' Plot growth as a function of time
#'
#' @inheritParams ggplot2::ggplot
#'
#' @return
#' @export
#'
#' @examples
#' data <- data.frame(year = 2001:2020, n = rpois(20, 100))
#' growth_curve(data, aes(year, n))
growth_curve <- function(data = NULL, mapping = aes()){
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}
