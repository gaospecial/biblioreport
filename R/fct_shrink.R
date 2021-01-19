
#' Truncate levels
#'
#' Only keep the first of these levels
#'
#' @inheritDotParams forcats::fct_other f other_level
#' @param only is the number of levels
#'
#' @return
#' @export
#'
#' @examples
#' a <- data.frame(let = letters[1:4], n = c(2,1,5,7))
#' (fct_ordered <- fct_reorder(a$let, a$n, .desc = TRUE))
#' (fct_shrinked <- fct_shrink(fct_order, only = 2))
fct_shrink <- function(f,
                       only = 9,
                       ...){
  keep <- head(levels(f), only)
  forcats::fct_other(f, keep = keep, ...)
}
