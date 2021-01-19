#' Find yearly growth of publications
#'
#' @param x a bibliometrixDB object
#' @param estimate_current_year set to TRUE if you want an estimation of current year
#' @param cumulative set to TRUE if you want a cumulative sum
#' @return a data frame (`yearly_growth()`) or ggplot object (`yearly_growth_curve()`)
#' @export
#' @name yearly_growth
#' @examples
#' data("garfield", package = "bibliometrix")
#'
#' d1 <- yearly_growth(garfield, estimate_current_year = TRUE)
#' yearly_growth_curve(d1)
#'
yearly_growth <- function(x,
                          estimate_current_year = FALSE,
                          na.rm = TRUE,
                          cumulative = FALSE){

  d <- x %>% dplyr::group_by(.data$PY) %>%
    dplyr::count() %>%
    dplyr::mutate_all(as.numeric())
  if (na.rm) d <- d %>% dplyr::filter(!is.na(.data$PY))
  if (estimate_current_year){
    this_year <- lubridate::year(Sys.time())
    d1 <- d %>% dplyr::filter(PY != this_year)
    d2 <-  d %>% dplyr::filter(PY == this_year)
    model <- loess(n ~ PY, d1, surface = "direct")
    d2$n <- predict(model, d2)
    d <- rbind(d1,d2)
  }
  if (cumulative){
    d <- d[order(d$PY),] %>%
      dplyr::mutate(n = cumsum(n))
  }
  return(d)
}

#' @rdname yearly_growth
yg <- yearly_growth

#' @rdname yearly_growth
yearly_growth_curve <- function(data, mapping = ggplot2::aes_string("PY", "n")){
  growth_curve(data, mapping) +
    ggplot2::labs(x = "Year",
                  y = "nRecord",
                  subtitle = "Yearly Growth of Publications")
}

#' @rdname yearly_growth
ygc <- yearly_growth_curve
