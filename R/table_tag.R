#' Tabulate elements from a Tag Field column
#'
#' It tabulates elements from a Tag Field column of a bibliographic data frame.
#'
#' @param data is a data frame
#' @param tag is tag name, which is presented in the colnames of data
#' @param sep is a field separator
#'
#' @return a sorted table
#' @export
#'
#' @examples
#' data <- data.frame(choice = c("a;b;c","a","d","a;d","c" ))
#' table_tag(data, tag = "choice")
table_tag <- function(data, tag = "AF", sep = ";"){
  if (!tag %in% colnames(data)) stop(paste0(tag, " is not presented in your data."))
  tab <- data %>%
    pull({{ tag }}) %>%
    strsplit(split = {{ sep }}) %>%
    unlist() %>%
    trimws() %>%
    table() %>%
    sort(decreasing = TRUE)
  return(tab)
}
