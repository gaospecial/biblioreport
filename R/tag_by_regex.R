# 根据检索词对文献进行分类
#' Tag record by regular expression search
#'
#' @param x is the search content
#' @param pattern a list of regular expression, which can be obtained by [keywords_from()]
#' @param pattern.names is a human readable abbreviation name
#' @param sep default is ";"
#'
#' @return
#' @export the name of new column
#'
#' @examples
tag_by_regex <- function(x, pattern, pattern.names = names(pattern), sep = ";"){
  require(stringr)
  nRecord <- length(x)
  result <- vector("list", length = nRecord)
  nPattern <- length(pattern)
  for (i in 1:nRecord){
    this_record <- x[[i]]
    idx <- vector(length = nPattern)
    for (j in 1:nPattern){
      this_pattern <- pattern[[j]]
      if (!is.na(this_record) & str_detect(this_record, this_pattern)) idx[[j]] <- TRUE
    }
    result[[i]] <- paste0(pattern.names[idx], collapse = sep)
  }
  return(unlist(result))
}


# 整合多个关键词为一个关键词
keywords_from <- function(..., list = NULL, name = "primary"){
  keyword_list <- list(...)
  if (!is.null(list)) keyword_list <- c(keyword_list, list)
  result <- lapply(keyword_list, function(x){
    x[[name]]
  })
  paste0(unlist(result), collapse = "|")
}
