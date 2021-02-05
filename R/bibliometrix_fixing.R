# fix or enhancement of [bibliometrix] packages

# remove duplications in fields
#' @export
uniq_tag <- function(M, Field = "AU_CO", sep = ";"){
  value <- M[[Field]]
  new_field_name <- paste0(Field, "_NR")
  new_field_value <- unlist(lapply(strsplit(value, split = sep), function(x){
    paste(unique(x), collapse = sep)
  }))
  M[[new_field_name]] <- new_field_value
  return(M)
}

#' @export
permanent_link <- function(base_url="https://doi.org/",
                           type=c("markdown","html"),
                           id=NULL,title=NULL,alt=NULL){
  type <- match.arg(type)
  if (type == "markdown") s <- paste0("[",title,"](",base_url,id," \"",alt,"\")")
  if (type == "html") s <- paste0('<a href="',base_url,id,'" alt="',alt,'">',title,'</a>')
  return(s)
}

# 提取历史引证网络中的论文
#' @export
extract_from_hist_graph <- function(M=NULL, g=NULL){
  require("stringr")
  name <- V(g)$name
  doi <- str_extract_all(name, "10\\.[0-9]+\\/\\S+")
  doi <- unlist(doi)
  M %>% filter(toupper(DI) %in% toupper(doi))
}

#' @export
authorProdOverTime2 <- function (M, k = 10, graph = TRUE) {
  # 修改 bibliometrix 的函数
  M$TC = as.numeric(M$TC)
  M$PY = as.numeric(M$PY)
  AU = names(tableTag(M, "AU"))
  k = min(k, length(AU))
  AU = AU[1:k]
  Y = as.numeric(substr(Sys.time(), 1, 4))
  if (!("DI" %in% names(M))) {
    M$DI = "NA"
  }

  list <- lapply(1:length(AU), function(i){
    ind = which(regexpr(AU[i], M$AU, fixed = TRUE) > -1)
    TCpY = M$TC[ind] / (Y - M$PY[ind] + 1)
    tibble( Author = rep(AU[i], length(ind)),
            year = M$PY[ind],
            TI = M$TI[ind],
            SO = M$SO[ind],
            DOI = M$DI[ind],
            TC = M$TC[ind],
            TCpY = TCpY )
  })
  df <- do.call("rbind", list)
  df2 <- dplyr::group_by(df, .data$Author, .data$year) %>%
    dplyr::summarise(
      freq = length(.data$year),
      TC = sum(.data$TC),
      TCpY = sum(.data$TCpY)
    )
  df2 = as.data.frame(df2)
  df2$Author = factor(df2$Author, levels = AU[k:1])
  g <- ggplot(df2, aes(year,Author)) +
    geom_point(aes(alpha = TCpY, size = freq),
               color = "dodgerblue4") +
    scale_size(range = c(2, 6)) +
    scale_alpha(range = c(0.3, 1)) +
    scale_x_continuous(breaks = function(x) seq(min(x),max(x),by = 2)) +
    guides(size = guide_legend(order = 1,
                               "N.Articles"),
           alpha = guide_legend(order = 2,
                                "TC/Year")) +
    labs(title = "Top-Authors' Production over the Time") +
    geom_line(
      aes(group = Author),
      size = 1,
      color = "firebrick",
      alpha = 0.3
    )
  df$DOI = as.character(df$DOI)
  res <- list(dfAU = df2,
              dfPapersAU = df,
              graph = g)
  if (isTRUE(graph)) {
    return(g)
  } else{
    return(res)
  }

}


# just a optimized ggplotly()
#' @export
plot.ly <- function (g, tooltip = c("text"), ...) {
  require(plotly)
  ggplotly(g, tooltip = tooltip, ...) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("sendDataToCloud", "pan2d",
                                      "select2d", "lasso2d", "toggleSpikelines",
                                      "hoverClosestCartesian", "hoverCompareCartesian")) %>%
    layout(xaxis = list(fixedrange = TRUE)) %>% layout(yaxis = list(fixedrange = TRUE))
}

# determine wheter it is part of China
#' @export
is.part_of_china <- function (x) {
  require(stringr)
  str_detect(x,
             pattern = stringr::regex("CHINA|TAIWAN|HONG KONG|MACAO",
                                      ignore_case = T))
}
