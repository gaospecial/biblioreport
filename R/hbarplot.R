#' Horizontal barplot
#'
#' @param d is a vector, table, or two column data frame
#' @param n is a number, only top n will be showed in the plot
#' @param show is the label of bar
#' @param sort whether to sort data `d`
#' @param decreasing whether sort in decreasing order
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' hbarplot(c("a","a","b","c","c"))
hbarplot <- function(d, n=NULL, show = c("rank","name"), sort = TRUE, decreasing = TRUE){
  show <- match.arg(show)
  require("forcats")
  require("ggplot2")
  require("dplyr")
  if (is.factor(d)) d <- fct_count(d)
  if (is.vector(d)) d <- table(d)
  if (is.table(d) & sort)  d <- sort(d, decreasing = decreasing)
  if (is.table(d)) d <- as.data.frame(d)
  if (ncol(d) != 2) stop("data must have only two columns.")
  if(is.numeric(n)) d <- head(d,n)
  colnames(d) <- c("name","value")
  d <- d %>%
    mutate(no=row_number()) %>%
    mutate(no=factor(no, levels = rev(no)),
           name = fct_rev(name))
  v <- max(d$value)/2
  v_label <- (max(d$value) - min(d$value)) / 40
  p <- ggplot(d)
  if (show == "rank") p <- p + aes(no, value, label = name)
  if (show == "name") p <- p + aes(name, value, label = value)
  p + geom_col() +
    scale_y_continuous(expand = expansion(mult = c(0,.02)))+
    geom_text(aes(y = value - v_label), size = 3,
              vjust = 0.3, hjust = 1, data = function(d) d[d$value > v, ], color = "white", fontface = "bold") +
    geom_text(aes(y = value + v_label), size = 3,
              vjust = 0.5, hjust = 0, data = function(d) d[d$value <= v, ]) +
    coord_flip() +
    theme_light() +
    theme(axis.text.y = element_text(face = "bold",
                                     margin = margin(t=1,r=0,b=0,l=0,unit = "pt")))
}

#' multiple dimension hbarplot
#'
#' @param M is a bibliometerixDB object here, but can be a ordinary data frame
#' @param tags is a vector of colnames
#'
#' @return
#' @export
#'
#' @examples
multi_dimension_hbarplot <- function(M, tags = c("AU","AU_CO_NR","AU_UN_NR","J9")){
  require(cowplot)
  if (any(!tags %in% colnames(M))){
    stop(tags[!tags %in% colnames(deterministic_assembly)], " is not a column in M.")
  }
  plots <- lapply(tags, function(x){
    tab <- table_tag(M, tag = x)
    hbarplot(tab)
  })
  plot_grid(plotlist = plots, labels = "AUTO")
}
