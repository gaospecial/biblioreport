## 进行（文献）网络分析的函数


## 简化网络
## 限制 node 数目和edge.weight


#' Simplified network
#'
#'
#' @param M is a bibliometrixDB object
#' @param from start year
#' @param to  stop year
#' @param nNode 最多允许的节点数目
#' @param edge_weight_cutoff  边的阈值（小于此值的边会被去掉）
#' @param analysis 分析的类型
#' @param network 网络的类型
#' @param field 网络中的节点来源的列名
#' @param remove_keyword 一个正则表达式
#' @param ...
#'
#' @return visNetwork object
#' @export
#'
#' @name simplified_network
#'
#' @examples
#' library(bibliometrix)
#' data("garfield")
#' author_network(garfield)
simplified_network <- function(M, from = NULL, to = NULL, nNode = 30,
                               remove_keyword = NULL,
                               edge_weight_cutoff = 1,
                               analysis,
                               network,
                               field,
                               delete_isolate = TRUE,
                               graph = FALSE,
                               ...
){
  if (!field %in% colnames(M)) stop(paste0("M doesn't have ", field))

  require(bibliometrix)
  require(tibble)
  require(dplyr)
  require(igraph)
  require(RColorBrewer)
  require(visNetwork)
  M$PY <- as.numeric(M$PY)
  PY_from <- min(M$PY, na.rm = TRUE)
  PY_to   <- max(M$PY, na.rm = TRUE)
  if (is.null(from)) from <- PY_from
  if (is.null(to)) to <- PY_to
  if (from > to) stop(paste0("from is bigger than to."))

  m <- M %>% filter(PY>=from, PY <= to)
  net_mat <- biblioNetwork(m,
                           analysis = analysis,
                           network = network, sep = ";", ...)

  if(is.na(field)) stop("must specify Field tag (ID, AU, etc).")

  members <- unlist(strsplit(m[,field], split = ";")) %>%
    trimws() %>%
    table() %>%
    sort(decreasing = T) %>%
    enframe(name = "field",value = "nRecord")
  if (!is.null(remove_keyword)){
    members <- members %>%
      dplyr::filter(!stringr::str_detect(field, remove_keyword))
  }
  idx <- rownames(net_mat) %in% head(members$field,nNode)
  net_mat_s <- net_mat[idx,idx]

  net <- graph.adjacency(net_mat_s,weighted = TRUE, mode = "undirected")

  g <- net
  vertex.attributes(g)$size <- degree(g)
  g <- delete.edges(g,E(g)[edge.attributes(g)$weight < edge_weight_cutoff])
  g <- igraph::simplify(g)
  if (delete_isolate) g <- bibliometrix:::delete.isolates(g)

  if(graph == TRUE) return(g)

  # 聚类结果
  member <- membership(cluster_louvain(g)) %>%
    enframe(name = "id", value = "cluster")
  color <-  colorRampPalette(brewer.pal(8,"Paired"))(length(unique(member$cluster)))
  names(color) <- unique(member$cluster)
  member$color <- color[member$cluster]

  visData <- toVisNetworkData(g)
  visData$nodes <- visData$nodes %>% left_join(degree(g) %>% enframe(name = "id")) %>% left_join(member)
  visData$edges$value <- visData$edges$weight
  visNetwork(visData$nodes, visData$edges,physics=FALSE) %>%
    visLayout(randomSeed = 20200721) %>%
    visOptions(manipulation = FALSE,
               highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
}


#' @export
country_network <- function(M,
                            analysis = "collaboration",
                            network = "countries",
                            field = "AU_CO_NR",
                            edge_weight_cutoff = 5,
                            nNode = 20,
                            graph = FALSE,
                            ...){
  simplified_network(M,
                     analysis = analysis,
                     network = network,
                     field = field,
                     nNode =  nNode,
                     edge_weight_cutoff = edge_weight_cutoff,
                     graph = graph,
                     ...)
}


#' 简化的作者合作网络
#'
#' @inheritParams simplified_network
#'
#' @return
#' @export
#'
#' @examples
#' @name simplified_network
author_network <- function(M,
                           analysis = "collaboration",
                           network = "authors",
                           field = "AU",
                           edge_weight_cutoff = 5,
                           nNode = 200,
                           graph = FALSE,
                           ...){
  simplified_network(M,
                     analysis = analysis,
                     network = network,
                     field = field,
                     nNode =  nNode,
                     edge_weight_cutoff = edge_weight_cutoff,
                     graph = graph,
                     ...)
}


#' 高校的合作网络
#'
#' @inheritParams simplified_network
#'
#' @return
#' @export
#'
#' @examples
#' @name simplified_network
university_network <- function(M,
                               analysis = "collaboration",
                               network = "universities",
                               field = "AU_UN_NR",
                               edge_weight_cutoff = 10,
                               nNode = 30,
                               graph = FALSE,
                               ...){
  simplified_network(M,
                     analysis = analysis,
                     network = network,
                     field = field,
                     nNode =  nNode,
                     edge_weight_cutoff = edge_weight_cutoff,
                     graph = graph,
                     ...)
}

#' 关键词的共现网络
#'
#' @inheritParams simplified_network
#'
#' @return
#' @export
#'
#' @examples
#' @name simplified_network
keyword_network <- function(M,
                            nNode = 100,
                            edge_weight_cutoff = 3,
                            field = "ID",
                            analysis = "co-occurrences",
                            network = "keywords",
                            graph = FALSE,
                            ...){
  simplified_network(M=M,
                     nNode=nNode,
                     field = field,
                     edge_weight_cutoff = edge_weight_cutoff,
                     analysis=analysis,
                     network = network,
                     graph = graph,
                     ...)
}


## 网络相关的函数

#' @export
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


#' 修改 graph 对象
#'
#' @param g igraph 对象
#'
#' @return  一个新的 igraph 对象
#' @export
#'
#' @name graph_add_node
#'
#' @examples
graph_add_node_pagerank <- function(g){
  V(g)$pagerank <- page.rank(g)[["vector"]]
  return(g)
}

#' @inheritParams  graph_add_node_pagerank
#' @name graph_add_node
graph_add_node_degree <- function(g){
  V(g)$degree <- degree(g)
  return(g)
}

#' 添加节点属性
#' @export
graph_add_node_attr <- function(g, data, id = "id", cols = colnames(data)){
  # 依据 id 的对应关系将 data 中的属性加入到graph中，
  # id 是 data 中 node id 的列名, cols 是 data 中用到的列名
  # ToDO: 跳过已有的属性还是覆盖？
  g.id <- names(V(g))
  data <- as.data.frame(data)
  rownames(data) <- data[,id]
  cols <- cols[!cols %in% id]
  for (i in 1:length(cols)){
    vertex_attr(g, name =  cols[[i]]) <- data[g.id, cols[[i]]]
  }
  return(g)
}


#' 设置 node size
#' @export
graph_set_node_size <- function(g, by = "degree", scale01 = TRUE, max_size = 10){
  value <- vertex_attr(g, name = by)
  if (isTRUE(scale01)){
    value <- range01(value)
  }
  size <- (value * max_size) + 1
  V(g)$size <- size
  return(g)
}



#' @export
graph_set_node_color <- function(g, by = "year", decreasing = FALSE, scale01 = FALSE, palette_name = "YlOrRd"){
  ## 为 graph 设置节点颜色
  ## 默认按年份着色，或者其它 node 属性着色
  value <- vertex_attr(g, name = by)
  if (isTRUE(scale01)){
    value <- range01(value)
  }
  uniq_value <- sort(unique(value),decreasing = decreasing)
  my_palette <- brewer.pal(n=7,name = palette_name)

  nColor <- 100
  if (length(uniq_value) < 100 ) nColor <- length(uniq_value)
  colors <- colorRampPalette(my_palette)(nColor)
  names(colors) <- uniq_value

  V(g)$color <- colors[as.character(value)]

  return(g)
}




#' @export
graph_subgraph <- function(g, by = "degree", slice = "PY", topN = 10, ratio = 0.1){
  if( !by %in% vertex_attr_names(g)) stop(by, " is not a graph attribute.\n")
  if( !slice %in% vertex_attr_names(g)) stop(slice, " is not a graph attribute.\n")
  data <- visNetwork::toVisNetworkData(g)
  nodes <- data$nodes %>% group_by(PY) %>%
    arrange(desc(degree)) %>%
    filter(row_number() <= topN)
  induced.subgraph(g, vids = nodes$id)
}



#' @export
vis_histNet <- function(g,
                        node.title = "title",
                        node.size = "size",
                        node.color = "color",
                        edge.color = "color",
                        layout = "layout_with_fr"){
  data <- toVisNetworkData(g)

  visNetwork(nodes = data$nodes,
             edges = data$edges) %>%
    visIgraphLayout(physics = FALSE, layout = layout) %>%
    visNodes(size = node.size, color = node.color, title=node.title) %>%
    visEdges(color = edge.color) %>%
    visOptions(highlightNearest = list(enabled=TRUE,hover=FALSE)) %>%
    visExport()

}



