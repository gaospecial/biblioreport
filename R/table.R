dt_article <- function(obj, show_columns = c("SR","link"), link = c("doi")){
  if (link == "doi") id = "DI"
  obj %>%
    dplyr::mutate(link = permanent_link(id = !! sym(id), title = TI, type = "html")) %>%
    dplyr::select({{show_columns}}) %>%
    DT::datatable(escape = FALSE,
                  rownames = FALSE)
}
