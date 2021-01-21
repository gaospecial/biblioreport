# fix or enhancement of [bibliometrix] packages

# remove duplications in fields
uniq_tag <- function(M, Field = "AU_CO", sep = ";"){
  value <- M[[Field]]
  new_field_name <- paste0(Field, "_NR")
  new_field_value <- unlist(lapply(strsplit(value, split = sep), function(x){
    paste(unique(x), collapse = sep)
  }))
  M[[new_field_name]] <- new_field_value
  return(M)
}
