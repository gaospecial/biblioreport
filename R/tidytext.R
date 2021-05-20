#' Two word phrase
#'
#' @param df probably a bibiliometrixDB object
#' @param field title, abstract or both of them
#'
#' @return a data.frame, defining a directed network
#' @export
#'
#' @examples
two_word_phrase = function(df, field = c("TI","AB","content"),
                           log_transform = TRUE,
                           word_pattern = "^[[:alpha:]]+"){
  field = match.arg(field)
  if (!field %in% colnames(df)) stop("Field is not in data: ", field)
  data = df %>%
    unnest_ngrams(bigram, {{field}}, n = 2) %>%
    separate(bigram, c("w1","w2"), sep = " ") %>%
    filter(!w1 %in% stop_words$word,
           !w2 %in% stop_words$word,
           str_detect(w1, word_pattern),
           str_detect(w2, word_pattern)) %>%
    count(w1,w2, sort = T, name = "weight")
  if (isTRUE(log_transform)) data = mutate_at(data, "weight", log10)
  return(data)
}
