## code to prepare `deterministic-assembly` dataset goes here

library(bibliometrix)
library(stringr)

files <- list.files(path = "data-raw/deterministic-assembly/",
                    pattern = ".txt$",
                    full.names = TRUE)
deterministic_assembly <- convert2df(file = files,
                                 dbsource = "wos",
                                 format = "plaintext")
deterministic_assembly <- metaTagExtraction(deterministic_assembly, Field = "AU_CO") %>%
  uniq_tag(Field = "AU_CO") %>%
  uniq_tag(Field = "AU_UN") %>%
  filter(str_detect(SC, "MICROBIOLOGY|ENVIRONMENTAL SCIENCE"))

usethis::use_data(deterministic_assembly, overwrite = TRUE)
