## code to prepare `deterministic-assembly` dataset goes here

library(bibliometrix)

files <- list.files(path = "data-raw/deterministic-assembly/",
                    pattern = ".txt$",
                    full.names = TRUE)
deterministic_assembly <- bibliometrix::convert2df(file = files,
                                 dbsource = "wos",
                                 format = "plaintext")

usethis::use_data(deterministic_assembly, overwrite = TRUE, internal = TRUE)
