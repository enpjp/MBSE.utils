## code to prepare `xml.triples.dataset` dataset goes here

the.file.path <- fs::path("data-raw", "xml-data")

#the.file.path <- system.file("extdata","Example1.xml", package = "MBSE.utils")

xml.file <- xml.file.to.triples(the.file.path, file.number = 1,
                                xpath = "/*")

xml.triples.dataset <- xml.file$triples

usethis::use_data(xml.triples.dataset, overwrite = TRUE)

