#' ---
#' output:
#'   md_document:
#'     pandoc_args: [
#'       '-f', 'markdown-implicit_figures',
#'       '-t', 'commonmark',
#'       --wrap=preserve
#'     ]
#' ---



#+ reprex-setup, include = FALSE
options(tidyverse.quiet = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)

#+ reprex-body
library(tidyverse)
library(pdftools)

link.ok <- "http://parlament.ba/oLaw/GetOwisDocument/?documentId=103320&data=9A4B80C1CB2A8BA6617949C64776F770&lang=bs"
link.notok <- "http://parlament.ba/oLaw/GetOwisDocument/?documentId=125266&data=BE111A5A0468C6C5A5A926FE35A37965&lang=bs"
link.notok2 <- "http://parlament.ba/oLaw/GetOwisDocument/?documentId=132385&data=9AAAAC17DF6FCD761F951F58799E63ED&lang=bs"

c(link.ok, link.notok, link.notok2) %>% 
map(., pdftools::pdf_text)

sessionInfo()



#' <sup>Created on `r Sys.Date()` by the [reprex package](https://reprex.tidyverse.org) (v`r utils::packageVersion("reprex")`)</sup>


