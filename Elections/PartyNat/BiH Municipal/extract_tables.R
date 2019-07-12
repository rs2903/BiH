list.of.packages <- c("tidyverse","tabulizer","glue")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)

wdr <- getwd() 

path <- paste0(wdr,"/PartyNat/BiH Municipal/extract_table_test/")
filename <- "PartyVotes.pdf"
file<- glue::glue("{path}{filename}")

table <- tabulizer::extract_tables(file=file)
as.data.frame(table[[1]])
