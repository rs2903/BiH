
# Load packages -----------------------------------------------------------

install.packages(c("htmlunitjars", "htmlunit"), repos = "https://cinc.rud.is", type="source")
list.of.packages <- c("tidyverse", "rvest", "janitor", "writexl")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')

lapply(list.of.packages, require, character.only = TRUE)

wdr <- getwd() 

# Get tables --------------------------------------------------------------

link <- "http://www.izbori.ba/rezultati_izbora_2016/?resId=13&langId=4#/8/{municip_id}/0"
municip_id <- seq(1,185,1)
df_links <- glue::glue(link) %>% 
  enframe(name=NULL, value="link")


fn_scrap <- function(link_mun) {
  
    link_mun %>% 
    read_html() %>%
    html_nodes("#rightBar > div.tableDiv > table") 
  }


fn_scrap(df_links$link[1])
//*[@id="rightBar"]/div[3]/table

#rightBar > div.tableDiv > table



