library(rvest)
library(tidyverse)
library(glue)

site.url <- "http://parlament.ba/oLaw/GetOLawsByStatus?page=7&MandateId=4&Status=-1"

seq.page<- seq(1:16)
seq.links <- glue("http://parlament.ba/oLaw/GetOLawsByStatus?page={seq.page}&MandateId=4&Status=-1") %>% 
  enframe(name=NULL)

pb <- progress_estimated(nrow(seq.links))


fn_scrap <- function(x){

  pb$tick()$print()
  
  x %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr(.,"href") %>% 
  enframe(name=NULL) %>% 
  filter(str_detect(value, "OLawDetails")) %>% 
  mutate(links=paste0("http://parlament.ba",value)) 
  
  }

df.links <- seq.links$value %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap, otherwise=NULL), .id="seq.links")


df.contents <- df.links$links[1] %>% 
  read_html() %>% 
  html_nodes("td") %>% 
  html_text(.,trim=T) %>% 
  enframe(name=NULL) %>% 
  slice(c(1:13))

df.names<- df.links$links[1] %>% 
  read_html() %>% 
  html_nodes("th") %>% 
  html_text(.,trim=T) %>% 
  enframe(name=NULL) %>% 
  slice(c(1:13))

bind_cols(df.names, df.contents) %>% 
  mutate(chamber=case_when(row_number()>7 ~ "DP",
                           TRUE ~ "PS"))


pb <- dplyr::progress_estimated(nrow(df.links))

fn_scrap_details(df.links$links[1])

fn_scrap_details <- function(x) { 

  pb$tick()$print()
  
  
df.contents <- x %>% 
  read_html() %>% 
  html_nodes("td") %>% 
  html_text(.,trim=T) %>% 
  enframe(name=NULL) %>% 
  slice(c(1:13))

df.names<- x %>% 
  read_html() %>% 
  html_nodes("th") %>% 
  html_text(.,trim=T) %>% 
  enframe(name=NULL) %>% 
  slice(c(1:13))

bind_cols(df.names, df.contents) %>% 
  mutate(chamber=case_when(row_number()>7 ~ "DP",
                           TRUE ~ "PS"))

}

df.details.all<- df.links$links %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_details, otherwise=NULL), .id="seq.links")

