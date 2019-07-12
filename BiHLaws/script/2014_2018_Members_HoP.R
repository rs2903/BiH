library(tidyverse)
library(rvest)
library(glue)

wd <- getwd()

page_index <- seq(1,2,1)
links <- glue("https://www.parlament.ba/Delegate/List?page={page_index}&mandateId=8")


pb <- dplyr::progress_estimated(length(links))

fn_scrap_MPs <- function(x) {
  
  pb$tick()$print()
  
  y <- x  %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(.,trim=T, fill=T) %>% 
    map_dfr(., as_tibble)
}


df.MPs <- links %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_MPs, otherwise = NULL), .id = "seq.links")

df.MPs<- df.MPs %>% 
  rename(name=X2,
         party2=X3,
         party=X4,
         constituency=X5) %>% 
  tidyr::separate(name, c("family_name", "first_name"), sep=", ") %>% 
  mutate(party2=stringr::str_remove(party2, "Klup poslanika ")) %>% 
  mutate_at(vars(contains("name"), party), iconv, from="UTF-8", to="windows-1253") 

df.MPs<- df.MPs %>%
  select(-X1) %>% 
  mutate(first_name=stringr::str_squish(first_name)) %>% #removes space between double names
  unite(name, c("first_name","family_name"), sep=" " , remove=FALSE) %>% 
  mutate(house="House of Peoples") %>% 
  rename(entity=constituency) %>% 
  mutate(party.original=party) %>% 
  mutate(party=stringr::word(party, 1))

write_csv2(df.MPs, paste0(wd,"/data/2014_2018_Members_HoP.csv"))

