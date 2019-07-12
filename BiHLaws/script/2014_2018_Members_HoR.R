library(tidyverse)
library(rvest)
library(glue)

wd <- getwd()

page_index <- seq(1,5,1)
links <- glue("https://www.parlament.ba/Representative/List?page={page_index}&mandateId=8")


pb <- dplyr::progress_estimated(length(links))

fn_scrap_MPs <- function(x) {
  
  pb$tick()$print()
  
  x  %>% 
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
#  mutate(name=map(., iconv, from="UTF-8", to="window-1253")) %>% 
  tidyr::separate(name, c("family_name", "first_name"), sep=", ") %>% 
  mutate(party2=stringr::str_remove(party2, "Klup poslanika ")) %>% 
  mutate_at(vars(contains("name"), party), iconv, from="UTF-8", to="windows-1253")


df.MPs<- df.MPs %>% 
  select(-X1) %>% 
  mutate(first_name=stringr::str_squish(first_name)) %>% #removes space between double names
  unite(name, c("first_name","family_name"), sep=" " , remove=FALSE) %>% 
  mutate(house="House of Representatives") %>% 
  mutate(entity=case_when(str_detect(constituency, "RS") ~ "RS",
                          str_detect(constituency, "FBiH") ~ "FBiH",
                          TRUE ~ as.character(constituency))) %>% 
  mutate(party.original=party) %>% 
  mutate(party=case_when(party.original=="Samostalni poslanik" ~ "independent",
                         str_detect(party.original, "SBB") ~ "SBB",
                         str_detect(party.original, "BPS") ~ "BPS",
                         str_detect(party.original, "fronta") ~ "DF",
                         str_detect(party.original, "Za evropsku") ~ "A-SDA",
                         str_detect(party.original, "Koalicija HDZ") ~ "HDZ Coalition",
                         TRUE ~ as.character(party.original)))


write_csv2(df.MPs, paste0(wd,"/data/2014_2018_Members_HoR.csv" ))
  