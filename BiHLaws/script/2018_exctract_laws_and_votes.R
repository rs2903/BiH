# description -------------------------------------------------------------

# scraps links to laws
# scraps info on developments pertaining to each law => basis for 2018_law_analysis
# scraps from each law development links to votes
# scraps pdfs behind each link on votes => basis for tesseract_extract_individual_votes.R


# setup -------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(glue)


wdr <- getwd() 

# scrap links for all laws ------------------------------------------------

page_seq<- seq(1:17)
page_links <- glue("http://parlament.ba/oLaw/GetOLawsByStatus?page={page_seq}&MandateId=4&Status=-1") %>% 
  enframe(name=NULL)


pb <- progress_estimated(nrow(page_links))


fn_scrap <- function(x){

  pb$tick()$print()
  
  x %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr(.,"href") %>% 
  enframe(name=NULL) %>% 
  filter(str_detect(value, "OLawDetails")) %>% 
  mutate(link_to_law=paste0("http://parlament.ba",value)) 
  
  }

df_links_to_laws <- page_links$value %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap, otherwise=NULL), .id="page_id")


write_csv(df_links_to_laws, paste0(wdr, "/data/2014_2018_links_to_laws.csv"))
df_links_to_laws <- readr::read_csv(paste0(wdr, "/data/2014_2018_links_to_laws.csv"))
nrow(df_links_to_laws) #163 laws


# law timeline ------------------------------------------------------------

df_links_to_laws <- readr::read_csv(paste0(wdr, "/data/2014_2018/2014_2018_links_to_laws.csv"))

pb <- dplyr::progress_estimated(nrow(df_links_to_laws))

fn_case.details <- function(x)  {
  pb$tick()$print()
  
  x %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(., trim = T, fill = T) %>%
    
    map(., ~ map(., as_tibble)) %>%
    map(., bind_cols) %>%
    bind_rows()
  
}

df_law_details <- df_links_to_laws$link_to_law %>%
  set_names() %>%
  map_dfr(.,  possibly(fn_case.details, otherwise = NULL), .id = "link_to_law") %>% 
  mutate_at(vars(value, value1), iconv, from="UTF-8", to="windows-1253")


write_csv2(df_law_details, paste0(wdr,"/data/2014_2018/2014_2018_law_details.csv"))

#here analysis on aggregate of outcomes
#origin of law



# get links to voting pdfs -------------------------------------------------------

df_links_to_laws <-readr::read_csv(paste0(wdr, "/data/2014_2018_links_to_laws.csv"))


fn_scrap_listing <- function(x) {
  
  pb$tick()$print()
  
  x %>% 
    read_html() %>% 
    #html_nodes("li") %>% 
    html_nodes(xpath="//a[contains(text(), 'Listing')]") %>%  #filters links based on text/name of links
    html_attr('href') %>%  #extracts links
    enframe(name=NULL) %>% 
    mutate(link_to_vote=paste0("http://parlament.ba", value))
  
}

pb <- dplyr::progress_estimated(nrow(df_links_to_laws))


df_links_to_votes <- df_links_to_laws$link_to_law  %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_listing, otherwise=NULL), .id="link_to_law")

#write_csv(df_links_to_votes, path=paste0(wdr, "/data/2014_2018_links_to_votes.csv"))


# download voting records -------------------------------------------------

df_links_to_votes <- readr::read_csv(paste0(wdr, "/data/2014_2018_links_to_votes.csv")) %>% 
  mutate(law_id=stringr::str_extract(link_to_law, "[:digit:]+")) %>% 
  mutate(vote_id=stringr::str_extract(link_to_vote, "[:digit:]+"))

length(unique(df_links_to_votes$link_to_law)) #only 151 laws; means 12 laws without votes?
length(unique(df_links_to_votes$link_to_vote)) #793 votes                                       

df_links_to_votes %>% 
  group_by(link_to_law) %>% 
  summarise(n_votes_per_law=n()) %>% 
  mutate(law_id=stringr::str_extract(link_to_law, "[:digit:]+")) %>% 
  group_by(n_votes_per_law) %>% 
  summarise(n_laws_in_group=n()) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(n_votes_per_law, -n_laws_in_group), y=n_laws_in_group),
           stat="identity")

download_destinaton <- glue("{wdr}/data/voting_records/law_id_{df_links_to_votes$law_id}_vote_id_{df_links_to_votes$vote_id}.pdf")

walk2(df_links_to_votes$link_to_vote, 
      download_destinaton, 
      download.file, mode = "wb") 

