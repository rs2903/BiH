# setup -------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(glue)


wdr <- getwd() 

# scrap links for all laws ------------------------------------------------



page_seq<- seq(1:17)
page_links <- glue("http://parlament.ba/oLaw/GetOLawsByStatus?page={seq.page}&MandateId=4&Status=-1") %>% 
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

df_links_to_laws <- seq.links$value %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap, otherwise=NULL), .id="page_id")


write_csv(df_links_to_laws, paste0(wdr, "/data/2014_2018_links_to_laws.csv"))
df_links_to_laws <- readr::read_csv(paste0(wdr, "/data/2014_2018_links_to_laws.csv"))
nrow(df_links_to_laws) #163 laws


# scrap law details based on link list ------------------------------------

df_links_to_laws$link_to_law[2]
pb <- dplyr::progress_estimated(nrow(df_links_to_laws))


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

bind_cols(df.names, df.contents) 

}

df_law_details<- df_links_to_laws$link_to_law %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_details, otherwise=NULL), .id="law_link")

#write_csv2(df_law_details, paste0(wdr,"/data/2014_2018_laws.csv"))


df_law_details.wide <- df_law_details %>% 
  spread(key=value, value=value1, seq.links, chamber)


df_law_details <- readr::read_csv2(paste0(wdr, "/data/2014_2018_laws.csv"))

# law timeline ------------------------------------------------------------

df_links_to_laws <- readr::read_csv2(paste0(wdr, "/data/2014_2018_law_links.csv"))

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
  map_dfr(.,  possibly(fn_case.details, otherwise = NULL), .id = "link_to_law")

#write_csv2(df_law_details, paste0(wdr,"/data/2014_2018_law_details.csv"))

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

pdf_destinaton <- glue("{wdr}/data/voting_records/law_id_{df_links_to_votes$law_id}_vote_id_{df_links_to_votes$vote_id}.pdf")

walk2(df_links_to_votes$link_to_vote, pdf_dest, download.file, mode = "wb") #downloads files


fn_extract_votes <- function(x) {
  
  pb$tick()$print()
  
  x  %>%
    tabulizer::extract_tables(method=c("stream")) %>%
    map(.,as_tibble) %>%
    keep(., map(., nrow)> 10) %>% 
    map(., ~mutate(.,link=x))
}

pb <- dplyr::progress_estimated(length(pdf_dest))

#scap download files
df.voting.results <- list.files(path=paste0(wdr,"/data/voting_records/"), 
                                pattern=".pdf$",
                                all.files=T,full.names = T)[c(1:3)] %>% 
  set_names() %>% 
  map(.,  possibly(fn_extract_votes, otherwise=NULL))

  
#scarp on the fly from web  
pb <- dplyr::progress_estimated(nrow(df.voting.results.links))

#on the fly works
df.voting.results <- df.voting.results.links$link[1:2] %>% 
  set_names() %>% 
#  map(.,  possibly(fn_extract_votes, otherwise=NULL), .id="seq.links") %>% 
  map(.,  possibly(fn_extract_votes, otherwise=NULL))

#write_csv2(df.voting.results, path=paste0(wdr,"/data/voting_results.csv"))
df.voting.results[1:4]

x <- df.voting.results.links$link[1:2] %>% 
  map(., fn_extract_votes)
df.x <- x[1[[1]]] %>% 
  map_dfr(.,bind_rows)

test <- df.voting.results[["http://parlament.ba/oLaw/GetOwisDocument/?documentId=177454&data=40A2D2573FA695DE4BEFA7D1254CFE0A&lang=bs"]]
test[1][1]


# _ -----------------------------------------------------------------------



# pdftools ----------------------------------------------------------------


# >> file list ------------------------------------------------------------

file.list  <- list.files(path=paste0(wdr,"/data/voting_records/"), 
                         pattern=".pdf$",
                         all.files=T,
                         full.names = T) 


raw_text <- file.list %>% 
  map(., pdftools::pdf_text) %>% 
  map(., stringr::str_subset, "Predstavničkog doma") %>% 
  map(., stringr::str_split, "\n", simplify=T) %>% 
  map(., stringr::str_subset, "Delegat") %>%
  map(., stringr::str_squish) %>% 
  map_dfr(., enframe, name=NULL) %>% 
  mutate(value1=str_extract_all(value, "[[:alpha:]-]+")) %>%   
  mutate(value1=map(value1, paste, collapse=" ") %>% flatten_chr()) 

df.x <- raw_text %>% 
  mutate(delegate_name=word(value1, 1, 2)) %>% 
  mutate(entity=word(value1, -2)) %>% 
  mutate(entity=str_extract(value1, " RS | FBiH ") %>% str_trim(., side=c("both"))) %>% 
  mutate(entity.pos=str_locate_all(value1, "RS|FBiH")) %>% 
  mutate(entity.pos.end=map(entity.pos, pluck, 2) %>% flatten_int) %>% 
  mutate(vote=str_sub(value1, entity.pos.end+1, end=-1L) %>% str_trim(., side=c("both"))) 

#133531.pdf dom narodna


# _ -----------------------------------------------------------------------
# differentiate HoR and HoP -----------------------------------------------

pb <- dplyr::progress_estimated(length(file.list))

fn_HoR_HoP <- function(filelist){ 

  pb$tick()$print()
  
  doc_string <- filelist  %>% 
  map(., pdftools::pdf_text) %>% 
  map(., glue_collapse)
 
  HoP <- doc_string %>% 
    map_chr(., str_detect, regex("dom narod | Doma naroda", ignore_case = T)) %>% 
    enframe(.,value="HoP", name=NULL)
  
  HoR <- doc_string %>% 
    map_chr(., str_detect, regex("Predstavni", ignore_case = T)) %>% 
    enframe(.,value="HoR", name=NULL)
 
  bind_cols(HoP, HoR)
  
  }

HoR_HoP <- file.list %>% 
  #str_subset(.,"103068.pdf|202301.pdf|103320.pdf|125266.pdf") %>% 
  #str_subset(.,"125266.pdf") %>% 
  set_names() %>% 
  map_dfr(., possibly(fn_HoR_HoP, otherwise=NULL), .id="law_id")


file.list %>% 
    str_subset(.,"103320.pdf|125266.pdf") %>%  #something wrong with text encoding? Arial and Helvetica
  map(., pdftools::pdf_text)

HoR_HoP %>% 
  gather(key=house, value=value, -law_id) %>% 
  filter(value==TRUE) %>% 
  group_by(house) %>% 
  summarise(nobs=n())

HoR_HoP %>% 
  filter(HoP==FALSE & HoR==FALSE)



#function to differentiate between HoR and HoP decisions; yields one string per decision
fn_text_raw_house <- function(files, house) {
  raw_HoP <- files  %>%
    map(., pdftools::pdf_text) %>%
    map(., stringr::str_subset, coll(house, ignore_case=T)) %>%
    map(., stringr::str_split, "\n", simplify = T)
}




x <- HoP_names.pdf %>% 
  map(., fn_house_raw, house="Dom narod")

#"Doma naroda"

raw_HoP <- file.list %>% 
  map(., pdftools::pdf_text) %>% 
  map(., stringr::str_subset, "Doma naroda") %>% 
  map(., stringr::str_split, "\n", simplify=T)
  
raw_text2 %>% 
  map2(., HoP_names$names, str_subset) %>%   #extracts names of delegates
  map(., str_subset, "ZA|PROTIV|PRISUTAN|SUZDRŽAN") #make sure not to include rows which state delegates unrealted to vote





# function with selection of house ----------------------------------------


# > fn_pdftool -----------------------------------------------------------------

pb <- dplyr::progress_estimated(length(file.list))


fn_pdftools<- function(files, house)  {
  
  pb$tick()$print()
  
clean <- files %>% 
  map(., pdftools::pdf_text) %>%  #creates character vector; each page one row
  map(., glue_collapse, sep="\n") %>%  #cobmines character rows
  map(., stringr::str_subset, coll(house, ignore_case=T)) %>% 
  map(., stringr::str_split, "\n", simplify = T) 


session.no <- clean %>% 
  map(., glue_collapse) %>%  #creates 1 character string
  str_split(., "\r") %>% 
  map(., str_subset,"sjednica") %>% 
  map(., str_extract,"[0-9]+")

vote.no <- clean %>% 
  map(., glue_collapse) %>%  #creates 1 character string
  str_split(., "\r") %>% 
  str_subset(.,"Glasanje br: |Redni broj glasanja: ") %>% 
  str_extract(.,"[0-9]+")

date_vote <- clean  %>% 
  map(., glue_collapse) %>%  #creates 1 character string
  str_extract(., "[0-9]{2}/[0-9]{2}/20[0-9]{2}|[0-9]{2}.[0-9]{2}.20[0-9]{2}")  #extracts date, assumes specific type of format

clean1 <- clean %>%
  
  #extract only rows on vote
  map(., stringr::str_subset, regex(" ZA\r| Nije prisutan\r| PROTIV\r| UKUPNO\r| SUZDRŽAN\r| NIJE GLASAO\r", 
                                    ignore_case=T))  %>%                #extracts rows which contain Delegate
  map(., stringr::str_subset, regex(" FBiH | RS |  Federacija | Republika Srpska"))
  
clean1 %>%   
  map(., stringr::str_squish) %>%
  map(., stringr::str_remove_all, "[0-9.]") %>% 
  map(., stringr::str_trim, side=c("left")) %>% 
  map_dfr(., enframe, name = NULL, value="value1") %>%
  #mutate(value1 = str_extract_all(value, "[[:alpha:]-]+")) %>%   #removes numbers => results in list
  #mutate(value1 = map(value1, paste, collapse = " ") %>% flatten_chr())  %>% #makes character vector out of list
  mutate(delegate_name = word(value1, 1, 2)) %>%
 # mutate(entity = word(value1,-2)) %>%
  mutate(entity = str_extract(value1, "RS | FBiH | Republika Srpska | Federacija ") %>% 
           str_trim(., side =c("both"))) %>%
  mutate(entity=case_when(entity=="Republika Srpska" ~ "RS",
                          entity=="Federacija" ~ "FBiH",
                          TRUE ~ as.character(entity))) %>% 
  mutate(vote=stringr::str_extract(value1, regex(" ZA| Nije prisutan| PROTIV| UKUPNO| SUZDRŽAN| NIJE GLASAO", 
                                                 ignore_case=T)) %>% 
           str_trim(., side=c("both")) %>% 
           stringr::str_to_lower()) %>% 
  mutate(date=lubridate::dmy(date_vote)) %>% 
  mutate(vote.no=vote.no) %>% 
  mutate(session.no=session.no)

}



# tests end ---------------------------------------------------------------

df.all <- file.list %>% 
  set_names() %>% 
  map_dfr(., house="Predstavn" , possibly(fn_pdftools, otherwise=NULL), .id="law_id")

df.all <- df.all %>% 
  mutate(vote.eng=case_when(vote=="za" ~ "yes",
                            vote=="protiv" ~ "no",
                            vote=="suzdržan" ~ "reserved",
                            vote=="nije glasao" ~ "no vote",
                            vote=="nje prisutan" ~ "not present")) %>% 
  mutate_at(vars(vote, vote.eng), as.factor) %>% 
  mutate_at(vars(delegate_name), iconv, from="UTF-8", to="windows-1253")


table(df.all$vote)


# _ -----------------------------------------------------------------------
# import party affiliation ------------------------------------------------


MPs <- read_csv2(paste0(wdr,"/data/2014_2018_MPs.csv")) %>% 
  unite(delegate_name, c("second_name", "first_name"), sep=" ")

df.all<- df.all %>% 
  left_join(., MPs %>% select(delegate_name, party))



# checks ------------------------------------------------------------------

delegate_party <- df.all %>% 
  distinct(delegate_name, party)

table(delegate_party$party, useNA = c("always"))

length(unique(df.all$law_id))#74 #778 files !! 704 files missing; however also include dom narod



# _ ----------------------------------------------------------------------
# ANALYSIS ----------------------------------------------------------------

df.sum <- df.all %>% 
  mutate_at(vars(law_id, entity, vote), as.factor) %>% 
  group_by(law_id, entity, vote, .drop = F) %>% 
  summarise(votes=n()) %>% 
  group_by(law_id, entity, .drop=F) %>% 
  mutate(votes.casted=sum(votes[vote=="PROTIV"|vote=="ZA"])) %>% 
  mutate(votes.rel=votes/votes.casted) %>% 
  mutate(entity.voting=case_when(votes>9 & vote=="PROTIV" ~ "entity veto",
                                 votes>18 & vote=="PROTIV" ~ "entity veto",
                                 TRUE ~ as.character("no entity veto"))) %>% 
  mutate(entity.voting2=case_when(votes.rel>0.66 & vote=="PROTIV" ~ "entity veto",
                                  votes.rel>0.66 & vote=="PROTIV" ~ "entity veto",
                                 TRUE ~ as.character("no entity veto"))) %>% 
  group_by(law_id) %>% 
  mutate(votes.total=sum(votes)) %>% 
  ungroup() %>% 
  mutate(chamber=case_when(votes.total==42 ~ "HoR",
                            votes.total==15 ~ "HoP",
                           TRUE ~ NA_character_))

  
df.x <- df.sum %>% 
  filter(vote=="PROTIV") %>% 
  arrange(law_id, entity) %>% 
  select(law_id, entity, entity.voting2) %>% 
  spread(key=entity, value=entity.voting2) %>% 
  mutate(entity.voting=case_when(FBiH=="entity veto"  & RS=="entity veto" ~ "both veto",
                                 FBiH!="entity veto" & RS=="entity veto" ~ "RS veto",
                                 FBiH=="entity veto" & RS!="entity veto" ~ "FBiH veto",
                                 TRUE ~ as.character("no veto")))

df.sum %>% 
  filter(vote=="ZA") %>% 
  select(law_id, entity, votes.rel) %>% 
  spread(key=entity, value=votes.rel) %>% 
  ggplot()+
  geom_jitter(aes(x=FBiH, y=RS))+
  hrbrthemes::theme_ipsum_rc(base_family="ArialNarrow")



df.x %>% 
  ggplot()+
  geom_bar(aes(x=entity.voting,
               fill=entity.voting),
           stat="count",
           position=position_dodge())+
  hrbrthemes::theme_ipsum_tw() +
  nord::scale_fill_nord(palette="algoma_forest")+
  scale_y_continuous(expand=expand_scale(add=c(0.4 ,0.02)))


# Yes votes ----------------------------------------------------------------
library(hrbrthemes)
library(ggbeeswarm)
library(ggridges)

theme_set(theme_minimal())


# jitter graph ------------------------------------------------------------


delegate.voting <- df.all %>% 
  group_by(entity, party, delegate_name, vote, .drop=F) %>% 
  summarise(nobs=n()) %>%
  mutate(total.votes=sum(nobs)) %>% 
  mutate(perc=nobs/total.votes*100)

delegate.voting %>% 
  filter(entity=="FBiH" & vote=="ZA") %>% 
  arrange(perc)



delegate.voting %>% 
  filter(vote=="ZA") %>% 
  ggplot()+
  geom_boxplot(aes(x=entity, y=perc,
                  fill=entity))+
  # geom_beeswarm(aes(x=entity, y=perc),
  #               color="firebrick",
  #               size=3)+
  
  geom_jitter(aes(x=entity, y=perc),
              width=0.03)+
  labs(subtitle="Each dot represents a member of HoR")+
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position=c(0.95,1),
        legend.direction = "horizontal",
        legend.title = element_blank())+
  nord::scale_fill_nord(palette="algoma_forest")+
  scale_color_manual(guide=FALSE)
  


skimr::skim(df.all$vote)
table(df.all$vote, df.all$entity, useNA = "always")

# density chart -----------------------------------------------------------


  df.all %>% 
    group_by(entity, delegate_name, vote) %>% 
    summarise(nobs=n()) %>%
    mutate(total.votes=sum(nobs)) %>% 
    mutate(perc=nobs/total.votes*100) %>% 
    filter(vote=="ZA") %>% 
    ggplot()+
    geom_density_ridges(aes(x=perc,
                            y=entity,
                            group=entity,
                            fill=entity))+
    scale_x_continuous(limits=c(0,100))+
    nord::scale_fill_nord(palette="silver_mine")
  
  
  

# bar chart voting behavior each representative ---------------------------


df.all %>% 
    select(delegate_name, entity) %>% 
    distinct() %>% 
    count(entity)
#15 RS; possibly change of delegate
#49 FBiH delegates!
  
delegaes <- df.all %>% 
  distinct(delegate_name, entity) %>% 
  arrange(entity, delegate_name)


df.all %>% 
  group_by(entity, party, delegate_name, vote.eng) %>% 
  summarise(votes.cat=n()) %>% 
  ggplot()+
  geom_bar(aes(x=delegate_name, y=votes.cat,
               fill=vote.eng),
           stat="identity",
           position="fill")+
  #facet_wrap(vars(entity), scales="free_x", shrink=T)+
  facet_grid(~entity, space="free", scales="free_x")+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.text.x=element_text(angle=90, hjust=1))


df.all %>% 
  group_by(entity, party, delegate_name, vote.eng) %>% 
  summarise(votes.cat=n()) %>% 
  ggplot()+
  geom_bar(aes(x=delegate_name, y=votes.cat,
               fill=vote.eng),
           stat="identity",
           position="fill")+
  #facet_wrap(vars(entity), scales="free_x", shrink=T)+
  facet_grid(~party, space="free", scales="free_x")+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# tile graph --------------------------------------------------------------

df.all %>% 
  mutate(id=str_extract(law_id, "[0-9]+.pdf")) %>% 
  ggplot()+
  geom_tile(aes(x=reorder(id, date), y=delegate_name,
                fill=vote.eng))+
  facet_wrap(vars(entity), scale="free_y",
             shrink = T)+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x=element_text(angle=90))+
  scale_fill_viridis_d()


votes.check <- df.all %>% 
  group_by(law_id, entity) %>% 
  summarise(n.delegates=n())
  



# __ ----------------------------------------------------------------------
# OLD ---------------------------------------------------------------------


fn_pdftools_old<- function(file, house)  {
  pb$tick()$print()
  
  clean <- file %>%
    map(., pdftools::pdf_text) %>%  #creates character vector; each page one row
    map(., glue_collapse, sep="\n") %>%  #cobmines character rows
    map(., stringr::str_subset, coll(house, ignore_case=T)) %>% 
    map(., stringr::str_split, "\n", simplify = T) 
  
  date_vote <- clean  %>% 
    glue_collapse() %>%  #creates 1 character string
    str_extract(., "[0-9]{2}/[0-9]{2}/20[0-9]{2}|[0-9]{2}.[0-9]{2}.20[0-9]{2}")  #extracts date, assumes specific type of format
  
  clean %>%
    #map(., stringr::str_subset, "Delegate") %>%                 #extracts rows which contain Delegate
    ###=> map(., stringr::str_subset, coll("ZA|PROTIV|SUZDRŽAN|NIJE PRISUTAN|UKUPNO", ignore_case=T)) %>%                 #extracts rows which contain Delegate
    
    map(., stringr::str_squish) %>%
    map_dfr(., enframe, name = NULL) %>%
    mutate(value1 = str_extract_all(value, "[[:alpha:]-]+")) %>%   #removes numbers => results in list
    mutate(value1 = map(value1, paste, collapse = " ") %>% flatten_chr())  %>% #makes character vector out of list
    mutate(delegate_name = word(value1, 1, 2)) %>%
    mutate(entity = word(value1,-2)) %>%
    mutate(entity = str_extract(value1, "RS | FBiH ") %>% str_trim(., side =
                                                                     c("both"))) %>%
    mutate(entity.pos = str_locate_all(value1, "RS|FBiH")) %>%    #calculates position of RS or FBiHH
    mutate(entity.pos.end = map(entity.pos, pluck, 2) %>% flatten_int) %>%        #extracts end of location position
    mutate(vote = str_sub(value1, entity.pos.end + 1, end = -1L) %>% str_trim(., side =
                                                                                c("both"))) %>%       #end of location position is start for extraction subsequent words
    mutate(date=date_vote)
  
}

