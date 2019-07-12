
# setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(glue)
library(janitor)
library(rvest)


wdr <- getwd()

theme_thesis <- function () { 
  theme_minimal() %+replace% 
    theme(legend.box.spacing = unit(0.1,"cm"),
          legend.box.margin = margin(0,0,0,0, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_blank(),
          legend.justification = "right",
          legend.text=element_text(size=6, color="grey30"),
          
          plot.title = element_text(size=10, face="bold", hjust=0),
          plot.subtitle = element_text(size=7, color="grey30", 
                                       margin=margin(t=3, b=6),
                                       hjust=0),
          plot.caption = element_text(size=6, color="grey30", hjust=1),
          
          strip.text.x= element_text(size = 7, color="grey30"),
          strip.text.y = element_text(size = 7, color="grey30"), #angle=0
          
          axis.title = element_blank(),
          axis.line.x = element_line(colour="grey92"),
          axis.text = element_text(size=6, color="grey30"))}

# import executive dates --------------------------------------------------

govs <- readxl::read_xlsx(paste0(wdr, "/data/BiHGovs.xlsx"),
                          sheet="Executive_Start_End") %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(start, contains("end")), as.Date) %>% 
  mutate(interval_effective=lubridate::interval(start, end_effective)) %>% 
  mutate(leaders=leaders %>% as.factor %>% fct_inorder)

glimpse(govs)
levels(govs$leaders)

# laws from 96-06 ---------------------------------------------------------
#import laws from excle file
#extract dates from house of peoples and house of representatives
#take later date as date when law was passed


sheet.list <- c("BiHLaws96-98", "BiHLaws98-00", "BiHLaws00-02", "BiHLaws02-06")

import_sheets <- function(sheet) { 

  laws <- readxl::read_xlsx(paste0(wdr, "/data/1997_2014/1997_2014_Laws.xlsx"), 
                          sheet=sheet)
}

df96_06 <- sheet.list %>% 
  set_names() %>% 
  map_dfr(., import_sheets, .id="sheet") %>% 
  janitor::clean_names()
  
df96_06<- df96_06 %>% 
  mutate_at(vars(contains("datum")), ~map_chr(., stringr::str_remove_all, " ")) %>% #removes spaces to ensure dates are properly spaced
  mutate_at(vars(contains("datum")), .funs=list(new= ~map_chr(., str_extract,"[:digit:]+.[:digit:]+.[:digit:]{4}"))) %>% 
  mutate_at(vars(contains("new")), lubridate::dmy) %>% 
  mutate(later_date=case_when(sjednica_dn_broj_datum_new > sjednica_pd_broj_datum_new ~ sjednica_dn_broj_datum_new,
                              TRUE ~ sjednica_pd_broj_datum_new)) %>% 
  select(title=zakon, date=later_date)


# get links to laws 2006-2014 ------------------------------------------------------

page_seq <- seq(1, 50,1)
mandate_seq <- c(6,7,8) #6=2006-2010 , 7=2010-2014, 8=2014-2018 (?)
category_seq <- c(1,2,4,5,6,7,8) 
#1 laws in procedure, 2 adopted laws, 4 rejected laws, 5 disputed laws, 6 suspended laws, 7 distressed laws, 8 previsouly adopted 

page_links<- expand.grid(page_seq=page_seq, mandate_seq=mandate_seq, category_seq=category_seq) %>% 
  glue_data("http://parlament.ba/Law/SearchLaws?page={page_seq}&CategoryId2={category_seq}&MandateId2={mandate_seq}") %>% 
  enframe(., name=NULL, value="link")

scrap_link <- function(link) { 
  
  pb$tick()$print()
  print(link)
  
  link %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr(.,"href") %>% 
    enframe(name=NULL) %>% 
    filter(str_detect(value, "LawDetails")) %>% 
    mutate(link_to_law=paste0("http://parlament.ba", value)) 
  
}

pb <- progress_estimated(nrow(page_links))

#scrap_link(link=page_links$link[1])

page_links2 <- page_links %>% 
  mutate(link_to_law=map(link, possibly(scrap_link, otherwise="missing", quiet = F)))

page_link2_unnest <- page_links2 %>% 
  unnest(link_to_law)


page_links$link[1] %>% 
  map(., scrap_link)


df_links_to_laws_list <- page_links$link %>% 
  set_names() %>% 
  map_df(., possibly(scrap_link, otherwise="missing", quiet = FALSE), .id="link_to_page")


df_links_to_laws <- page_links$link %>% 
  set_names() %>% 
  map_df(., possibly(scrap_link, otherwise="missing", quiet = FALSE), .id="link_to_page")

df_links_to_laws %>% 
  filter(value=="missing")


df_links_to_laws <- page_links$link %>% 
  set_names() %>% 
  map_dfr(., possibly(scrap_link, otherwise=NA), .id="link_to_page")

#df_links_to_laws_dfr_NA <- df_links_to_laws

df_links_to_laws_dfr_NA %>% 
  filter(is.na(value))

df_links_to_laws <- df_links_to_laws %>% 
  mutate(session=str_extract(link_to_page, "(?<=MandateId2=)[:digit:]") %>% as.numeric()) %>% 
  mutate(category=str_extract(link_to_page, "(?<=CategoryId2=)[:digit:]")%>% as.numeric()) %>% 
  mutate(page=str_extract(link_to_page, "(?<=page=)[:digit:]")%>% as.numeric())

df_links_to_laws %>% 
  group_by(session) %>% 
  summarise(n_obs=n()) #only 38 laws in total for 2014-2018 = wrong! get 2014-2018 from other link

df_links_to_laws <- df_links_to_laws %>% 
  filter(session!=8)


# _get law details ---------------------------------------------------------


fn_law_details <- function(link) { 
  
  pb$tick()$print()
  
  title <- link %>% 
    read_html() %>% 
    html_node("h1") %>% 
    html_text() %>% 
    data.frame(X1="title", X2=., stringsAsFactors = F)
  
  table <- link %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table(., fill=T) %>% 
    data.frame(., stringsAsFactors = F)
  
  bind_rows(title, table)
  
}

pb <- progress_estimated(nrow(df_links_to_laws))

df_law_details <- df_links_to_laws$link_to_law %>% 
  set_names() %>% 
  map_dfr(., possibly(fn_law_details, otherwise=NA), .id="link_to_law")

df_law_details_wide <- df_law_details %>% 
  mutate_at(vars(X1, X2), iconv, from="UTF-8", to="windows-1253") %>% 
  spread(key=X1, value=X2) %>% 
  rename(date_non_adoption="Datum neusvajanja",
         date_entry_procedure="Datum ulaska u proceduru",
         date_adoption_HoP="Usvojen u Domu naroda",
         publication_no="Broj sluzbenog glasnika BiH",
         date_adoption_HoR="Usvojen u Predstavničkom domu") %>% 
  mutate_at(vars(contains("date")), .funs=list(new=lubridate::dmy)) %>%  #2 failed to parse %>% 
  mutate(later_date=case_when(date_adoption_HoP_new  > date_adoption_HoR_new ~ date_adoption_HoP_new,
                              TRUE ~ date_adoption_HoR_new)) %>%  #date of conclusion
  mutate(duration_adoption=later_date-date_entry_procedure_new) 


glimpse(df_law_details)
df_law_details_wide <- df_law_details_wide %>% 
  left_join(., df_links_to_laws, by=c("link_to_law"="link_to_law"))

#only passed laws
df_law_details_wide %>% 
  filter(category %in% c(2, 8)) %>% 
  filter(!is.na(publication_no)) %>%  #only those where law was published
  filter(!is.na(duration_adoption)) %>% 
  select(duration_adoption) %>% 
  ggplot()+
  geom_bar(aes(x=duration_adoption),
           stat="count")
  
df06_14 <- df_law_details_wide %>% 
  filter(category %in% c(2, 8)) %>% #adopted #only 77 laws
  filter(!is.na(publication_no)) %>% 
  select(title, date=later_date)

glimpse(df06_14)



bind_rows(df96_06, df06_14) %>% 
  mutate(law_year=lubridate::year(date)) %>% 
  ggplot()+
  geom_bar(aes(x=law_year),
           stat="count",
           position=position_stack())

df_laws <- bind_rows(df96_06, df06_14) %>% 
  filter(!is.na(date))


# Laws 2014-2018 ----------------------------------------------------------

df_2014_2018_details <-readr::read_csv2(paste0(wdr, "/data/2014_2018/2014_2018_law_details.csv"))

df_2014_2018 <- df_2014_2018_details %>% 
  mutate(law_id=str_extract(link_to_law, "[:digit:]+")) %>% 
  filter(str_detect(value, regex("status", ignore_case = T)) &
           !str_detect(value, regex("faza", ignore_case=T))) %>% 
  mutate(house=case_when(value=="Konacni status u PSBiH" ~ "status_PSBiH",
                         value=="Status u PDPSBiH" ~ "status_PDPSBiH",
                         value=="Status u DNPSBiH" ~ "status_DNPSBiH",
                         TRUE ~ as.character(value))) %>% 
  mutate(status=case_when(value1=="Ceka na pokretanje procedure" ~ "waiting to start procedure",
                          value1=="Donesen i objavljen" ~ "delivered and published",
                          value1=="Nije razmatran" ~ "not considered",
                          value1=="Obustavljen zakonodavni postupak" ~ "legislative procedure suspended",
                          value1=="Odbijen" ~ "rejected",
                          value1=="Povucen" ~ "retired",
                          value1=="Procedura" ~ "procedure",
                          value1=="Procedura - nije preuzet" ~ "procedure not taken",
                          value1=="Usvojen" ~ "adopted",
                          TRUE ~ NA_character_)) %>% 
  filter(status %in% c("delivered and published")) %>% 
  select(-value, -value1) %>% 
  spread(key=house, value=status)

df_dates_published <- df_2014_2018_details %>% 
  filter(str_detect(value, regex("Broj i datum objavljenog dokumenta", ignore_case=T))) %>% 
  mutate(date_draft_law=str_extract(value1, regex("[:digit:]+\\.[:digit:]+\\.[:digit:]{4}"))) %>% 
  mutate(number_published=str_remove_all(value1, date_draft_law) %>% stringr::str_sub(., end=-7)) %>% 
  mutate(date_published=lubridate::dmy(date_draft_law)) 

df_2014_2018 <- df_2014_2018 %>% 
  left_join(., df_dates_published, by=c("link_to_law"="link_to_law")) %>% 
  select(title=law_id, date=date_published)



df_laws <- bind_rows(df96_06, df06_14, df_2014_2018)
skimr::skim(df_laws$date)

df_laws %>% 
  filter(is.na(date))


# merge govs with laws / check intervals ----------------------------------------------------

df_laws <- df_laws %>% 
  mutate(gov=as.character(""))

df_laws$start <- lubridate::dmy("25/12/1980")
df_laws$end <-  lubridate::dmy("26/12/1982")
df_laws$interval <- lubridate::interval(df_laws$start, df_laws$end)

govs <- govs %>% #remove empty row
  filter(!is.na(interval_effective))

for(i in 1:nrow(df_laws)) {
  df_laws$gov[i] <- as.character(govs$leaders[df_laws$date[i] %within% govs$interval_effective])
  df_laws$interval[i] <- govs$interval_effective[df_laws$date[i] %within% govs$interval_effective]
  df_laws$start[i] <- govs$start[df_laws$date[i] %within% govs$interval_effective]
  df_laws$end[i] <- govs$end_effective[df_laws$date[i] %within% govs$interval_effective]
}

df_laws <- df_laws %>% 
   mutate(gov_label=paste0(gov, "\n(", month(start), "/", stringr::str_sub(year(start), 3, 4),"-",
                           month(end), "/", stringr::str_sub(year(end), 3,4 ),")")) %>% 
  mutate(alliance=case_when(str_detect(gov, "Matic|Lagumdzija|Mikerevic") ~ "alliance",
                            TRUE ~ as.character("not alliance")))

glimpse(df_laws)



# graph -------------------------------------------------------------------


df_laws %>% 
  mutate(gov=fct_inorder(gov)) %>% 
  filter(!is.na(gov)) %>% 
  group_by(gov, gov_label, interval, alliance) %>% 
  summarise(n_laws=n()) %>% 
  mutate(interval_weeks=lubridate::time_length(interval, unit="week")) %>% 
  ungroup() %>% 
  mutate(laws_per_week=n_laws/interval_weeks) %>% 
  mutate(gov_label=fct_inorder(gov_label)) %>% 
  ggplot()+
  geom_bar(aes(x=gov_label, 
               y=laws_per_week,
               fill=alliance),
           stat="identity")+
  labs(title="Legislative output per government",
       subtitle="average number of laws passed per week",
       caption="data: Parliament.ba")+
  scale_x_discrete(expand=expand_scale(mult=c(0,0)))+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.1)),
                     breaks=seq(0, 1.4, 0.2))+
  scale_fill_manual(values=c("not alliance"="#4C566A", "alliance"="#FFCC4A"),
                    guide=guide_legend(title="")) + 
  theme_thesis()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1,
                                   size=6),
        axis.title=element_blank(),
        legend.justification = "right",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        plot.subtitle = element_text(vjust=1),
        plot.caption = element_text(hjust=1, vjust=1),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())

folder <- paste0(wdr,"/graphs/")
Rfile <- "passed_laws_per_gov.R"
graphname <- "Laws_per_week"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=8, height=6, unit="cm")
