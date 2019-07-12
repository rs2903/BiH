
# Load packages -----------------------------------------------------------

list.of.packages <- c("tidyverse", "rvest", "janitor", "writexl")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')

lapply(list.of.packages, require, character.only = TRUE)

wdr <- getwd() 

# Get tables --------------------------------------------------------------
site.url <- "https://en.wikipedia.org/wiki/2018_Bosnian_general_election"

site <- site.url %>% 
  read_html() %>%
  html_nodes(".wikitable") 


# HoR table ---------------------------------------------------------------
hor18<- html_table(site[2], fill=TRUE) %>% 
  as.data.frame %>% 
  slice(-c(30:33)) %>% 
  slice(-c(1)) %>% 
  janitor::remove_empty("cols") 


#>> rename ------------------------------------------------------------------
names <- c("party", "fed_votes", "fed_perc", "fed_seats",
           "rs_votes", "rs_perc", "rs_seats",
           "total_votes", "total_perc", "total_seats",
           "change")
names(hor18) <- names

hor18<- hor18 %>%
  map_df(~stringr::str_replace_all(string=.x, ",","")) %>% 
  mutate_at(vars(starts_with("fed"), 
                 starts_with("rs"),
                 starts_with("total")), as.numeric)

hor18.1 <- hor18 %>% 
  select("party", contains("rs"), contains("fed")) %>% 
  select(-contains("perc")) %>% 
  mutate(year=2018,
         date=as.Date("2018-10-07"))

class(hor18.1$date)



hor.18.rs <- hor18.1 %>% 
  select(party, starts_with("rs"), year, date) %>% 
  mutate(entity="RS") %>% 
  rename(votes="rs_votes",
         seats="rs_seats") %>% 
  filter(votes>0)


hor.18.fed <- hor18.1 %>% 
  select(party, starts_with("fed"), year, date) %>% 
  mutate(entity="Federation") %>% 
  rename(votes="fed_votes",
         seats="fed_seats") %>% 
  filter(votes>0)


hor18 <- bind_rows(hor.18.fed, hor.18.rs) %>% 
  mutate(seats=replace_na(seats, 0))

hor18 %>% 
  group_by(entity) %>% 
  summarise(sum.seats=sum(seats))


writexl::write_xlsx(hor18, path=paste0(wdr,"/data/ElectionHOR2018.xlsx"))
  


# ___ ---------------------------------------------------------------------

# Get 2018 detailed results from Commission site --------------------------

#doesn't work; site not html?

site.url <- "http://www.izbori.ba/rezultati_izbora?resId=25&langId=4#/2/1/511/0/0/0"

site <- site.url %>% 
  read_html() %>%
  html_nodes("#electoralUnitParentDetails") 


# HoR table ---------------------------------------------------------------
hor18<- html_table(site[2], fill=TRUE) %>% 
  as.data.frame %>% 
  slice(-c(30:33)) %>% 
  slice(-c(1)) %>% 
  janitor::remove_empty("cols") 

