library(tidyverse)
library(pdftools)
library(tesseract)

wdr <- getwd()


theme_thesis <- function () { 
  theme_minimal() %+replace% 
    theme(legend.box.spacing = unit(0,"cm"),
          legend.box.margin = margin(0,0,0,0, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          # plot.margin = margin(0,0,0,0, "cm"),
          legend.title = element_blank(),
          legend.text=element_text(size=6, color="grey30",
                                   margin=margin(0,0,0,0, "cm")),
          
          plot.title = element_text(size=10, face="bold", hjust=0),
          plot.subtitle = element_text(size=7, color="grey30", hjust=0,
                                       margin=margin(2.5,0,7.5,0, unit="pt")),
          plot.caption = element_text(size=6, color="grey30", hjust=1,
                                      margin=margin(0,0,0,0, unit="cm")),
          strip.text.x= element_text(size = 7, color="grey30"),
          strip.text.y = element_text(size = 7, color="grey30"), #angle=0
          
          axis.title.y = element_text(hjust=1, vjust=1, angle=90, margin=margin(0, 2.88, 0,0, unit="pt"),
                                      size=6, color="grey30"),
          axis.title.x = element_text(hjust=1, vjust=0, margin=margin(0, 2.88, 0,0, unit="pt"),
                                      size=6, color="grey30"),
          axis.text = element_text(size=6, color="grey30"))}


file.list  <- list.files(path=paste0(wdr,"/data/voting_records/"), 
                         pattern=".pdf$",
                         all.files=T,
                         full.names = T) 



# define tesseract engine -------------------------------------------------

path_bos <- paste0(wdr, "/data/tesseract_lang/")
tesseract_download("bos", datapath=paste0(wdr, "/data/tesseract_lang/"))
myengine<- tesseract(language = "bos",
                     datapath =paste0(wdr, "/data/tesseract_lang/") )
tesseract_info()


# define and apply function to create images from pdfs --------------------

pb <- dplyr::progress_estimated(length(file.list))

fn_create_imgs <- function(pdf_file) {  
  
  pb$tick()$print()
  pdftools::pdf_convert(pdf_file, 
                        format="png",
                        dpi=600) 
  }


df.images <- file.list %>%
  map(., fn_create_imgs) 
  

# safe df with name of images ---------------------------------------------
df.images %>% 
  set_names() %>% 
  map(., enframe, name=NULL) %>% 
  map_dfr(., bind_rows, .id="id") %>% 
  mutate(id2=str_extract(id, "[0-9]{6}")) %>% 
  write_csv2(., "images.csv")



# filter images -----------------------------------------------------------
image_ids  <- readr::read_csv2(paste0(wdr,"/data/voting_records/images/images.csv")) %>% 
  distinct(id2) %>% 
  mutate(id2=as.character(id2))

image_list  <- list.files(path=paste0(wdr,"/data/voting_records/images"), 
                         pattern=".png$",
                         all.files=T,
                         full.names = T)   

image_list_id<- image_ids$id2 %>% 
  set_names() %>% 
  map(., ~str_subset(image_list, .))

length(image_list_id)


fn_text_only <- function(x) {  
  
  pb$tick()$print()
  
  x %>% 
    tesseract::ocr(., engine=myengine) %>% 
    glue::glue_collapse() %>% 
    enframe(., name=NULL, value="txt_raw") %>% 
    mutate_at(vars(txt_raw), iconv, from="UTF-8", to="windows-1253")}


pb <- dplyr::progress_estimated(length(image_list_id))

df_text <- image_list_id %>% 
  map_dfr(., possibly(~fn_text_only(.), otherwise=NULL), .id="file.id") 




# write_csv2(df_text, paste0(wdr, "/data/raw_text_600dpi.csv"),
#            col_names = T,
#            append = F)



# import file -------------------------------------------------------------


df_raw <- readr::read_csv2(paste0(wdr, "/data/raw_text_600dpi.csv"))

# check for missing ids ---------------------------------------------------


output_ids<- df_raw %>% distinct(file.id)
nrow(output_ids) #778

input_ids <- image_list_id %>% 
  names() %>% 
  enframe(., name=NULL, value="file.id") %>% 
  mutate(file.id=as.numeric(file.id))
nrow(input_ids)

missing.ids <- anti_join(input_ids, output_ids)



# data wrangling ----------------------------------------------------------

df_raw <- df_raw %>%
  mutate(house= ifelse(str_detect(txt_raw, regex("Predstavni", ignore_case = T)),
                       "HoR",
                       ifelse(str_detect(txt_raw, regex("dom narod | Doma naroda", ignore_case = T)),
                              "HoP", "missing"))) %>% 
  mutate(house=as.factor(house)) %>% 
  mutate(session.no=str_extract(txt_raw, regex("\\d+(?=. Sjednic)", ignore_case=T)) %>% as.numeric()) %>% 
  mutate(session.date=str_extract(txt_raw, "[0-9]{2}/[0-9]{1,2}/20[0-9]{2}|[0-9]{2}.[0-9]{2}.20[0-9]{2}|[0-9]{4}-[0-9]{2}-[0-9]{2}|[0-9]{2}.[:alpha:]+20[1-9]{2}."))
  



df_raw %>% 
  group_by(house) %>% 
  summarise(nobs=n())

#475 HoR
#302 HoP

df <- df_raw %>% 
  filter(file.id=="121404")

df_HoR <- df_raw %>% 
  filter(house=="HoR") %>% 
  mutate(txt_split=str_split(txt_raw, "\n", simplify = F)) %>% 
  mutate(delegates.raw=map(txt_split, str_subset, regex(" ZA| PRO| PRISUTAN| SUZDRZAN|Nije glasao", ignore_case=T)) %>% 
                             map(., str_subset, regex("FBiH|RS|Federacija|Republika Srpska"))) %>% 
  mutate(delegates.no=map(delegates.raw, length)) %>% 
  mutate(entity=map(delegates.raw, str_extract, regex("FBiH|RS|Federacija|Republika Srpska")) %>% 
           map(., str_trim, side=c("both"))) %>% 
  unnest(delegates.raw) %>% 
  mutate(vote=str_extract(delegates.raw,
                       regex(" ZA| PROTIV$| nije PRISUTAN| SUZDRZAN| Nije glasao$", ignore_case=T)) 
         %>% stringr::str_to_lower()) %>% 
  mutate(vote=str_extract(delegates.raw, 
                          regex("ZA$| Nije prisutan$|PROTIV$|UKUPNO$| SUZDRZAN$|NIJE GLASAO$", 
                                     ignore_case=T)) %>% stringr::str_to_lower() %>% stringr::str_squish()) %>% 
    mutate(vote.eng=case_when(vote=="za" ~ "yes",
                            vote=="protiv" ~ "against",
                            vote=="suzdrzan" ~ "reserved",
                            vote=="nije glasao" ~ "no vote",
                            vote=="nije prisutan" ~ "not present")) %>% 
  mutate_at(vars(vote, vote.eng), as.factor) %>% 
  mutate(entity=str_extract(delegates.raw, regex("FBiH|RS| Federacija| Republika Srpska")) %>% 
           str_trim(., side=c("both"))) %>% 
  mutate(entity=case_when(entity=="Federacija" ~ "FBiH",
                          entity=="Republika Srpska" ~ "RS",
                          TRUE ~ as.character(entity))) %>%
  mutate(entity=as.factor(entity)) %>% 
  mutate(delegate.name=str_extract(delegates.raw, "[:alpha:]+[:blank:][:alpha:]+")) %>% 
  mutate(delegate.name=case_when(delegate.name=="Maja Gasal" ~ "Maja Gasla-Vrazalica",
                                 TRUE ~ as.character(delegate.name))) %>% 
  mutate(session.date=case_when(session.date=="11/20/2013" ~ "20/11/2013",  #error in sheet;103628
                                TRUE ~ as.character(session.date))) %>% 
  mutate(session.date2=lubridate::dmy(session.date)) #%>% 
  mutate(session.date2=case_when(is.na(session.date2) ~ lubridate::ymd(session.date),
                                 TRUE ~ as.Date(session.date2)))
  
#dates not properly parsed
files_wo_dates <- df_HoR %>% 
  filter(is.na(session.date2)) %>% 
  distinct(file.id)

df_files_wo_dates <- df_HoR %>% 
  filter(file.id %in% files_wo_dates$file.id)

missing_dates<- df_HoR %>% 
  select(session.date, session.date2) %>% 
  filter(is.na(session.date2))

lubridate::ymd(unique(df_files_wo_dates$session.date))
  

# > checks ----------------------------------------------------------------
not42 <- df_HoR %>% 
  group_by(file.id) %>% 
  summarise(nobs=n()) %>% 
  filter(nobs<42)  #only few not properly read
not42
# 1  112397    15
# 2  121404    40
# 3  121406    39
# 4  124062    15
# 5  128393    15

df_not42 <- df_HoR %>% 
  filter(file.id %in% not42$file.id) %>% 
  group_by(file.id) %>% 
  mutate(n_index=row_number()) %>% 
  select(file.id, n_index, txt_raw, delegates.raw, vote, vote.eng)

no_votes <- df_HoR %>% 
 filter(is.na(vote.eng)) %>% 
 count()   #102 indiviudal delegate votes are na missing
no_votes

no_votes.id <- df_HoR %>% 
  filter(is.na(vote)) %>% 
  distinct(file.id)
no_votes.id
#ids where is.na(votes)  
# 121403
# 121404
# 121405
# 121406



# harmonize names ---------------------------------------------------------
names <- df_HoR %>% 
  arrange(delegate.name) %>% 
  distinct(delegate.name) %>% 
  mutate(delegate.first=word(delegate.name, 1)) %>% 
  mutate(delegate.second=word(delegate.name, 2)) %>% 
  mutate(delegate.rump=str_sub(delegate.second, 1, 3))

names.rump <- names %>% 
  arrange(delegate.rump) %>% 
  distinct(delegate.rump)

#write_csv2(names, paste0(wdr, "/data/HoR_standardized_names.csv"))
harmonized_names <- readr::read_csv2(paste0(wdr, "/data/HoR_standardized_names.csv")) %>% 
  mutate(delegate.name=paste(delegate.first, delegate.second))

library(fuzzyjoin)

df_HoR<- df_HoR %>% 
  fuzzyjoin::stringdist_left_join(., harmonized_names, 
                  by="delegate.name",
                  max_dist=2) %>% 
  mutate(delegate.name=delegate.name.y) 

df_HoR <- df_HoR %>% 
  filter(!is.na(delegate.name)) %>% 
  select(-c(delegate.name.x, delegate.name.y))

# df_HoR %>% 
#   distinct(delegate.name.x, delegate.name.y) %>% 
#   group_by(delegate.name.y) %>% 
#   select(delegate.name.x, delegate.name.y) %>% 
#   mutate(n_obs=n()) %>% 
#   filter(n_obs > 1) %>% 
#   arrange(delegate.name.y)
  

# import parties ----------------------------------------------------------

df_party <- readr::read_csv2(paste0(wdr, "/data/2014_2018_MPs.csv")) %>% 
  janitor::remove_empty(., which=c("cols")) %>% 
  mutate(delegate.name=paste(second_name, first_name))
table(df_party$party, useNA = c("always"))

df_HoR<- df_HoR %>% 
  left_join(., df_party, by=c("delegate.name")) %>% 
  mutate(party=case_when(is.na(party)~"missing party",
                        TRUE ~ as.character(party))) #%>% 
  filter(!party=="missing party")


# > ethnicity -------------------------------------------------------------

Serb <- c("SDS", "SNSD", "PDP-NDP", "DNS")
Bosniak <- c("SDA", "SBB - Fahrudin Radoncic", "BPS - Sefer Halilovic", 
             "A – SDA – Za evropsku Bosnu i Hercegovinu – Zajedno")
Croat <- c("Koalicija HDZ BiH, HSS, HKDU BiH, HSP DR. Ante Starcevic, HSP Herceg-Bosne", 
           "HDZ 1990")
other <- c("Samostalni poslanik", "Demokratska fronta - Zeljko Komsic", 
           "SDP BiH")

df_ethnicity<- list(Serb=Serb, Bosniak=Bosniak, Croat=Croat, other=other) %>% 
  map(., as_tibble) %>% 
  map_dfr(., bind_rows, .id="ethnicity")%>% 
  rename(party=value)

df_HoR <- df_HoR %>% 
  left_join(., df_ethnicity, by=c("party")) 

table(df_HoR2$party, useNA=c("always"))

delegates_wo_parties<- df_HoR2 %>% 
  filter(is.na(party)) %>% 
  distinct(delegate.name)
  
table(df_HoR2$session.date, df_HoR2$party, useNA = c("always"))

df_HoR %>% 
  mutate(fill.c=case_when(party=="missing" ~ "red",
                          TRUE ~ "grey")) %>% 
  ggplot()+
  geom_bar(aes(x=session.date2),
           stat="count") + 
  facet_wrap(vars(party))

df_HoR %>% 
  group_by(file.id, party) %>% 
  summarise(n_obs=n()) %>% 
  ggplot()+
  geom_bar(aes(x=file.id, y=n_obs),
           stat="identity")+
  facet_wrap(vars(party))

# tile plot ---------------------------------------------------------------

tile_plot <- df_HoR %>% 
  filter(!is.na(vote.eng)) %>% 
  mutate(vote.eng=fct_drop(vote.eng)) %>% 
  filter(!is.na(ethnicity)) %>% 
  mutate(file.id=fct_reorder(as.factor(file.id), session.date2)) %>% 
  mutate(fill.c=case_when(party=="missing party" ~ "red",
                          TRUE ~ "grey")) %>% 
  filter(!party=="missing party") %>% 
  ggplot()+
  geom_tile(aes(x=file.id, y=delegate.name,
                fill=vote.eng))+
  theme_thesis()+
  labs(title="HoR: Individual voting behavior 2014-2018",
       subtitle="1 tile = 1 vote",
       caption="data:parliament.ba")+
  theme(axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.ticks.x = element_line(size=2),
        legend.position="bottom",
        legend.justification = "right",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title=element_blank(),
     #   axis.title=element_blank(),
        strip.placement = "outside")+
  scale_fill_viridis_d()+
  facet_grid(vars(party), 
             switch="y",
           #  ncol=1, 
             space="free",
             scales="free_y",
            labeller=as_labeller(function(x) stringr::str_trunc(x, 18, side=c("right"))))+
 theme(strip.text.y = element_text(angle=180,
                                    hjust=1,
                                    vjust=1))+
  guides(fill=guide_legend(reverse = T, nrow=1), color=FALSE)


plot(tile_plot)

folder <-"graphs/"
#time <- format(Sys.time(),"%Y%m%d-%H%M%S")
Rfile <- "2018_laws_tesseract_2"
graphname <-"tile_plot"
format <- ".png"
ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=16, height=15, unit="cm", dpi=300)


# bar - yes/no/present/not vote -------------------------------------------

# plot1 -------------------------------------------------------------------

df_plot1 <- df_HoR %>% 
  group_by(entity, vote.eng, .drop=F) %>% 
  summarise(n_votes=n()) %>% 
  mutate(perc_votes=n_votes/sum(n_votes, na.rm=T)) %>% 
  arrange(desc(vote.eng), .by_group=T) %>% 
  mutate(pos=cumsum(perc_votes)-perc_votes/2)
df_plot1


plot1<- df_plot1 %>% 
  ggplot()+
  geom_bar(aes(x=entity, 
               y=perc_votes, 
               color=vote.eng,
               fill=vote.eng),
           stat="identity",
           position=position_stack())+
  geom_text(data=. %>% filter(vote.eng %in% c("against","not present")), 
            aes(x=entity, y=pos,
                label=round(perc_votes*100,2)),
            size=2,
            color="grey80")+ 
  geom_text(data=. %>% filter(vote.eng %in% c("yes")), 
            aes(x=entity, y=pos,
                label=round(perc_votes*100,2)),
            size=2,
            color="black")+
  labs(title="Distribution of votes per \nentity 2014-2018",
       caption="data:parliament.ba")+
  # scale_fill_viridis_d()+
  # scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_thesis()+
  theme(axis.title=element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        panel.grid = element_blank(),
        legend.justification = "bottom")+
  scale_y_continuous(labels=scales::percent, expand=expand_scale(mult=c(0,0.02)))+
  scale_x_discrete(expand=expand_scale(mult=c(0,0.6)))

plot(plot1)

folder <-"graphs/"
#time <- format(Sys.time(),"%Y%m%d-%H%M%S")
Rfile <- "2018_laws_tesseract_2"
graphname <-"votes_entity"
format <- ".png"
ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=6, height=6, unit="cm", dpi=300)


df_HoR %>% 
  filter(is.na(vote.eng)) %>% 
  distinct(vote, vote.eng)

df_HoR %>% 
  group_by(file.id, entity, vote.eng) %>% 
  summarise(n_votes=n()) %>% 
  mutate(perc_votes=n_votes/sum(n_votes)) %>% 
  group_by(file.id) %>% 
  mutate(state_yes=sum(n_votes[vote.eng=="yes"])) %>% 
  mutate(state_casted=sum(n_votes[vote.eng=="yes" | vote.eng=="no"]))



  

# individual voting behavior ----------------------------------------------
delegates_vote <- df_HoR %>% 
  #filter(vote.eng %in% c("yes","no"))  %>% 
  group_by(delegate.name, entity, vote.eng, party, ethnicity, .drop=F) %>% 
  summarise(votes_n=n()) %>% 
    group_by(delegate.name, entity, party, ethnicity) %>% 
  mutate(votes_perc=votes_n/sum(votes_n, na.rm = T)) %>% 
    filter(!is.na(party))

delegates_vote %>% 
  filter(votes_perc==1)

delegates_vote$ethnicity

# snsd plot ---------------------------------------------------------------

delegates.vote.box <- delegates_vote %>% 
  filter(!is.na(ethnicity)) %>% 
  filter(!is.na(party)) %>% 
  mutate(party.id=case_when(party=="SNSD" ~ "SNSD",
                            TRUE ~ "other")) %>% 
  filter(vote.eng %in% c("yes","against","not present")) %>% 
  filter(!is.na(votes_perc)) %>% 
  ggplot()+
  geom_boxplot(aes(x=vote.eng, 
                   y=votes_perc,
                   weight=votes_n))+
  geom_jitter(aes(x=vote.eng,
                 y=votes_perc,
                 color=party.id,
              size=votes_n),
              width=0.2)+
  labs(title="Voting behavior of SNSD \nmembers in HoR (2014-2018)",
       subtitle="1 dot = 1 delegate\n% of all decisions casted in HoR",
       caption="data: parliament.ba, compilation by author")+
  scale_y_continuous(limits=c(0,1), labels=scales::percent)+
  scale_color_manual(values=c("SNSD"="#FFCC4A", "other"="#4C566A"))+
  scale_size_continuous(range=c(1,3))+
  theme_thesis()+
  theme(axis.title = element_blank(),
        legend.justification = "bottom",
        plot.caption=element_text(hjust=1))
  
delegates.vote.box

delegates.vote.bar <- delegates_vote %>% 
  filter(!is.na(votes_perc)) %>% 
  ggplot()+
  geom_bar(aes(x=delegate.name, y=votes_n,
               fill=vote.eng),
           stat="identity",
           position=position_stack())+
  scale_fill_viridis_d(option="D")+
  #nord::scale_fill_nord()+
  theme_minimal()+
  facet_wrap(vars(party),
             scale="free_x")
  
delegates.vote.bar


# decision level ----------------------------------------------------------

table(df_HoR$vote.eng)

decision_overall_majority <- df_HoR %>% 
  filter(vote.eng %in% c("yes", "against")) %>% 
  group_by(file.id, vote.eng, .drop=T) %>% 
  summarise(n_votes=n()) %>% 
  mutate(total_votes_casted=sum(n_votes)) %>% 
  mutate(votes_perc_casted=n_votes/total_votes_casted) %>% 
  filter(vote.eng=="yes") %>% 
  mutate(majority=case_when(votes_perc_casted > 0.5 ~ "majority",
                            TRUE ~ as.character("no majority")))

table(decision_overall_majority$majority)



decisions_entity <- df_HoR %>% 
  filter(vote.eng %in% c("yes", "against")) %>% 
  mutate(vote.eng=fct_drop(vote.eng, only = c("no vote", "not present", "reserved"))) %>% 
  group_by(file.id, entity, vote.eng, .drop=F) %>% 
  summarise(n_votes=n()) %>% 
  mutate(votes_perc_yes_entity=n_votes/sum(n_votes)) %>% 
  filter(vote.eng=="yes") %>% 
  select(file.id, entity, votes_perc_yes_entity) %>% 
  spread(key=entity, value=votes_perc_yes_entity) %>% 
  mutate(Fed_veto=case_when(FBiH<0.33 ~ "yes",
                              TRUE ~ "no")) %>% 
  mutate(RS_veto=case_when(RS<0.33 ~ "yes",
                           TRUE ~ "no")) %>% 
  mutate(veto=case_when(RS_veto=="yes"  & Fed_veto=="yes" ~ "both",
                        Fed_veto=="yes"  & RS_veto=="no" ~ "Fed",
                        Fed_veto=="no" & RS_veto=="yes" ~ "RS",
                        Fed_veto=="no" & RS_veto=="no" ~ "no veto"))
  
decisions <- decision_overall_majority %>% 
  left_join(., decisions_entity, by=c("file.id")) #3 decisions get lost
decisions$majority

devtools::install_github("liamgilbey/ggwaffle")

library(ggwaffle)

waffle_data <- waffle_iron(decisions, aes_d(group=veto), rows=10)

ggplot(waffle_data, aes(x, y, fill = group)) + 
  coord_equal() + 
  geom_waffle() + 
  labs(title="HoR: application of entity veto (2014-2018)",
       x="", y="")+
  #scale_fill_waffle() + 
  scale_fill_viridis_d()+
  theme_waffle()+
  scale_y_discrete(expand=expand_scale(mult=c(0,0)))+
  scale_x_discrete(expand=expand_scale(mult=c(0,0)))+
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank())#+
  



decisions %>% 
  select(file.id, majority, contains("veto")) %>% 
  filter(majority=="majority") %>% 
  ggplot() + 
  geom_bar(aes(x=majority, fill=veto),
           stat="count",
           #position=position_stack(),
           position="fill")


decisions.entity.jitter<- decisions %>% 
  #filter(majority=="majority") %>% 
  ggplot()+
  geom_jitter(aes(x=FBiH, y=RS,
                  color=majority),
              width=0.25,
              height=0.1)+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,1))+
  geom_vline(xintercept=0.33)+
  geom_hline(yintercept = 0.33)+
  theme_minimal()+
  coord_fixed()
decisions.entity.jitter  



# combine plots -----------------------------------------------------------

library(patchwork)
plot1+delegates.vote.box

folder <-"graphs/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
Rfile <- "2018_laws_tesseract_2"
graphname <-"combined"
format <- ".png"
ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=14, height=6, unit="cm", dpi=300)


# old ---------------------------------------------------------------------


fn_text <- function(x) {  
  
  pb$tick()$print()
  
  txt_string <- x %>% 
    tesseract::ocr(., engine=myengine) %>% 
    glue::glue_collapse() 
  
  session.no <- str_extract(txt_string, regex("\\d+(?=. Sjednic)", ignore_case=T)) #extract numerous digigs before pattern . Sjednic
  house <- ifelse(str_detect(txt_string, regex("Predstavni", ignore_case = T)),
                  "HoR",
                  ifelse(str_detect(txt_string, regex("dom narod | Doma naroda", ignore_case = T)),
                         "HoP", "missing"))
  date <- str_extract(txt_string, "[0-9]{2}/[0-9]{2}/20[0-9]{2}|[0-9]{2}.[0-9]{2}.20[0-9]{2}|[0-9]{4}-[0-9]{2}-[0-9]{2}")  #extracts date, assumes specific 2 type of format
  
  txt_vectors<- txt_string %>%
    str_split(., "\n", simplify = F) 
  
  df.delegates <- txt_vectors %>%
    map(., str_subset, regex("ZA$|PROTIV$|PRISUTAN$|SUZDRŽAN$|Nije glasao$", ignore_case=T)) %>% #only if at end of row
    #map(., str_subset, regex("Delegat", ignore_case=T)) %>% 
    unlist() %>% 
    enframe(., name=NULL, value="txt")
  
  df.delegates<- df.delegates %>% 
    mutate(vote=str_extract(txt, regex("ZA|Nije prisutan|PROTIV|UKUPNO|SUZDRŽAN|NIJE GLASAO", 
                                       ignore_case=T)) %>% stringr::str_to_lower()) %>% 
    mutate(entity=str_extract(txt, regex(" FBiH | RS |  Federacija | Republika Srpska")) %>% 
             str_trim(., side=c("both"))) %>% 
    mutate(entity=case_when(entity=="Federacija" ~ "FBiH",
                            entity=="Republika Srpska" ~ "RS",
                            TRUE ~ as.character(entity))) %>% 
    mutate(house=house) %>% 
    mutate(session.no=session.no) %>% 
    mutate(date=date) %>% 
    mutate(n.delegates=nrow(.)) %>% 
    mutate(txt_string=txt_string)
  
  df.delegates 
  
  # tibble(session.no=session.no,
  #        house=house,
  #        date=date,
  #        n.delegates=n.delegates,
  #        df.delegates=list(df.delegates),
  #        txt_vector=list(txt_vectors))
  
}

x <- image_list_id %>% 
  map_dfr(., possibly(~fn_text(.), otherwise=NULL), .id="file.id") %>% 
  mutate_at(vars(txt, txt_string), iconv, from="UTF-8", to="windows-1253")




# identification of missing ids -------------------------------------------
output_ids<- df.votes %>% distinct(file.id)
nrow(output_ids)

input_ids <- image_list_id %>% 
  names() %>% 
  enframe(., name=NULL, value="file.id") %>% 
  mutate(file.id=as.numeric(file.id))

missing.ids <- anti_join(input_ids, output_ids)

x <- missing.ids$file.id[5] %>% 
  as.character(.) %>% 
  map(., ~str_subset(image_list, .)) %>% 
  map_dfr(., ~fn_text(.))

missing.ids$file.id[1:2] %>% 
  as.character(.) 

x %>% 
  map(., ocr, engine=myengine) %>% 
  map(., glue::glue_collapse) %>% 
  map(., str_split, "\n", simplify = F) %>% 
  map(., str_subset, regex("ZA$|PROTIV$|PRISUTAN$|SUZDRŽAN$|Nije glasao$", ignore_case=T)) %>% #only if at end of row
  #map(., str_subset, regex("Delegat", ignore_case=T)) %>% 
  unlist() %>% 
  enframe(., name=NULL, value="txt")
