#  Load pacakges ----------------------------------------------------------

list.of.packages <- c("tidyverse","scales","glue","grid","gridExtra","kableExtra",
                      "formattable","sparkline","janitor","skimr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)


ggpreview <- function (..., device = "png") {spark
  fname <- tempfile(fileext = paste0(".", device))
  ggplot2::ggsave(filename = fname, device = device, ...)
  system2("open", fname)
  invisible(NULL)
}

wdr <- getwd() 


# import data -------------------------------------------------------------


NIRL.imp <- readxl::read_xlsx(paste0(wdr,"/data/NIRL/NIRL_election.xlsx"))

BIH.imp <- readxl::read_xlsx(paste0(wdr, "/data/BiH_HoR_consolidated.xlsx")) %>% 
      mutate(ethnicity=case_when(party.standard=="Ekonomski Blok HDU - Za Boljitak" ~ "C",
                                 str_detect(party.standard, fixed("(A-SDA)")) ~ "B",
                                 str_detect(party.standard, fixed("(PDA)")) ~ "B",
                                 str_detect(party.standard, fixed("(BPS)")) ~ "B",
                                 str_detect(party.standard, fixed("Sloga")) ~ "S",
                                 
                                 str_detect(party.standard, "BOSS") ~ "B",
                                 party.standard=="Independent Bloc" ~ "B",
              TRUE ~ as.character(ethnicity)))


#http://www.parties-and-elections.eu/bosnia-herzegovina.html
names(NIRL.imp)

NIRL <- NIRL.imp %>% 
  select(-party3) %>% 
  rename(party=party2,
         ethnicity=designation) %>% 
  mutate(year=as.numeric(year))


BIH <- BIH.imp %>% 
  select(-party.original, -date, -entity) %>% 
  rename(party=party.standard) %>% 
  mutate(election="House of Representatives") %>% 
  group_by(party, year, ethnicity, election) %>% 
  summarise(votes=sum(votes),
            seats=sum(seats))


Croat <- BIH.imp %>% 
  filter(ethnicity=="C") %>% 
  distinct(party.standard, entity, year, seats) %>% 
  filter(seats>0) %>% 
  mutate(party.collapse=case_when(str_detect(party.standard, "HDZ 1990") ~ "HDZ 1990",
                                  str_detect(party.standard, "HDZ") ~ "HDZ",
                                  str_detect(party.standard, "Nova Hrvatska") ~ "NHI",
                                  TRUE ~  as.character(party.standard)))  
  
Serb <- BIH.imp %>% 
  filter(ethnicity=="S") %>% 
  filter(seats>0) %>% 
  distinct(party.standard, entity, year, seats) %>% 
  mutate(party.collapse=case_when(str_detect(party.standard, "SNSD") ~ "SNSD",
                                  str_detect(party.standard, fixed("(SDS)")) ~ "SDS",
                                  str_detect(party.standard, fixed("(SRS)")) ~ "SRS",
                                  str_detect(party.standard, fixed("(DNZ)")) ~ "DNZ",
                                  str_detect(party.standard, fixed("(PDP)")) ~ "PDP",
                                  str_detect(party.standard, fixed("(RS)")) ~ "RS",
                                  str_detect(party.standard, fixed("(SNS)")) ~ "SNS",
                                  str_detect(party.standard, fixed("(DNS)")) ~ "DNS",
                                  str_detect(party.standard, fixed("Koalicija - PDP, SNS")) ~ "PDP",
                                  str_detect(party.standard, fixed("SDS–NDP–NS–SRS")) ~ "SDS",
                                  TRUE ~ as.character(party.standard)))


Bosniak <- BIH.imp %>% 
  filter(ethnicity=="B") %>% 
  filter(seats>0) %>% 
  distinct(party.standard, entity, year, seats) %>% 
  mutate(party.collapse=case_when(str_detect(party.standard, fixed("(SDA)")) ~ "SDA",
                                  str_detect(party.standard, fixed("(SzBiH)")) ~ "SzBiH",
                                  str_detect(party.standard, fixed("(SBB)")) ~ "SBB",
                                  str_detect(party.standard, fixed("(A-SDA)")) ~ "A-SDA",
                                  TRUE ~ as.character(party.standard)))

Multi <- BIH.imp %>% 
  filter(ethnicity=="M") %>% 
  filter(seats>0) %>% 
  distinct(entity, seats, year, party.standard) %>% 
  mutate(party.collapse=party.standard)

df.collapse <- bind_rows(Bosniak, Serb, Croat, Multi) %>% 
  distinct(year, party.standard, party.collapse)

df.collapse$party.collapse

BIH <- BIH %>% left_join(., df.collapse, by=c("party"="party.standard","year"))
NROW(BIH)


df <- bind_rows("NIRL"=NIRL, "BIH"=BIH, .id="country") %>%   #naming dfs allows to get names of dfs into .id
        mutate(party.collapse=case_when(country=="NIRL" ~ party,
              TRUE ~ as.character(party.collapse)))


## BIH same number of obs in BIH and df; BIH includes different results for RS and FED
df %>% 
  filter(country=="BIH") %>% 
  NROW() #211
NROW(BIH) #211

  



# theme -------------------------------------------------------------------

theme_thesis <- function () { 
  theme_minimal() %+replace% 
    theme(legend.box.spacing = unit(0.1,"cm"),
          legend.box.margin = margin(0,0,0,0, "cm"),
          legend.margin = margin(0,0,0,0, "cm"),
          legend.title = element_blank(),
          legend.text=element_text(size=6, color="grey30"),
          
          plot.title = element_text(size=10, face="bold", hjust=0),
          plot.subtitle = element_text(size=7, color="grey30", hjust=0,
                                       margin=margin(2.5,0,7.5,0, unit="pt")),
          plot.caption = element_text(size=6, color="grey30", hjust=1,
                                      margin=margin(5,0,0,0, unit="pt")),
          
          strip.text.x= element_text(size = 7, color="grey30"),
          strip.text.y = element_text(size = 7, color="grey30"), #angle=0
          
          axis.title.y = element_text(hjust=1, vjust=1, angle=90, margin=margin(0, 2.88, 0,0, unit="pt"),
                                      size=6, color="grey30"),
          axis.title.x = element_text(hjust=1, vjust=0, margin=margin(0, 2.88, 0,0, unit="pt"),
                                      size=6, color="grey30"),
          axis.text = element_text(size=6, color="grey30"))}



# add short party names ---------------------------------------------------

df <- df %>% 
  mutate(party.short=str_extract(party, "\\([A-Za-z]+\\)")) %>% 
  mutate(party.short=str_extract(party.short, "[A-Za-z]+")) %>% 
  mutate(party.short=case_when(is.na(party.short) ~ party,
                               TRUE ~ party.short)) %>% 
  mutate(party.short=case_when(str_detect(party.short, "BOSS - Bosanska Stranka") ~ "BOSS",
                                          TRUE ~ party.short)) %>% 
  mutate(party.short=str_replace_all(party.short, "Koalicija - ", ""))




# order factors -----------------------------------------------------------

df$ethnicity <- forcats::fct_relevel(df$ethnicity, "B","S","C","M", "unionist","nationalist","other")
df$ethnicity <- forcats::fct_collapse(df$ethnicity, nk=c("nk","unknown"))
levels(df$ethnicity)
df$ethnicity <- forcats::fct_recode(df$ethnicity, Bosniaks="B",Serbs="S",Croats="C",Unionists="unionist",
                                     Nationalists="nationalist", Multiethnic="M", Others="other")

ethn.color=c("Bosniaks"="green","Croats"="steelblue","Serbs"="firebrick","Multiethnic"="grey80",
             "Unionists"="orange","Nationalists"="darkgreen", "Others"="grey80")




# color definitions -------------------------------------------------------

country.colors=c("BIH"="#104E8B","NIRL"="#008B00")



# share of segments -------------------------------------------------------


# __size of electoral segments VOTES --------------------------------------------

df1 <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  filter(seats>0) %>% 
  group_by(country, election, year, ethnicity) %>% 
  summarise(sum.votes=sum(votes)) %>% 
  mutate(rel.votes=sum.votes/sum(sum.votes)) %>% 
  filter(!ethnicity=="nk") 
  
df2 <- df1 %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=T) %>% 
  mutate(rel.votes=lag(rel.votes))

df.x <- bind_rows(df1, df2, .id="source") %>% 
  ungroup() %>% 
  arrange(country, ethnicity, year, desc(source)) #source important


ggplot(df.x, aes(x=year, y=rel.votes))+
   geom_ribbon(data=df, aes(x=year, 
                            ymin=0,
                            ymax=rel.votes,
                   fill=ethnicity))+
  #geom_step()+
  scale_fill_manual(values=ethn.color)+
  labs(title="Inter-segment stability: size of vote share per segment",
       subtitle="% of votes casted in legislative elections; only parties which gained seats in Assembbly included;")+
  theme_thesis()+
  theme(legend.position="none",
        axis.title.y=element_blank())+
  scale_x_continuous(breaks=seq(1995, 2015, 5),
                     labels=substr(seq(1995, 2015, 5), 3,4))+
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     expand=expand_scale(mult=c(0,0.01)),
                     breaks=seq(0,0.6,0.2),
                     limits=c(0,0.6))+
  facet_wrap(~country+ethnicity, ncol=4,labeller = labeller(.cols = label_parsed, .multi_line = FALSE))


folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL.R"
graphname <- "segment_share"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=12, height=6, unit="cm", dpi=300)



# __ volatility of segments VOTES-----------------------------------------------------------

vol.seg <- df1 %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=T) %>% 
  mutate(rel.votes.lag=lag(rel.votes)) %>% 
  mutate(abs.diff=abs(rel.votes-lag(rel.votes))) %>% 
  group_by(country, ethnicity, year) %>% 
  summarise(sum=abs.diff) %>% 
  mutate(vol=sum/2)


# ______boxplot -------------------------------------------------------------


vol.seg %>% 
  ggplot()+
  geom_boxplot(aes(x=ethnicity, y=vol,
                   fill=ethnicity),
               size=0.2)+
  geom_point(aes(x=ethnicity, y=vol))+
  ggrepel::geom_text_repel(aes(x=ethnicity, y=vol,
                 label=paste0("'",substr(year,3,4))),
                 size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  labs(title="Inter-segment stability: volatility of segments' electoral size",
       subtitle=str_wrap(str_squish("volatility between years based % of votes casted in legislative elections; 
                         only parties which gained seats in Assembbly included; numbers indicate years"), 120))+
  theme_thesis()+
  theme(axis.title=element_blank())+
  theme(legend.position="none")+
  scale_fill_manual(values=ethn.color)+
  facet_wrap(vars(country), scales="free", shrink=T)
  

folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL.R"
graphname <- "segment_vol_boxplot"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=12, height=6, unit="cm", dpi=300)



# __ volatility of segments SEATS -----------------------------------------------------------

df.seats <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  filter(seats>0) %>% 
  group_by(country, election, year, ethnicity) %>% 
  summarise(sum.seats=sum(seats)) %>% 
  mutate(total.seats=sum(sum.seats)) %>% 
  mutate(rel.seats=sum.seats/total.seats) %>% 
  filter(!ethnicity=="nk") 


df.seats.lag <- df.seats %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=T) %>% 
  mutate(rel.seats=lag(rel.seats))

df.seats.x <- bind_rows(df.seats, df.seats.lag, .id="df") %>% 
  arrange(country, ethnicity, year, desc(df))


# ______geom_step plot (size of segment ) -------------------------------------------------------

  ggplot()+
    geom_ribbon(data=df.seats.x, 
              aes(x=year, ymin=0, ymax=rel.seats,
                  fill=ethnicity))+
    geom_step(data=df.seats,
              aes(x=year, y=rel.seats))+
    facet_wrap(vars(country, ethnicity))


# ______boxplot vol based on seats -----------------------------------------

vol.seats.seg <- df.seats %>% 
  group_by(country, ethnicity, year) %>% 
  arrange(year, .by_group=T) %>% 
  group_by(country, ethnicity) %>% 
  mutate(rel.seats.lag=lag(rel.seats)) %>% 
  mutate(abs.diff=abs(rel.seats-rel.seats.lag)) 


vol.seats.seg <-  vol.seats.seg %>% 
  group_by(country, ethnicity, year) %>% 
  summarise(vol=sum(abs.diff)/2)
  
vol.seats.seg %>% 
  ggplot()+
  geom_boxplot(aes(x=ethnicity, y=vol,
                   fill=ethnicity),
               size=0.2)+
  geom_point(aes(x=ethnicity, y=vol))+
  ggrepel::geom_text_repel(aes(x=ethnicity, y=vol,
                               label=paste0("'",substr(year,3,4))),
                           size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  labs(title="Inter-segment stability: volatility of segments' electoral size (seats)",
       subtitle=str_wrap(str_squish("volatility between years based % of votes casted in legislative elections; 
                         only parties which gained seats in Assembbly included; numbers indicate years"), 120))+
  theme_thesis()+
  theme(axis.title=element_blank())+
  theme(legend.position="none")+
  scale_fill_manual(values=ethn.color)+
  facet_wrap(vars(country), scales="free", shrink=T)



# > vote share each party -------------------------------------------------


df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  group_by(country, election, year, party) %>% 
  summarise(votes=sum(votes)) %>% 
  mutate(vote.share=votes/sum(votes)) %>% 
  group_by(country, election, party) %>% 
  mutate(min=min(vote.share)) %>% 
  filter(!min<0.02) %>% 
  filter(str_detect(party, "SDA|SDP|DF|SBB|SBiH")) %>% 
  ggplot()+
  geom_line(aes(x=year, y=vote.share))+
  facet_wrap(vars(country, party), ncol=1)





#  volatility VOTES / establishing links ---------------------------------------------

party.selection <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  group_by(country, election, year) %>% 
  mutate(share=round(votes/sum(votes)*100,2)) %>% 
  filter(share>1) %>% 
  ungroup() %>% 
  distinct(party)

nrow(party.selection) #66 parties with more than 1 % at one point
  
  
vol <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  group_by(country, election, year) %>% 
  mutate(share=votes/sum(votes)) %>% 
  filter(party %in% party.selection$party) %>%  #keeps all observations for all parties which have at one point > 1 % votes
  #filter(share>1) %>% 
 # filter(country=="BIH") %>% 
  arrange(year) %>% 
  ungroup() %>% 
  select(-c(votes, seats, party.short, party.collapse, election)) %>% 
  spread(key=year, value=share, fill=0) %>% 
  select(country, ethnicity, party, everything())  


vol <- vol %>% 
  mutate(party.link=case_when(str_detect(party, "BOSS") ~ "BOSS",
                              str_detect(party, "BPS")~ "BPS",
                              str_detect(party, "Coalition for Unity and Democracy") ~ "SDA",
                              str_detect(party, "Independent Bloc") & country=="BIH" ~ "Independent Bloc",
                              str_detect(party, "PDA") & country=="BIH" ~ "PDA",
                              str_detect(party, "SBB") & country=="BIH" ~ "SBB",
                              str_detect(party, fixed("(A-SDA)")) & country=="BIH" ~ "A-SDA", 
                              str_detect(party, fixed("(SzBiH)")) & country=="BIH" ~ "SzBiH",
                              str_detect(party, fixed("(DNZ)")) & country=="BIH" ~ "DNZ",
                              str_detect(party, fixed("(DNS)")) & country=="BIH" ~ "DNS",
                              
                              str_detect(party, fixed("Demokratska Front")) & country=="BIH" ~ "Demokratska Front",
                              
                              str_detect(party, fixed("Demokratski Narodni Savez (DNS) - NS - SRS")) & country=="BIH" ~ "DNS",
                              str_detect(party, fixed("SDS–NDP–NS–SRS")) & country=="BIH" ~ "SDS",
                              str_detect(party, fixed("Koalicija - PDP, SNS")) & country=="BIH" ~ "PDP",
                              str_detect(party, fixed("(PDP)")) & country=="BIH" ~ "PDP",
                              str_detect(party, fixed("(RS)")) & country=="BIH" ~ "RS",
                              str_detect(party, fixed("(SNSD)")) & country=="BIH" ~ "SNSD",
                              str_detect(party, fixed("(SDS)")) & country=="BIH" ~ "SDS",
                              str_detect(party, fixed("(SNS)")) & country=="BIH" ~ "SNS",
                              str_detect(party, fixed("(SRS)")) & country=="BIH" ~ "SRS",
                              str_detect(party, fixed("(SDP)")) & country=="BIH" ~ "SDP",
                              str_detect(party, fixed("(SPU)")) & country=="BIH" ~ "SPU",
                              
                              str_detect(party, fixed("Koalicija - SNSD, DSP")) & country=="BIH" ~ "SNSD",
                              
                              str_detect(party, fixed("(HDZ)")) & country=="BIH" ~ "HDZ",
                              str_detect(party, fixed("Koalicija - HDZ, HNZ, HSP")) & country=="BIH" ~ "HDZ",
                              str_detect(party, fixed("Koalicija - HDZ, Demokrscani")) & country=="BIH" ~ "HDZ",
                              str_detect(party, fixed("HDZ BIH, HSS, HKDU BIH, HSP DR. Ante Starcevic, HS Herceg-Bosne")) & country=="BIH" ~ "HDZ",
                              str_detect(party, fixed("HDZ–HSS–HSP-HNS–HKDU–HSP-AS BiH–HDU BiH")) & country=="BIH" ~ "HDZ",
                              str_detect(party, coll("HDZ 1990")) & country=="BIH" ~ "HDZ 1990",
                              
                              str_detect(party, fixed("Nova Hrvatska Incijativa - Hkdu")) & country=="BIH" ~ "HKDU",
                              str_detect(party, fixed("Nova Hrvatska Inicijativa (NHI)")) & country=="BIH" ~ "NHI",
                              str_detect(party, fixed("Ekonomski Blok HDU - Za Boljitak")) & country=="BIH" ~ "HDU",
                              
                              str_detect(party, fixed("NSRzB")) & country=="BIH" ~ "NSRzB",
                              
                              
                              str_detect(party, "Alliance for Progress") & country=="BIH" ~ "Alliance for Progress and Peace", 
                              str_detect(party, fixed("(SDA)")) & country=="BIH" ~ "SDA",
                              TRUE ~ as.character(party))) %>%
  select(country, ethnicity, party.link, party, everything()) %>% 
  arrange(country, ethnicity, party.link, party)




BiH.election.years <- df %>%   filter(country=="BIH") %>%   distinct(year) %>%   pull()
NIRL.election.years <- df %>%
  filter(country=="NIRL" & election %in% c("Assembly","Forum Elections")) %>%
  distinct(year) %>%   
  pull()

unique(df$election[df$country=="NIRL"])

vol2 <- vol %>% 
  group_by(country, ethnicity, party.link) %>% 
  mutate(parties.linked=paste0(party, collapse=", ")) %>% 
  group_by(country, ethnicity, party.link, parties.linked) %>% 
  summarise_if(is.numeric, sum) %>% 
  gather(key=year, value=share, -c(country:parties.linked)) %>% 
  filter((country=="NIRL" & year %in% NIRL.election.years) | (country=="BIH" & year %in% BiH.election.years)) %>% 
  arrange(year, .by_group=TRUE) %>% 
  mutate(share.lead=lead(share, order_by=year),
         share.lag=lag(share, order_by=year),
         diff.share=share-share.lag) %>% 
  filter(share>0.005 | share.lag>0.005) %>%   #takes only neighboring observations of observation > 1
  select(country, ethnicity, party.link, parties.linked, year, everything())


# >-- plot volatility per party -------------------------------------------


vol2 %>% 
  mutate(vol.party=abs(diff.share)/2) %>% 
  group_by(country, ethnicity, year) %>% 
  arrange(vol.party, .by_group=TRUE) %>% 
  select(country, ethnicity, party.link, year, vol.party) %>% 
  mutate(vol.party.cum=cumsum(vol.party)) %>% 
  ggplot+
  geom_bar(aes(x=year, y=vol.party,
               group=party.link,
               fill=party.link),
           stat="identity", position=position_stack())+
  geom_text(aes(x=year, 
                y=vol.party,
                group=party.link, 
                label=party.link),
            size=2,
            stat="identity", 
            position = position_stack(vjust=0.5))+
  facet_wrap(vars(ethnicity))+
  theme(legend.position = "none")


# >--plot volatility ethnicity ---------------------------------------------

vol3 <- vol2 %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(vol=(sum(abs(diff.share)))/2) 

vol3 %>% 
  ggplot()+
  geom_line(aes(x=year, y=vol,
                color=as.factor(country),
                group=country))+
  geom_point(aes(x=year, y=vol,
                 color=as.factor(country)))+
  facet_wrap(vars(ethnicity))



# >--plot volatility countries --------------------------------------------

attenuation.dates<- data.frame(stringsAsFactors=FALSE,
     country = c("NIRL", "BIH"),
     attenuation.start = c(2003, 2006))

vol.countries <- vol2 %>% 
  group_by(country, year) %>% 
  summarise(vol=(sum(abs(diff.share)))/2) %>% 
  mutate(year=as.numeric(year)) %>% 
  ungroup() %>% 
  left_join(.,attenuation.dates, by=c("country")) %>% 
  mutate(attenuation.phase=case_when(year<attenuation.start ~ "no",
                                     TRUE ~ as.character("yes")))
  

method <- c("Calculated on the basis of relaxed party linkages, transition pairs and 0.5 % threshold; see Casal Bértoa et al (2017)")

vol.countries.plot <- vol.countries %>% 
  ggplot()+
    geom_line(data=. %>% filter(attenuation.start<=year), aes(x=year, y=vol,
                color=country,
                group=country),
                linetype="dotted")+
  geom_line(data=. %>% filter(attenuation.start>=year),
            aes(x=year, y=vol,
                color=country,
                group=country),
                linetype="solid")+
  geom_point(aes(x=year, y=vol,
                 color=country,
                 group=country))+
  geom_text(data=vol.countries %>% 
              group_by(country) %>% 
              arrange(year, .by_group=TRUE) %>% 
              slice(n()),
                    aes(x=year+1, y=vol, label=country),
            size=2,
            color="black")+
  labs(title="Party system volatility (Pedersen Index)",
       subtitle=str_wrap(method, 65),
       caption=str_c("Graph:author, Data:www.ark.ac.uk/elections, www.izbori.ba"))+
  theme_thesis()+
  theme(panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust=0),
        legend.position = "none",
        axis.title=element_blank())+
   scale_x_continuous(breaks=unique(vol.countries$year), 
                      labels=paste0("'",substr(unique(vol.countries$year),3,4)),
                      limits=c(1998,2020),
                      minor_breaks = NULL)+
   scale_y_continuous(minor_breaks=NULL, limits = c(0,0.5), 
                      breaks=seq(0,0.5,.1),
                      labels=scales::percent(seq(0,0.5,.1), accuracy = 1),
                      expand=expand_scale(mult=c(0,0)))+
  scale_color_manual(values=country.colors)
  
plot(vol.countries.plot )
  #ggpreview(vol.countries.plot, width=8, height=6, unit="cm", dpi=300)  

folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL.R"
graphname <- "vol.countries"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(vol.countries.plot, file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=8, height=6, unit="cm", dpi=300)



# Volatility of parties - aggregated to ethnic level (within) - seats ------------------------------------------------------

unique(df$election)

vol.seats<- df %>% 
  filter(election %in% c("Assembly","Forum Electtions","House of Representatives")) %>% 
  filter(seats>0) %>% 
  group_by(country, election, ethnicity) %>% 
  mutate(share.seats=seats/sum(seats)) %>% 
  ungroup() %>% 
  select(-c(votes, party.collapse, party.short, seats)) %>% 
  spread(key=year, value=share.seats, fill=0) %>% 
  arrange(country, election, ethnicity) %>% 
  mutate(party.link=case_when(str_detect(party, "BOSS") ~ "BOSS",
                              # str_detect(party, "BPS")~ "BPS",
                              # str_detect(party, "Coalition for Unity and Democracy") ~ "SDA",
                              # str_detect(party, "Independent Bloc") & country=="BIH" ~ "Independent Bloc",
                              # str_detect(party, "PDA") & country=="BIH" ~ "PDA",
                              # str_detect(party, "SBB") & country=="BIH" ~ "SBB",
                              # str_detect(party, fixed("(A-SDA)")) & country=="BIH" ~ "A-SDA", 
                              # str_detect(party, fixed("(SzBiH)")) & country=="BIH" ~ "SzBiH",
                              # str_detect(party, fixed("(DNZ)")) & country=="BIH" ~ "DNZ",
                              # str_detect(party, fixed("(DNS)")) & country=="BIH" ~ "DNS",
                              # 
                              # str_detect(party, fixed("Demokratska Front")) & country=="BIH" ~ "Demokratska Front",
                              # 
                              # str_detect(party, fixed("Demokratski Narodni Savez (DNS) - NS - SRS")) & country=="BIH" ~ "DNS",
                              # str_detect(party, fixed("SDS–NDP–NS–SRS")) & country=="BIH" ~ "SDS",
                              # str_detect(party, fixed("Koalicija - PDP, SNS")) & country=="BIH" ~ "PDP",
                              # str_detect(party, fixed("(PDP)")) & country=="BIH" ~ "PDP",
                              # str_detect(party, fixed("(RS)")) & country=="BIH" ~ "RS",
                              # str_detect(party, fixed("(SNSD)")) & country=="BIH" ~ "SNSD",
                              # str_detect(party, fixed("(SDS)")) & country=="BIH" ~ "SDS",
                              # str_detect(party, fixed("(SNS)")) & country=="BIH" ~ "SNS",
                              # str_detect(party, fixed("(SRS)")) & country=="BIH" ~ "SRS",
                              # str_detect(party, fixed("(SDP)")) & country=="BIH" ~ "SDP",
                              # str_detect(party, fixed("(SPU)")) & country=="BIH" ~ "SPU",
                              # 
                              # str_detect(party, fixed("Koalicija - SNSD, DSP")) & country=="BIH" ~ "SNSD",
                              # 
                              # str_detect(party, fixed("(HDZ)")) & country=="BIH" ~ "HDZ",
                              # str_detect(party, fixed("Koalicija - HDZ, HNZ, HSP")) & country=="BIH" ~ "HDZ",
                              # str_detect(party, fixed("Koalicija - HDZ, Demokrscani")) & country=="BIH" ~ "HDZ",
                              # str_detect(party, fixed("HDZ BIH, HSS, HKDU BIH, HSP DR. Ante Starcevic, HS Herceg-Bosne")) & country=="BIH" ~ "HDZ",
                              # str_detect(party, fixed("HDZ–HSS–HSP-HNS–HKDU–HSP-AS BiH–HDU BiH")) & country=="BIH" ~ "HDZ",
                              # str_detect(party, coll("HDZ 1990")) & country=="BIH" ~ "HDZ 1990",
                              # 
                              # str_detect(party, fixed("Nova Hrvatska Incijativa - Hkdu")) & country=="BIH" ~ "HKDU",
                              # str_detect(party, fixed("Nova Hrvatska Inicijativa (NHI)")) & country=="BIH" ~ "NHI",
                              # str_detect(party, fixed("Ekonomski Blok HDU - Za Boljitak")) & country=="BIH" ~ "HDU",
                              # 
                              # str_detect(party, fixed("NSRzB")) & country=="BIH" ~ "NSRzB",
                              # 
                              # 
                              # str_detect(party, "Alliance for Progress") & country=="BIH" ~ "Alliance for Progress and Peace", 
                              # str_detect(party, fixed("(SDA)")) & country=="BIH" ~ "SDA",
                              TRUE ~ as.character(party))) %>% 
  select(country, election, ethnicity, party.link, party, everything())

vol.seats2 <- vol.seats %>% 
  group_by(country, election, ethnicity, party.link) %>% 
  summarise_if(is.numeric, sum) %>% 
  gather(key=year, value=share.seats, -c(country:party.link)) %>% 
  filter((country=="NIRL" & year %in% NIRL.election.years) | (country=="BIH" & year %in% BiH.election.years)) %>% 
  group_by(country, election, ethnicity, party.link) %>% 
  arrange(year, .by_group=TRUE) %>% 
  mutate(share.seats.lag=lag(share.seats),
         share.seats.lead=lead(share.seats)) %>% 
  ungroup() %>% 
  mutate(abs.diff=abs(share.seats-share.seats.lag)) %>% 
  filter(share.seats > 0 | share.seats.lag > 0) %>% 
  group_by(country, ethnicity, year) %>% 
  summarise(vol=sum(abs.diff)/2)


vol.seats2 %>% 
  filter(!ethnicity %in% c("nk")) %>% 
  ggplot()+
  geom_line(aes(x=year, y=vol,
                color=ethnicity,
                group=country))+
  facet_wrap(vars(country, ethnicity))





# n.parties __ number of parties candidating -------------------------------------------

n.parties  <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  group_by(country, year) %>% 
  summarise(n.parties=n()) %>% 
  gather(key=var, value=value, -c(country, year))


n.parties %>%   
  ggplot()+
  geom_line(aes(x=year, y=value,
                 color=country))+
  geom_line(aes(x=year, y=value,
                color=country))+
  labs(title="number of running parties")+
  scale_y_continuous(limits=c(0,40))



# n.parties.seats __ parties in assembly -----------------------------------------------------

n.parties.seats <- df %>% 
  filter(election %in% c("Assembly","House of Representatives")) %>% 
  filter(seats>0) %>% 
  group_by(country, year) %>% 
  summarise(n.parties.seats=n()) %>% 
  gather(key=var, value=value, -c(country, year))


n.parties.seats %>% 
  ggplot()+
  geom_point(aes(x=year, y=value,
                 color=country))+
  geom_line(aes(x=year, y=value,
                color=country))+
  labs(title="number of parties in assembly")+
  scale_y_continuous(limits=c(0,20))


# n.parties.eff __ number of effective parties based on seats & votes ------------------------------

 n.parties.eff <- df %>% 
  filter(election %in% c("Assembly","House of Representatives")) %>% 
  group_by(country, year) %>% 
  mutate(seats.total=sum(seats)) %>% 
  mutate(seats.perc.2=(seats/seats.total)^2) %>% 
  mutate(votes.total=sum(votes)) %>% 
  mutate(votes.perc.2=(votes/votes.total)^2) %>% 
  
  summarise(n.eff.seats=1/sum(seats.perc.2),
            n.eff.votes=1/sum(votes.perc.2)) %>% 
  gather(key=var, value=value, -c(country, year)) 


n.parties.eff %>% 
  ggplot()+
  geom_point(aes(x=year, y=value,
                 color=country))+
  geom_line(aes(x=year, y=value,
                linetype=var,
                color=country))+
  geom_text(aes(x=year, y=value,
                label=round(value,2)),
            nudge_y=0.05,
            vjust=0)+
  labs(title="Number of effective parties in assembly based on seats and votes",
       subtitle="Based on total votes and seats of parties in the assembly")+
  scale_y_continuous(limits=c(0,10))


tab <- dplyr::bind_rows(n.parties, n.parties.seats, n.parties.eff)



# gini seats --------------------------------------------------------------
library(reldist)

gini.seats<- df %>% 
  filter(seats>0) %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  select(country, election, votes, party, year) %>% 
  group_by(country, year) %>%
  summarise(value=reldist::gini(votes)) %>% 
  mutate(var="gini_votes")

gini.votes.ethnicity<- df %>% 
  filter(seats>0) %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  select(country, election, votes, ethnicity, party, year) %>% 
  group_by(country, ethnicity, year) %>%
  summarise(value=reldist::gini(votes)) %>% 
  mutate(var="gini_votes")

gini.votes.ethnicity %>% 
  ggplot()+
  geom_line(aes(x=year, y=value,
                color=ethnicity)) +
  facet_wrap(vars(country, ethnicity))


df %>% 
  filter(seats>0) %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  filter(ethnicity=="Bosniaks") %>% 
  ggplot()+
  geom_line(aes(x=year, y=votes,
                color=party))
  

x <- data.frame(
               V1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               V2 = c(1, 1, 5, 5, 1, 1, 1, 1, 1, 8)
     )


y <- data.frame(
               V1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               V2 = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 50)
     )


gini(y$V2) #0.37
#higher => higher concentrated
#0 perfect equality

# function for sparkline --------------------------------------------------

fn_spark <- function(df, key) 
    {tidy <- df %>% gather(key=key, value=value, 2:ncol(df)) %>% 
      group_by(var) %>% 
      summarise(development=spk_chr(
        value, 
        chartRangeMin = 0,
        type="line"))
    
    all <- left_join(df, tidy, by=c("var")) 
    
    # indicator <- df %>% gather(key=key, value=value, 2:ncol(df)) %>% 
    #   group_by(var)  %>% 
    #   arrange(var, key) %>% 
    #   slice(c(1,n())) %>% 
    #   mutate(diff=value-lag(value)) %>% 
    #   mutate(change=case_when(diff>0 ~"increase",
    #                          diff<0 ~ "decrease",
    #                          diff==0 ~ "stable")) %>% 
    #   filter(!is.na(diff))
    # 
    # all <- left_join(all, indicator %>% select(var, diff2), by=c("var"))
    
    r_col <- ncol(all)-2
    
    all<- all %>% 
      mutate(var=case_when(var=="n.eff.seats" ~ "# effective parties (seats)",
                           var=="n.eff.votes" ~ "# effective parties (votes)",
                           var=="n.parties" ~ "# parties running",
                           var=="n.parties.seats" ~ "# parties winning seats")) %>% 
      rename(indicator=var)  
      
    
    all$indicator <- forcats::fct_relevel(all$indicator, "# parties running",
                                 "# parties winning seats",
                                 "# effective parties (votes)",
                                 "# effective parties (seats)")
    
    all <- all %>% 
      group_by(indicator) %>% 
      arrange(indicator)
    
    all %>%
      formattable(align=c("l",rep("r", r_col),"c","c")) %>% 
      as.htmlwidget() %>% 
      spk_add_deps()
    }



# >> table -------------------------------------------------------------------
df.x<- tab1$data[[1]]

df.x %>% gather(key=key, value=value, 2:ncol(df.x)) %>% 
  group_by(var) %>% 
  summarise(development=spk_chr(
    value, 
    chartRangeMin = 0,
    type="line"))

df.x %>% gather(key=key, value=value, 2:ncol(df.x)) %>% 
  group_by(var)  %>% 
  arrange(var, key) %>% 
  slice(c(1,n())) %>% 
  mutate(diff=value-lag(value)) %>% 
  mutate(diff2=case_when(diff>0 ~"increase",
                         diff<0 ~ "decrease",
                         diff==0 ~ "stable")) %>% 
  filter(!is.na(diff))

tab1 <- tab %>% 
  mutate(value=round(value, 2)) %>% 
  group_by(country) %>% 
  group_nest() %>% 
  mutate(data=map(data, 
                  ~ spread(.x, key=year, value=value))) %>% 
  mutate(data=map(data,
                  ~fn_spark(.x)))
  
BiH.tab <- tab1[2][[1]][1]
Nir.tab <- tab1[2][[1]][2]



# >> combine tables -------------------------------------------------------

tab_excle <- bind_rows(n.parties, n.parties.seats, n.parties.eff)

df.graph.country <- tab_excle

g1 <- tab_excle %>% 
  filter(!var %in% c("n.parties","n.eff.seats")) %>% 
  ggplot()+
  geom_line(aes(x=year, y=value,
                color=var))+
  geom_point(aes(x=year, y=value,
                color=var))+
  scale_y_continuous(limits=c(0,15))+
  facet_wrap(vars(country))
  
g1  





tab_excle <- tab_excle %>% 
  spread(key=year, value=round(value, 2)) %>% 
  mutate(var=case_when(var=="n.eff.seats" ~ "# effective parties (seats)",
                               var=="n.eff.votes" ~ "# effective parties (votes)",
                               var=="n.parties" ~ "# parties running",
                               var=="n.parties.seats" ~ "# parties winning seats",
                       var=="gini_seats" ~ "Gini coef seats")) %>% 
  
  mutate(var=fct_relevel(var, "# parties running",
                                        "# parties winning seats",
                                        "# effective parties (votes)",
                                        "# effective parties (seats)")) %>% 

  select(var, country, 3:ncol(.)) %>% 
  group_by(var, country) %>% 
  arrange(var, country)

writexl::write_xlsx(tab_excle, "inter-country.xlsx")


spark<- tab_excle %>%
  gather(key=key, value=value, 3:ncol(tab_excle)) %>% 
  group_by(country, var) %>% 
  filter(!is.na(value)) %>% 
  summarise(development=spk_chr(
    value, 
    chartRangeMin = 0,
    type="bar"))


tab_excle1 <- right_join(tab_excle, spark, by=c("country","var")) %>% 
  group_by(var) %>% 
  arrange(var, country)
  
tab_excle1 %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate_all(as.character) %>% 
  replace(is.na(.),"") %>% 
  ungroup() %>% 
  rename(indicator=var) %>% 
 # select(-var) %>% 
  formattable(align=c("l","l", rep("r",ncol(tab_excle1)-2))) %>% 
  as.htmlwidget() %>% 
  spk_add_deps() 
  
  # kable("html", caption="Between Countries", escape=T) %>% 
  # kable_styling("striped", full_width = F) %>%
  # group_rows("# parties running", 1, 2) %>%
  # group_rows("# parties winning seats", 3, 4) %>% 
  # group_rows("# effective parties (votes)", 5, 6) %>%
  # group_rows("# effective parties (seats)", 7, 8) %>% 
  
 
# PER SEGMENT -------------------------------------------------------------


# n.parties.segment - absolute number of assembly parties per segment -------------------------------------------

n.parties.segment <- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  filter(seats>0) %>%   #only those with seats
  group_by(country, year, ethnicity) %>% 
  summarise(value=n(),
            parties=paste(party, collapse=", ")) %>% 
  mutate(var="n.parties.segment")


n.parties.segment %>% 
  ggplot()+
  geom_point(aes(x=year, y=value,
                 color=country))+
  geom_line(aes(x=year, y=value,
                color=country))+
  labs(title="number of parties in assembly per segment")+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(country~ethnicity)
  


spark_n.parties.segment<- n.parties.segment %>% 
  group_by(country, ethnicity, var) %>% 
  filter(!is.na(value)) %>% 
  summarise(development=spk_chr(
    value, 
    chartRangeMin = 0,
    type="line"))

change.indicator <- n.parties.segment %>%
  select(country, ethnicity, var, value, year) %>% 
  group_by(country, ethnicity, var) %>% 
  slice(c(1,n())) %>% 
  mutate(change=value-lag(value)) %>% 
  filter(!is.na(change)) %>% 
  mutate(change=case_when(change > 0 ~ "increase",
                          change==0 ~ "stable",
                          change < 0 ~ "decrease"))

change.indicator


tab_n.parties.segment <- n.parties.segment%>% 
  select(-parties) %>% 
  spread(key=year, value=value) %>% 
  select(country, var, everything())

tab_n.parties.segment <- left_join(tab_n.parties.segment, spark_n.parties.segment, by=c("country","var","ethnicity")) %>% 
  left_join(.,change.indicator %>% select(-year, -value)) %>% 
  mutate(var=case_when(var=="n.parties.segment" ~ "# parties with seats per segment"))
  
fn_spark.table<- function(df) {
   df  %>% 
      mutate_if(is.numeric, round, 2) %>% 
      mutate_all(as.character) %>% 
      replace(is.na(.),"") %>% 
      ungroup() %>% 
      rename(indicator=var) %>% 
      arrange(country, indicator, ethnicity) %>% 
      formattable::formattable(align=c("l","l","l", rep("r",ncol(df)-2)),
                               list(change=formatter("span", style = x ~ ifelse(x=="increase",
                                                                           style(color="green"),
                                                                           ifelse(x=="decrease",
                                                                                  style(color="red"),
                                                                                  ifelse(x=="stable",
                                                                                         style(color="grey"),NA)))))) %>% 
      as.htmlwidget() %>% 
      spk_add_deps()}

spark.tab_n.parties.segment<- fn_spark.table(tab_n.parties.segment %>% 
                                               select(var, country, everything()))
spark.tab_n.parties.segment
  

# n.parties.eff.seg - number of effective parties based on votes (based on total votes to assembly) ------------------------------

n.parties.eff.seg<- df %>% 
  filter(election %in% c("Assembly","House of Representatives", "Forum Elections")) %>% 
  filter(seats>0) %>% 
  group_by(country, year, ethnicity) %>% 
  mutate(seats.total=sum(seats)) %>% 
  mutate(seats.perc.2=(seats/seats.total)^2) %>% 
  mutate(votes.total=sum(votes)) %>% 
  mutate(votes.perc.2=(votes/votes.total)^2) %>% 
  
  summarise(n.eff.seats=1/sum(seats.perc.2),
            n.eff.votes=1/sum(votes.perc.2)) %>% 
  gather(key=var, value=value, -c(country, year, ethnicity))


n.parties.eff.seg %>% 
  #filter(type=="n.eff.votes") %>% 
  #split(.$country) %>% 
  #map(~
  ggplot()+
  geom_point(aes(x=year, y=value,
                 color=country))+
  geom_line(aes(x=year, y=value,
                linetype=var,
                color=country))+
  geom_text(aes(x=year, y=value,
                label=round(value,2)),
            size=2,
            nudge_y=0.05,
            vjust=0)+
  labs(title="Number of effective parties per segment in assembly based on seats and votes",
       subtitle="Based on total votes and seats of parties in the assembly")+
  scale_y_continuous(limits=c(0,5))+
  theme(legend.position = "none")+
  facet_wrap(country~ethnicity, ncol=3, labeller = label_wrap_gen(multi_line=FALSE))#)
  
  # lay <- rbind(c(1,1,1),
  #              c(2,2,NA))
  # effective<- arrangeGrob(plot[[1]], plot[[2]])
  # grid.draw(effective)


spark_n.parties.eff.seg<- n.parties.eff.seg %>% 
  group_by(country, ethnicity, var) %>% 
  filter(!is.na(value)) %>% 
  summarise(development=spk_chr(
    value, 
    chartRangeMin = 0,
    type="line"))


change.indicator <- n.parties.eff.seg %>%
  select(country, ethnicity, var, value, year) %>% 
  group_by(country, ethnicity, var) %>% 
  slice(c(1,n())) %>% 
  mutate(change=value-lag(value)) %>% 
  filter(!is.na(change)) %>% 
  mutate(change=case_when(change > 0 ~ "increase",
                          change==0 ~ "stable",
                          change < 0 ~ "decrease"))


tab_n.parties.eff.seg <- left_join(n.parties.eff.seg %>% 
                                     spread(key=year, value=value), spark_n.parties.eff.seg) %>% 
  left_join(.,change.indicator %>% select(country, var, ethnicity, change))


tab_n.parties.eff.seg  %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate_all(as.character) %>% 
  replace(is.na(.),"") %>% 
  ungroup() %>% 
  rename(indicator=var) %>% 
  mutate(indicator=case_when(indicator=="n.eff.seats" ~ "# effective parites (seats)",
                             indicator=="n.eff.votes" ~ "# effective parteis (votes)")) %>% 
  select(country, indicator, everything()) %>% 
  filter(ethnicity!="nk") %>% 
  mutate(ethnicity=forcats::fct_relevel(ethnicity, c("Bosniaks", "Serbs","Croats", "Multiethnic",
                                                     "Unionists", "Nationalists","Others"))) %>% 
  arrange(country, indicator, ethnicity) %>% 
  mutate(indicator=case_when(indicator==lag(indicator) ~ "",
                             TRUE ~ as.character(indicator))) %>% 
  mutate(country=case_when(country==lag(country)~ "",
                           TRUE ~ as.character(country))) %>% 
  formattable::formattable(align=c("l","l","l", rep("r",ncol(df)-2)),
                           list(change=formatter("span", style = x ~ ifelse(x=="increase",
                                                                            style(color="green"),
                                                                            ifelse(x=="decrease",
                                                                                   style(color="red"),
                                                                                   ifelse(x=="stable",
                                                                                          style(color="grey"),NA)))))) %>% 
  as.htmlwidget() %>% 
  spk_add_deps()


# spark.tab_n.parties.eff.seg<- fn_spark.table(tab_n.parties.eff.seg)
# spark.tab_n.parties.eff.seg


#shows increase of effective numbers of parties for BSC; stable or decline in NIRL



# __ ----------------------------------------------------------------------
# combined graph parties effecitve and absolute per segment ------------------------

n.parties
n.parties.seats
n.parties.eff

x <- bind_rows(n.parties.segment, n.parties.eff.seg)


unique(x$var)

x.plot <- x %>% 
  filter(!ethnicity=="nk") %>% 
  filter(!var=="n.eff.seats") %>% 
  ungroup() %>% 
  group_split(country) %>% 
  map(~ggplot(.)+
  geom_point(aes(x=year, y=value,
                 color=var))+
  geom_line(aes(x=year, y=value,
                 color=var))+
  labs(title="Intra-segment fragmentation: number of effective parties",
       subtitle="only parties with seats included")+
  theme_thesis()+
  theme(axis.title=element_blank(),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.85,0.25),
        strip.text = element_text(hjust=0))+
  scale_x_continuous(breaks=unique(.$year),
                     labels=paste0("'",substr(unique(.$year),3,4)),
                     minor_breaks = NULL)+
  scale_color_manual(labels=c(`n.eff.seats`="# effective parties based on seats",
                              `n.eff.votes`="# effective parties based on votes",
                              `n.parties.segment`="# parties"),
                     values=c(`n.eff.seats`="orange",
                              `n.eff.votes`="firebrick",
                              `n.parties.segment`="steelblue"))+
  scale_y_continuous(limits=c(0,7), breaks=seq(0,7,2),
                     minor_breaks = NULL,
                     expand=expand_scale(mult=c(0,0.01)))+                  
  facet_wrap(country~ethnicity, nrow=1,
            labeller = labeller(.cols = label_parsed, .multi_line = FALSE)))


folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL"
graphname <- "n_parties_segment"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=17, height=14, unit="cm", dpi=300)
  

# >__ggpubr / ggarrange (no layoutmatrix, but shared heading ) ---------------------------------

# library(ggpubr)
# p1 <- ggpubr::ggarrange(plotlist=plot.x, 
#                   common.legend = TRUE,
#                   nrow=2,
#                   legend="bottom")
# 
# p1
# p2 <- ggpubr::annotate_figure(p1, top=text_grob("Intra-segment fragmentation: Number of effective parties"))
# p2



# __cow plot --------------------------------------------------------------

cowplot::plot_grid(plot.x[[1]]+theme(legend.position="none"), 
                   plot.x[[2]]+theme(legend.position="none"), 
                   align="v", 
                   axis="l",
                   ncol=1,
                   rel_widths = c(4,3))




# GridExtra Package -------------------------------------------------------

lay <- rbind(c(1,1,1,1),
             c(2,2,2,3))

get.legend <- cowplot::get_legend(plot.x[[1]])

p1 <- gridExtra::arrangeGrob(plot.x[[1]]+theme(legend.position="none"), 
                             plot.x [[2]]+theme(legend.position="none"), 
                             get.legend,
                             top=textGrob(str_wrap("Elite predominance: Difference of instra-segment voteshare between first and second largest party", 70), gp=gpar(fontsize=12)),
                             layout_matrix = lay)
p1
grid.draw(p1)

g <- arrangeGrob(x.plot[[1]]+theme(legend.position = "none"), 
             x.plot[[2]]+theme(legend.position="right",
                               plot.title = element_blank(),
                               plot.subtitle = element_blank()), 
             layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2)))


folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL"
graphname <- "n_parties_segment"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(g, file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=17, height=14, unit="cm", dpi=300)



# __ ----------------------------------------------------------------------
# [skipped] Gini Seats -------------------------------------------------------------------

library(reldist)

df %>% 
  filter(election %in% c("Assembly", "House of Representatives")) %>% 
  filter(seats>0) %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(gini.seats=reldist::gini(seats),
            gini.votes=reldist::gini(votes)) %>% 
  gather(key=type, value=gini, -c(country, year, ethnicity)) %>% 
  filter(type=="gini.seats") %>%
  filter(!ethnicity %in% c("nk",NA)) %>% 
  group_split(.$country) %>% 
  map(~ggplot(.)+
  geom_point(aes(x=year, y=gini,
                 color=ethnicity))+
  geom_line(aes(x=year, y=gini,
                linetype=type,
                color=ethnicity))+
  labs(title="Concentration of votes and seats (Gini coefficient)")+
  scale_y_continuous(limits=c(0,1))+
  facet_wrap(vars(country, ethnicity), ncol=2, labeller=label_wrap_gen(multi_line = FALSE)))
  



# voteshare largest party -------------------------------------------------------

share.largest.party<- df %>% 
  filter(election %in% c("Assembly", "House of Representatives")) %>% 
  filter(seats>0) %>% 
  group_by(country, year) %>% 
  mutate(vote.share=votes/sum(votes)) %>% 
  group_by(country, year, ethnicity) %>% 
  arrange(desc(vote.share), .by_group=TRUE) %>% 
  slice(1)
  
tab.share.largest.party <- share.largest.party %>% 
  select(country, ethnicity, year, vote.share) %>% 
  gather(key=vote.share, value=value, -c(country, year, ethnicity)) %>% 
  spread(key=year, value=round(value,2)) %>% 
  select(country, indicator=vote.share, ethnicity, everything()) %>% 
  mutate_if(is.numeric, round, 2)
  
change.indicator <- share.largest.party %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=TRUE) %>% 
  slice(c(1,n())) %>% 
  mutate(change.indicator=case_when((vote.share - lag(vote.share)) > 0 ~ "increase",
                                    (vote.share - lag(vote.share)) < 0 ~ "decrease",
                                    (vote.share - lag(vote.share)) == 0 ~ "stable")) %>% 
  filter(!is.na(change.indicator))


spark <- share.largest.party %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=TRUE) %>% 
  summarise(spark=sparkline::spk_chr(vote.share,
                          chartRangeMin=0,
                          type="line")) %>% 
  ungroup()
  

# empty_cells <- function(df, var) {
#   var <- enquo(var)
#   df %>% 
#     mutate(var:=case_when(!!var==lag(!!var) ~ "",
#                          TRUE ~ as.character(!!var)))
# }
 

tab.share.largest.party <- tab.share.largest.party %>% 
  left_join(.,change.indicator %>% 
              select(country, ethnicity, change=change.indicator)) %>%
  left_join(.,spark %>% select(country, ethnicity, development=spark)) %>% 
  mutate(indicator=case_when(indicator=="vote.share" ~ "vote share of segment's largest party")) %>% 
  filter(!ethnicity %in% c("nk",NA)) %>% 
  mutate_all(as.character) %>% 
  replace(is.na(.),"") %>% 
  select(indicator, country, everything()) #%>% 
  # ungroup() %>% 
  # mutate(indicator=case_when(indicator==lag(indicator) ~ "",
  #                            TRUE ~ as.character(indicator))) %>% 
  # ungroup() %>% 
  # mutate(country=case_when(indicator==lag(country, order_by = c(ethnicity)) ~ "",
  #                          TRUE ~ as.character(country)))


# empty_cells <- function(df, var) {
#   var <- enquo(var)
#   df %>% 
#     mutate(var:=case_when(!!var==lag(!!var) ~ "",
#                           TRUE ~ as.character(!!var)))
# }




         
tab.share.largest.party  %>%        
  formattable::formattable(align=c(rep("l",3), rep("r",ncol(tab.share.largest.party)-2)),
                           list(change=formatter("span", style = x ~ ifelse(x=="increase",
                                                                            style(color="green"),
                                                                            ifelse(x=="decrease",
                                                                                   style(color="red"),
                                                                                   ifelse(x=="stable",
                                                                                          style(color="grey"),NA)))))) %>% 
  as.htmlwidget() %>% 
  spk_add_deps()





# intra-voteshare of largest party ----------------------------------------------


# __ table ----------------------------------------------------------------


intra.share.largest.party <- df %>% 
  filter(election %in% c("Assembly", "House of Representatives")) %>% 
  filter(seats>0) %>% #!!
  group_by(country, year, ethnicity) %>% 
  mutate(votes.eth=sum(votes)) %>% 
  mutate(vote.share=votes/votes.eth) %>% 
  arrange(country, year, ethnicity, desc(vote.share)) %>% 
  group_by(country, year, ethnicity) %>% 
  slice(1) %>% 
  select(country, year, ethnicity, vote.share) %>% 
  gather(key=vote.share, value=value, -c(country, year, ethnicity)) %>% 
  mutate(value=round(value, 2))


tab.intra.share.largest.party <- intra.share.largest.party %>% 
  spread(key=year, value=value) 


# __ change ---------------------------------------------------------------
change <- intra.share.largest.party %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=TRUE) %>% 
  slice(c(1,n())) %>% 
  mutate(change=case_when(value - lag(value) > 0 ~ "increase",
                          value - lag(value) < 0 ~ "decrease",
                          value - lag(value) == 0 ~ "stable")) %>% 
  filter(!is.na(change))

# __ spark -----------------------------------------------------------------
spark <- intra.share.largest.party %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=TRUE) %>%
  summarise(spark=sparkline::spk_chr(value,
                                     chartRangeMin=0,
                                     type="line")) %>% 
  ungroup()
  


# __merge -----------------------------------------------------------

tab.intra.share.largest.party <- tab.intra.share.largest.party %>% left_join(.,change %>% select(country, ethnicity, change)) %>% 
  left_join(.,spark) %>% 
  select(indicator=vote.share, country, ethnicity, everything()) %>% 
  mutate(indicator="intra-segment vote share largest party") %>% 
  mutate_if(is.numeric, as.character) %>% 
  replace(is.na(.),"") %>% 
  filter(!ethnicity %in% c("nk",NA))
  

# __spark table -----------------------------------------------------------

tab.intra.share.largest.party %>% 
formattable::formattable(align=c(rep("l",3), rep("r",ncol(tab.share.largest.party)-2)),
                         list(change=formatter("span", style = x ~ ifelse(x=="increase",
                                                                          style(color="green"),
                                                                          ifelse(x=="decrease",
                                                                                 style(color="red"),
                                                                                 ifelse(x=="stable",
                                                                                        style(color="grey"),NA)))))) %>% 
  as.htmlwidget() %>% 
  spk_add_deps()






# >>plot ------------------------------------------------------------------


intra.share.largest.party  %>% 
  filter(!ethnicity %in% c("nk", NA)) %>% 
  ggplot()+
  geom_line(aes(x=year, y=value,
                color=ethnicity))+
  facet_wrap(vars(country, ethnicity))



# Difference between 1st and 2nd per segment ------------------------------


plot <- df %>% 
  filter(election %in% c("Assembly", "House of Representatives")) %>% 
  filter(seats>0) %>% 
  group_by(country, year, ethnicity) %>% 
  mutate(votes.total=sum(votes)) %>% 
  mutate(vote.share=votes/votes.total) %>% 
  arrange(country, year, ethnicity, desc(vote.share)) %>% 
  slice(1:2) %>% 
  mutate(vote.share.diff=vote.share-lead(vote.share)) %>% 
  fill(vote.share.diff, .direction=c("down")) %>% 
  mutate(indicator=row_number(),
         pos=case_when(indicator==1 ~ +.1,
                           indicator==2 ~ -.1)) %>% 
  filter(!ethnicity %in% c("nk","unknown", NA)) %>% 
  ungroup() %>% 
  group_split(country) %>%
  map(
    ~ggplot(.)+
    geom_line(aes(x=year, y=vote.share.diff,
                  color=ethnicity),
              size=1)+
    geom_path(aes(x=year, 
                  y=vote.share,
                  group=year),
              alpha=0.5)+
    geom_point(aes(x=year, y=vote.share),
               alpha=0.5)+
    geom_text(aes(x=year,
                  y=pos+vote.share,
                  label=stringr::str_trunc(party.short,7),
                  group=year),
              size=2)+
    geom_hline(yintercept=0.5, 
               linetype="dashed",
               alpha=0.5)+
    scale_y_continuous(labels=scales::percent, breaks = seq(0,1,0.25), 
                       minor_breaks = NULL,
                       limits=c(0,1.1))+
    scale_x_continuous(breaks=unique(.$year),
                       labels=stringr::str_sub(unique(.$year),3,4),
                       limits=c(1996,2018),
                       minor_breaks = NULL) +
    scale_color_manual(values=ethn.color)+
    labs(title=glue("{.$country}"),
      y="% of segment's total vote")+
      theme_thesis()+
      theme(axis.title.x=element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "none",
            strip.text = element_text(hjust=0, face="bold"),)+
    facet_wrap(vars(ethnicity), nrow=1))


lay <- rbind(c(1,1,1),
             c(2,2,NA))

dominance<- arrangeGrob(plot[[1]], plot[[2]], layout_matrix=lay, 
                        top=textGrob(str_wrap("Elite predominance: Difference of instra-segment voteshare between first and second largest party", 70), gp=gpar(fontsize=12)))
                        
                        
grid.draw(dominance)

folder <- paste0(wdr,"/graphs/")
Rfile <- "IntraSegmentDiff_VoteShare"
graphname <- "dominance"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(dominance, file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=17, height=14, unit="cm", dpi=300)



# effecttive threshold ----------------------------------------------------

threshold <- df %>% 
  filter(election %in% c("Assembly", "House of Representatives")) %>% 
  group_by(country, year, election) %>% 
  summarise(sum.votes=sum(votes),
            sum.seats=sum(seats)) %>% 
  mutate(eff.threshold=sum.votes/sum.seats) %>% 
  mutate(eff.threshold.perc=eff.threshold/sum.votes)
threshold


threshold %>% 
  ggplot()+
  geom_line(aes(x=year, y=eff.threshold,
                color=country))




unique(df$election)

# new parties entering ----------------------------------------------------


new.entrants<- df %>% 
  filter(election %in% c("Assembly", "Forum Elections", "House of Representatives")) %>% 
  filter(seats>0) %>% 
  distinct(country, year, party.collapse, ethnicity) %>% 
  group_by(country, ethnicity, year) %>% 
  arrange(year, .by_group=TRUE) %>% 
  group_nest() %>% 
  mutate(n.parties=map(data, pluck, "party.collapse") %>% map(length) %>% flatten_dbl(.)) %>% 
  group_by(country, ethnicity) %>% 
  mutate(data.lag=lag(data, order_by=year)) %>% 
  mutate(different=map2(data, data.lag, safely(dplyr::setdiff))) %>%   #! 1st data.lag, then data => parties which are new entering
  mutate(parties.diff= map(different, pluck,"result","party.collapse") %>% map(length) %>% flatten_dbl(.)) %>% 
  filter(!is.na(data.lag)) %>% 
  mutate(perc.diff=parties.diff/n.parties)

spark <- new.entrants %>% 
  group_by(country, ethnicity) %>% 
  arrange(year, .by_group=TRUE) %>%
  summarise(development=sparkline::spk_chr(parties.diff,
                                     chartRangeMin=0,
                                     chartRangeMax=3,
                                     type="bar")) 

new.entrants %>% 
  filter(!ethnicity %in% c("unknown","nk")) %>% 
  select(country, ethnicity, year, value=parties.diff) %>% 
  mutate(indicator="# parties newly entering Assembly") %>% 
  spread(key=year, value=value) %>% 
  left_join(.,spark, by=c("country", "ethnicity")) %>% 
  select(indicator, country, everything()) %>% 
  mutate_if(is.numeric, as.character) %>% 
  replace(is.na(.),"")  %>% 
  ungroup() %>% 
  arrange(country) %>% 
  mutate(country=case_when(country==lag(country) ~"",
                           TRUE ~ as.character(country))) %>% 
  mutate(indicator=case_when(indicator==lag(indicator) ~ "",
                             TRUE ~ as.character(indicator))) %>% 
  formattable::formattable(align=c(rep("l",3), rep("r",length(unique(new.entrants$year))+1))) %>% 
  as.htmlwidget() %>% 
  spk_add_deps()


x <- list("a","a","a")
length(unique(x))

x<- df %>% 
  filter(seats>0) %>% 
  filter(party!=party.collapse) %>% 
  filter(country=="BIH") %>% 
  distinct(party, party.collapse, year) %>% 
  group_by(party.collapse, party) %>% 
  summarise(years=paste(year, collapse=", ")) %>% 
  mutate(x=paste0(party," [", years, "]")) %>% 
  group_by(party.collapse) %>% 
  summarise(parties.collapsed=paste0(x, collapse=", "),
            length=n()) %>% 
  mutate(parties.collapsed=stringr::str_to_title(parties.collapsed)) %>% 
  filter(length>1) %>% 
  select(-length)

x<- df %>% 
  filter(seats>0) %>% 
  filter(party!=party.collapse) %>% 
  filter(country=="BIH") %>% 
  distinct(party, party.collapse, year) %>% 
  group_by(party) %>% 
  group_nest() %>% 
  mutate(n.years=map(data, pluck, "year") %>% map(max))





  nest() %>% 
  mutate(identical=map(data, pluck, "party") %>%  map(unique) %>% map(length)) %>% 
  filter(identical>1) %>% 
  unnest(data)
  

mutate(n.parties=map(data, pluck, "party.collapse") %>% map(length) %>% flatten_dbl(.)) %>% 
    mutate(party=paste0(party, " [", year, "]")) %>% 
  select(party.collapse, party) %>% 
  summarise(parties=paste(party, collapse=", "))
  
df %>% 
  filter(str_detect(party, "HDU"))

# _ -----------------------------------------------------------------------


# NIRL - constituency resuls ----------------------------------------------

NIRL.const <- readxl::read_xlsx(paste0(wdr,"/data/NIRL/ElectionResults.xlsx"), sheet="Assembly98-17") %>% 
  janitor::remove_empty(.,which=c("cols"))


# > difference between largest parties per constituency (Assembly Elections) ---------------------

NIRL.const %>%
  group_by()
  







# >> plot droop quota -----------------------------------------------------

droop <- NIRL.const %>% 
  group_by(year, constituency) %>% 
  summarise(sum.seats=sum(seats),
            sum.votes=sum(votes)) %>% 
  mutate(quota=sum.votes/sum.seats) %>% 
  mutate(quota.rel=round(quota/sum.votes*100, 2)) %>% 
  distinct(year, quota.rel, sum.seats)


plot.droop<- droop %>% 
  ggplot()+
  geom_line(aes(x=year,
                y=quota.rel))+
  geom_point(aes(x=year, 
                 y=quota.rel))+
  theme_thesis()+
  labs(title="Droop Quota (%)",
       subtitle=stringr::str_wrap("District magnitude reduced from 6 to 5 seats since 2017", 35),
       caption="graph: author\ndata: www.ark.ac.uk/elections")+
  scale_y_continuous(limits=c(0,20), expand=expand_scale(mult=c(0,0.1)),
                     minor_breaks = NULL)+
  scale_x_continuous(breaks=unique(droop$year), minor_breaks = NULL, 
                     labels=stringr::str_sub(unique(droop$year), 3,4))

ggpreview(plot.droop, width=5, height=5, unit="cm")

folder <- paste0(wdr,"/graphs/")
Rfile <- "BiH_NIRL.R"
graphname <- "droop"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=5, height=5, unit="cm", dpi=300)



# Droop Quota -------------------------------------------------------------


# _96-98 ------------------------------------------------------------------
BiH.96_98 <- readxl::read_xlsx(paste0(wdr,"/PartyNat/data/BiH1996-1998.xlsx")) %>% 
  janitor::clean_names() %>% 
  filter(stringr::str_starts(electoral_district, "51|52"))

BiH.96_98 %>% 
  count(electoral_district)



# _02 ---------------------------------------------------------------------


BiH.02 <- readxl::read_xlsx(paste0(wdr,"/PartyNat/data/BiH2002detailed.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(electoral_district=district) %>% 
  filter(stringr::str_starts(electoral_district, "51|52"))

BiH.02 %>% 
  distinct(electoral_district) %>% 
  arrange(electoral_district)

BiH.02 <- BiH.02 %>%
  mutate(votes=redovni+postom+odsustvu+potvrdeni) %>% 
  select(-c(redovni:potvrdeni)) %>% 
  group_by(year, electoral_district) %>% 
  summarise(district.votes=sum(votes),
            district.mandate=sum(mandate))


# _06_10 ------------------------------------------------------------------

BiH.06_10 <- readxl::read_xls(paste0(wdr,"/PartyNat/data/BiHTotal.xls")) %>% 
  janitor::clean_names()

BiH.long <- BiH.const %>% 
  select(-croatian_coalition) %>% 
  gather(key=party, value=votes, -c(year:district_name))

BiH.long %>% 
  distinct(electoral_district) %>% 
  arrange(electoral_district)

BiH.06_10 <- BiH.long %>% 
  group_by(electoral_district, year) %>% 
  summarise(district.votes=sum(votes, na.rm = T)) %>% 
  arrange(year, .by_group=T)
  
BiH.06_10 %>%  ggplot()+
  geom_line(aes(x=year, y=district.votes))+
  facet_wrap(vars(electoral_district))


# _combine ----------------------------------------------------------------

df <- bind_rows(BiH.06_10=BiH.06_10, BiH.02=BiH.02, .id="source") %>% 
  ungroup()

#check completness of observations
df %>% 
  select(year, electoral_district) %>% 
  distinct() %>% 
  mutate(value=1) %>% 
  spread(key=year, value=value)

df %>% 
  group_by(year) %>% 
  summarise(nobs=n())




# ___ ---------------------------------------------------------------------


# [pending] Intra-Segment Volatility --------------------------------------------------------------

all <- df %>% 
  #filter(seats>0) %>%    #only for those parties which had at one point SEATS in Assembly
  group_by(country) %>% 
  expand(party, year)    #creates panel
  

all %>% 
  filter(str_detect(party, "Hrvatska Demokratska Zajednica"))
all %>% 
  filter(str_detect(party, "Alliance"))


df2<- right_join(df, all, by=c("country","year","party"))  #merges panel with dataset 



# >> intra-group vote share -----------------------------------------------
#volatility as variation within group share

volatility <- df2 %>% 
  group_by(country, year, ethnicity) %>%         #based on intra- group voteshare
  mutate(votes.ethn=sum(votes, na.rm=TRUE)) %>% 
  mutate(votes.rel.eth=votes/votes.ethn) %>% 
  group_by(country, ethnicity, party) %>% 
  mutate(votes.rel.eth.lag=lag(votes.rel.eth, order_by = year)) %>% 
  mutate(diff.votes.rel.eth=votes.rel.eth-votes.rel.eth.lag) %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(vol=sum(abs(diff.votes.rel.eth), na.rm=TRUE)/2)

# >> vote share -----------------------------------------------
# volatility based on overall vote share; aggregated per group

volatility <- df2 %>% 
  group_by(country, year) %>%
  mutate(votes.total=sum(votes, na.rm=TRUE)) %>% 
  mutate(vote.share=votes/votes.total) %>% 
  group_by(country, party) %>% 
  mutate(vote.share.lag=lag(vote.share, order_by = year)) %>% 
  mutate(diff.vote.share=vote.share-vote.share.lag) %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(vol=sum(abs(diff.vote.share), na.rm=TRUE)/2)


volatility


  mutate(vote.share=votes/votes.total) %>% 
  group_by(country, party) %>% 
  mutate(vote.share.lag=lag(vote.share, order_by = year)) %>% 
  mutate(diff.vote.share=vote.share-vote.share.lag) %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(vol=sum(abs(diff.vote.share), na.rm=TRUE)/2)





# >>  plot ----------------------------------------------------------------

plot <- volatility %>% 
  group_split(.$country) %>% 
  map(~ggplot(.)+
  geom_point(aes(x=year, y=vol,
                 colour=ethnicity))+
  geom_line(aes(x=year, y=vol,
                colour=ethnicity))+
    geom_text(aes(x=year, y=vol,
                  label=round(vol,2)))+
    scale_x_continuous(limits=c(1996,2018))+
    scale_y_continuous(limits=c(0,.2))+
  facet_wrap(vars(country, ethnicity), labeller=label_wrap_gen(multi_line = FALSE)))

vol_plot<- arrangeGrob(plot[[1]], plot[[2]],  
                        top=textGrob(str_wrap("Intra-segment volatility based on votes", 70), gp=gpar(fontsize=12)))

plot(intra_vol_plot) 


#Forum Elections for NIRL to be added
#what about Croat coalitions?


# Inter-segment Volatility ------------------------------------------------

# >> volatility of group share (SEGMENT STABILITY) -----------------------------------------------

unique(df2$ethnicity)

volatility <- df2 %>% 
  filter(!ethnicity %in% c(NA, "nk")) %>% 
  group_by(country, year) %>%
  mutate(votes.total=sum(votes, na.rm=TRUE)) %>% 
  group_by(country, year, ethnicity, votes.total) %>% 
  summarise(votes.ethnicity=sum(votes)) %>% 
  mutate(ethn.share=votes.ethnicity/votes.total) %>% 
  group_by(country, ethnicity) %>% 
  mutate(ethn.share.lag=lag(ethn.share, order_by = year)) %>% 
  mutate(diff=ethn.share-ethn.share.lag) %>% 
  group_by(country, year) %>% 
  summarise(vol=sum(abs(diff), na.rm=TRUE)/2)


plot <- volatility %>% 
  ggplot()+
        geom_point(aes(x=year, y=vol,
                       colour=country))+
        geom_line(aes(x=year, y=vol,
                      colour=country))+
        geom_text(aes(x=year, y=vol,
                      label=round(vol,2)))+
        scale_x_continuous(limits=c(1996,2018))+
        scale_y_continuous(limits=c(0,.2))
# +
#         facet_wrap(vars(country), labeller=label_wrap_gen(multi_line = FALSE))

plot(plot)



# Size of Segments --------------------------------------------------------


size_segment <- df %>% 
  filter(seats>0) %>% 
  group_by(country, year, ethnicity) %>% 
  summarise(sum.eth=sum(votes)) %>% 
  group_by(country, year) %>% 
  mutate(sum.year=sum(sum.eth)) %>% 
  mutate(eth.share=sum.eth/sum.year) 
  

size_segment %>% 
  ggplot()+
  geom_bar(aes(x=year, y=eth.share,
               group=country,
               fill=ethnicity),
           stat="identity",position=position_stack())+
  facet_wrap(vars(country))


size_segment %>% 
  ggplot()+
  geom_line(aes(x=year, y=eth.share,
                color=ethnicity))+
  facet_wrap(vars(country, ethnicity))
              
              


# Number of parties in Executive ------------------------------------------




