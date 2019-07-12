#  Load packages ----------------------------------------------------------

list.of.packages <- c("tidyverse","scales","glue","grid","gridExtra","kableExtra",
                      "formattable","sparkline", "drlib", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)

wdr <- getwd() 




# define theme ------------------------------------------------------------

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



# import data -------------------------------------------------------------

CoM <- readxl::read_xlsx(paste0(wdr,"/data/Electoral Results Bosnia.xlsx"), sheet="CoM") %>% 
  mutate(date.end=janitor::excel_numeric_to_date(as.numeric(date.end)),
         date.start=as.Date(date.start)) %>% 
 # mutate(date.end=lubridate::rollback(date.end)) %>% 
  filter(!str_detect(position, "zamjen") | str_detect(position, "zamjenik preds")) %>%  
  mutate(position.orig=position) %>% 
  mutate(position=case_when(!str_detect(position, "zamjenik") & str_detect(position, "ekon") ~ "ministar za vanjsku trgovinu i ekonomske odnose",
                             
                             !str_detect(position, "zamjenik") & str_detect(position, "poslova i komunik") ~ "ministar za civilne poslove i komunikacije",
                             TRUE ~ as.character(position)))
  




# #split entries which cove two offices -----------------------------------

presidents  <- CoM %>% 
  filter(str_detect(position, "preds") & str_detect(position, "i ministar") & !str_detect(position, "zamjenik preds")) %>% 
  mutate(position=str_sub(position, 1,16))

ministers <- CoM %>% 
  filter(str_detect(position, "i ministar") & !str_detect(position, "zamjenik pred")) %>% 
  mutate(position=str_extract(position, "ministar[a-z ]+")) #extracts everything after ministar

ministers2 <- CoM %>% 
  filter(str_detect(position, "zamjenik pred")) %>% 
  mutate(position=case_when(str_detect(position, "ministar vanjskih poslova") ~ "ministar vanjskih poslova",
                            str_detect(position, "sigurno") ~ "ministar sigurnosti",
                            str_detect(position, "finansija") ~ "ministar finansija i trezora",
                            str_detect(position, "ekonom") ~ "ministar  vanjske  trgovine  i  ekonomskih  odnosa",
                            TRUE ~ as.character(position))) %>% 
  filter(!str_detect(position, "zamjenik"))


defense <- CoM %>% 
  filter(position == "ministar odbrane")

civil<- CoM %>% 
  filter(str_detect(position, "civil"))


# add translations ------------------------------------------------------

nrow(CoM) #102
CoM<- CoM %>% 
  filter(!str_detect(position, "i ministar")) %>% #removes double functions
  filter(!(str_detect(position, "ministar") & str_detect(position, "zamjenik"))) %>% 
  bind_rows(., presidents) %>% #adds presidents
  bind_rows(., ministers) %>%  #adds ministries of presidents
  bind_rows(., ministers2) %>% #adds ministerial position of vice presidents
  mutate(position=str_trim(position, side=c("both"))) %>% 
  mutate(position.bih=position) %>% 
  mutate(position=case_when(str_detect(position.bih, "^kopred") ~ "Co-Chair",
                             str_detect(position.bih, "^dopred") ~ "Vice-Chair",
                            str_detect(position.bih, "^pred") ~ "Chair",
                             position.bih=="ministar inostranih poslova" ~ "Foreign Affairs",
                             position.bih=="ministar vanjskih poslova" ~ "Foreign Affairs",
                             position.bih=="ministar civilnih poslova" ~ "Civil Affairs",
                             position.bih=="ministar za civilne poslove i komunikacije" ~ "Civil Affairs",
                             #position.bih=="ministar za vanjsku trgovinu i ekonomske odnose" ~ "Foreign Trade and Economic Relations",
                             str_detect(position.bih, "ekonom") ~ "Foreign Trade and Economic Relations",
                             position.bih=="ministar za ljudska prava i izbjeglice" ~ "Human Rights and Refugees",
                             position.bih=="ministar za evropske integracije" ~ "European Integration",
                             position.bih=="ministar finansija i trezora" ~ "Finance and Treasury",
                             position.bih=="ministar za trezor institucija BiH" ~ "Finance and Treasury",
                             position.bih=="ministar za trezor institucija" ~ "Finance and Treasury",
                             position.bih=="ministar odbrane" ~ "Defense",
                             position.bih=="ministar pravde" ~ "Justice",
                             position.bih=="ministar sigurnosti" ~ "Security",
                             position.bih=="ministar komunikacija i prometa" ~ "Communications and Transport",
                             TRUE ~ as.character(position.bih))) %>% 
  mutate(position=as.factor(position))

nrow(CoM) #105
levels(CoM$position)

CoM %>% 
  group_by(position) %>% 
  summarize(min.d=min(date.start)) %>% 
  arrange(min.d)



CoM <- CoM %>% 
  mutate(position=forcats::fct_relevel(position, "Co-Chair", "Vice-Chair", "Chair",
                                       "Civil Affairs", "Foreign Affairs",
                                       "Foreign Trade and Economic Relations",
                                       "European Integration",
                                       "Finance and Treasury",
                                       "Human Rights and Refugees",
                                       "Justice",
                                       "Communications and Transport",
                                       "Defense",
                                       "Security"))

nrow(CoM) #105

# import election dates -------------------------------------------------

#get election dates
Election.dates <- readxl::read_xlsx(paste0(wdr,"/data/BiH_HoR_consolidated.xlsx")) %>% 
  distinct(date) %>% 
  mutate(year=lubridate::year(date)) %>% 
  #rename(election.date=date) %>% 
  mutate(election.date=as.Date(date))

CoM$period

  CoM <- CoM %>% 
  mutate(end.cylce.year=word(period, -2) %>% parse_number(.)) %>%
  left_join(., Election.dates, by=c("end.cylce.year"="year")) %>% 
  mutate(date.end.old=date.end) %>%
  rename(election.date.end=election.date) #%>%   
  # mutate(date.end=case_when(date.end > election.date.end ~ election.date.end,    #ends teure with elections even if remains in office
  #                            TRUE ~ as.Date(date.end)))

#critical  
  
CoM <- CoM %>% 
  mutate(start.cycle.year=word(period, 2) %>% parse_number(.)) %>% 
  left_join(., Election.dates, by=c("start.cycle.year"="year")) %>% 
  rename(election.date.start=election.date) 
  

df <- CoM %>% 
  select(period, party, position, name) %>% 
  group_by(period) %>% 
  nest() %>% 
  mutate(parties.unique=map(data, "party") %>% map(., unique) %>% map(., paste, collapse=", ") %>% flatten_chr) %>% 
  mutate(parties.unique.n=map(data, "party") %>% map(., unique) %>% map(., length) %>% flatten_int()) %>% 
  mutate(parties.n=map(data, "party") %>% map(., length) %>% flatten_int()) %>% 
  #mutate(SDA.gov=map(data, "party") %>% str_detect(.,"SDA")) %>% 
  mutate(Chairman=map(data, ~ filter(.,str_detect(position,"pred"))) %>% map(.,"name"))
  
CoM %>% 
  group_by(period, party) %>% 
  summarise(nobs=n()) %>% 
  spread(key=party, value=nobs)

library(padr)

df <- CoM %>% 
  filter(!str_detect(position,"zamjenik")) %>% 
  select(period, date.start, date.end, position, party, name) %>% 
  #filter(is.na(date.end))
  filter_at(vars(date.start, date.end), any_vars(!is.na(.))) %>% 
  group_by(period, position) %>% 
  arrange(date.start, .by_group=TRUE) %>% 
  mutate(n.position=row_number()) %>% 
  ungroup() %>% 
  gather(key=interval, value=date, -c(period, position, party, name, n.position)) %>% 
  group_by(period, position, n.position) %>% 
  arrange(date, .by_group=TRUE) 

df.month<- df %>% 
  group_by(period, position, party, n.position) %>% 
  thicken(interval="month", colname="month") %>% 
  padr::pad(group=c("period", "position", "party","name","n.position"), interval="month", by="month") %>% 
  #arrange(month, .by_group=TRUE) %>% 
  fill(party, .direction=c("down")) %>% 
  fill(name, .direction=c("down")) %>% 
  ungroup()

#removes overlaps
df.month <- df.month %>% 
  group_by(position, month) %>% 
  mutate(check=n()) %>% 
  ungroup() %>% 
  mutate(dupes=group_indices(., position, month)) %>% 
  mutate(check2=case_when((!position=="Co-Chair" & check >1 & interval=="date.end") ~ 1,
                          TRUE ~ as.numeric(0))) %>% 
  filter(check2==0) %>% 
  filter(!(month==as.Date("1999-02-01") & interval=="date.end"))
  
library(lubridate)
df.month %>% 
 # filter(month %within% lubridate::interval(ymd("2001-01-01"),ymd("2004-02-01"))) %>% 
  ggplot()+
  geom_bar(aes(x=month,
               fill=position,
               color=position),
           stat="count")+
  scale_y_continuous(breaks=seq(0,10,1), limits=c(0,10))#+
  #facet_wrap(vars(position))

df.x3 <- df.month %>% 
  group_by(position, month) %>% 
  summarise(nobs=n()) %>% 
  group_by(month) %>% 
  rename(var=position,
         value=nobs)

sum <- df.month %>% 
  group_by(month) %>% 
  summarise(sum=n()) %>% 
  gather(key=var, value=value, -month)

df.x4 <- bind_rows(df.x3, sum) %>% 
  spread(key=var, value=value)


df %>% 
  group_by(period, position) %>% 
  slice(1) %>% 
  group_by(period) %>% 
  summarise(n=n())


df.x<- df.month %>% 
  group_by(month) %>% 
  summarise(n=n()) %>% 
  mutate(change=n-lag(n)) %>% 
  filter(!change==0)
  
df.month %>% 
  group_by(month, position) %>% 
  summarise(nobs=n()) %>% 
  ggplot()+
  geom_bar(aes(x=month, y=nobs),
           stat="identity")+
  scale_y_continuous(limits=c(0,3))+
  facet_wrap(vars(position))

x <- right_join(df.month, df.x, by=c("month"))






#continue here
check <- df.month %>% 
  group_by(position, month) %>% 
  mutate(check=n()) %>% 
  ungroup() %>% 
  mutate(check.index=group_indices(.,position, month)) %>% 
  filter(!str_detect(position, "Co-Chair")) %>% 
  filter(check>1) %>% 
  group_by(position, n.position) %>% 
  arrange(position, month, .by_group=TRUE)
#122 obs  



write_excel_csv2(x=df.month, path=paste0(wdr,"/data/",Sys.Date(),"BiH-CouncilOfMinister_monthly.csv"))




df.month.wide <- df.month %>% 
  #filter(period=="RAZDOBLJE 1996. – 1998. godine") %>% 
  group_by(period, month, position) %>% 
  summarise(party=paste0(party, collapse=", ")) %>% 
  ungroup() %>% 
  spread(key=position, value=party)

df.month2 <- df.month %>% 
  group_by(period, month, party) %>% 
  summarise(nobs=n()) %>% 
  spread(key=party, value=nobs) %>% 
  ungroup() 
df.month2

df.month2 %>% 
  group_by_at(vars(-month)) %>%   #groups over all variables except month
  slice(1) %>% 
  group_by(period) %>% 
  arrange(month, .by_group=TRUE) %>% 
  janitor::adorn_totals("col")



# ethnicities -------------------------------------------------------------

Croat <- c("HDZ", "HDZ 1990")
Bosniak <- c("SDA", "SBB", "SBiH")
Serb <- c("SDS","SNSD","PDP","SP")
`multi-ethnic` <- c("DF","SDP")

ethnic.colors <- c("Croat"="steelblue", "Bosniak"="seagreen", "Serb"="firebrick",
                   "multi-ethnic"="orange")

CoM <- CoM %>% 
  mutate(ethnicity=case_when(party %in% Croat ~ "Croat",
                             party %in% Serb ~ "Serb",
                             party %in% Bosniak ~ "Bosniak",
                             TRUE ~ as.character("multi-ethnic"))) %>% 
  mutate(ethnicity=forcats::fct_relevel(ethnicity, "Bosniak", "Serb", "Croat", "multi-ethnic"))


levels(CoM$ethnicity)


# _#number of ministries ---------------------------------------------------

check.interval <- interval(ymd("1997-10-01"),ymd("1998-02-01"))

df.month %>% 
  filter(month %within% check.interval) %>% 
  group_by(month) %>% 
  summarise(n.ministeres=n()) %>% 
  ggplot()+
  geom_step(aes(x=month, y=n.ministeres))+
  scale_y_continuous(breaks=seq(0,10,1))


df.check <- df.month %>% 
  filter(month %within% check.interval) 

df.check %>% 
  group_by(month) %>% 
  summarise(nobs=n())

df.check %>% 
  select(position, party, name, month) %>% 
  group_by(month, position) %>% 
  summarise(nobs=n()) %>% 
  spread(key=month, value=nobs)


# _ number of different parties in government -----------------------------

df.month %>% 
  group_by(month) %>% 
  summarise(n.parties=n_distinct(party)) %>% 
  ggplot()+
  geom_step(aes(x=month, y=n.parties),
            color="orange")+
  scale_y_continuous(breaks=seq(0,10,1))+
  hrbrthemes::theme_ft_rc()
  



# _composition of CoM -----------------------------------------------------

df.month %>% 
  #group_by(month) %>% 
  ggplot()+
  geom_bar(aes(x=month, 
               fill=party,
               color=party),
           stat="count", position=position_stack())



# _ line graph ------------------------------------------------------------

levels(CoM$position)


CoM %>% 
  # mutate(position=stringr::str_replace_all(position, "and", "and\n")) %>% 
  # mutate(position=as.factor(position)) %>% 
  ggplot(.,aes(x=date.start, 
               xend=date.end,
               y=drlib::reorder_within(party, desc(date.start), within=position), #descening work within reorder_within
               yend=drlib::reorder_within(party, desc(date.start), within=position),
               color=ethnicity),
               alpha=1)+
  geom_segment(size=4)+

  #ggplot()+
  # geom_segment(aes(x=date.start, xend=date.end,
  #                  y=party, yend=party,
  #                  color=ethnicity),
  #              size=4,
  #              alpha=1)+

 #  geom_text(aes(x=date.start+1,
 #                y=party,
 #                label=party),
 #            hjust=0,
 #            size=3,
 #            color="grey20")+
  #hrbrthemes::theme_ipsum_ps()+
  labs(title="Composition of BiH Council of Ministers",
        caption="Data: start/end dates as published in Official Gazette")+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.justification = "right",
        axis.title=element_blank(),
        #panel.grid.major.y = element_line(color="grey50"),
        #panel.grid.minor.y = element_blank(),
        #panel.background = element_blank(),
        strip.text.y = element_text(angle=180, hjust=1,
                                    vjust=1, face=c("bold")),
        strip.background.y = element_rect(fill="white", color="white"),
        strip.placement = "outside")+
  scale_color_manual(values=ethnic.colors, name="Ethnicity")+
  scale_y_reordered()+
  scale_x_date(breaks=unique(Election.dates$election.date),
               label=lubridate::year(unique(Election.dates$election.date)))+
  facet_grid(vars(position), 
             #ncol=1, 
             #strip.position = c("left"),
             shrink=TRUE,
             switch = "y",
             space = "free_y",
             scales="free_y",
             labeller=as_labeller(function(x) str_replace_all(x, "and", "and\n")))+  
  geom_vline(data=Election.dates, aes(xintercept=election.date),
             show.legend=FALSE)+
  geom_vline(aes(xintercept=as.Date("2002-12-01")), color="red")


# gov formation duration --------------------------------------------------

gov.formation1 <- CoM %>% 
  group_by(period) %>% 
  summarise(election.date=min(election.date.start),
            gov.formation.start=min(date.start)) %>% 
  mutate(length.gov.formation=gov.formation.start-election.date) %>% 
  
gov.formation2 <- Election.dates %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  mutate(length.gov.formation=as.Date("2019-02-01") - election.date)


gov.formation <- bind_rows(gov.formation1, gov.formation2) %>% 
  select(-c(period, gov.formation.start, date, year)) %>% 
  mutate(year=lubridate::year(election.date))


gov.formation %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(year), y=length.gov.formation),
           stat="identity")+
  theme_thesis()+
  coord_flip()


  


# _ alternation --------------------------------------------------------------

vol <- df.month %>% 
  group_by(month, party) %>% 
  summarise(n.seats=n()) %>% 
  mutate(share.seats=n.seats/sum(n.seats)) %>% 
  mutate(total.seats=n()) %>% 
  group_by(party) %>% 
  arrange(month, .by_group=TRUE) %>% 
  mutate(abs.diff.share=abs(share.seats-lag(share.seats, default=0)))
vol

vol %>% 
  select(month, party, n.seats) %>% 
  spread(key=party, value=n.seats)



df.vol<- vol %>% 
  group_by(month) %>% 
  summarise(vol=sum(abs.diff.share)/2,
            composition=paste0(party, "(",n.seats, ")", collapse=", ")) %>% 
  mutate(alternation=case_when(vol*100<50 ~ 100-vol*100,
                               TRUE ~ as.numeric(vol*100))) %>% 
  mutate(formula=case_when(composition==lag(composition, order_by=month) ~ 100))
  
vol
df.vol           

df.vol %>% 
  ungroup() %>% 
  arrange(month) %>% 
  mutate(vol.lag=lag(vol),
         composition.lag=lag(composition)) %>% 
  
  filter(!(vol==vol.lag & composition== composition.lag)) #get number of seats per party into one string


# _access (perc of old parties) -----------------------------------

access <- vol %>% 
  select(month, party, n.seats, share.seats) %>% 
  arrange(month) %>% 
  group_by(month) %>% 
  nest()

access

access <- access %>% 
  mutate(list.parties=map(data, "party")) %>% 
  mutate(prev.parties=accumulate(list.parties, union)) %>% 
  mutate(new.party.ls=map2(list.parties, lag(prev.parties), setdiff)) %>% 
  mutate(new.party.df=map(new.party.ls, enframe, name=NULL, value="party")) %>%
  mutate(share.old.parties=(1-map2(data, new.party.df, inner_join) %>% 
                              map(.,"share.seats") %>% 
                              map(.,sum) %>% 
                              flatten_dbl())*100)

class(access$share.new.party2)



# _ formula ---------------------------------------------------------------

remove_empty <- function(x){
  if(is.list(x)) {
    x %>%
      purrr::discard(rlang::is_na) %>%
      purrr::map(remove_empty)
  } else {
    x
  }
}

formula <- vol %>% 
  select(month, party, share.seats) %>% 
  group_by(month) %>% 
  arrange(month) %>% 
  nest() %>% 
  mutate(prev.govs=map(data, "party")) %>% 
  mutate(prev.govs.c=map2(prev.govs, lag(prev.govs), identical)) %>% 
  mutate(prev.govs=ifelse(prev.govs.c==FALSE, prev.govs, NA)) %>% 
  mutate(prev.govs.list2=map(prev.govs, list)) %>%  #moves elements in list into nested list
  mutate(prev.govs.list=accumulate(prev.govs.list2, append)) %>% #appends nested lists
  mutate(prev.govs.list=map(prev.govs.list, remove_empty)) %>% #removes empty lists
  mutate(lag_prev.govs.list=lag(prev.govs.list)) %>% 
  mutate(overlap=map2(prev.govs.list2, lag_prev.govs.list, ~map2(.x, .y, intersect))) 

formula1<- formula%>%  #nested map2 same effect as modify_depth ! ; x and y have tob be same depth; #check with vec_depth whether same depth
  mutate(overlap.n=map_depth(overlap, 2, length)) %>% 
  mutate(overlap.n2=map(overlap, function(x){map(x, length)})) %>%  #identical to map_depth(overlap, 2, length)
  mutate(overlap.max=map(overlap.n, flatten_dbl) %>% map(., max)) %>% 
  mutate(overlap.df=map_depth(overlap, 2, enframe)) %>% #conversts nested lists to tibbles
  mutate(overlap.df.l=map_depth(overlap.df, 2, ~mutate(.x, l=nrow(.)))) %>% #add length to each nested tibble
  mutate(overlap.df2=map(overlap.df.l, bind_rows, .id="id") %>%  map(., ~mutate(.x,l.max=max(l))) %>% map(.,~filter(.,l.max==l)))# %>%  
  mutate(share.overlap=map2(data, overlap.df2, right_join, by=c("party"="value"))) #%>% 
  #mutate(share.overlap=map2(data, overlap.df2, ~map2(.x, .y, right_join, by=c("party"="value"))))
  #mutate(formula=map(share.overlap, "share.seats") %>% map(., sum))

f2<- formula %>% 
  mutate(overlap.df=map_depth(overlap, 2, enframe, value="party")) %>% 
  mutate(dat=map(data, list)) %>% 
  mutate(overlap.df_j=map2(overlap.df, data, ~map2(.x,.y, right_join, by=c("party"))))
  
a<- head(f2$data)
b <- head(f2$overlap.df[49])

left_join(b, a, by="party")
map2(b, a, left_join, by="party")

a <- map(a, list)
map2(b, a, ~map2(.x, .y, left_join, by="party"))



purrr::vec_depth(f2$overlap.df)  
vec_depth(f2$data)
   

formula %>% 
  select(data, overlap.df2)

overlap <- formula$overlap.df2 %>% 
  bind_rows(.,.id="period")
  
share <- formula$data %>% 
  bind_rows(.,.id="period")

formula.fin<- left_join(overlap, share, by=c("value"="party","period"="period")) %>% 
  mutate(period=as.numeric(period)) %>% 
  group_by(period, id) %>% 
  summarise(parties.overlap=paste(value, collapse=", "),
            sum.share.seats=sum(share.seats)) %>% 
  arrange(desc(sum.share.seats), .by_group=TRUE) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(period)
formula.fin

class(vol$period)

# _ closure ---------------------------------------------------------------

closure <- left_join(df.vol, access %>% select(month, new.party, access=share.old.parties),
                     by="month") #

closure<- closure %>% 
  mutate(new.party.f=map(new.party, paste0, collapse=", ") %>% flatten_chr())
closure





# df <- df
#   mutate(n.gov.ministers=map_chr(gov.parties, length)) %>% 
#   #mutate(n.gov.ministers.char=flatten_chr(n.gov.ministers.list)) %>% 
#   #mutate(gov.parties.unique=map_depth(gov.parties, 2, function(x) unlist(x))) %>% 
#   mutate(gov.parties.unique.n=map(gov.parties, function(x) unique(x))) %>% 
#   mutate(final=map_depth(gov.parties.unique.n, 2, function(x) unlist(x))) %>% 
#   mutate(lag.data=lag(data)) %>% 
#   mutate(party.overlap=map2(data, lag.data, safely(intersect))) %>% 
#   mutate(parties=map(party.overlap, pluck, "result", "party")) %>% 
#   group_by(period) %>% 
#   mutate(parties2=map(parties, unlist, paste(.,collapse=", ")))
#   




# df$gov.parties.unique.n
# class(df$gov.parties.unique.n)
# map_chr(df$gov.parties.unique.n, paste)
# unlist(df$gov.parties.unique.n[[7]])
# unlist(df$gov.parties.unique.n)
# x <- map(df$gov.parties.unique.n, paste0, collapse=", ")  #collapse inside list
# flatten_chr(x)
# 
# map(df$gov.parties.unique.n, as_vector)
     