# load packages -----------------------------------------------------------
require(ggplot2)
#require(plotly)
require(stringi)
require(tidyr)
require(purrr)
require(dplyr)
#require(rio)
require(lubridate)
require(readxl)
require(stringdist)
library(xlsx)
library(reldist)
library(ggthemes)
library(highcharter)


#setwd("//fs.univie.ac.at/homedirs/schmidr9/Documents/ro - ceu/R")
setwd("C:/Users/Roland/Google Drive/CEU/THESIS/R")
#setwd("~/Google Drive/CEU/THESIS/R")
#setwd("Z:/Documents/ro - ceu/R")
Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic



# file 1996 ---------------------------------------------------------------

res96 <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="HoR 1996")

hor96 <- res96 %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  select(-(source))


# IMPORT FILE 1998 & 2000 -------------------------------------------------------------

#df <- import("Electoral Results Bosnia.xlsx")
res98_00 <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="HoR 1998 2000")
res98_00$seats98 <- as.numeric(res98_00$seats98)

#drop perc since only realte to entity
res98_00 <- subset(res98_00, select = -c(perc98,perc00) )


# 1998 --------------------------------------------------------------------

#only parties which ran in 98
hor98 <- res98_00 %>%
  select(entity,party,type,seats98,abs98) %>%
  filter(type=="Total:") %>%
  select(-(type)) %>%
  filter(abs98>0) %>%
  rename(seats=seats98,
         votes=abs98)

hor98 <- hor98 %>%
  mutate(date=as.Date("1998-9-13", format="%Y-%m-%d"),
         year=year(date))

hor98$seats[is.na(hor98$seats)] <- 0





# 2000 --------------------------------------------------------------------

#only parties which ran in 00
hor00 <- res98_00 %>%
  select(entity,party,type,seats00,abs00) %>%
  filter(type=="Total:") %>%
  select(-(type)) %>%
  filter(abs00>0) %>%
  rename(seats=seats00,
         votes=abs00)

hor00 <- hor00 %>%
  mutate(date=as.Date("2000-11-11", format="%Y-%m-%d"),
         year=year(date))

hor00$seats[is.na(hor00$seats)] <- 0




# 2002 --------------------------------------------------------------------

res02 <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="HoR 2002")

hor02 <- res02 %>%
  select(entity, party, votes, seats, year) %>%
  mutate(date=as.Date("2002-10-5", format="%Y-%m-%d"))

class(hor02$date)



# import 2006 - 2014 ------------------------------------------------------

res06_14 <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="HoR 2006 - 2014")

hor06_14 <- res06_14 %>%
  select(date.election, entity, party, votes, seats) %>%
  mutate(date=as.Date(date.election),
         year=year(date),
         seats=as.numeric(seats)) %>%
  select(-(date.election))

hor06_14$seats[is.na(hor06_14$seats)] <- 0


# bind results  -----------------------------------------------------------

hor <- bind_rows(hor96, hor98, hor00, hor02, hor06_14)

i <- hor %>%
  group_by(year, entity) %>%
  summarise(sum.seats=sum(seats))
i  


# unify names -------------------------------------------------------------
# length(unique(hor$party)) #180 different party names
# p <- as.data.frame(unique(hor$party))
# export(p,"partynames.xlsx")


# >> import df w with standardized names ----------------------------------
#party.names2 from party.nat project

party.names <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="party.names2")

hor <- hor %>%
  rename(party.original=party)

#hor$party.standard <- party.names$name.unified2[match(gsub("\\s","",tolower(hor$party.original)),gsub("\\s","",tolower(party.names$name.original)))]
hor$party.standard <- party.names$name.unified2[amatch(gsub("\\s","",tolower(hor$party.original)),gsub("\\s","",tolower(party.names$name.original)), maxDist = 5)]

length(unique(hor$party.original))
length(unique(hor$party.standard))

d<- hor %>% 
  filter(is.na(hor$party.standard)) %>% #non-matched party names
  filter(seats > 0)

non.matched<- unique(d$party.original)

length(unique(d$party.original))
#write.xlsx(non.matched, "Elections/NonMatchedNames.xlsx")

non.matched.seats <- hor %>%
          filter(seats>0)%>%
          filter(is.na(party.standard))%>%
          select(party.original,party.standard)



# Unique standardized party names with seats ------------------------------

unique.standard.seats <- hor %>%
  filter(seats > 0) %>%
  select(party.standard) %>%
  distinct(party.standard)
class(unique.standard.seats)
unique.standard.seats <- as.data.frame(unique.standard.seats)

# write.xlsx2(unique.standard.seats, "Elections/Electoral Results Bosnia.xlsx",
#             sheetName="UniquePartyNamesSeats",
#             col.names=TRUE,row.names = FALSE,
#             append=TRUE, showNA=FALSE)


# >> Percentage in votes & seats in Entity and State -------------------------------------------

hor <- hor %>%
  group_by(year) %>%
  mutate(votes.perc=round(votes/sum(votes)*100, 2))%>%  #wrong: doesn't consider 1 party in RS & FED in same year
  mutate(seats.perc=round(seats/sum(seats)*100, 2))%>%
  group_by(year, entity) %>%
  mutate(votes.perc.ent=round(votes/sum(votes)*100,2)) %>%
  mutate(seats.perc.ent=round(seats/sum(seats)*100,2))



# FILTER - only parties with at least 1 seat in HoR -----------------------

x <- hor %>% filter(seats>0)
party.filter <- unique(x$party.standard)

# > percentage of votes & abs seats over year entire state -------------------------

year.sums<- hor %>%
  select(year, entity, party.original, party.standard, votes, seats) %>%
  group_by(year, party.standard) %>%
  summarize(votes.annual=sum(votes),
            seats.annual=sum(seats)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(vote.perc.annual=round(votes.annual/sum(votes.annual)*100, 2))

length(unique(year.sums$party.standard))



# >> add ethnicity and coalition to results -------------------------------


# >>> load ethnicity from file --------------------------------------------

eth.coal <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="UniquePartyNamesSeats")


year.sums$ethnicity <- eth.coal$ethnicity[match(year.sums$party.standard,eth.coal$party.standard)]
year.sums$coalition <- eth.coal$coalition[match(year.sums$party.standard,eth.coal$party.standard)]

# Creating Table on Election Results  -------------------------------------

# not based on harmonized party names
#https://stackoverflow.com/questions/24929954/is-it-possible-to-use-spread-on-multiple-columns-in-tidyr-similar-to-dcast
#gather - unite - spread sequence

hor.table <- hor %>%
  filter(seats > 0) %>%
  select(year, entity, party.original, seats, votes.perc.ent) %>%
  gather(key, value, -c(1:3)) %>%
  unite(year.key,year,key)%>%
  spread(year.key,value)

hor.table <- as.data.frame(hor.table) #if not dataframe write.xlsx crashes

class(hor.table)  

# write.xlsx2(hor.table,"Electoral Results Bosnia.xlsx",
#            sheetName="ResultTable",col.names = TRUE,row.names = FALSE,
#            append=TRUE, showNA=FALSE)  



# > plot - annual % and seats ---------------------------------------------


# >>  seats p.a. ----------------------------------------------------------

seats.plot <- year.sums %>%
           filter(party.standard %in% party.filter) %>%
           ggplot(.,aes(year, seats.annual)) +
                  geom_bar(stat="identity",aes(fill=party.standard))+
                  labs(x="year", y="seats",title="Seats in House of Representatives")+
                  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
                  theme_minimal()
print(seats.plot)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"seat.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  


# highchart bar -------------------------------------------------------

wide <- year.sums %>%
  filter(party.standard %in% party.filter) %>%
  select(year,party.standard, seats.annual)%>%
  spread(.,year,seats.annual) %>%
  gather("year","n",2:8)

  #data is spread and gathered; spread creates cells with NA for years without observations
  #transforming the data back with gather does not result in a loss of rows/years with NA
  #NAs are needed as element in list; otherwise data is projected to wrong year

wide.list <- wide %>%
  group_by(name=party.standard)%>%
  do(data=.$n)
  
  #creates list

series <- list_parse(wide.list)

#see http://stackoverflow.com/questions/38093229/multiple-series-in-highcharter-r-stacked-barchart
hc <- highchart() %>%
  hc_chart(type = "column") %>% 
  hc_xAxis(categories=unique(wide$year)) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)) %>% 
  hc_add_series_list(series)
hc  
  #unique for years since otherwise always frist year




# >> votes perc per year ---------------------------------------------------

year.sums %>%
  filter(party.standard %in% party.filter) %>%
  ggplot(.,aes(year, vote.perc.annual)) +
  geom_line(aes(color=party.standard))+
  xlab("year")+ylab("votes %")+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) #+
  theme_minimal()+



# distribution of votes btw entities per party ----------------------------

v <- hor %>%
  select(year, party.standard, votes, entity,seats) %>%
  group_by(year, party.standard) %>%
  mutate(entity.perc=round(votes/sum(votes),2),
        number=max(row_number(party.standard)))

v %>%
  filter(party.standard %in% party.filter) %>%
  ggplot(.,aes(year, seats))+
    geom_bar(stat="identity", aes(fill=entity, color="red"))+
    facet_wrap(~party.standard)+# +
    xlab("year")+ylab("seats in total")+
    scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) 


hor1 <- hor %>%
  filter(party.standard %in% party.filter)
nrow(hor1)

ggplot(hor1, aes(year, votes.perc.ent))+
   # geom_line()+
    geom_point(aes(color=entity, shape=entity))+
    facet_wrap(~party.standard)+
    xlab("year")+ylab("% of votes in entity")+
    scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) 

 
d <- hor1 %>%
  group_by(year, party.standard) %>%
  mutate(number=max(row_number(party.standard)))



# Number of Parties per Ethnic Group --------------------------------------

hor$ethnicity <- eth.coal$ethnicity[match(hor$party.standard,eth.coal$party.standard)]
hor$coalition <- eth.coal$coalition[match(hor$party.standard,eth.coal$party.standard)]
hor$party.abbrev <-  eth.coal$party.abbrev[match(hor$party.standard,eth.coal$party.standard)]

hor$ethnicity <- as.factor(hor$ethnicity)
levels(hor$ethnicity)
hor$ethnicity <-  ordered(hor$ethnicity, c("B","C","S","M","nk"))

#number of parties per ethnic group with a seat in HoR per year
n <- hor %>%
  filter(seats>0)%>%
  group_by(year,ethnicity) %>%
  summarise(n.parties=n_distinct(party.standard),
            n.coalition=n_distinct(party.standard[coalition=="y"], na.rm=TRUE),
            name.parties=paste(party.standard, sep=" ",collapse=";"))

#reordering levels of ethnic groups
n$ethnicity <- as.factor(n$ethnicity)
levels(n$ethnicity)
n$ethnicity <- ordered(n$ethnicity, c("B","C","S","M","nk"))

n.parties.ethnicity.plot <- n %>%
  filter(ethnicity %in% c("B","C","S","M")) %>%
  ggplot(.,aes(year,n.parties))+
  geom_step(aes(color=ethnicity), size=2)+
  labs(x="year",y="seats",
       title=paste0("Number of Parties/Coalitions per Ethnic Group with Seats in the House of Representatives"),
       subtitle="1 coalition = 1 party")+
  scale_x_continuous(breaks=c(1996,1998,2000,2002,2006,2010,2014)) +
  scale_y_continuous(limits = c(0,5))+
  theme_minimal()+
  theme(legend.position="bottom", panel.grid.minor.x = element_blank())+
  facet_grid(ethnicity~.)
  
print(n.parties.ethnicity.plot)  

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"n.parties.ethnicity.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  



### >> differentiation parties/coalitions (STACKED & FACET bar plot) -----------------------------------------------------------

parties.col.facet.ethnicity.plot <- n %>%
  mutate(n.single.party = n.parties-n.coalition) %>%
  gather(key,number, n.single.party,n.coalition) %>%
  ggplot(.,aes(as.factor(year),number,group=ethnicity))+
    geom_bar(stat="identity",aes(fill=key))+
    labs(x="year", y="seats",
         title="Number of parties and coalitions with seats in House of Representatives",
         subtitle="Disaggregated by ethnic affiliation of party/coalition")+
   # scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
    scale_fill_manual(values=c("red","darkblue"), labels=c("coalitions","single parties"))+
    #scale_fill_discrete(labels=c("coalitions","single parties"), values=c("red","darkblue"))+
    theme_minimal()+ 
    theme(legend.position="bottom", legend.title = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())+
    #facet_grid(ethnicity~.)
    facet_grid(.~ethnicity)


print(parties.col.facet.ethnicity.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"parties.col.facet.ethnicity.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  

## >> differentiation parties/coalitions (STACKED & NO FACET bar plot) -----------------------------------------------------------

parties.col.facet.yar.plot <- n %>%
  mutate(n.single.party = n.parties-n.coalition) %>%
  filter(ethnicity %in% c("B","C","S","M")) %>%
  gather(key,number, n.single.party,n.coalition) %>%
  ggplot(.,aes(ethnicity, number))+
  geom_bar(stat="identity",aes(fill=key))+
  labs(y="seats",
        title="Number of parties and coalitions with seats in House of Representatives",
        subtitle="Disaggregated by ethnic affiliation of party/coalition")+
  scale_fill_manual(values=c("red","darkblue"), labels=c("coalitions","single parties"))+
  # #scale_fill_discrete(labels=c("coalitions","single parties"), values=c("red","darkblue"))+
    theme_minimal()+ 
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  facet_grid(~year)

print(parties.col.facet.yar.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"parties.col.facet.yar.plot .pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  


# >> Gini Seats in HOR per Ethic groups ---------------------------------------------------

gini.seats <- hor %>%
  ungroup() %>%
  filter(seats > 0) %>%
  select(year, party.standard,seats,votes,ethnicity)%>%
  group_by(year, ethnicity, party.standard) %>%
  summarize(seats.year=sum(seats)) %>%
  group_by(year, ethnicity) %>%
  summarize(gini.seats=gini(seats.year))

gini.seats.plot <- gini.seats %>%
  ggplot(.,aes(year,gini.seats))+
  geom_point(aes(color=ethnicity), size=3)+
  geom_step(aes(color=ethnicity), size=2)+
  labs(y="Gini Coefficient",
       title="Intra-Group Concentration of seats in House of Represenatitves",
       subtitle="Gini Coefficient on seats per ethnic group")+
  # scale_fill_manual(values=c("red","darkblue"), labels=c("coalitions","single parties"))+
  # #scale_fill_discrete(labels=c("coalitions","single parties"), values=c("red","darkblue"))+
  scale_color_manual(values=c("darkgreen","darkblue","darkred","orange","grey"))+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
  theme_minimal()+ 
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  facet_grid(.~ethnicity)

print(gini.seats.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"gini.seats.plot .pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  


#>>  Composition of HoR ------------------------------------------------------

comp.hor.plot <- hor %>%
  select(entity,votes,year,party.standard,ethnicity,coalition,seats)%>%
  filter(seats>0) %>%
  ggplot(.,aes(year,seats))+
  geom_col(aes(fill=ethnicity,color=party.standard))+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
  scale_fill_manual(values=c("darkgreen","darkblue","darkred","orange","grey"))+
  #scale_color_manual(values = c("blacks"))+
  labs(title="Composition of House of Representatives",
       subtitle="categorized by ethnic affiliation",
       caption="Federation: 28 seats; RS: 14 seats")+
  theme_minimal()+ 
  theme(legend.position="none", legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_text(data=subset(hor, party.standard %in% c("Stranka Demokratske Akcije (SDA)")),
            aes(label = party.standard),
            size=2,
            position = position_stack())+
  facet_grid(.~entity, scales="free_y") +coord_flip()


print(comp.hor.plot)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"comp.hor.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  



# >> Composition of HoR - Geom_Area ---------------------------------------

comp.hor.area <- hor %>%
  filter(seats>0)%>%
  select(year,seats, party.standard, ethnicity)%>%
  group_by(year,party.standard,ethnicity)%>%
  summarise(seats.y=sum(seats))%>%
  group_by(year,ethnicity)%>%
  summarise(seats.eth=sum(seats.y))
comp.hor.area$ethnicity <-  ordered(comp.hor.area$ethnicity, c("M","B","C","S"))

comp.hor.area.plot <- comp.hor.area %>%
  ggplot(.,aes(year,seats.eth))+
  geom_area(aes(fill=ethnicity))+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
  scale_fill_manual(values=c("orange","darkgreen","darkblue","darkred","orange"))+
  labs(title="Composition of House of Representatives",
       subtitle="categorized by ethnic affiliation",
       caption="comp.hor.area.plot")+
  theme_minimal()+ 
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

print(comp.hor.area.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"comp.hor.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  
  
  
print(comp.hor.area.plot)
  


# >> Composition of HoR - Tree map -------------------------------------------

# install.packages("devtools")
# library(devtools)
# install_github("wilkox/ggfittext")
# install_github("wilkox/treemapify")
#https://github.com/wilkox/treemapify

library(treemapify)

tree <- hor %>%
  ungroup(entity) %>%
  select(year, party.abbrev, ethnicity, seats, votes.perc, entity)%>%
  filter(seats>0)%>%
  group_by(year,ethnicity,party.abbrev)%>%
  summarise(seats.y=sum(seats))%>%
  ungroup()


tree.plot <- tree %>%
  ggplot(., aes(area=seats.y,
                fill=ethnicity, 
                subgroup=ethnicity, label=paste(party.abbrev," (",seats.y,")",sep="")))+
 geom_treemap()+
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    reflow = T)+
  theme_minimal()+
  scale_fill_manual(values=c("darkgreen","darkblue","darkred","orange","grey"))+
  labs(title="Ethnic segmentation of parties in House of Representative",
       subtitle="intertemporal changes largely within segments")+
  facet_wrap(~year)

print(tree.plot)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"tree.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  

#  >> Election results per Ethnic Group and Entity------------------------------------------------------------------------

require(ggalt)

hor.res <- hor %>%
  select(entity,seats, votes,year, party.standard,coalition,ethnicity, party.abbrev)%>%
  group_by(year, entity) %>%
  mutate(votes.perc.ent=round(votes/sum(votes)*100,2)) %>%
  ungroup()%>%
  filter(votes.perc.ent>1)%>%
  filter(seats>0)%>%
  arrange(year, entity, votes.perc.ent)%>%
  mutate(order=row_number())%>%
  ungroup()
  
hor.res.plot  <- ggplot(hor.res,
         aes(as.factor(order),votes.perc.ent))+
      geom_lollipop(aes(color=ethnicity), size=1, horizontal = FALSE)+
      scale_color_manual(values=c("darkgreen","darkblue","darkred","orange","grey"))+
 scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
 scale_x_discrete(breaks = hor.res$order,labels = hor.res$party.abbrev, expand = c(0.1,.1)) +
 labs(title="Election Results: House of Representatives",
       subtitle="Categorized by ethnic affiliation and entity")+
  theme_minimal()+
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title=element_blank(),
         #       axis.text.x=element_text(angle=90, hjust=5),
        #axis.text.y=element_blank(),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+
  #facet_wrap(year~entity, scales="free",drop=TRUE, space="free")+coord_flip() 
  facet_wrap(year~entity, labeller=label_parsed, scales="free",drop=TRUE, ncol=2)+coord_flip() 

print(hor.res.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"hor.res.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=21, height=29.7, unit="cm")  


#  >> Number of parties  ----------------------
#discrepancy when running on party.original
#run on original; standradization of party names leads to NAs with some parties who did not get seats
library(dplyr)

n.parties <- hor %>%
  group_by(year) %>%
  summarise(n.parties.run=n_distinct(party.original),
            n.parties.seats=n_distinct(party.original[seats>0]))%>%
  ungroup()


# >>> plot number of parties ---------------------------------------------------------------------

plot.n.parties <- n.parties %>%
  gather(parties,number,n.parties.run, n.parties.seats)%>%
  ggplot(.,aes(year,number))+
    geom_line(aes(color=parties),size=2)+
    geom_point(aes(color=parties), size=3)+
    scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
    labs(title="Number of Parties" ,
         subtitle="running for or winning seat for House of Representatives",
         caption="plot.n.parties",
         x="",y="")+
    theme_minimal()+ 
    theme(legend.position="bottom", legend.title = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())+
    scale_y_continuous(limits=c(0,40))
  print(plot.n.parties)


  folder <-"graphs/draft/"
  time <- format(Sys.time(),"%Y%m%d-%H%M%S")
  scriptname <- "BosniaHoR"
  plotname <-"plot.n.parties.png"
  ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=21, height=29.7, unit="cm")    
  
  
  
  
freq.table <- hor %>%
  ungroup()%>%
  select(party.standard, year) %>%
  distinct()%>%
  ungroup()%>%
  count(party.standard)%>%
  arrange(-n)%>%
  ungroup()

  freq.table %<>% 
  group_by(n) %>%
  summarise(n.parties=n_distinct(party.standard),
            names=paste(party.standard, sep=";",collapse=";"))



# >> Inter-Segment volatility ---------------------------------------------

hor <- hor %>% ungroup()  

# >>> inter-seg volatility based on votes [pending]----------------------------------

vol.seg <- hor %>%
    ungroup() %>%
    select(year, ethnicity, votes)%>%
    group_by(ethnicity, year)%>%
    summarise(sum.y.ethno=sum(votes))%>%
    group_by(year)%>%
    mutate(sum.y=sum(sum.y.ethno))%>%
    mutate(ethno.share=round(sum.y.ethno/sum.y*100,2))%>%
    arrange(ethnicity, year) %>%
    group_by(ethnicity) %>%
    mutate(diff.ethno=ethno.share-lag(ethno.share))%>%
    filter(ethnicity %in% c("B","C","S","M")) %>%
    group_by(year)%>%
    summarise(vol.y=sum(abs(diff.ethno))/2) 


# >>> inter-seg volatility based on seats ----------------------------------
  
vol.seg <- hor %>%
    ungroup()%>%
    filter(seats>0)%>%
    select(year, ethnicity, seats) %>%
    group_by(year, ethnicity) %>%
    summarise(y.eth.total=sum(seats))%>%
    group_by(year) %>%
    mutate(y.total=sum(y.eth.total))%>%
    ungroup()%>%
    mutate(share=round(y.eth.total/y.total,2))%>%
    group_by(ethnicity)%>%
    mutate(vol.eth.annual=share-lag(share))%>%
    group_by(year)%>%
    summarise(vol.y=sum(abs(vol.eth.annual))/2)


# >>> inter-seg volatility plot (seats)----------------------------------
  
vol.seg.plot<- vol.seg %>%
  ggplot(.,aes(year,vol.y))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014)) +
  labs(title="Inter-Segment Volatility",
       subtitle="volatility of relative size of Bosniak, Croat, Serb, and mulitethnic/civic electoral segment; based on seats in HoR",
       caption="vol.seg.plot",
       x="",y="")+
  theme_minimal()+ 
  theme(legend.position="none", legend.title = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_y_continuous(limits=c(0,1))
print(vol.seg.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"vol.seg.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=21, height=29.7, unit="cm")  

# >> Intra-Segment Volatility ---------------------------------------------

vol.intra <- hor %>%
  filter(seats>0)%>%
  group_by(ethnicity, year, party.standard)%>%
  summarise(votes.y=sum(votes))%>%
  arrange(year, party.standard) %>%
  group_by(year, ethnicity)%>%
  mutate(sum.y=sum(votes.y))%>%
  mutate(perc=round(votes.y/sum.y*100,2))%>%
  group_by(party.standard)%>%
  mutate(diff.perc=perc-lag(perc))%>%
  group_by(year)%>%
  summarise(vol.y=sum(abs(diff.perc))/2) 






# number of parties running & getting seats-----------------------------------------------

n.parties.entity <- hor %>%
  select(party.original, year, entity, seats, votes) %>%
  group_by(year, entity) %>%
  summarize(parties.all=n_distinct(party.original),
            parties.seats=n_distinct(party.original[seats>0]),
            gini.votes=gini(votes),
            gini.seats=gini(seats))

n.parties.state <- hor %>%
  select(party.original, year, entity, seats, votes) %>%
  group_by(year, party.original) %>%
  summarize(votes.year=sum(votes),
            seats.year=sum(seats)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(parties.all=n_distinct(party.original),
            parties.seats=n_distinct(party.original[seats.year>0]),
            gini.votes=gini(votes.year),
            gini.seats=gini(seats.year))
            
n.parties <- bind_rows(n.parties.state,n.parties.entity)
n.parties$entity[is.na(n.parties$entity)] <- "total"
#number of parties for the state level is calculated not as the 
#sum of fed + rs since one party may run in two entities; avoid double counting

n.parties <- n.parties %>%
  gather(parties,number, c(parties.all,parties.seats))

n.parties.plot <- n.parties %>%
        ggplot(.,aes(year, number, group=entity))+
        geom_line(aes(color=entity))+
        labs(x="year", y="number of parties",
             title="Number of Parties",
             subtitle="Coalitions are counted as parties")+
        scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014))+
        theme_minimal()+  
        theme(legend.position="bottom", panel.grid.minor.x = element_blank())+
        facet_wrap(.~parties, scales = "free")

print(n.parties.plot)
        
folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BosniaHoR"
plotname <-"n.parties.plot.pdf"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=7)  


# vote & seat concentration in entites -------------------------------------------

n.parties %>%
  select(year, entity, gini.votes, gini.seats) %>%
  gather(concentration, number, c(gini.votes, gini.seats)) %>%
  ggplot(.,aes(year, number, group=entity))+
  geom_line(aes(color=entity))+
  xlab("year")+ylab("Gini Coefficient")+
  scale_x_continuous(breaks=c(1996,1998,2000,2002, 2006,2010,2014))+
  facet_grid(concentration~.)





 

         
         




seats$name.original <- seats$party
seats$party <- partynames$name.harmonized[match(seats$party, partynames$name.original)]
length(unique(seats$party))
length(unique(seats$name.original))
p.s<- as.data.frame(unique(seats$party))
colnames(p.s) <- "name"

setdiff(p.l[,1], p.s[,1])
anti_join(p.l, p.s, by="name")

# add Ethnicity to each party ---------------------------------------------

# str(partynames)
# party.ethnicity <- as.data.frame(unique(partynames$name.harmonized))
# colnames(party.ethnicity) <- c("name.harmonized")
# #party.ethnicity<- export(party.ethnicity,"party.ethnicity.xlsx")
# class(party.ethnicity$name.harmonized)
# 
# party.ethnicity.list <- read_excel("party.ethnicity1.xlsx")
# class(party.ethnicity.list$name.harmonized)
# 
# #check whether new parties are in party.ethnicity which were previously not included
# new2 <- anti_join(party.ethnicity, party.ethnicity.list, by="name.harmonized")


#graph
ggplot(data=seats,aes(x=as.factor(year), y=seats, fill=party, label=party))+
  geom_bar(stat="identity") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #geom_bar(stat = "identity", position = "fill")#+
  geom_density(adjust=1.5, position="fill")+
  theme(legend.position="none")
  facet_grid(.~entity)


# Number of Parties in HoR ------------------------------------------------

n.parties <- seats %>%
    group_by(year) %>%
    summarise(n.parties=length(party))
    
  
  ggplot(n.parties, aes(x=as.Date(as.character(year),"%Y"), n.parties, group=1))+
    geom_point()+
    geom_line()+
    xlab("year")+ylab("number of parties in HoR")+
    expand_limits(y=0) +
    scale_x_date(date_breaks="years", date_labels="%Y") 

  #scale_x_continuous(breaks=1996:2014)
class(n.parties$year)


# % Seats SDA, HDZ, SNSD, SDS ---------------------------------------------

u <- seats %>%
  group_by(party, year) %>%
  summarise(party.seats=sum(seats))%>%
  filter(party %in% c("SDA", "HDZ", "SDS", "SNSD"))
class(u)
u <- as.data.frame(u)
dput(u)

ggplot(u, aes(x=as.Date(as.character(year), "%Y"), party.seats, group=party, color=party))+
  geom_point()+
  geom_step()+
  xlab("year")+ylab("seats in HoR")+
  scale_x_date(date_breaks="years", date_labels="%Y") #+


# > joint share of seats --------------------------------------------------------


ggplot(u, aes(x=as.Date(as.character(year), "%Y"), y = party.seats, fill = party)) + 
    geom_area(position = "stack")+
    xlab("year")+ylab("seats in HoR")+
    scale_x_date(date_breaks="years", date_labels="%Y") 

  #https://www.safaribooksonline.com/library/view/r-graphics-cookbook/9781449363086/ch04.html
  #http://stackoverflow.com/questions/29807080/geom-area-not-stacking-ggplot
  #problem: lines run as if seats would in/decrease during legislative period
  d  <- as.data.frame(xtabs(party.seats~year+party, u))
  ggplot(d,aes(x=as.Date(as.character(year), "%Y"), y = Freq, fill = party)) + 
    #geom_bar(position="stack")+
    geom_area(position = "stack")+
    xlab("year")+ylab("seats in HoR")+
    scale_x_date(date_breaks="years", date_labels="%Y") 

