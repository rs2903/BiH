# load packages -----------------------------------------------------------
require(ggplot2)
require(plotly)
require(stringi)
require(tidyr)
require(purrr)
require(dplyr)
require(rio)
require(lubridate)
require(readxl)
require(stringdist)
library(xlsx)
library(reldist)
library(ggthemes)
library(highcharter)


#setwd("//fs.univie.ac.at/homedirs/schmidr9/Documents/ro - ceu")
#setwd("~/Google Drive/CEU/THESIS/R")
setwd("Z:/Documents/ro - ceu/R")
Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic

rs.pres<- read_excel("Electoral Results Bosnia.xlsx",sheet="RSPres")
str(rs.pres)


# > Plot - distributio of votes per candidate per election -------------------------

#gives an idea how fragmented and contested the seat for the
#presidency was; number of parties & distance between candiates

rs.pres %>%
  # group_by(slot, year) %>%
  arrange(year, votes) %>%
  mutate(order=row_number()) %>%
  ggplot(.,aes(order, votes))+ 
  facet_wrap(~year, scales="free_x", drop=TRUE)+
  geom_bar(stat="identity")+
  xlab("party")+ylab("votes")+
 #scale_x_continuous(breaks=c(1996,1998,2002,2006,2007,2010,2014)) +
  theme_minimal()+
 # scale_fill_manual(values=c("darkgreen", "blue", "darkred"))+
  scale_x_continuous(
    breaks = order,
    #  labels = party,
    expand = c(0,0))

# >> interpretation -------------------------------------------------------
#presidency seat is getting more and more contested; gr
# 2002 boycot? - very few votes casted


# > number of parties for presidency------------------------

rs.pres %>%
  group_by(year) %>%
  #summarise(n.parties=row_number(votes)) %>%
  filter(!is.na(votes))%>% #since na for years without results are counted as 1 party
  summarise(n.parties=max(length(unique(party))))%>%
  # spread(.,year,n.parties) %>%
  # gather("year","n.parties",2:7) %>%
  ggplot(.,aes(year,n.parties))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_continuous(breaks=c(1996,1998,2002,2006,2007,2010,2014)) +
  theme_minimal()


# > Plot - Gini  ----------------------------------------------------------
# gini per slot & election: how concentrated are the votes = how dominant is one candidate/party

rs.pres %>%
  group_by(year) %>%
  summarise(gini.pres=gini(votes)) %>%
  ggplot(.,aes(year,gini.pres))+
  #geom_line(aes(color=slot))+
  geom_point()+
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  scale_x_continuous(breaks=c(1996,1998,2002,2006,2007,2010,2014)) +
  ggtitle("Concentration of Votes for Presidency per Seat (Gini coefficient)")

# > Plot - difference between frist and second ------------------------------
# in % of votes and absolute

diff.votes<- rs.pres %>%
  group_by(year)  %>%
  arrange(year, desc(votes))

%>%
  mutate(rank=row_number(desc(votes)))%>%
  filter(rank<3)%>%
  summarise(leaders=paste(party, collapse=" -\n"), 
            vote.diff=diff(votes))

ggplot(diff.votes,aes(as.factor(year),vote.diff*-1, label=leaders))+
  geom_bar(stat="identity", position="dodge")+
  geom_label(size=3, aes(y=200))+
  #scale_x_continuous(breaks=c(1996,1998,2002,2006,2010,2014))+
  # scale_y_continuous(trans="identity")+
 # scale_y_continuous(labels = function(x) format(x/10000))+
  #expand_limits(y=c(0,10000))+
  theme_minimal()+
  ggtitle("Difference of votes btw 1st and 2nd presidential candidate (10,000)")+
  coord_flip()
