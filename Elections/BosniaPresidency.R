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
library(ggalt)

#setwd("//fs.univie.ac.at/homedirs/schmidr9/Documents/ro - ceu")
#setwd("~/Google Drive/CEU/THESIS/R")
setwd("Z:/Documents/ro - ceu/R")
Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic

pres.res <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="Presidency")
str(pres.res)


# votes cast at presidential elections ------------------------------------

#what is this good for?

pres.res %>%
  ggplot(.,aes(year, votes))+ 
  geom_bar(stat="identity",aes(fill=slot))+
  xlab("year")+ylab("votes")+
  scale_x_continuous(breaks=c(1996,1998,2002,2006,2010,2014)) +
  theme_minimal()#+
#scale_fill_ptol()

str(pres.res)


# > Plot - distributio of votes per slot per election -------------------------

#gives an idea how fragmented and contested the seat for the
#presidency was; number of parties & distance between candiates

pres.res %>%
  # group_by(slot, year) %>%
  arrange(year, slot, votes) %>%
  mutate(order=row_number()) %>%
  ggplot(.,aes(order, votes))+ 
  facet_grid(slot~year, scales="free_x", drop=TRUE)+
  geom_bar(stat="identity",aes(fill=slot))+
  xlab("party")+ylab("votes")+
#  scale_x_continuous(breaks=c(1996,1998,2002,2006,2010,2014)) +
  theme_minimal()+
  scale_fill_manual(values=c("darkgreen", "blue", "darkred"))+
  scale_x_continuous(
    breaks = order,
  #  labels = party,
    expand = c(0,0))
  #  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='gray50'))

# > Plot 2- distributio of votes per slot per election -------------------------
library(scales) 

pres.res <- pres.res %>%
  group_by(slot, year)%>%
  arrange(slot, year, votes)%>%
  mutate(order=row_number())

pres.plot2 <- pres.res%>%
  mutate(party=factor(party))%>%
  ggplot(.,aes(order,votes/1000))+
  geom_bar(aes(fill=party),stat="identity", position="dodge")+
  #geom_lollipop(aes(color=slot), size=1)+
  facet_wrap(~slot+year, labeller=label_parsed,scales="free_x", nrow=3)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(0,max(pres.res$votes/1000)))
  
print(pres.plot2)

class(pres.res$party)

# > number of parties for presidency per ethnicity ------------------------

pres.res %>%
  group_by(year, slot) %>%
  summarise(n.parties=max(length(unique(party)))) %>%
  spread(.,year,n.parties) %>%
  gather("year","n.parties",2:7) %>%
  ggplot(.,aes(year,n.parties, group=slot))+
  geom_bar(stat="identity", position="dodge", aes(fill=slot))+
  scale_fill_manual(values=c("darkgreen", "darkblue", "darkred"))+
  theme_minimal()
  

# > Plot - Gini  ----------------------------------------------------------
# gini per slot & election: how concentrated are the votes = how dominant is one candidate/party

pres.res %>%
  group_by(year, slot) %>%
  summarise(gini.pres=gini(votes)) %>%
  spread(.,year,gini.pres) %>%
  gather("year","gini.pres",2:7) %>%
  ggplot(.,aes(year,gini.pres, group=slot))+
  #geom_line(aes(color=slot))+
  geom_point(aes(color=slot))+
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  ggtitle("Concentration of Votes for Presidency per Seat (Gini coefficient)")
  
# > Plot - difference between frist and second ------------------------------
# in % of votes and absolute

diff.votes<- pres.res %>%
    group_by(year, slot)  %>%
    arrange(year, slot, desc(votes))%>%
    mutate(rank=row_number(desc(votes)))%>%
    filter(rank<3)%>%
    summarise(leaders=paste(party, collapse=" vs.\n"), 
              vote.diff=diff(votes))%>%
    ungroup()

diff.votes.df<-as.data.frame(diff.votes)

library(xlsx) #load the package
write.xlsx(x = diff.votes.df, file = "Elections/Electoral Results Bosnia.xlsx",
           sheetName = "PresidencyDiffVotes", row.names = TRUE, append=TRUE)

        
pres.vote.diff.plot<- ggplot(diff.votes,aes(as.factor(year),vote.diff*-1, group=slot, label=leaders))+
                  geom_bar(stat="identity", position="dodge", aes(fill=slot))+
                  geom_label(size=2, aes(y=750000))+
                  scale_fill_manual(values=c("darkgreen", "darkblue", "darkred"))+
                   #scale_x_continuous(breaks=c(1996,1998,2002,2006,2010,2014))+
                   # scale_y_continuous(trans="identity")+
                  scale_y_continuous(labels = function(x) format(x/10000))+
                  expand_limits(y=c(0,1000000))+
                  theme_minimal()+
                  theme(legend.position = "none",
                        plot.caption = element_text(size=8),
                        axis.title = element_text(size=8))+
                  labs(subtitle="Difference of votes between 1st and 2nd presidential candidate (10,000)",
                       title="Increasing contestation in presidential elections",
                    x="year", y="lead in 10,000 votes")+
                  facet_grid(.~slot)+
                  coord_flip()

print(pres.vote.diff.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-pres.vote.diff.plot.jpg"
ggsave(paste(folder,time,filename, sep=""), width=25, height=10, unit="cm")



#beware of group_by followed by arrange; arrang requires repetition
#of grouping variables! See https://github.com/hadley/dplyr/issues/1206
#comment by onurfiliz; also
#https://rpubs.com/Instantkaffee/192066





