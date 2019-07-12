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
library(hrbrthemes)


#setwd("//fs.univie.ac.at/homedirs/schmidr9/Documents/ro - ceu/R")
#setwd("~/Google Drive/CEU/THESIS/R")
setwd("C:/Users/Roland/Google Drive/CEU/THESIS/R")
#setwd("Z:/Documents/ro - ceu/R")
Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic


restable <- read_excel("Elections/Electoral Results Bosnia.xlsx",sheet="RestTableEdit")

restable <- restable %>%
  filter(keep=="y") %>%
  select(entity, party.original,
         ethnicity, 
         ends_with("seats"),
         ends_with("coalition"),
         -keep) %>%
  gather(year, seats, ends_with("seats"))%>%
  mutate(year = as.numeric(gsub("_.*", "", year)))%>%
  gather(year2,coalition,ends_with("coalition"))%>%
  mutate(year2= as.numeric(gsub("_.*","",year2)))%>%
  mutate(coalition.yes=ifelse(is.na(coalition),"no","yes"))%>%
  filter(year==year2)

#entity.filter <- c("RS")
#entity.filter <- c("Federation")
entity.filter <- c("Federation","RS")


party.filter <- c("SDS","SzBiH","SBB","SDA",
                  "HDZ","HDZ 1990",
                  "SDS","SNSD","SNS","SRS RS", "RS RS",
                  "SDP","DF")
  
for (p in  entity.filter) { 
restable.plot <- restable %>%
  filter(party.original %in% party.filter) %>%
  filter(entity %in% p) %>%
  ggplot(.,aes(year, seats))+
  geom_line()+
  geom_point(aes(color=coalition))+
#  geom_point(subset(restable,coalition.yes=="yes"),aes(year, seat, shape=coalition.yes))+
  labs(x="year",y="seats",
        title=paste0("Seats in House of Representatives"," Entity: ", p),
        subtitle=paste0(entity.filter))+
  scale_x_continuous(breaks=c(1996,1998,2000,2002,2006,2010,2014)) +
  scale_y_continuous(limits = c(0,28))+
#  theme_ipsum_rc(grid="XY")+
  theme_minimal()+
  theme(legend.position="none", panel.grid.minor.x = element_blank())+
  facet_wrap(entity~party.original)

print(restable.plot)
}

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
entity <- paste0("-",entity.filter)
filename <-"-restable.plot.pdf"
ggsave(paste(folder,time,entity,filename, sep=""), width=15, height=7)  

