# load packages -----------------------------------------------------------
library(tidyverse)


# BONN POWERS -------------------------------------------------------------

# >> load dataset ---------------------------------------------------------
wdr <- getwd()
Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic
Bonn <- read_excel("/data/graphs.xlsx",sheet="BonnPower")


# > manipulate data -------------------------------------------------------
B <-  Bonn %>%
  mutate(Total=rowSums(.[-1]))

  

# Plot --------------------------------------------------------------------

filter.type <-  c("Removals and suspensions from office",
                  "Decisions relating to state symbols, statelevel matters and constitutional issues")

filter.type <-  c("Total")


#names(B)

# Bar Plot ----------------------------------------------------------------

bonn.bar <- B %>%
  gather("type","frequency",2:10)%>%
  filter(!type %in% filter.type) %>%
  ggplot(.,aes(Year,frequency))+
         geom_col(position="fill", aes(fill=type))+
        # geom_bar(position="dodge",stat="identity", aes(fill="type"))+
         labs(y="Number of Bonn Power decisions",
              title="Number of Bonn Power decisions per year",
              subtitle="selected categories")+
  theme(legend.position="bottom",
        panel.grid.major.x=element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_continuous(breaks=seq(min(B$Year), max(B$Year),1))
print(bonn.bar)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-BonnPowerBar.jpg"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Facet All ---------------------------------------------------------------


# function split lines facet titles ---------------------------------------

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

B$type <- swr(B$type)


# >> Plot -----------------------------------------------------------------

bonn.facet <- B %>%
  gather("type","frequency",2:10)%>%
  filter(type!="Total") %>%
  filter(type!="Decisions relating to individuals indicted for war crimes in the former Yugoslavia") %>%
 # transmute(type=funs(swr(type)))%>%
  ggplot(.,aes(Year,frequency))+
  geom_col(position="dodge", aes(fill=type))+
  labs(x="year", y="Number of Bonn Power decisions",
       title="Number of Bonn Power decisions per year",
       subtitle="selected categories")+
  theme(legend.position="bottom",
        panel.grid.major.x=element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_continuous(breaks=seq(min(B$Year), max(B$Year),1))+
  annotate(geom="text",x=2000, y=0, label="Petritsch")+
  facet_grid(type~.)

print(bonn.facet)
ggsave("graphs/BonnPower-facet.png", width=10, height=5)

  
  












