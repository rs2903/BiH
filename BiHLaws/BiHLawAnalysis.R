# load packages -----------------------------------------------------------
require(ggplot2)
require(plotly)
library(stringi)
require(tidyr)
require(purrr)
require(dplyr)
require(rio)
require(lubridate)
require(readxl)
require(stringdist)
library(xlsx) #
library(reldist)
library(ggthemes)
library(ggalt) #for lollipop charts
library(highcharter)
#theme_set(theme_bw())
theme_set(theme_minimal())
library(tidyquant)
#library(XLConnect)

wdr <- getwd()


# >> load dataset ---------------------------------------------------------

#setwd("C:/Users/Roland/Downloads")

Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic
laws <- readxl::read_xlsx(paste0(wdr, "/data/1997_2014_Laws.xlsx"), sheet="BiHLaws")


# recode type of law ------------------------------------------------------
laws$category[laws$category==1] <- "under consideration"
laws$category[laws$category==2] <- "adopted" 
#laws$category[laws$category==3] does not exist
laws$category[laws$category==4] <- "rejected" #Odbijeni zakoni
laws$category[laws$category==5] <- "disputed" #Povuceni zakoni 
laws$category[laws$category==6] <- "suspended"
laws$category[laws$category==7] <- "expired"
laws$category[laws$category==8] <- "previously adopted"

laws$date.start2 <- as.Date(laws$date.start, "%d.%m.%Y")

#category 3 is unrelated; link leads somewhere else on the page
laws <- laws %>%   filter(category!=3)

# laws$year <- year(laws$date.start2)
# class(laws$year)

laws$year <- as.numeric(strftime(laws$date.start2, format="%Y"))
class(laws$year)

# Plot --------------------------------------------------------------------

laws.bar <- laws %>%
  #filter(area %in% filter.area) %>%
  ggplot(.,aes(date.start2))+
  geom_bar(aes(fill=category)) +
  labs(y="Number of Laws",
       x=NULL,
       title="Number of Laws",
       subtitle="selected categories")+
  theme(legend.position = "none",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_date(date_labels="%Y", date_breaks = "1 year")+
  scale_y_continuous(limits=c(0,10))+
  facet_grid(category~.)#+
  #geom_vline(data=OHR, aes(xintercept=as.numeric(start.date)), linetype=4)+
  #geom_text(data=OHR, aes(label=HR, x=start.date, y=10), size=3 ,angle = 60, hjust = 0)

print(laws.bar)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws.bar.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Yearly summs ------------------------------------------------------------

laws.year <- laws %>%
  group_by(category, year) %>%
  summarise(freq=n()) %>%
  ungroup()

class(laws.year$freq)
laws.year <- as.data.frame(laws.year)

laws.year.bar <- laws.year %>%
  #filter(area %in% filter.area) %>%
  ggplot(.,aes(year, freq))+
  geom_col(aes(fill=category)) +
  labs(y="Number of Laws per year",
       x=NULL,
       title="Number of Laws",
       subtitle="selected categories")+
  theme(legend.position = "bottom",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_continuous(breaks=seq(min(laws.year$year),max(laws.year$year),1)) #+
  # scale_y_continuous(limits=c(0,10))+
  #facet_grid(category~.)#+
#geom_vline(data=OHR, aes(xintercept=as.numeric(start.date)), linetype=4)+
#geom_text(data=OHR, aes(label=HR, x=start.date, y=10), size=3 ,angle = 60, hjust = 0)

print(laws.year.bar)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws.year.bar.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)

# Laws 1996 - 1998 --------------------------------------------------------
laws9698 <- read_excel("graphs.xlsx",sheet="BiHLaws96-98")
names(laws9698) <- c("No","law","date.HoP","date.HoR","session")
library(stringr)
laws9698$date.HoP <- k <- str_extract_all(laws9698$date.HoP, "\\([^()]+\\)")
laws9698$date.HoP <- substring(laws9698$date.HoP, 2, nchar(k)-2)
laws9698$date.HoP <- as.Date(laws9698$date.HoP, "%d.%m.%Y")
class(laws9698$date.HoP)

laws9698$date.HoR <- k <- str_extract_all(laws9698$date.HoR, "\\([^()]+\\)")
laws9698$date.HoR <- substring(laws9698$date.HoR, 2, nchar(k)-2)
laws9698$date.HoR <- as.Date(laws9698$date.HoR, "%d.%m.%Y")
class(laws9698$date.HoR)

laws9698$date <- pmax(laws9698$date.HoP, laws9698$date.HoR)
laws9698$year <- year(laws9698$date)


# Laws 98-2000 ------------------------------------------------------------

laws9800 <- read_excel("graphs.xlsx",sheet="BiHLaws98-00")
names(laws9800) <- c("No","law","date.HoP","date.HoR","session")
library(stringr)
laws9800$date.HoP <- k <- str_extract_all(laws9800$date.HoP, "\\([^()]+\\)")
laws9800$date.HoP <- substring(laws9800$date.HoP, 2, nchar(k)-2)
laws9800$date.HoP <- as.Date(laws9800$date.HoP, "%d.%m.%Y")
class(laws9800$date.HoP)

laws9800$date.HoR <- k <- str_extract_all(laws9800$date.HoR, "\\([^()]+\\)")
laws9800$date.HoR <- substring(laws9800$date.HoR, 2, nchar(k)-2)
laws9800$date.HoR <- as.Date(laws9800$date.HoR, "%d.%m.%Y")
class(laws9800$date.HoR)

laws9800$date <- pmax(laws9800$date.HoP, laws9800$date.HoR)
laws9800$year <- year(laws9800$date)

# Laws 2000-02 ------------------------------------------------------------

laws0002 <- read_excel("graphs.xlsx",sheet="BiHLaws00-02")
names(laws0002) <- c("No","law","date.HoP","date.HoR","session")
library(stringr)
laws0002$date.HoP <- k <- str_extract_all(laws0002$date.HoP, "\\([^()]+\\)")
laws0002$date.HoP <- substring(laws0002$date.HoP, 2, nchar(k)-2)
laws0002$date.HoP <- as.Date(laws0002$date.HoP, "%d.%m.%Y")
class(laws0002$date.HoP)

laws0002$date.HoR <- k <- str_extract_all(laws0002$date.HoR, "\\([^()]+\\)")
laws0002$date.HoR <- substring(laws0002$date.HoR, 2, nchar(k)-2)
laws0002$date.HoR <- as.Date(laws0002$date.HoR, "%d.%m.%Y")
class(laws0002$date.HoR)

laws0002$date <- pmax(laws0002$date.HoP, laws0002$date.HoR)
laws0002$year <- year(laws0002$date)

# Laws 2002-06 ------------------------------------------------------------

laws0206 <- read_excel("graphs.xlsx",sheet="BiHLaws02-06")
names(laws0206) <- c("No","law","date.HoP","date.HoR","session")

library(stringr)
laws0206$date.HoP <- k <- str_extract_all(laws0206$date.HoP, "\\/[^()]+\\/")
laws0206$date.HoP <- substring(laws0206$date.HoP, 2, nchar(k)-2)
laws0206$date.HoP <- as.Date(laws0206$date.HoP, "%d.%m.%Y")
class(laws0206$date.HoP)

laws0206$date.HoR <- k <- str_extract_all(laws0206$date.HoR, "\\/[^()]+\\/")
laws0206$date.HoR <- substring(laws0206$date.HoR, 2, nchar(k)-2)
laws0206$date.HoR <- as.Date(laws0206$date.HoR, "%d.%m.%Y")
class(laws0206$date.HoR)

laws0206$date <- pmax(laws0206$date.HoP, laws0206$date.HoR)
laws0206$year <- year(laws0206$date)


# Laws 96 - 2006 ----------------------------------------------------------

laws9606 <- rbind(laws9698, laws9800, laws0002, laws0206)


# Laws 96 - 2014 monthly --------------------------------------------------
laws0614 <- laws %>%
  select(date.start2, law.title, category)%>%
  rename(date=date.start2,
         law=law.title)

laws9606 <- laws9606 %>%
  select(date, law) %>%
  mutate(category="adopted")

laws9614 <- rbind(laws9606, laws0614)

laws9614 <- laws9614 %>%
  filter(date < "2015-01-01")

# dichotomy passed / not passed / under conisderation ---------------------

adopted <-  c("adopted","previously adopted")

laws9614$category2<- ifelse(laws9614$category %in% adopted, "passed","disputed, rejected, suspended")
laws9614$category2[laws9614$category=="under consideration"] <- "under consideration"

library(zoo)
laws9614.m <- laws9614 %>%
  mutate(yearmonth=as.yearmon(date)) %>%
  group_by(yearmonth, category) %>%
  summarise(n=n())%>%
  ungroup()


# plot laws monthly -------------------------------------------------------

laws.month.bar <- laws9614.m %>%
  #filter(area %in% filter.area) %>%
  ggplot(.,aes(yearmonth, n))+
  
print(laws.month.bar)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws.month.bar.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Plot laws 96 - 14 yearly ------------------------------------------------

laws9614$year <- as.numeric(strftime(laws9614$date, format="%Y"))
class(laws9614$year)

laws9614.y <- laws9614 %>%
  group_by(year, category) %>%
  summarise(freq=n())


laws9614.year.bar <- laws9614.y %>%
  #filter(area %in% filter.area) %>%
  ggplot(.,aes(year, freq))+
  geom_col(aes(fill=category)) +
  labs(y="Number of Laws per year",
       x=NULL,
       title="Number of Laws",
       subtitle="selected categories")+
  theme(legend.position = "bottom",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_continuous(breaks=seq(min(laws9614.y$year),max(laws9614.y$year),1)) #+

print(laws9614.year.bar)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws9614.year.bar.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Plot laws 96 - 14 yearly Passed vs Non-Passed ------------------------------------------------

library(padr)
library(scales)
class(laws9614$year)


# laws9614.y <- laws9614 %>%
#   select(-year)%>%
#   group_by(category2) %>%
#   arrange(date)%>%
#   thicken(interval="year",colname="year", by="date")%>%
#   group_by(category2,year)%>%
#   summarize(freq=n())%>%
#   #ungroup()%>%
#   pad(group=c("category2"), interval="year", start=as.Date(min(laws9614.y$year))) %>%
#   fill_by_value(value=0)

class(laws9614.y$year)

laws9614.y <- laws9614%>%
  group_by(year,category2)%>%
  summarise(freq=n())%>%
  spread(key=category2, value=freq, fill=0, drop=FALSE)%>%
  gather(key=category2, value=freq, 2:4)%>%
  mutate(category2=factor(category2, levels=c("passed", "disputed, rejected, suspended", "under consideration")))

passed.nonpassed.laws9614.year.bar <- laws9614.y %>%
  #filter(area %in% filter.area) %>%
  ggplot(.,aes(year, freq))+
  geom_col(aes(fill=category2), position="dodge") +
  labs(y=NULL,
       x=NULL,
       title="Number of laws per year in BiH Parliament",
       subtitle="",
       caption="Data: www.parlament.ba")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(size=8))+
  guides(fill=guide_legend(keywidth = 2, keyheight = 0.7))+
  scale_y_continuous(limit=c(0,100))+
  geom_vline(xintercept = 1998, linetype="dashed", color="black")+   #Dez 1997 = Bonn Powers
  geom_vline(xintercept = 2006, linetype="dashed", color="black")+
  scale_fill_manual(values=c("#008B00", "#CD2626","#FF8C00"))+
  scale_x_continuous(limit=c(1995, 2015), breaks=seq(1995,2015), labels = paste(substring(as.character((seq(1995,2015))),3)))

#in case x axis is of type date 
 # scale_x_date(limits=as.Date(c('1995-05-01','2015-01-01')), date_breaks="1 year", labels=date_format("%Y"))+
  # geom_vline(xintercept = as.Date('1998-01-01'), linetype="dashed", color="black")+   #Dez 1997 = Bonn Powers
  # geom_vline(xintercept = as.Date('2006-01-01'), linetype="dashed", color="black")

#creates 2 character long label: paste(substring(as.character((seq(1995,2015))),3))
  
print(passed.nonpassed.laws9614.year.bar)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-passed.nonpassed.laws9614.year.bar.png"
ggsave(paste(folder,time,filename, sep=""), width=16, height=7, unit="cm")




# Bar Plot - only passed laws ---------------------------------------------


passed.laws9614.year.bar <- laws9614 %>%
  filter(category %in% c("adopted","previously adopted")) %>%
  group_by(year, category)%>%
  summarise(freq=n())

passed.laws9614.year.bar <- laws9614 %>%
  filter(category %in% c("adopted","previously adopted")) %>%
  group_by(year)%>%
  summarise(freq=n())%>%
  spread(year,freq)

#write.table(passed.laws9614.year.bar, file = "passedlaws.txt", sep = ",", quote = FALSE, row.names = F)

passed.laws9614.year.bar.plot <- passed.laws9614.year.bar %>%
  ggplot(.,aes(year, freq))+
  #geom_col(aes(fill=category))+
  geom_line()+
  labs(title="Number of passed Laws",
       subtitle="adopted and previoiusly adopted laws")+
  theme(legend.position = "none", 
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_continuous(limit=c(1995, 2015), breaks=seq(1995,2015))

print(passed.laws9614.year.bar.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-passed.laws9614.year.bar..plot.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# [pending] Bar Plot - only non-passed laws ---------------------------------------------

# filter.category <-  c("adopted","previously adopted","under consideration")
# 
# not.passed.laws9614.year.bar <- laws9614.y %>%
#   filter(!category %in% filter.category) %>%
#   ggplot(.,aes(year, freq))+
#   geom_col() +
#   #geom_col(aes(fill=category)) +
#   labs(y=NULL,
#        x=NULL,
#        title="Number of not-passed Laws",
#        subtitle="laws which are disputed, rejected, or suspended")+
#   theme(legend.position = "none",
#         panel.grid.major.x=element_blank(),
#         panel.grid.minor.x = element_blank())+
#   scale_fill_brewer(palette="Set1")+
#   #scale_x_continuous(breaks=seq(min(laws9614.y$year),max(laws9614.y$year),1)) #+
#   scale_y_continuous(limit=c(0,100))+
#   scale_x_continuous(limit=c(1995, 2015), breaks=seq(1995,2015))
# 
# print(not.passed.laws9614.year.bar)
# 
# folder <-"graphs/draft/"
# time <- format(Sys.time(),"%Y%m%d-%H%M%S")
# filename <-"-not.passed.laws9614.year.bar.pdf"
# ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Laws per Government -----------------------------------------------------

# >> load government periods ----------------------------------------------

Gov <- read_excel("graphs.xlsx",sheet="BiHgovs")
Gov$interval <- interval(Gov$start, Gov$end)
Gov$name <- paste(Gov$no, Gov$chair, sep = " - ")
Gov$interval2<- paste(strftime(Gov$start, "%y/%m"), strftime(Gov$end, "%y/%m"),sep="-")
class(Gov$interval2)
Gov$name2 <- paste(Gov$name,Gov$interval2,sep="\n")


# >> Gov duration ---------------------------------------------------------

Gov$duration <- difftime(Gov$end, Gov$start, units = "days") 

# >> loop to match government period with date of law ---------------------

laws9614$gov <- as.character("")
for(i in 1:nrow(laws9614)) {
  laws9614$gov[i] <- Gov$name2[laws9614$date[i] %within% Gov$interval]
}
laws9614$gov <- as.factor(laws9614$gov)

laws9614$gov <- factor(laws9614$gov, 
                             levels=c("1 - Bosic/Silajdzic\n97/01-99/02",     
                             "2 - Silajdzic/Mihajlovic\n99/02-00/06",
                             "3 - Tusevljak\n00/06-00/10",           
                             "4 - Raguz\n00/10-01/02",               
                             "5 - Matic\n01/02-01/07",               
                             "6 - Lagumdzija\n01/07-02/03",          
                             "7 - Mikerevic\n02/03-02/12",           
                             "8 - Terzic\n02/12-07/01",              
                             "9 - Spric\n07/01-07/12", 
                             "10 - Spiric\n07/12-12/01",             
                             "11 - Bevanda\n12/01-15/02",
                             "12 - Zvizdic\n15/02-17/05"))  


# >> dichotomy passed vs non-passed laws ----------------------------------

laws9614 <- laws9614 %>%
  mutate(status=case_when(category=="adopted" ~ "passed",    
        category=="previously adopted" ~ "passed",
        category=="rejected" ~ "not passed",
        category=="disputed" ~ "not passed",
        category=="suspended" ~ "not passed",
        category=="under consideration" ~ "under consideration"))
  

# >> sum passed laws per government ---------------------------------------

laws.gov <-  laws9614 %>%
  group_by(gov, status) %>%
  summarise(freq=n())


#  >> plot laws per government ------------------------------------------------------------------------

laws.gov.plot <- laws.gov %>%
  filter(status=="passed")%>%
  ggplot(.,aes(gov, freq))+
  geom_col(aes(),fill="darkblue", position="dodge") +
  labs(y=NULL,
       x=NULL,
       title="Number of Laws",
       subtitle="selected categories")+
  theme_tq()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())
  
print(laws.gov.plot)



# >> Laws per day ---------------------------------------------------------

laws.day <- laws9614 %>%
  filter(status=="passed") %>%
  group_by(gov)%>%
  summarise(n.laws=n())

laws.day<- full_join(laws.day, Gov[c("name2","duration")], by=c("gov"="name2"))

laws.day <- laws.day %>%
          mutate(duration.day=as.numeric(duration),
                 duration.week=duration.day/7,
                 ratio.day=n.laws/duration.day,
                 ratio.week=n.laws/duration.week)

levels(laws9614$gov)

# >> plot laws per week ---------------------------------------------------

laws.day$gov <- factor(laws.day$gov, 
                       levels=c("1 - Bosic/Silajdzic\n97/01-99/02",     
                                "2 - Silajdzic/Mihajlovic\n99/02-00/06",
                                "3 - Tusevljak\n00/06-00/10",           
                                "4 - Raguz\n00/10-01/02",               
                                "5 - Matic\n01/02-01/07",               
                                "6 - Lagumdzija\n01/07-02/03",          
                                "7 - Mikerevic\n02/03-02/12",           
                                "8 - Terzic\n02/12-07/01",              
                                "9 - Spric\n07/01-07/12", 
                                "10 - Spiric\n07/12-12/01",             
                                "11 - Bevanda\n12/01-15/02",
                                "12 - Zvizdic\n15/02-17/05"))  

AllianceOfChange <- c("5 - Matic\n01/02-01/07",
                   "6 - Lagumdzija\n01/07-02/03",
                   "7 - Mikerevic\n02/03-02/12")


#geom_rect over adjacent factor levels on x-axis; 
#https://stackoverflow.com/questions/31381053/changing-the-background-color-of-a-ggplot-chart-with-a-factor-variable?noredirect=1&lq=1
rects <- data.frame(xstart=seq(0.5,11.5,1),xend=seq(1.5,12.5,1),col=levels(laws.day$gov))

laws.week.plot <- laws.day %>%
  filter(gov!="12 - Zvizdic\n15/02-17/05") %>%
  mutate(AoF=ifelse(gov %in% AllianceOfChange, "AoF",""))%>%
  ggplot(.,aes(gov, ratio.week))+
  geom_lollipop(color="#20B2AA", size=2)+
  geom_rect(data=rects[rects$col %in% AllianceOfChange,], aes(xmin=xstart, xmax=xend,
                ymin=0, ymax=Inf),
                fill="grey", alpha=0.3, inherit.aes = FALSE)+
  geom_label(aes(x=5, y=1,label="'Alliance of Change'"), label.size=0, fill="lightgrey",
            size=3, fontface="italic", hjust="left",inherit.aes=FALSE)+
  labs(title="Legislative Output per Government",
       subtitle="Average number of laws passed per week",
       caption="Data: Parlament.ba",
       y="laws per week", x="")+
  theme_minimal()+
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        plot.caption = element_text(size=8),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
 # scale_color_discrete(breaks="AoF",name="", labels=c("Alliance of Change"))+
  scale_y_continuous(limits = c(0,1.2))

print(laws.week.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "BiHLawAnalysis"
plotname <-"laws.week.plot.png"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=9, unit="cm")  



# >> Ratio Passed - Non-Passed Laws ---------------------------------------

passed.ratio <- laws9614 %>%
  group_by(year, category2)%>%
  summarise(freq=n())%>%
  filter(year>2005)%>%
  filter(!year>2014)%>%
  filter(category2!="under consideration")%>%
  spread(category2, freq)%>%
  ungroup()%>%
  mutate(ratio=round(.[[3]]/.[[2]]*100, 2))%>%
  gather("category2","n", 2:4)


# >>> ratio Plot ----------------------------------------------------------




plot.passed.ratio <- passed.ratio %>%
  filter(category2=="ratio")%>%
  ggplot(.,aes(year,n))+
  geom_rect(aes(xmin=-Inf,xmax=+Inf,ymin=0, ymax=100), fill="grey", alpha=0.3)+
  annotate("text",2009, 51, label="more laws are rejected than passed")+
  geom_line()+
  labs(title="Ratio of Passed to Non-Passed Laws",
     subtitle="Number of adoped to disputed, rejected or suspended laws;\nno data prior to 2006 available",
     caption="Data: Parlament.ba",
     y="%", x="")+
  #geom_hline(yintercept=100)+
  theme_minimal()+
  theme(legend.position = "none", 
        plot.caption = element_text(size=8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank())+
   scale_x_continuous(limit=c(2006, 2014), breaks=seq(2006,2014))+
  scale_y_continuous(breaks=seq(0,600,100))

print(plot.passed.ratio)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-plot.passed.ratio.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=9, unit="cm")



# >>> ratio plot 2 - passed vs non-passed ----------------------------------
###
plot.passed.ratio2 <- passed.ratio %>%
  filter(category2!="ratio")%>%
  ggplot(.,aes(year,n))+
  geom_col(aes(fill=category2), position="dodge")+
  labs(y="number of laws",
     x="",
     title="Annual totals of passed and not-passed laws",
     subtitle="no data prior to 2006",
     caption="plot.passed.ratio2"     )+
  theme_tq()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
scale_fill_brewer(palette="Set1")+
scale_x_continuous(breaks=seq(2006,2014))
  
print(plot.passed.ratio2)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-plot.passed.ratio2.png"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# >> Annual Laws vs Annual Bonn Powers  ---------------------------------------------------------

# >>> Bonn Power decisions per year ----------------------------------------

BP <- read_excel("graphs.xlsx",sheet="BonnPowersScrapped")
nrow(BP)
BP$year <- year(BP$date.publish)

# >>> mark lifts of removals ----------------------------------------

BP.removals <-  BP %>%
  filter(area=="Removals and Suspensions from Office") %>%
  mutate(area=ifelse(grepl("lift", decision.name,
                           ignore.case = TRUE, 
                           perl = FALSE, 
                           fixed = FALSE, 
                           useBytes = FALSE), 
                     "Lifted Removals and Suspensions from Office",
                     "Removals and Suspensions from Office"))

BP.nonremovals <- BP %>%
  filter(area!="Removals and Suspensions from Office")

BP <- bind_rows(BP.removals, BP.nonremovals)
nrow(BP)


# >> Bonn Powers per year ---------------------------------------------------------------------

BP.year <- BP %>%
  group_by(year,area)%>%
  summarise(freq=n())%>%
  group_by(year)%>%
  mutate(BP.y=sum(freq[area!="Lifted Removals and Suspensions from Office"]))%>%  #not to include in total!
  spread(key=area,value=freq, fill=0)

# >> Laws per year ---------------------------------------------------------------------

laws.year <- laws9614 %>%
  group_by(year, category2)%>%
  summarise(freq=n())%>%
  spread(key=category2, value=freq, fill=0)%>%
  filter(year<2015)
  
laws.bonn.year <- inner_join(BP.year, laws.year, by="year")

# >> Plot Bonn Powers vs Laws ---------------------------------------------------------------------

laws.bonn.year.plot <- laws.bonn.year %>%
  select(year,BP.y, passed, `disputed, rejected, suspended`) %>%
  gather(key=bonn.laws, value=freq, BP.y, passed,`disputed, rejected, suspended`)%>%
  #filter(bonn.laws!="disputed, rejected, suspended" & year>2005) 
  filter(bonn.laws!="disputed, rejected, suspended"  |
         bonn.laws=="disputed, rejected, suspended" & year > 2005) %>%
  #filter(bonn.laws!="Removals and Suspensions from Office") %>%
  ggplot(.,aes(year,freq))+
  geom_line(aes(color=bonn.laws))+
  labs(y="annual total",
       x="",
       title="Annual total of Bonn Power decisions and passed laws",
       subtitle="excluding decisions to lift previous Bonn Power decisions",
       caption="laws.bonn.year.plot")+
  theme_tq()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
 # scale_fill_brewer(palette="Set1")+
  scale_x_continuous(breaks=seq(1996,2014))+
  scale_color_discrete(labels=c("annual total of Bonn Power decisions", 
                                "annual total of not-passed laws",
                                "annual total of passed laws"))

print(laws.bonn.year.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws.bonn.year.plot.png"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)
  
 
# >> (pending )Scatter Plot Bonn Powers vs Laws ---------------------------------------------------------------------
#Interpetation: there seems to be a lag between Bonn Powers decisions and laws being passed

scatter.laws.bonn <- laws.bonn.year %>%
  select(year,BP.y, passed) %>%
  ggplot(.,aes(BP.y,passed))+
  geom_point()

print(scatter.laws.bonn)
  

# Laws per OHR ------------------------------------------------------------
OHR.mandate <- read_excel("graphs.xlsx",sheet="OHRMandates")
OHR.mandate$interval <- interval(OHR.mandate$start.date,OHR.mandate$end.date)
OHR.mandate$length.w <- as.numeric(round(difftime(OHR.mandate$end.date, OHR.mandate$start.date, units=c("weeks")),0))

# > assign OHR to each law ------------------------------------------------

laws9614$HR <- as.character("")

for(i in 1:nrow(laws9614)) {
  laws9614$HR[i] <- OHR.mandate$HR[laws9614$date[i] %within% OHR.mandate$interval]
}

# > legislative output per OHR, per week ----------------------------------------------

laws.ohr <- laws9614 %>%
  filter(year<2015)%>%
  filter(category2=="passed") %>%
  group_by(HR) %>%
  summarise(laws.n=n())

x <- OHR.mandate %>%
  select(-interval)

laws.ohr <- inner_join(laws.ohr, x, by=c("HR"))
laws.ohr <- laws.ohr %>%
  mutate(laws.w=laws.n/length.w)  

# !! ERROR:  --------------------------------------------------------------
#length.w for inzko is misleading; we have laws only until the end of 2014, but Inzko's tenure is calculated 
#up until May 2017; Bonn Power decisions after 2014 have to be removed; length has to be corrected


laws.ohr$label <- paste(laws.ohr$HR,"\n",format(laws.ohr$start.date, "%y/%m"),"-",format(laws.ohr$end.date, "%y/%m"), sep="")

laws.ohr$label <- factor(laws.ohr$label, 
                          levels=c("Westendorp\n97/06-99/08",
                                   "Petritsch\n99/08-02/05",
                                   "Ashdown\n02/05-06/01",
                                   "Schwarz-Schilling\n06/02-07/06",
                                   "Lajcak\n07/07-09/02",
                                   "Inzko\n09/03-14/12"))

# > graph legislative output per OHR ----------------------------------------------

laws.ohr.plot <- laws.ohr %>%
  select(label, laws.n, laws.w) %>%
  gather(key="laws",value="number",laws.n, laws.w)%>%
  mutate(number=round(number,2),
         laws=case_when(laws=="laws.n" ~ "total",    
                          laws=="laws.w" ~ "avg. week"))%>%
  ggplot(.,aes(label,number))+
  geom_lollipop(aes(color=laws), size=1)+
  labs(title="Passed Laws per HR",
       subtitle="Total and average per week",
       caption="Data: OHR.int, Parlament.ba",
       y="number of laws", x="")+
  theme_minimal()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.title = element_text(size=8),
        plot.caption = element_text(size=8))+
  scale_color_manual(values=c("lightblue","darkblue"))+
  facet_wrap(~laws, scales="free",nrow=2)

print(laws.ohr.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-laws.ohr.plot.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=10, unit="cm")


# Laws and Bonn Power Decisions per OHR per week --------------------------
  
BonnWeek <- read_excel("graphs.xlsx",sheet="BonnWeek", 
                       col_names = TRUE)

Bonn.laws.w <- inner_join(BonnWeek, laws.ohr, by="HR")

Bonn.laws.w.plot <- Bonn.laws.w %>%
  select(label, laws.w, BP.w)%>%
  ggplot(.,aes(BP.w, laws.w, label=label))+
  #geom_point()+
  geom_label(size=2, fill="grey", alpha=0.1)+
  labs(title="Laws and 'Bonn Power' decisions per HR",
       subtitle="Average per week; decisions lifting previous 'Bonn Power' decisions excluded; \nonly passed laws",
       caption="Data: OHR.int, Paralment.ba",
       y="avg. weekly number of laws", x="avg. weekly number of 'Bonn Power' decisions")+
  theme_minimal()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.title = element_text(size=8),
        plot.caption = element_text(size=6))+
  scale_y_continuous(limits=c(0,1.25), breaks=seq(0,1.25, 0.25))+
  scale_x_continuous(limits=c(0,1.75), breaks=seq(0,1.75, 0.25))

print(Bonn.laws.w.plot)


folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-Bonn.laws.w.plot.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=10, unit="cm")
  
  

