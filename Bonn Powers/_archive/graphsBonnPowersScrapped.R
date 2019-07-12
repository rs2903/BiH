# load packages -----------------------------------------------------------

require(stringdist)
library(xlsx)
library(reldist)

library(highcharter)
library(tidyquant)

#  Load pacakges ----------------------------------------------------------

list.of.packages <- c("tidyverse","readit","hrbrthemes","skimr","lubridate","XML","UpSetR","nord",
                      "formattable", "webshot", "htmltools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)

# define working directory ------------------------------------------------
wdr <- getwd() 



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



# >> load dataset ---------------------------------------------------------


Sys.setlocale("LC_CTYPE", "russian") #allows displaying party names in cyrillic
Bonn <- readxl::read_excel("data/graphs.xlsx",sheet="BonnPowersScrapped")
nrow(Bonn)
OHR <- readxl::read_excel("data/graphs.xlsx",sheet="OHRMandates")
class(OHR$start.date)
Gov <- readxl::read_excel("data/graphs.xlsx",sheet="BiHgovs")



# Remove Bonn Power Decisions after 31-12-2014 --------------------------
# purpose is to have have same cut-off point as observation period for legislative output

Bonn <- Bonn %>%
  filter(date.publish < "2015-01-01")
  

# Mark decisions which lift bans ------------------------------------------

Bonn.removals <-  Bonn %>%
  filter(area=="Removals and Suspensions from Office") %>%
  mutate(area=ifelse(grepl("lift", decision.name,
                         ignore.case = TRUE, 
                         perl = FALSE, 
                         fixed = FALSE, 
                         useBytes = FALSE), 
                     "Lifted Removals and Suspensions from Office",
                     "Removals and Suspensions from Office"))

Bonn.nonremovals <- Bonn %>%
  filter(area!="Removals and Suspensions from Office")

Bonn <- bind_rows(Bonn.removals, Bonn.nonremovals)
nrow(Bonn)
unique(Bonn$area)
         
# Plot --------------------------------------------------------------------

# >> Filter ---------------------------------------------------------------


filter.area <-  c("Removals and Suspensions from Office",
                  "Lifed Removals and Suspensions from Office",
                  "State Symbols, State-Level Matters and Const. Issues")

Bonn

# Bar Plot ----------------------------------------------------------------

Bonn$date.publish1 <- as.Date(Bonn$date.publish)
class(Bonn$date.publish1)
OHR$start.date <- as.Date(OHR$start.date)
OHR$end.date <- as.Date(OHR$end.date)

bonn.bar <- Bonn %>%
  filter(area %in% filter.area) %>%
  ggplot(.,aes(date.publish1))+
  geom_bar(aes(fill=area)) +
  labs(y="Number of Bonn Power decisions",
        x=NULL,
       title="Number of Bonn Power decisions",
       subtitle="selected categories")+
   theme(legend.position = "none",
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_date(date_labels="%Y", date_breaks = "1 year")+
  scale_y_continuous(limits=c(0,15))+
  facet_grid(area~.)+
  geom_vline(data=OHR, aes(xintercept=as.numeric(start.date)), linetype=4)+
  geom_text(data=OHR, aes(label=HR, x=start.date, y=10), size=3 ,angle = 60, hjust = 0)

print(bonn.bar)

folder <-"graphs/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-BonnPowerBar.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)

# Bonn Powers decisions per year ---------------------------

Bonn$year <- year(Bonn$date.publish1)

Bonn.year.table <- Bonn %>%
  #filter(area %in% filter.area) %>%
  group_by(year, area) %>%
  summarise(freq=n())%>%
  spread(area, freq)
Bonn.year.table[is.na(Bonn.year.table)] <- 0
Bonn.year.table$total=rowSums(Bonn.year.table[,-1])

#write.table(Bonn.year.table, file = "Bonn.year.table .txt", sep = "/", quote = FALSE, row.names = F)

Bonn.year <- Bonn %>%
  group_by(year, area) %>%
  summarise(freq=n())

# >>> Plot all per year ---------------------------------------------------

Bonn.year.plot.all <- Bonn.year %>%
  filter(area %in% c("Lifted Removals and Suspensions from Office",
                     "Removals and Suspensions from Office")) %>%
  spread(key=area, value=freq, fill=0, drop=FALSE)%>%
  gather(key=area, value=freq, -year )%>%
  ggplot(.,aes(year, freq))+
  geom_bar(aes(fill=area),stat="identity",position="dodge")+
  labs(title="'Bonn Power' decisions: Removal and suspension from office",
       caption="Data: OHR.int",
       x=NULL,y=NULL)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(size=6))+
  guides(fill=guide_legend(keywidth = 2, keyheight = 0.7))+
    scale_x_continuous(limit=c(1995, 2015), breaks=seq(1995,2015), labels=substring(as.character(seq(1995,2015)),3))+
  scale_fill_manual(values=c("lightblue","darkblue"))

print(Bonn.year.plot.all)

folder <-"graphs/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-Bonn.year.plot.all.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=9, unit="cm")




# >>> Bar Plot Supsensions ------------------------------------------------------------

unique(Bonn.year$area)
title <- c("'Bonn Power' Decisions") #"Bonn Power decisions pertaining to the removal and suspension from office"
areas.selected <- c("State Symbols, State-Level Matters and Const. Issues","Removals and Suspensions from Office")

class(Bonn.year$year)
Bonn.year.plot <- Bonn.year %>%
  filter(area %in% areas.selected) %>%
  ggplot(.,aes(year, freq))+
  geom_bar(aes(fill=area), stat="identity")+
    labs(title=title,
         subtitle="Selected areas",
         caption="Data: www.OHR.int",
         x=NULL,y=NULL)+
  theme_thesis()+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        # legend.position = c(0,1),
        # legend.justification=c(0,1))+
        legend.title=element_blank(),
        #legend.justification = c(1,1)+
        plot.caption = element_text(size=8),
        legend.position = "bottom")+
  guides(fill=guide_legend(keywidth = 2, keyheight = 0.7))+
  geom_vline(xintercept = 1998, linetype="dashed", color="black")+   #Dez 1997 = Bonn Powers
  geom_vline(xintercept = 2006, linetype="dashed", color="black")+
  scale_fill_manual(values=  c("#20B2AA", "#FFC125"))+
 # scale_y_continuous(limit=c(0,100))+
  scale_x_continuous(limit=c(1995, 2015), breaks=seq(1995,2015),labels = paste(substring(as.character((seq(1995,2015))),3)))+
  scale_y_continuous(limits = c(0,65))

  print(Bonn.year.plot)

  folder <-"graphs/"
  time <- format(Sys.time(),"%Y%m%d-%H%M%S")
  filename <-"-Bonn.year.plot.png"
  ggsave(paste(folder,time,filename, sep=""), width=16, height=7, unit="cm")


library(zoo)
Bonn.year$year <- as.Date(as.yearmon(Bonn.year$year))


# >>> Facet Plot Gov ----------------------------------------------------------
Bonn.year.facet.gov <- Bonn.year %>%
    filter(!area %in% c("Lifted Removals and Suspensions from Office",
                        "Removals and Suspensions from Office",
                        "State Symbols, State-Level Matters and Const. Issues",
                        "Federation, Mostar and H-N Canton",
                        "Judicial Reform","Individuals indicted for War Crimes in the former Yugoslavia")) %>%
    ggplot(.,aes(year, freq))+
    # geom_rect(data=subset(Bonn.year,area %in% c(),
    #             fill = "orange",
    #         aes(xmin = as.Date("1995", "%Y"),xmax = as.Date("2016", "%Y"),
    #             ymin = -Inf,ymax = Inf), alpha = 0.1) +
    #geom_col(fill="darkblue")+
    geom_col()+
    labs(title="Bonn Powers as a means to govern",
         subtitle="Annual number of Bonn Power decisions",
         caption="Bonn.year.facet",
         x=NULL,y=NULL)+
    theme_tq()+
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x = element_blank(),
          # legend.position = c(0,1),
          # legend.justification=c(0,1))+
    #      strip.text = element_text(face="bold", size=10, color="black"),
    #     strip.background = element_rect(fill="lightgrey", colour="black"),
          legend.title=element_blank(),
          legend.position = "none")+
    scale_fill_brewer(palette="Set1")+
    scale_y_continuous(limits=c(0,40))+
    scale_x_date(date_breaks = "1 year", date_labels =  "%y") +
    facet_wrap(~area, ncol=3)
    
print(Bonn.year.facet.gov)

folder <-"graphs/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-Bonn.year.facet.gov.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)

  

# >> Instiutiton building -------------------------------------------------

Bonn.year.facet.inst <- Bonn.year %>%
  filter(area %in% c("State Symbols, State-Level Matters and Const. Issues",
                      "Federation, Mostar and H-N Canton",
                      "Judicial Reform")) %>%
  ggplot(.,aes(year, freq))+
  # geom_rect(data=subset(Bonn.year,area %in% c(),
  #             fill = "orange",
  #         aes(xmin = as.Date("1995", "%Y"),xmax = as.Date("2016", "%Y"),
  #             ymin = -Inf,ymax = Inf), alpha = 0.1) +
  geom_col(fill="darkblue")+
  labs(title="Bonn Powers as a means to govern",
       subtitle="Annual number of Bonn Power decisions",
       caption="Bonn.year.facet",
       x=NULL,y=NULL)+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        # legend.position = c(0,1),
        # legend.justification=c(0,1))+
        strip.text = element_text(face="bold", size=10, color="black"),
        strip.background = element_rect(fill="orange", colour="black"),
        legend.title=element_blank(),
        legend.position = "none")+
  scale_fill_brewer(palette="Set1")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%y") +
  scale_y_continuous(limits=c(0,40))+
  facet_wrap(~area, ncol=3)

print(Bonn.year.facet.inst)

folder <-"graphs/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-Bonn.year.facet.inst.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


    


# Bonn Powers decisions per High Representative ---------------------------

OHR$interval <- interval(OHR$start.date,OHR$end.date)
str(OHR$interval)


# >> Loop - assing HR to each Bonn Decision -------------------------------

Bonn$HR <- as.character("")
for(i in 1:nrow(Bonn)) {
Bonn$HR[i] <- OHR$HR[Bonn$date.publish[i] %within% OHR$interval]
}


# >> Graph Bonn Powers decisions per High Representative ---------------------------
Bonn.HR <- Bonn %>%
  group_by(HR, area) %>%
  summarise(freq=n())

Bonn.HR.total <- Bonn %>%
  group_by(HR) %>%
  summarise(freq=n())

# >>> label OHR & date ----------------------------------------------------

OHR$label <- paste(OHR$HR,"\n",format(OHR$start.date, "%y/%m"),"-",format(OHR$end.date, "%y/%m"), sep="")

Bonn.HR$OHR.label <- as.character("")
Bonn.HR$OHR.label <- OHR$label[match(Bonn.HR$HR,OHR$HR)]




  
# >>> graph ---------------------------------------------------------------


HR.sequence <- c(unique(OHR$HR))
HR.sequence <- c(unique(OHR$label))
class(Bonn.HR$freq)

Bonn.HR %>% 
  group_by(HR) %>% 
  mutate(period=str_sub(OHR.label, -11)) %>% 
  mutate(total=sum(freq)) -> Bonn.HR
unique(Bonn.HR$HR)

## Reordering Bonn.HR$HR
Bonn.HR$HR <- factor(Bonn.HR$HR, levels=c("Westendorp", "Petritsch", "Ashdown", "Schwarz-Schilling", "Lajcak", "Inzko"))
## Reordering Bonn.HR$OHR.label
Bonn.HR$OHR.label <- factor(Bonn.HR$OHR.label, levels=c("Westendorp\n97/06-99/08", "Petritsch\n99/08-02/05", "Ashdown\n02/05-06/01", "Schwarz-Schilling\n06/02-07/06", "Lajcak\n07/07-09/02", "Inzko\n09/03-14/12"))


# >>> Formattable ---------------------------------------------------------

Bonn.HR.tab<- Bonn.HR %>% 
  ungroup() 

Bonn.HR.tab %>% 
  select(-c(HR, period, total)) %>% 
  spread(key=OHR.label , value=freq, 
         fill=0) -> Bonn.HR.tab

#add row and column totals
Bonn.HR.tab$Total <- rowSums(Bonn.HR.tab[2:7])
## Reordering Bonn.HR.tab$area
Bonn.HR.tab$area <- factor(Bonn.HR.tab$area, levels=c("Removals and Suspensions from Office", "State Symbols, State-Level Matters and Const. Issues", "Federation, Mostar and H-N Canton", "Judicial Reform", "Individuals indicted for War Crimes in the former Yugoslavia", "Lifted Removals and Suspensions from Office", "Economic Field", "Media Restructuring Decisions", "Property Laws, Return of Displaced Persons and Refugees and Reconciliation", "Total"))

Bonn.HR.tab <- Bonn.HR.tab[order(Bonn.HR.tab$area),]


Bonn.HR.tab %>% 
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) -> Bonn.HR.tab

#304890", "#7890A8", "#90A8C0", "#A8A8A8", 

library(formattable)
allign.columns<- c(rep("l",1),rep("r",7))

tab1<- formattable(Bonn.HR.tab, align=allign.columns, 

             list(area(col=8, row=1:9) ~ color_bar("orange"),
                  area(col=2:7, row=1) ~ color_bar("#304890"),
                  area(col=2:7, row=2) ~ color_bar("#7890A8"),
                  area(col=2:7, row=3) ~ color_bar("#90A8C0"),
                  area(col=2:7, row=10) ~ color_bar("orange"),
             area=formatter("strand",style=style(font.weight="bold"))))
                  
tab1            

class(tab1)



export_formattable(tab1,"tab1.png")

#position_stacK: important to add group= 
Bonn.HR.col <- Bonn.HR %>%
  ggplot()+
  geom_bar(aes(as.factor(OHR.label),
               as.numeric(freq),
               group=area,
               fill=area), stat="identity", position="stack") +
  labs(y=NULL,
       x=NULL,
       title="Number and categories of 'Bonn Power' decisions per High Representative",
       subtitle="",
       caption="graph: author; data: OHR.int")+
  theme_thesis()+
       theme(legend.position = "bottom",
             panel.grid.major.y = element_blank())+
  geom_text(aes(x=as.factor(OHR.label), y=as.numeric(freq), label=freq,
                group=area), 
            position=position_stack(vjust=0.5), size=2)+
  geom_text(data=Bonn.HR %>% group_by(HR) %>% slice(1), 
            aes(label=total, y=total+10, x=as.factor(OHR.label)),
            size=2,
            fontface="italic")+
  scale_fill_nord(palette="baie_mouton", discrete=TRUE)+
  
  # scale_fill_manual(values=c("#BDBDBD", "#CDCD00","#8B475D", "#CD7054", "#CDAD00", "#EE7600", "#1874CD", "#698B22", "#668B8B"),
  #                   name="Area to which decision pertains:", labels=gsub('(.{1,20})(\\s|$)', '\\1\n', unique(Bonn.HR$area)))+

  scale_x_discrete(limits=HR.sequence, expand=expand_scale(mult=c(0,0.1)))+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.1)), minor_breaks = NULL)+
  guides(fill=guide_legend(ncol=2, keywidth = 0.3, keyheight=0.3, reverse = TRUE))+
  coord_flip()
  
print(Bonn.HR.col)
library(ggrepel)

folder <-"graphs/"
#time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-BonnHR.png"
ggsave(paste(folder,time,filename, sep=""), width=16, height=7, unit="cm")




# Bonn Power Decisions / week / per Mandate Holder ------------------------

OHR$length.week <- as.numeric(round(difftime(OHR$end.date, OHR$start.date, units=c("weeks")),0))

Bonn.HR.week <- Bonn.HR %>%
  #filter(area!="Lifted Removals and Suspensions from Office")%>%
  group_by(HR)%>%
  summarise(n=sum(freq))

x <- inner_join(Bonn.HR.week, OHR[,c("length.week", "HR", "label")], by="HR") 
  

class(x$length.week)

Bonn.week <- x %>% 
  select(HR, n, length.week, label) %>%
  mutate(BP.w=n/length.week)%>%
  ungroup()
Bonn.week <- as.data.frame(Bonn.week)

names(Bonn.week) <- c("HR","Number of decisions", "mandate in weeks", "label", "weekly ratio")

Bonn.week <- Bonn.week %>% 
  #select(-label) %>% 
  mutate(`weekly ratio`=round(`weekly ratio`,2)) %>% 
  select(label, everything()) %>% 
  select(-HR) %>% 
  rename(HR=label)

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


allign.columns<- c(rep("l",1),rep("r",3))

tab1<- formattable(Bonn.week, align=allign.columns, 
                   list(area=formatter("HR",style=style(font.weight="bold")),
                        "weekly ratio" = color_bar("orange")),
                   #caption="<h4>Bonn Power decisions per HR and per week</h4>")
                  caption="<span style=font-size:18pt;color:black>Bonn Power decisions per HR and per week</span>")

tab1
export_formattable(tab1,"graphs/Bonn.weekly.png", width="50%",height="5")






#Export so that file can be read by BiHLawAnalysis.R / Laws and Bonn Power Decisions per OHR per week
# write.xlsx2(Bonn.week, "graphs.xlsx",
#              sheetName="BonnWeek",
#              col.names=TRUE,row.names = FALSE,
#              append=TRUE, showNA=FALSE)



# >> plot Bonn Power / week / Mandate Holder ------------------------------

Bonn.week$label <- factor(Bonn.week$label, 
                       levels=c("Westendorp\n97/06-99/08",
                                "Petritsch\n99/08-02/05",
                                "Ashdown\n02/05-06/01",
                                "Schwarz-Schilling\n06/02-07/06",
                                "Lajcak\n07/07-09/02",
                                "Inzko\n09/03-14/12"))
unique(Bonn.week)

Bonn.week.plot <- Bonn.week %>%
  select(label, BP.w) %>%
  ggplot(.,aes(label, BP.w))+
  geom_lollipop(size=2, color="#1874CD")+
  labs(title="'Bonn Power' decisions per OHR, per week",
       subtitle="Average number of decisions passed per week; excluding decisions to lift previous orders",
       caption="Data: OHR.int",
       y="decisions per week", x="")+
  theme_minimal()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(size=8),
        panel.grid.minor.y=element_blank())+
  scale_y_continuous(limits=c(0,2))

print(Bonn.week.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
scriptname <- "graphsBonnPowerScrapped"
plotname <-"Bonn.week.plot.png"
ggsave(paste(folder,time,scriptname,plotname, sep="-"), width=15, height=9, unit="cm")  



# # Joyplot ---------------------------------------------------------------

library(ggjoy)
library(padr)

Bonn.joy <- Bonn %>%
  group_by(area, year, HR)%>%
  summarise(freq=n())%>%
  spread(key=area, value=freq, fill=0, drop=FALSE)%>%
  gather(key=area, value=freq,3:11)

#Bonn.joy <- Bonn

Bonn.joy$phase <- Bonn.joy$area
Bonn.joy$phase[Bonn.joy$area == "Lifted Removals and Suspensions from Office"] <- "Means to sanction"
Bonn.joy$phase[Bonn.joy$area == "Removals and Suspensions from Office"] <- "Means to sanction"
Bonn.joy$phase[Bonn.joy$area == "Economic Field"] <- "Means to govern"
Bonn.joy$phase[Bonn.joy$area == "State Symbols, State-Level Matters and Const. Issues"] <- "Means for state-building and agreement recalibration"
Bonn.joy$phase[Bonn.joy$area == "Judicial Reform"] <- "Means for state-building and agreement recalibration"
Bonn.joy$phase[Bonn.joy$area == "Federation, Mostar and H-N Canton"] <- "Means for state-building and agreement recalibration"
Bonn.joy$phase[Bonn.joy$area == "Media Restructuring Decisions"] <- "Means to govern"
Bonn.joy$phase[Bonn.joy$area == "Property Laws, Return of Displaced Persons and Refugees and Reconciliation"] <- "Means to govern"
Bonn.joy$phase[Bonn.joy$area == "Individuals indicted for War Crimes in the former Yugoslavia"] <- "Means to govern"
Bonn.joy$phase <- factor(Bonn.joy$phase)

wrap_strings <- function(vector_of_strings,width)
{as.character(sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")}))}


## Reordering Bonn.joy$area
Bonn.joy$area <- factor(Bonn.joy$area, levels=c("Removals and Suspensions from Office", "Lifted Removals and Suspensions from Office", "Property Laws, Return of Displaced Persons and Refugees and Reconciliation", "Economic Field", "Media Restructuring Decisions", "Individuals indicted for War Crimes in the former Yugoslavia", "State Symbols, State-Level Matters and Const. Issues", "Judicial Reform", "Federation, Mostar and H-N Canton"))
levels(Bonn.joy$area)

Bonn.joy$area.wrap <- as.factor(wrap_strings(Bonn.joy$area,20))
class(Bonn.joy$area.wrap)
levels(Bonn.joy$area.wrap)

## Reordering Bonn.joy$area.wrap
Bonn.joy$area.wrap <- factor(Bonn.joy$area.wrap, levels=c("Removals and\nSuspensions from\nOffice", "Lifted Removals and\nSuspensions from\nOffice", "Property Laws,\nReturn of Displaced\nPersons and\nRefugees and\nReconciliation", "Economic Field", "Media Restructuring\nDecisions", "Individuals\nindicted for War\nCrimes in the\nformer Yugoslavia", "State Symbols,\nState-Level Matters\nand Const. Issues", "Judicial Reform", "Federation, Mostar\nand H-N Canton"))


comments <- data.frame(stringsAsFactors = TRUE,
                       area.wrap=c("Removals and\nSuspensions from\nOffice", "Lifted Removals and\nSuspensions from\nOffice"),
                       text=c("Attenuation Phase",NA),
                       x=c(2009,NA),
                       y=c(30, NA))


# >> Plot gov -------------------------------------------------------------

#add annoate; instaed of geom_rect
#https://stackoverflow.com/questions/17521438/geom-rect-and-alpha-does-this-work-with-hard-coded-values

plot.gov <- Bonn.joy %>%
  filter(phase=="Means to govern")%>%
  #ggplot(.,aes(year, freq), group=area)+
  ggplot(.,aes(year, freq))+
  #  geom_bar(fill="#CD7054", stat="identity")+
  annotate("rect", xmin=2006,xmax=+Inf,ymin=0, ymax=40, fill="#E2F0F0B3", alpha=0.5)+
  geom_bar(aes(fill=HR), stat="identity", position="stack")+
  labs(y="",
       x="")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.y = element_text(size=6, angle=360),
        axis.text = element_text(size=6),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks=seq(min(Bonn.joy$year),max(Bonn.joy$year),1), 
                     labels = paste(substring(as.character((seq(1997,2014))),3)),
                     limits=c(1997,2014))+
  scale_y_continuous(limits=c(0,40))+
  scale_fill_manual(values=c("#8B475D", "#CD7054", "#EE9A49", "#668B8B", "#B22222", "#36648B"))+
  #facet_grid(wrap_strings(area,20)~phase)
  facet_grid(area.wrap~phase)

  #facet_null(phase~area, ncol=1)
print(plot.gov)


# >> plot recal -----------------------------------------------------------


plot.recal <- Bonn.joy %>%
  filter(phase=="Means for state-building and agreement recalibration")%>%
  ggplot(.,aes(year, freq), group=area)+
  #geom_rect(aes(xmin=2006,xmax=+Inf,ymin=0, ymax=40), fill="#E2F0F0B3")+
  annotate("rect", xmin=2006,xmax=+Inf,ymin=0, ymax=40, fill="#E2F0F0B3", alpha=0.5)+
    #geom_bar(fill="#8B475D", stat="identity")+
  geom_bar(aes(fill=HR), stat="identity", position="stack")+
    labs(y="",
       x="")+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size=7),
        legend.key.height = unit(0.5,"line"),
        legend.key.size=unit(0.5, "cm"),
        panel.grid.major.x = element_blank(),
        strip.text.y = element_text(size=6, angle=360),
        axis.text = element_text(size=6),
        panel.grid.minor.x = element_blank())+
  guides(fill=guide_legend(nrow=1))+
  scale_x_continuous(breaks=seq(min(Bonn.joy$year),max(Bonn.joy$year),1), 
                     labels = paste(substring(as.character((seq(1997,2014))),3)),
                     limits=c(1997,2014))+
  scale_y_continuous(limits=c(0,40))+
  scale_fill_manual(values=c("#8B475D", "#CD7054", "#EE9A49", "#668B8B", "#B22222", "#36648B"),
                    breaks=c("Westendorp","Petritsch", "Ashdown", "Schwarz-Schilling","Lajcak","Inzko"))+
  #facet_grid(wrap_strings(area,20)~phase)
  facet_grid(area.wrap~phase)
print(plot.recal)  


# >> plot sancti ----------------------------------------------------------

plot.sanct <- Bonn.joy %>%
  filter(phase=="Means to sanction")%>%
  ggplot(.,aes(year, freq), group=area)+
  annotate("rect", xmin=2006,xmax=+Inf,ymin=0, ymax=40, fill="#E2F0F0B3", alpha=0.5)+
  #geom_bar(fill="#EE9A49", stat="identity")+
  geom_bar(aes(fill=HR), stat="identity", position="stack")+
  geom_text(data=comments, aes(x=x, y=y,label=text), size=3, fontface="italic")+
  labs(y="",
       x="",
       title="'Bonn Power' decisions as different means of intervention")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.y = element_text(size=6, angle=360),
        axis.text = element_text(size=6),
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks=seq(min(Bonn.joy$year),max(Bonn.joy$year),1), 
                     labels = paste(substring(as.character((seq(1997,2014))),3)),
                     limits=c(1997,2014))+
  scale_y_continuous(limits=c(0,40))+
  scale_fill_manual(values=c("#8B475D", "#CD7054", "#EE9A49", "#668B8B", "#B22222", "#36648B"))+
  #facet_grid(wrap_strings(area,20)~phase)
  facet_grid(area.wrap~phase)

print(plot.sanct)  


# >> combine graph --------------------------------------------------------
library(gridExtra)

library(ggpubr)
combined.graph <- ggarrange(plot.sanct, 
                              plot.gov,
                              plot.recal,
                              heights=c(3,4,4),
                              ncol=1, nrow=3)

print(combined.graph)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-combined.graph.png"
ggsave(paste(folder,time,filename, sep=""), width=16, height=20, unit="cm")




plot(combined.graph)

# 
# print(joy.plot)

joy.plot <- Bonn %>%
  ggplot(.,aes(x=year, y=area, height=..density..))+
  geom_joy(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE)+
  facet_wrap(.~phase)

print(joy.plot)


# Bonn decisions per Government -------------------------------------------

# >> assign Gov to each Bonn decision -------------------------------------

Gov$interval <- interval(Gov$start, Gov$end)
Gov$name <- paste(Gov$no, Gov$chair, sep = "-")
Gov$interval2<- paste(strftime(Gov$start, "%y/%m"), strftime(Gov$end, "%y/%m"),sep="-")
class(Gov$interval2)
Gov$name2 <- paste(Gov$name,Gov$interval2,sep="\n")



#options(warn=1)
Bonn$gov <- as.character("")
for(i in 1:nrow(Bonn)) {
  Bonn$gov[i] <- Gov$name2[Bonn$date.publish[i] %within% Gov$interval]
}

Bonn.gov <- Bonn %>%
  group_by(gov,area) %>%
  summarise(freq=n())

gov.sequence <- c(unique(Gov$name2))

Bonn.gov.col <- Bonn.gov %>%
  ggplot(.,aes(as.factor(gov),freq))+
  geom_col(aes(fill=area)) +
  labs(y="Number of Bonn Power decisions",
       x=NULL,
       title="Bonn Power decisions per Government")+
  theme(legend.position = "bottom",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  scale_x_discrete(limits=gov.sequence)

print(Bonn.gov.col)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-BonnGov.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)


# Graph: Bonn power decisions over OHR and Gov ----------------------------

class(Bonn$gov)

Bonn$govf <- factor(Bonn$gov,
                       levels=c("1-Bosic/Silajdzic\n97/01-99/02",     
                                "2-Silajdzic/Mihajlovic\n99/02-00/06",
                                "3-Tusevljak\n00/06-00/10",           
                                "4-Raguz\n00/10-01/02",               
                                "5-Matic\n01/02-01/07",               
                                "6-Lagumdzija\n01/07-02/03",          
                                "7-Mikerevic\n02/03-02/12",           
                                "8-Terzic\n02/12-07/01",              
                                "9-Spric\n07/01-07/12", 
                                "10-Spiric\n07/12-12/01",             
                                "11-Bevanda\n12/01-15/02"))            

Bonn$HRf <- factor(Bonn$HR, levels=c("Westendorp",
                                    "Petritsch",
                                    "Ashdown",
                                    "Schwarz-Schilling",
                                    "Lajcak",
                                    "Inzko"))

Bonn.Hr.gov <- Bonn %>%
  group_by(HRf, govf, area)%>%
  summarise(freq=n())


Bonn.HR.gov.plot <- Bonn.Hr.gov  %>%
  ggplot(.,aes(area, freq))+
  geom_bar(stat="identity", aes(fill=area))+
  facet_grid(HRf~govf)+
  labs(y="Number of Bonn Power decisions",
       x=NULL,
       title="Bonn Power decisions per Government")+
  theme(legend.position = "bottom",
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())+
  scale_fill_brewer(palette="Set1")
print(Bonn.HR.gov.plot)
  
folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-Bonn.HR.gov.plot.pdf"
ggsave(paste(folder,time,filename, sep=""), width=20, height=)







ix <- interval("2004-06-01","2004-12-31")

d<- Bonn %>%
  filter(date.publish1 %within% ix) %>%
  group_by(area) %>%
  summarise(freq=n())



