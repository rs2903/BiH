library(wbstats)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(tidyquant)

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

# Economic development  -------------------------------------------------------------

#https://cran.r-project.org/web/packages/wbstats/vignettes/Using_the_wbstats_package.html


search_vars <- wbsearch(pattern = "unemployment")

BiH <- wbsearch(country="BIH")

vars <- c("SL.UEM.TOTL.ZS",      #Unemployment, total (% of total labor force) (modeled ILO estimate)
          "SL.UEM.1524.ZS",      #Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)
    #       "NY.GNP.PCAP.PP.KD",   #GNI per capita, PPP (constant 2011 international $)
         # "SI.SPR.PCAP.05", 
     #     "SI.SPR.PCAP",       
     #     "NY.GDP.MKTP.PP.CD",   #GDP, PPP (current international $)
         # "SI.POV.5DAY",
          "SI.POV.NAPR.ZS",     #Poverty Rate (in % of population),
         # "SL.EMP.TOTL.SP.ZS",   #Employment to population ratio, 15+, total (%) (modeled ILO estimate)
     #      "5.51.01.10.gdp",      #Per capita GDP growth
          "NY.GDP.PCAP.PP.KD")#,   #GDP per capita, PPP (constant 2011 international $)
    #"GB.XPC.WAGE.CN")
    

econ <- wb(indicator = vars,
           country="BIH",
           startdate = 1995, enddate = 2019,
           POSIXct=TRUE)

EU <- c("Ireland","United Kingdom","Belgium","Netherlands",
        "France", "Spain","Portugal","Italy,","Slovenia","Croatia",
        "Hungary","Bulgaria","Romania", "Estonia","Latvia","Poland", "Greece",
        "Slovakia","Czech Republic","Luxembourg","Austria","Germany","Denmark","Sweden","Finland","Lithuania")

Balks <- c("Serbia","Macedonia","Albania","Montenegro")

EU_isoc3<- countrycode::countrycode(EU, origin="country.name", destination="iso3c")
Balks<- countrycode::countrycode(Balks, origin="country.name", destination="iso3c")

econ <- wb(indicator = vars,
               country=c("BIH",EU_isoc3,Balks),
               startdate = 1995, enddate = 2019,
               POSIXct=TRUE)


class(econ)
glimpse(econ)

unique(econ$indicator)

plot_gdp<- econ %>% 
  mutate(fill.col=case_when(iso3c=="BIH" ~ "BiH",
                            iso3c %in% Balks ~"Western Balkans",
                             TRUE ~ "EU")) %>% 
  filter(date=="2017") %>% 
  filter(stringr::str_detect(indicator,"GDP"))%>% 
  ggplot()+
  geom_bar(aes(x=reorder(iso2c, -value), y=value, 
               fill=fill.col),
           stat="identity")+
  labs(title="2017 GDP per capita, PPP \n(constant 2011 international $)",
       caption="data:World Bank")+
  theme_thesis()+
  theme(axis.title=element_blank(),
        axis.text.x=element_text(angle=90),
        legend.position = "bottom",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.justification = "right")+
  scale_fill_manual(values=c("BiH"="#FFCC4A", 
                             "Western Balkans"="#006400",
                             "EU"="#4C566A"))+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.2)))


plot_unemployment<- econ %>% 
  # mutate(fill.col=case_when(iso3c=="BIH" ~ "BiH",
  #                           iso3c %in% Balks ~"Western Balkans",
  #                           TRUE ~ "EU")) %>% 
  filter(iso3c=="BIH") %>% 
 # filter(date=="2017") %>% 
  filter(stringr::str_detect(indicator,"Unemployment"))%>% 
  ggplot()+
  geom_line(aes(x=date_ct, y=value),
            color="#4C566A")+
  labs(title="Unemployment rates BiH",
       caption="data: World Bank")+
  theme_thesis()+
  theme(axis.title=element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        strip.text = element_text(hjust=0, face="bold"),
        legend.justification = "right")+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.2)),
                     limits=c(0,70))+
  facet_wrap(vars(indicator), nrow=2)


library(patchwork)
plot_gdp + plot_unemployment+plot_layout(ncol=2)

folder <- paste0(wdr,"/")
Rfile <- "WordBankR"
graphname <- "BiHIndicators"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
format <-".png"

ggsave(file=paste(folder, Rfile, graphname, time, format, sep="_"), 
       width=14, height=7, unit="cm")



econ.plot <-   econ %>% 
  filter(iso3c=="BIH") %>% 
  ggplot(econ, aes(date_ct, value))+
  geom_line(size=1, color="black")+
  labs(title="Economic Development",
       subtitle="Selected indicators",
       caption="Data: World Bank", y="%",x="")+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.title=element_blank(), 
        plot.caption = element_text(size=8),
        panel.grid.minor= element_blank())+
  scale_x_date(limits= as.Date(strptime(c("1996-01-01","2019-01-01"), format = "%Y-%m-%d")),
               date_labels="%y", date_breaks = "1 year")+
  #scale_y_continuous(limits=c(0, max(econ$value)))+
  facet_wrap(~indicator, nrow=3, scales="free")
print(econ.plot)
  
folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-econ.plot.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=10, unit="cm")



# >> GDP / cap growth  ----------------------------------------------------

gdp <- wb(indicator = "NY.GDP.PCAP.PP.KD",
           country=c("BIH","HRV","SRB"),
           startdate = 1995, enddate = 2016,
           POSIXct=TRUE)

gdp.plot <- ggplot(gdp, aes(date_ct, value))+
  geom_line(aes(color=country), size=1.5)+
  theme_minimal()+
  #theme_tq()+
  theme(legend.position = "bottom", 
        legend.title=element_blank(), 
        panel.grid.minor= element_blank(),
        plot.caption = element_text(size=8))+
  scale_x_date(limits= as.Date(strptime(c("1996-01-01","2015-01-01"), format = "%Y-%m-%d")),
               date_labels="%y", date_breaks = "1 year")+
  scale_color_manual(breaks=c("Croatia","Serbia","Bosnia and Herzegovina"), 
                     values=c("#2E8B57", "#1874CD", "#B22222"),
                     labels=c("Croatia","Serbia","Bosnia"))+
  #scale_y_continuous(limits=c(0, max(econ$value)))+
  labs(title="Economic Development",
       subtitle="GDP per capita, PPP (constant 2011 international $)",
       caption="Data: World Bank", 
       y="",
       x="")#

print(gdp.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-gdp.plot.png"
ggsave(paste(folder,time,filename, sep=""), width=15, height=8, unit="cm")



# ## Political Stability --------------------------------------------------
find <- wbsearch(pattern = "violence")

indicator <- c("GE.EST","GE.STD.ERR",
               "PV.EST","PV.STD.ERR")

data <- wb(indicator = indicator,
           country="BIH",
           startdate = 1995, enddate = 2016,
           POSIXct=TRUE)

data.n <- data %>% 
  select(-indicator) %>%
  spread(key=indicatorID, value=value)%>%
  gather(key=indicator, value=value, "GE.EST","PV.EST")%>%
  gather(key=indicator_sd, value=sd, ends_with("ERR"))%>%
  filter(substr(indicator, 0, 2)==substr(indicator_sd, 0, 2))

worldbank.plot <- data.n %>%
  ggplot(., aes(date_ct, value))+
  geom_bar(aes(fill=indicator), stat="identity")+
  geom_line(aes(color=indicator, group=indicator))+
  geom_point()+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.5, size=1)+
  theme_minimal()+
  scale_x_date(limits= as.Date(strptime(c("1996-01-01","2018-01-01"), format = "%Y-%m-%d")),
               date_labels="%y", date_breaks = "1 year")+
  geom_vline(xintercept=as.numeric(as.Date("2006-01-01")))+
    theme(legend.position = "bottom", legend.title=element_blank(), panel.grid.minor= element_blank())+
  labs(title="World Bank Indicators",
     caption="worldbank.plot", y="",x="")+
  facet_grid(indicator~.)
  
print(worldbank.plot)

folder <-"graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-worldbank.plot.pdf"
ggsave(paste(folder,time,filename, sep=""), width=10, height=5)




