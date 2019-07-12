# descripton --------------------------------------------------------------

# analysis of laws in 2014-2018 legislative period; 
# analyses info contained on pages with overview of each law
# requires that 2014_2018_Members_HoP.R and 2014_2018_Members_HoR.R were already run and
# dfs on members are available

# setup -------------------------------------------------------------------

list.of.packages <- c("rvest","tidyverse","glue","extrafont","ggupset","patchwork",
                      "paletteer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)


extrafont::loadfonts()
extrafont::font_import()
y

wdr <- getwd() 



df_law_details<- readr::read_csv2(paste0(wdr,"/data/2014_2018_law_details.csv"))

length(unique(df_law_details$link_to_law)) #163
glimpse(df_law_details)


df_status <- df_law_details %>% 
  mutate(law_id=str_extract(link_to_law, "[:digit:]+")) %>% 
  filter(str_detect(value, regex("status", ignore_case = T)) &
           !str_detect(value, regex("faza", ignore_case=T))) %>% 
  mutate(house=case_when(value=="Konacni status u PSBiH" ~ "status_PSBiH",
                         value=="Status u PDPSBiH" ~ "status_PDPSBiH",
                         value=="Status u DNPSBiH" ~ "status_DNPSBiH",
                         TRUE ~ as.character(value))) %>% 
  mutate(status=case_when(value1=="Ceka na pokretanje procedure" ~ "waiting to start procedure",
                          value1=="Donesen i objavljen" ~ "delivered and published",
                          value1=="Nije razmatran" ~ "not considered",
                          value1=="Obustavljen zakonodavni postupak" ~ "legislative procedure suspended",
                          value1=="Odbijen" ~ "rejected",
                          value1=="Povucen" ~ "retired",
                          value1=="Procedura" ~ "procedure",
                          value1=="Procedura - nije preuzet" ~ "procedure not taken",
                          value1=="Usvojen" ~ "adopted",
                          TRUE ~ NA_character_)) %>% 
  select(-value, -value1) 

table(df_status$status, useNA=c("always"))  


# extract dates -----------------------------------------------------------

df_dates <- df_law_details %>% 
  filter(str_detect(value, regex("Broj i datum Prijedloga zakona u", ignore_case=T))) %>% 
  mutate(date_draft_law=str_extract(value1, regex("[:digit:]+\\.[:digit:]+\\.[:digit:]{4}"))) %>% 
  mutate(number_draft_law=str_remove_all(value1, date_draft_law) %>% stringr::str_sub(., end=-7)) %>% 
  mutate(date_draft_law=lubridate::dmy(date_draft_law)) %>% 
  mutate(house=stringr::word(value, -1))


#remove laws which were initiated before 2014-2018 session
laws_preceeding_session<- df_dates %>% 
  filter(!date_draft_law>lubridate::dmy("09/12/2014"))


df_status <- df_status %>% 
  filter(!link_to_law %in% laws_preceeding_session$link_to_law)

df_law_details <- df_law_details %>% 
  filter(!link_to_law %in% laws_preceeding_session$link_to_law)

# number of draft bill by date --------------------------------------------

df_dates %>% 
  ggplot()+
  geom_bar(aes(x=date_draft_law,
               fill=house,
               color=house),
           stat="count",
           position="stack")+
  labs(title="Number of draft bills by date")+
  scale_fill_viridis_d(option="C")+
  scale_color_viridis_d(option="C")+
  facet_wrap(vars(house))+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position="none")

  
  


# barplot overview --------------------------------------------------------

house_labeller <- c(status_DNPSBiH="House of Peoples",
                    status_PDPSBiH="House of Representatives",
                    status_PSBiH="Overall outcome")




# overview faceted ------------------------------------------------------


df_status %>% 
  ggplot()+
  geom_bar(aes(x=status),
           stat="count")+
  hrbrthemes::theme_ft_rc()+
  theme(axis.text.y=element_text(hjust=1,
                                 vjust=0.5,
                                 size=9))+
  scale_y_continuous(expand=expand_scale(mult=c(0,0)))+
  facet_grid(col=vars(house), 
             drop=TRUE,
             labeller=labeller(house=house_labeller))+
  coord_flip()



# overview stacked --------------------------------------------------------

df_status %>% 
  ggplot()+
  geom_bar(aes(x=house,
               color=status,
               fill=status),
           stat="count",
           position = position_stack())+
  hrbrthemes::theme_ft_rc()+
  scale_x_discrete(labels=house_labeller)+
  scale_fill_viridis_d(option="C")+
  scale_color_viridis_d(option="C")+
  theme(legend.position = "bottom")+
  # facet_wrap(vars(house), drop=T, scales="free_y",
  #            nrow=3)+
  coord_flip()+
  guides(fill=guide_legend(reverse = T),
         color=guide_legend(reverse=T))



# df_wide -----------------------------------------------------------------
df_status_wide <-  df_status %>%   
  spread(key=house, value=status) %>% 
  unite(col="status_combined", c("status_PSBiH", "status_PDPSBiH","status_DNPSBiH") , 
        sep="-",
        remove=F) %>% 
  mutate_at(vars(contains("status")), as.factor)


# heatmap with marginals --------------------------------------------------
# _heatmap plot ---------------------------------------------------------------

df_tile_plot <- df_status_wide %>% 
  mutate(status_PDPSBiH=fct_explicit_na(status_PDPSBiH, na_level="no action")) %>% 
  mutate(status_DNPSBiH=fct_explicit_na(status_DNPSBiH, na_level="no action")) %>% 
  group_by(status_PDPSBiH, status_DNPSBiH, .drop=F) %>% 
  summarise(n_laws=n()) %>%
  ungroup() %>% 
  mutate(n_laws_perc=n_laws/sum(n_laws)) 

plot_tile_plot <- df_tile_plot %>%   
  ggplot()+
  geom_tile(aes(x=status_PDPSBiH,  y=status_DNPSBiH,
                fill=n_laws),
            stat="identity")+
  geom_text(aes(x=status_PDPSBiH, y=status_DNPSBiH,
                label=round(n_laws_perc,2 )),
            color="orange",
            size=4) +
  theme(axis.text.x=element_text(angle=45,
                                 hjust=1,
                                 vjust=1),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0, unit="cm"),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank())+
  scale_x_discrete(expand=expand_scale(mult=c(0,0)))+
  scale_y_discrete(expand=expand_scale(mult=c(0,0)), position="right")+
  #scale_fill_continuous(guide=guide_colorbar(title = "number of laws"), direction=-1)
  scale_fill_continuous(low="lightblue", high="blue")
plot_tile_plot



# _HoP rel (left marginal)-----------------------------------------------------------------


df_HoP_rel <- df_status_wide %>% 
  #mutate(status_DNPSBiH=fct_explicit_na(status_DNPSBiH, na_level="no action")) %>% 
  filter(!is.na(status_DNPSBiH)) %>% 
  mutate(status_DNPSBiH=fct_expand(status_DNPSBiH, "")) %>% 
  group_by(status_DNPSBiH, .drop=F) %>% 
  summarise(n_obs=n()) %>% 
  mutate(rel_obs=n_obs/sum(n_obs))
  
  
plot_HoP_rel <- df_HoP_rel %>% 
  ggplot()+
  geom_bar(aes(x=status_DNPSBiH, y=rel_obs),
           stat="identity") +
  geom_text(aes(x=status_DNPSBiH, y=rel_obs+0.06,
                label=round(rel_obs*100,1)))+
  labs(x="% of all laws \nconsidered in HoP")+
  theme(axis.ticks = element_blank(),
       # axis.title.x=element_text(vjust=30, hjust=0),
      #  axis.title.y = element_blank(),
        plot.caption=element_blank(),
        plot.subtitle = element_text(hjust=0),
        plot.margin = margin(0,0,0,0, unit="cm"),
        axis.text.x=element_blank(),
        plot.background = element_rect(fill="white"),
      #  axis.title.y = element_blank(),
        panel.background = element_rect(fill="white"))+
  scale_x_discrete(position="bottom", 
                   expand=expand_scale(mult=c(0,0)))+
  scale_y_continuous(expand=expand_scale(mult=c(0.09,0.0)),
                     trans = "reverse",
                     position="left",
                     labels=scales::percent)+
  coord_flip()

plot_HoP_rel




# _HoR rel (top marginal)-----------------------------------------------------------------

df_HoR_rel <- df_status_wide %>% 
  mutate(status_PDPSBiH=fct_explicit_na(status_PDPSBiH, na_level="no action")) %>% 
  group_by(status_PDPSBiH) %>% 
  summarise(n_obs=n()) %>% 
  mutate(rel_obs=n_obs/sum(n_obs))

plot_HoR_rel <- df_HoR_rel %>% 
  ggplot()+
  geom_bar(aes(x=status_PDPSBiH, y=rel_obs),
           stat="identity") +
  labs(y="% of all laws considered in HoR")+
  theme(axis.text.x = element_blank(),
        plot.margin = margin(0,0,0,0, unit="cm"),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        plot.caption = element_blank(),
        axis.text.y=element_text(hjust=0, vjust=0),
        axis.title.y=element_text(hjust=0, vjust=0),
        axis.title.x = element_blank())+
  scale_y_continuous(expand=expand_scale(mult=c(0,0)),
                     position="right",
                     labels=scales::percent)+
  scale_x_discrete(expand=expand_scale(mult=c(0,0)))

plot_HoR_rel


# _combine plots -----------------------------------------------------------

plot_spacer()+plot_HoR_rel+plot_HoP_rel+plot_tile_plot+
  plot_layout(widths=c(1,2), heights=c(1,2))+
  plot_annotation(title="Laws considered in both chambers")


# library(multipanelfigure)
# figure1 <- multi_panel_figure(
#   width = 180, height = 180,
#   columns = 4, rows = 2,
#   panel_label_type = "none")
# 
# figure1 %<>% fill_panel(plot_HoR_rel, row = 1, column = 3:4) %<>% 
#   fill_panel(plot_HoP_rel, row=2, column = 1:2) %<>% 
#   fill_panel(plot_tile_plot, row=2, column = 3:4)


# MPs initating -----------------------------------------------------------

df_initiation <- df_law_details %>% 
  filter(str_detect(value, regex("Predlagac u", ignore_case = T))) %>% 
  group_by(link_to_law) %>% 
  mutate(n_obs=n()) %>% 
  ungroup() %>% 
  rename(name_initiator=value1)


labeller_house2 <- c("Predlagac u DNPSBiH"="House of Peoples",
                     "Predlagac u PDPSBiH"="House of Representatives")

df_initiation <- df_initiation %>% 
  separate_rows(name_initiator, sep=", ") %>% 
  mutate(house=stringr::str_remove(value, "Predlagac u ") %>% str_trim(., side=c("both"))) %>% 
  left_join(., df_status %>% mutate(house=str_remove(house, "status_")),
                                                by=c("house"="house", "link_to_law"="link_to_law")) %>% 
  mutate(name_initiator=str_replace_all(name_initiator, " - ", "-"))  #Maja Gasal-Vrazalica  %>% 



# _import party membership & constituency -----------------------------------------------

df_party_HoR <- readr::read_csv2(paste0(wdr, "/data/2014_2018_Members_HoR.csv"))
df_party_HoP <- readr::read_csv2(paste0(wdr, "/data/2014_2018_Members_HoP.csv")) 

df_MPs <- bind_rows(df_party_HoP %>% select(first_name, family_name, party, entity_MP=entity, house_MP=house),
                    df_party_HoR %>% select(first_name, family_name, party, entity_MP=entity, house_MP=house)) %>% 
  unite(name, c("first_name", "family_name"), remove=F, sep=" ") 

  
df_initiation <- df_initiation %>% 
  left_join(., df_MPs, by=c("name_initiator"="name")) %>% 
  mutate(name_initiator=case_when(name_initiator=="Dom naroda" ~ "House of Peoples",
                         name_initiator=="Predstavnicki dom" ~ "House of Representatives",
                         name_initiator=="Vijece ministara BiH" ~ "Council of Ministers",
                         name_initiator=="Predsjednistvo BiH" ~ "Presidency",
                         name_initiator=="Komisija za ostvarivanje ravnopravnosti spolova" ~ "Gender Equality Commission",
                         TRUE ~ as.character(name_initiator))) %>% 
  mutate(category_initiator=case_when(is.na(party) ~ "other institutions",
                                      TRUE ~ "MPs")) %>% 
  mutate(inst_initiator=dplyr::coalesce(party, name_initiator)) %>% 
  mutate_at(vars(party, status, inst_initiator, name_initiator), as.factor) %>% 
  mutate_at(vars(status, inst_initiator, name_initiator), fct_explicit_na, na_level="missing") %>% 
  mutate(name_initiator_w_party=ifelse(is.na(party), paste0(name_initiator), paste0(name_initiator, " (", party, ")")))

table(df_initiation$category_initiator, df_initiation$name_initiator)

# superfluous MPs? --------------------------------------------------------

MPs_wo_party <- df_initiation %>% 
  filter(inst_initiator=="missing") %>% 
  distinct(name_initiator)

df_initiation_w_dates<- df_initiation %>% 
  left_join(., df_dates, by=c("house"="house", "link_to_law"="link_to_law"))


# time row of initiation --------------------------------------------------

df_initiation_w_dates %>% 
  #filter(stringr::str_detect(MP_name, "Uzunovic|Zivkovic")) %>% 
  ggplot()+
  geom_tile(aes(y=name_initiator, x=date_draft_law,
            fill=ifelse(str_detect(name_initiator, "Uzunovic|Zivkovic"), "red", "grey")))+
  scale_fill_identity()+
  facet_wrap(vars(house), nrow=2, scale="free_y")

#Uzunovic and Zivkovic were MPs in 2010-2014 HoR; initiated law which was decided in later legislative
#period

x <- df_initiation_w_dates %>% 
  select(name_initiator_w_party, date_draft_law, party, house, category_initiator) %>% 
  mutate(month_draft_law=zoo::as.yearmon(date_draft_law)) %>% 
  group_by(name_initiator_w_party, month_draft_law, house, party, category_initiator) %>% 
  summarise(n_obs=n()) %>% 
  ungroup() %>% 
  arrange(house, desc(category_initiator), name_initiator_w_party) %>% 
  mutate(rank_index=group_indices(., house, desc(category_initiator), party, name_initiator_w_party)) %>% 
  mutate(name_initiator_w_party=forcats::fct_reorder(name_initiator_w_party, rank_index) %>% 
           fct_rev()) %>% 
  ggplot()+
  geom_point(aes(x=month_draft_law, y=name_initiator_w_party,
                 size=n_obs,
                 color=party))+
  scale_size_continuous(guide=guide_legend(title="number of bills initiated", nrow=1))+
  scale_color_discrete(guide=F)+
  facet_wrap(vars(house), scales="free_y")+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position="bottom")

  



# Number and outcome of bills by initiator" ------------------------

df_initiation %>% 
  group_by(house, name_initiator_w_party, status) %>% #wrong because double counting of bill tabled by two MPs from same party
  summarise(n_obs=n()) %>% 
  mutate(n_total=sum(n_obs)) %>% 
  ungroup() %>% 
  mutate(name_initiator=fct_rev(fct_reorder2(name_initiator_w_party, house, n_total))) %>% 
  ggplot()+
  geom_bar(aes(x=name_initiator, y=n_obs,
               fill=status, 
               color=status),
           width=.80,
           stat="identity",
           position = position_stack())+
  labs(title="Number and outcome of bills by initiator")+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.02)))+
  # scale_fill_viridis_d(option="B",
  #                      guide=guide_legend(title = "outcome",
  #                                         keywidth = unit(0.3, "cm"),
  #                                         keyheight = unit(0.3, "cm"),
  #                                         nrow=1))+
  scale_fill_paletteer_d(ghibli, YesterdayMedium,
                         guide=guide_legend(title = "outcome",
                                            nrow=1))+
  scale_color_paletteer_d(ghibli, YesterdayMedium,
                          guide=guide_legend(title = "outcome"))+
  # scale_color_viridis_d(option="B",
  #                       guide=guide_legend(title = "outcome"))+
  facet_grid(row=vars(house), scales = "free", space="free")+
  coord_flip()+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position="bottom",
        strip.text=element_text(color="orange"),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=7),
        panel.grid.major.y = element_blank())

# Number and outcome of bills by initiator & category" ------------------------

df_initiation %>% 
  group_by(house, name_initiator_w_party, status, category_initiator) %>% 
  summarise(n_obs=n()) %>% 
  group_by(house, name_initiator_w_party, category_initiator) %>% 
  mutate(n_total=sum(n_obs)) %>% 
  ungroup() %>% 
  mutate(name_initiator=fct_rev(fct_reorder2(name_initiator_w_party, category_initiator, n_total))) %>% 
  ggplot()+
  geom_bar(aes(x=name_initiator, y=n_obs,
               fill=status, 
               color=status),
           width=.80,
           stat="identity",
           position = position_stack())+
  labs(title="Number and outcome of bills by initiator")+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.02)))+
  # scale_fill_viridis_d(option="B",
  #                      guide=guide_legend(title = "outcome",
  #                                         keywidth = unit(0.3, "cm"),
  #                                         keyheight = unit(0.3, "cm"),
  #                                         nrow=1))+
  scale_fill_paletteer_d(ghibli, YesterdayMedium,
                         guide=guide_legend(title = "outcome",
                                            nrow=1))+
  scale_color_paletteer_d(ghibli, YesterdayMedium,
                          guide=guide_legend(title = "outcome"))+
  # scale_color_viridis_d(option="B",
  #                       guide=guide_legend(title = "outcome"))+
  facet_wrap(vars(house, category_initiator), 
             #space="free")+
             scales = "free")+ 
  coord_flip()+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position="bottom",
        strip.text=element_text(color="orange"),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=7),
        panel.grid.major.y = element_blank())


# #number by party and outcome --------------------------------------------


df_initiation %>% 
  group_by(house, law_id, status) %>% 
  distinct(inst_initiator, .keep_all = FALSE) %>%   #avoid double counting if e.g. two MPs from same party file 1 draft bill
  ungroup() %>% 
  mutate(inst_initiator=fct_rev(fct_infreq(inst_initiator))) %>% 
  ggplot()+
  geom_bar(aes(x=inst_initiator, fill=status, color=status),
           stat="count",
           position=position_stack())+
  coord_flip()+
  labs(title="Number of filed draft laws by party and outcome")+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position = "bottom")+
  scale_fill_viridis_d(option="E")+
  scale_color_viridis_d(option="E")+
  facet_grid(vars(house), scales="free_y", space="free_y")


# #party combinations filing  ---------------------------------------------

df_initiation %>% 
  mutate(inst_initiator=as.character(inst_initiator)) %>% 
  group_by(law_id, status, house) %>% 
  distinct(inst_initiator, .keep_all = FALSE) %>% 
  summarise(inst_initiators=list(inst_initiator)) %>%  #creates list column
  ggplot(aes(x=inst_initiators))+
  geom_bar(aes(fill=status),
           position = position_stack())+
  scale_x_upset(n_intersections=20)+
  theme_combmatrix()+
  # theme(panel.spacing.x = unit(7, "cm"),
  #       plot.background = element_rect(fill="#252a32"),
  #       panel.background = element_rect(fill="#252a32"))+
  facet_wrap(vars(house), 
             scales="free",
             shrink=F)


#constituency of MP filing draft law and outcome

