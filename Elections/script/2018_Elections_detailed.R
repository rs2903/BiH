#  Load pacakges ----------------------------------------------------------

list.of.packages <- c("tidyverse","scales","glue","grid","gridExtra","kableExtra",
                      "formattable","sparkline","janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)


ggpreview <- function (..., device = "png") {
  fname <- tempfile(fileext = paste0(".", device))
  ggplot2::ggsave(filename = fname, device = device, ...)
  system2("open", fname)
  invisible(NULL)
}

wdr <- getwd() 


BIH18 <- readxl::read_xlsx(paste0(wdr,"/data/ElectionHOR2018.xlsx"), sheet="detailed") %>% 
          janitor::clean_names(., case=c("snake"))
BIH18 %>% 
  mutate(number_total_check=regular+mail+absence_mobile_team_and_dkp+confirmed) %>% 
  filter(number_total_check!=number_of_votes)

#allocation Sainte lague standard
f <- BIH18 %>% 
  group_by(entity, district) %>% 
  select(entity, district, political_subject, number_of_votes, regular_mandate, compensatory_mandate) %>% 
  mutate(sum_regular_mandate=sum(regular_mandate)) %>% 
  mutate(r2=number_of_votes/2) %>% 
  mutate(r3=number_of_votes/3) %>% 
  mutate(r4=number_of_votes/4) %>% 
  mutate(r5=number_of_votes/5) %>% 
  gather(key=votes, value=value, -c(entity, district, political_subject, regular_mandate, compensatory_mandate, sum_regular_mandate)) %>% 
  arrange(desc(value), .by_group=TRUE) %>% 
  mutate(rown=row_number()) %>% 
  filter(sum_regular_mandate>=rown) %>% 
  group_by(entity, district, political_subject, regular_mandate) %>% 
  summarise(mandates.new=n()) %>% 
  mutate(diff=regular_mandate-mandates.new)
  

f %>% 
  group_by(entity, political_subject) %>% 
  summarise_at(vars(regular_mandate, mandates.new), sum) %>% 
  mutate(diff=mandates.new-regular_mandate)



BIH_thresh<- BIH18 %>%
  group_by(entity, district) %>% 
  summarize(sum_regular_mandate=sum(regular_mandate),
            sum_compensatory_mandate=sum(compensatory_mandate)) %>% 
  mutate(sum_mandates=sum_regular_mandate+sum_compensatory_mandate) %>% 
  mutate(eff_threshold_regular=75/(sum_regular_mandate+1)) %>% 
  mutate(eff_threshold_total=75/(sum_mandates+1)) 

names(BIH18)

BIH_thresh %>% 
  group_by(entity) %>% 
  summarize(sum_regular=sum(sum_regular_mandate),
            sum_total=sum(sum_mandates),
            n.districts=n()) %>% 
  mutate(av_mag_reg=sum_regular/n.districts,
         av_mag_total=sum_total/n.districts) %>% 
  mutate(T_regular=75/((av_mag_reg+1)*sqrt(n.districts)),
         T_total=75/((av_mag_total+1)*sqrt(n.districts))) %>% 
  mutate(M_regular=(75/T_regular)-1,
         M_total=(75/T_total)-1)
  

BiH_droop <- BIH18 %>% 
  group_by(district) %>% 
  summarise_at(vars(number_of_votes, regular_mandate, compensatory_mandate), sum) %>% 
  mutate(total_mandate=regular_mandate + compensatory_mandate) %>% 
  gather(key=mandate.type, value=seats, -c(district, number_of_votes)) %>% 
  arrange(district, number_of_votes) %>% 
  mutate(droop.votes=(number_of_votes/seats+1)+1) %>% 
  mutate(droop.share=droop.votes/number_of_votes*100) %>% 
  filter(mandate.type!="compensatory_mandate")
           


names(BIH)
BIH18 %>% 
  select(district, political_subject, )





NIRL <- data.frame(district=c(seq(1,18,1)), magnitude=c(rep(6,18))) %>% 
        mutate(eff_threshold=round(75/(magnitude+1),2))

eff_nat_threshold <- function(df, district, magnitude) {
  
  E_n_districts <- length(unique(df$district))
  print(E_n_districts)
  
  S_assembly_size <- df %>% 
    summarise(sum=sum(magnitude)) %>% 
    pull(sum)
  print(S_assembly_size)
  
  M_aver_size <- S_assembly_size/E_n_districts
  print(M_aver_size)
  
  
  T <-  75/((M_aver_size+1)*sqrt(E_n_districts))
  print(T)
  
}

NIRL.t<- eff_nat_threshold(NIRL, district=district, magnitude=magnitude)
M <- (75/NIRL.t)-1
M #28.69
