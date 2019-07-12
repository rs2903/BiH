#  Load pacakges ----------------------------------------------------------

list.of.packages <- c("tidyverse","scales","glue","grid","gridExtra","kableExtra",
                      "formattable","sparkline","janitor","skimr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com/')
lapply(list.of.packages, require, character.only = TRUE)


ggpreview <- function (..., device = "png") {spark
  fname <- tempfile(fileext = paste0(".", device))
  ggplot2::ggsave(filename = fname, device = device, ...)
  system2("open", fname)
  invisible(NULL)
}

wdr <- getwd() 


# Droop Quota -------------------------------------------------------------


# _96-98 ------------------------------------------------------------------
BiH.96_98 <- readxl::read_xlsx(paste0(wdr,"/PartyNat/data/BiH1996-1998.xlsx")) %>% 
  janitor::clean_names() %>% 
  filter(stringr::str_starts(electoral_district, "51|52"))

BiH.96_98 %>% 
  count(electoral_district)



# _02 ---------------------------------------------------------------------


BiH.02 <- readxl::read_xlsx(paste0(wdr,"/PartyNat/data/BiH2002detailed.xlsx")) %>% 
  janitor::clean_names() %>% 
  rename(electoral_district=district) %>% 
  filter(stringr::str_starts(electoral_district, "51|52"))

BiH.02 %>% 
  distinct(electoral_district) %>% 
  arrange(electoral_district)

BiH.02 <- BiH.02 %>%
  mutate(votes=redovni+postom+odsustvu+potvrdeni) %>% 
  select(-c(redovni:potvrdeni)) %>% 
  group_by(year, electoral_district) %>% 
  summarise(district.votes=sum(votes),
            district.mandate=sum(mandate))


district.magnitudes <- BiH.02 %>% 
  ungroup() %>% 
  select(electoral_district, district.mandate)

# _06_10 ------------------------------------------------------------------

BiH.06_10 <- readxl::read_xls(paste0(wdr,"/PartyNat/data/BiHTotal.xls")) %>% 
  janitor::clean_names()

BiH.long <- BiH.const %>% 
  select(-croatian_coalition) %>% 
  gather(key=party, value=votes, -c(year:district_name))

BiH.long %>% 
  distinct(electoral_district) %>% 
  arrange(electoral_district)

BiH.06_10 <- BiH.long %>% 
  group_by(electoral_district, year) %>% 
  summarise(district.votes=sum(votes, na.rm = T)) %>% 
  arrange(year, .by_group=T)

BiH.06_10 %>%  ggplot()+
  geom_line(aes(x=year, y=district.votes))+
  facet_wrap(vars(electoral_district))


# _combine ----------------------------------------------------------------

df <- bind_rows(BiH.06_10=BiH.06_10, BiH.02=BiH.02, .id="source") %>% 
  ungroup()

#check completness of observations
df %>% 
  select(year, electoral_district) %>% 
  distinct() %>% 
  mutate(value=1) %>% 
  spread(key=year, value=value)

df %>% 
  group_by(year) %>% 
  summarise(nobs=n())



# National level effective electoral threshold ----------------------------

#https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/Docts/effthresh.php
#formula M * (1 + log(E))

M.all <- c(3,4,7,7,7,4,4,6) #including compesnatory seats
M.direct <- c(3,3,4,6,5,3,3,3) #without compensatory seats
M <- mean(M.direct) #average district magnitude
M
E <- length(M.direct)
E

eff.threshold <- round(M*(1+log(E)),2) 
eff.threshold 
#direct 11.55
#all 16.17

#constituency level
#eff thresh = 75% / (m + 1)
map(M.direct, function(x) {75/(x+1)})

district.magnitudes %>% 
  mutate(eff.threshold=map_dbl(district.mandate, ~75/(.x+1)))

#Nirl
75/(5+1)
6*(1+log(18)) #23 national eff. for nirl

