library(rvest)
library(tidyverse)
library(glue)
library(xlsx)


wdr <- getwd()

seq_categories <- c(318, 326, 334, 342, 350, 358, 366, 374)
seq_pages <- seq(1, 20, 1)
df_seq <- expand.grid(seq_pages=seq_pages, seq_categories=seq_categories)
df_links <- df_seq %>%
  mutate(link=glue("http://www.ohr.int/?cat={seq_categories}&paged={seq_pages}"))

scr_bonn <- function(fn_link) {
  
  pb$tick()$print()
  
  print(fn_link)
  site <- read_html(fn_link)
  
  date.publish <- site %>% 
    html_nodes(".date-publish") %>%
    html_text() %>% 
    enframe(name=NULL, value="date.publish")
  
  decision.name <- site %>%
    html_nodes(".name") %>%
    html_text %>% 
    enframe(name=NULL, value="decision.name")
  
  bind_cols(date.publish=date.publish, decision.name=decision.name)
  
#  Sys.sleep(2)
  
}

pb <- progress_estimated(nrow(df_links))

df_Bonn <- df_links$link %>% 
  set_names() %>%
  map(., possibly(scr_bonn, otherwise=NULL, quiet=F))
 
df_Bonn <- df_Bonn %>% 
  map_df(., bind_rows, .id="link_to_page")
 

# df <- df_Bonn %>% 
#     map_df(., bind_rows, .id="id") 

df_Bonn <- df_Bonn %>%   
    mutate(date.publish.orig=date.publish,
         date.publish=lubridate::mdy(date.publish)) %>% 
  mutate(category=link_to_page %>%  str_extract(., "(?<=cat\\=)[:digit:]+") %>% as.numeric) %>%  #\\ to escape second =
  mutate(page=link_to_page %>%  str_extract(., "(?<=paged\\=)[:digit:]+"))     #\\ to escape second =
  

df_categories <- data.frame(stringsAsFactors=FALSE,
       category_name = c("State Symbols, State-Level Matters and Const. Issues",
                         "Economic Field", 
                         "Judicial Reform", 
                         "Federation, Mostar and H-N Canton",
                         "Removals and Suspensions from Office", 
                         "Media Restructuring Decisions", 
                         "Property Laws, Return of Displaced Persons and Refugees and Reconciliation",
                         "Individuals indicted for War Crimes in the former Yugoslavia"),
     category_number = c(318, 326, 334, 342, 350, 358, 366, 374)
  )
  
df_Bonn <- df_Bonn %>% 
  left_join(., df_categories,
            by=c("category"="category_number"))
  
glimpse(df_Bonn)

df_Bonn %>% 
  group_by(category_name, category) %>% 
  count()
  
df_Bonn %>% 
  filter(category==374)
  
write_csv2(df_Bonn, paste0(wdr,"/data/OHRDecisions.csv"))

