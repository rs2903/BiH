library(rvest)
library(dplyr)
library(xlsx)


# decisions.all <- data.frame(area=character(),
#                             date.publish=character(),
#                             decision.name=character(),
#                             stringsAsFactors=FALSE)

for (i in 1:1){ 
  url <- paste0("http://www.ohr.int/?cat=374&paged=",i)

state_symbols <- read_html(url)

date.publish <- state_symbols %>%
  html_nodes(".date-publish") %>%
  html_text()
date.publish <- as.data.frame(date.publish)

decision.name <- state_symbols %>%
  html_nodes(".name") %>%
  html_text
decision.name <- as.data.frame(decision.name)

decisions <- bind_cols(date.publish, decision.name)
#decisions$area <- "State Symbols, State-Level Matters and Const. Issues" #318
#decisions$area <- "Economic Field" #326
#decisions$area <- "Judicial Reform" #334
#decisions$area <-"Federation, Mostar and H-N Canton" #342
#decisions$area <-"Removals and Suspensions from Office" #350
#decisions$area <-"Media Restructuring Decisions" #358
#decisions$area <-"Property Laws, Return of Displaced Persons and Refugees and Reconciliation" #366
decisions$area <-"Individuals indicted for War Crimes in the former Yugoslavia" #374


decisions.all <- bind_rows(decisions.all, decisions)

}


write.csv(decisions.all, file="OHRDecisions.csv")

unique(decisions.all$area)
# decisions.all <- decisions.all %>%
#   filter(area!="Judicial Reform")
