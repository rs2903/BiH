library(rvest)
library(dplyr)
library(xlsx)
library(dplyr)


laws.all <- data.frame(page=integer(),
			 category=integer(),
                   date.start=character(),
                   law.title=character(),
                   stringsAsFactors=FALSE)

for (c in 1:8){ 
	for (p in 1:20){
	url<-paste0("https://www.parlament.ba/law/Read?page=",p,"&categoryId=",c)

laws<-read_html(url)

date.start <- laws %>%
  html_nodes(".date") %>%
  html_text()
date.start <- as.data.frame(date.start)
date.start
 
if(nrow(date.start)==0) { break }
 
law.title <- laws %>%
  html_nodes(".withdates .title") %>%
  html_text()
law.title <- as.data.frame(law.title)
law.title

laws.bind <- bind_cols(date.start, law.title)
laws.bind
laws.bind$page<-p
laws.bind$category<-c

laws.all<-bind_rows(laws.all, laws.bind)
 }
}


write.xlsx(x = laws.all, file = "laws.all.xlsx",
        sheetName = "TestSheet", row.names = FALSE)

