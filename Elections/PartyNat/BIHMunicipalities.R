library(maptools) 
library(dplyr)
library(ggplot2)
library(gpclib) 
library(psych)

#setwd("~/Google Drive/CEU/R/PartyNat")
setwd("Z:/Documents/ro - ceu/R/Elections/PartyNat")





# IMPORT MUNICIPALITIES ---------------------------------------------------
# delete variables which are not needed now; add variables and set them to zero where missing; all
# data.frames must have for every year the same variables in order to be merged;

bih.mun.1997<-read.csv("BiH Municipal/BiH Municipal Elections 1997.csv")
#bih.mun.1997<-read.csv("Election Results CSV/BiH Municipal/BiH Municipal Elections 1997.csv")

bih.mun.1997$votes.out.municipality <- as.numeric(bih.mun.1997$votes.out.municipality)
bih.mun.1997$control.sum  <- bih.mun.1997$X <- NULL
bih.mun.1997[is.na(bih.mun.1997)] <- 0   #there is one observation which has a na on votes.out.municipality
bih.mun.1997$votes <- bih.mun.1997$votes.in.municipality+bih.mun.1997$votes.out.municipality
bih.mun.1997$votes.redovni <- bih.mun.1997$votes.postom   <- bih.mun.1997$votes.odsustvo.mobilni.tim <- bih.mun.1997$votes.odsustvu <- bih.mun.1997$votes.nepotvrdenim <- bih.mun.1997$votes.potvrdeni <- bih.mun.1997$minority <- bih.mun.1997$minority.elected <- 0            

bih.mun.2000<-read.csv("BiH Municipal/BiH Municipal Elections 2000.csv")
bih.mun.2000$control.sum  <- NULL
bih.mun.2000$votes <- bih.mun.2000$votes.in.municipality+bih.mun.2000$votes.out.municipality
bih.mun.2000$votes.redovni <- bih.mun.2000$votes.postom   <- bih.mun.2000$votes.odsustvo.mobilni.tim <- bih.mun.2000$votes.odsustvu <- bih.mun.2000$votes.nepotvrdenim <- bih.mun.2000$votes.potvrdeni <- bih.mun.2000$minority <- bih.mun.2000$minority.elected <- 0            

bih.mun.2004<-read.csv("BiH Municipal/BiH Municipal Elections 2004.csv")
bih.mun.2004$votes <-  bih.mun.2004$votes.redovni+bih.mun.2004$votes.postom+bih.mun.2004$votes.odsustvu+bih.mun.2004$votes.nepotvrdenim
bih.mun.2004$votes.in.municipality <- bih.mun.2004$votes.out.municipality  <- bih.mun.2004$votes.odsustvo.mobilni.tim <- bih.mun.2004$votes.potvrdeni  <- bih.mun.2004$minority  <- bih.mun.2004$minority.elected <- 0

bih.mun.2008<-read.csv("BiH Municipal/BiH Municipal Elections 2008.csv")
bih.mun.2008$votes.postom[is.na(bih.mun.2008$votes.postom) & bih.mun.2008$minority=="y"] <- 0
bih.mun.2008$votes.odsustvo.mobilni.tim[is.na(bih.mun.2008$votes.odsustvo.mobilni.tim) & bih.mun.2008$minority=="y"] <- 0
bih.mun.2008$votes.potvrdeni[is.na(bih.mun.2008$votes.potvrdeni) & bih.mun.2008$minority=="y"] <- 0
bih.mun.2008$votes <- bih.mun.2008$votes.redovni+bih.mun.2008$votes.odsustvo.mobilni.tim+bih.mun.2008$votes.postom+bih.mun.2008$votes.potvrdeni  
bih.mun.2008$votes.in.municipality  <- bih.mun.2008$votes.out.municipality <- bih.mun.2008$votes.odsustvu <- bih.mun.2008$votes.nepotvrdenim  <- 0

bih.mun.2012<-read.csv("BiH Municipal/BiH Municipal Elections 2012.csv")
bih.mun.2012$total  <- bih.mun.2012$percentage <- bih.mun.2012$party.code <- NULL
bih.mun.2012$votes <- bih.mun.2012$votes.redovni+bih.mun.2012$votes.postom+bih.mun.2012$votes.odsustvo.mobilni.tim+bih.mun.2012$votes.potvrdeni
bih.mun.2012$votes.in.municipality  <- bih.mun.2012$votes.out.municipality <- bih.mun.2012$votes.odsustvu <- bih.mun.2012$votes.nepotvrdenim  <- bih.mun.2012$votes <- bih.mun.2012$votes.redovni+bih.mun.2012$votes.odsustvo.mobilni.tim+bih.mun.2012$votes.postom+bih.mun.2012$votes.potvrdeni  

Map<-read.csv("BiHShapeFile/BIH_adm3.csv") #Map file - info on entity, canton

# Appending electoral results from different years ------------------------

bih.mun <- rbind(bih.mun.1997, bih.mun.2000, bih.mun.2004, bih.mun.2008, bih.mun.2012)


# Homogenize district names (2012 names are usded) & link to map id  ---------------------------------

library(reshape)

harmonized.names <- read.csv("BiH Municipal/BiH - district names unification - map id.csv")
bih.mun <- merge(bih.mun, harmonized.names, by=c("district.id"), all.x=TRUE)

bih.mun$comment <- bih.mun$district.id.mean <- NULL
bih.mun <- rename(bih.mun, c(district.name.x="district.name.original", district.name.y="district.name"))

# Identify districts not present in all elections -------------------------

library(data.table)
dt <- data.table(bih.mun)
setkey(dt, year)
x <- dt[, list(number.districts=length(unique(district.id))), by=key(dt)] #number of unique distirct.ids per election => reveals that number is different btw elections

y <- dt[, list(number.districts=unique(district.id)), by=key(dt)]
y1 <- y$number.districts
a <- rle(sort(y1))
b <- data.frame(district.id=a$values, freq=a$lengths)  #freq
b1 <- subset(b, freq<max(freq))                        #distirct.ids which are not always present

bih.mun$district.freq <- b$freq[match(bih.mun$district.id, b$district.id)]  
bih.mun.district.freq <- subset(bih.mun, district.freq<max(district.freq, na.rm=TRUE))  #districts which are not included in every elections
d <- aggregate(district.freq~year+district.id+district.name, data=bih.mun.district.freq, FUN="mean")
d <- d[order(d$district.id),]  


# Homogenize party names ------------------------------------------------------
# party names are stated differently during different elections; have to be assigned a unique party.id or unique spelling

## Convert party names written in capital letters

capitalize <- function(x) {            # creat a function
  x <- tolower(x)                      # convert all party names to lower case
  x <- strsplit(x, " ")                # make only the first letter capital
  for (i in seq(along = x)) {
    substr(x[[i]], 1, 1) <- toupper(substr(x[[i]], 1, 1))
  }
  sapply(x, function(z) paste(z, collapse = " "))
}

bih.mun$party <- capitalize(as.character(bih.mun$party))  #call the function

##identify different party names
a <- length(unique(bih.mun$party))          # number of different party spellings
b <- as.data.frame(unique(bih.mun$party))   # list with different spellings
names(b) <- c("party")            
b <- b[order(as.character(b$party)),]
b <- as.data.frame(b)

#write.table(b, "Party Names",  sep="\t")   # was exported to Excle; harmonization of party names made manually; + coalition composition and ethnicity


## import file with HARMONIZED PARTY NAMES & ETHNICITY ------------------------

harmonized.df <- read.csv("BiH Municipal/Party Names Unification.csv", sep=";")
harmonized.df$X1 <- harmonized.df$X <- NULL

library(reshape)
harmonized.df <- rename(harmonized.df, c(name.original="party"))

harmonized.df[duplicated(harmonized.df$party),]
harmonized.df <- harmonized.df[!duplicated(harmonized.df$party),]                  # removing duplicates in the harmonization file

bih.mun.x <- merge(bih.mun, harmonized.df, by.x="party", by.y="party")             # merges two datasets based on party names as written in official results

bih.mun.x <- rename(bih.mun.x, c(party="name.original", name.unified1="party"))    #makes unified party names as primary party variable

df.final <- bih.mun.x

## identify those rows which were not properly merged (not relevant anylonger) ----------------------

a1 <- as.data.frame(bih.mun.x[,c("party", "year", "district.id")])
length(unique(a1, by=c("party","year","district.id")))

a2 <- as.data.frame(bih.mun[,c("party", "year", "district.id")])
a3 <- as.data.frame(harmonized.df[,c("party")])

require(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2') 
a2NotIna1 <- sqldf('SELECT * FROM a2 EXCEPT SELECT * FROM a1') 



#  VOTES AND DISTRICT MAGNITUDE -----------------------------------

mun <- bih.mun.x[, c("year", "district.id", "district.name", "party", "votes", "seats", "coalition", "ethnicity")]
mun <- ddply(mun, .(year, district.id, party), transform, votes=sum(votes))        # adds minority candidates to party
mun <- ddply(mun, .(year, district.id), transform, votes.district=sum(votes))      # all votes casted in district
mun <- ddply(mun, .(year, district.id), transform, district.magnitude=sum(seats))  # number of seats in district/district magnitude

mun$seats.district.relat <- round(mun$seats/mun$district.magnitude*100, 2)
mun$votes.district.relat <- round(mun$votes/mun$votes.district*100, 2)

mun <- mun[order(mun$year, mun$district.id, -mun$votes),]
mun <- mun[, c("year", "district.id","district.name","party", "votes", "votes.district.relat","votes.district" ,"seats", "seats.district.relat", "district.magnitude", "coalition", "ethnicity")]


# GINI of votes WITHIN district -------------------------------------------


library(reldist) # includes function Gini
library(plyr)

mun.d <- ddply(mun, .(year, district.id, district.name), summarize, 
               gini.votes=round(gini(votes), 4), 
               # gini.seats=round(gini(seats), 4),          #gini seats based all parties (those who didn't enter council doesn't make munch sense)
               votes.district=sum(votes),
               district.magnitude=sum(seats),
               party.numbers=length(party))                 #alll parties who took part in elections
mun.d <- mun.d[order(mun.d$district.id, mun.d$year),]



# PLOT Gini of votes WITHIN districts -------------------------------------

x <- mun.d[,c("year","district.id", "district.name", "gini.votes", "party.numbers")]
y <- merge(x, harmonized.names, by="district.id")
y$district.name.y  <- y$district.id.mean <- y$comment <- NULL
y <- rename(y, c(district.name.x="district.name"))
y <- merge(y, Map, by.x="map_id3", by.y="ID_3")
y <- y[,c("district.id", "district.name", "year", "gini.votes", "party.numbers", "NAME_1", "ID_1", "ID_2", "NAME_2")]


for(n in unique(y$NAME_1)){  
  
     y.tmp <- subset(y, NAME_1==n)
      
        gini.within.plot <- ggplot(data=y.tmp, aes(x=as.factor(year), y=gini.votes, group=district.id, color=district.id)) + 
        geom_line(aes(color=district.id)) + 
        geom_point()+
        ggtitle(paste("Gini of votes within districts","\n"))+
        labs(x="year", y="Gini")+
        theme(legend.position="none")+
        facet_wrap(~ NAME_1 + NAME_2, ncol=2)
        
      print(gini.within.plot)  
      
      path="Z:/Documents/ro - ceu/R/graphs/draft/"
      filename1="GiniWithinDistricts"
      entity=n
      ext=".png"
      filename=paste(path,filename1,entity,ext)
      png(file=filename)
      print(gini.within.plot) 
      plot(gini.within.plot)
      dev.off()
      
    }
 

# EFFECTIVE NUMBER OF PARTIES in each District / Year Laakso/Taagepera Index) BASED ON SEATS not votes---------------------

Taagepera <- subset(mun, seats>0)
Taagepera <- ddply(Taagepera, .(year, district.id, district.name), summarize, Taagepera=round(1/(sum((seats.district.relat/100)^2)),4))

y <- merge(Taagepera, harmonized.names, by="district.id")
y$district.name.y  <- y$district.id.mean <- y$comment <- NULL
y <- rename(y, c(district.name.x="district.name"))
y <- merge(y, Map, by.x="map_id3", by.y="ID_3")
y <- y[,c("district.id", "district.name", "year", "Taagepera", "NAME_1", "ID_1", "ID_2", "NAME_2")]

Taagepera <- y

# PLOT LAAKSO/TAAGEPERA - effective number of parties ----------------------------------------

for(n in unique(y$NAME_1)){  
  
  Taagepera.tmp <- subset(Taagepera, NAME_1==n)
  
  Taagepera.plot <- ggplot(data=Taagepera.tmp, aes(x=as.factor(year), y=Taagepera, group=district.id, color=district.id)) + 
    geom_line(aes(color=district.id)) + 
    geom_point()+
    ggtitle(paste("Number of Effective Parties (based on seats in Council)","\n"))+
    labs(x="year", y="Taagepera")+
    theme(legend.position="none")+
    facet_wrap(~ NAME_1 + NAME_2, ncol=2)
  
  path=path
  filename1="Taagepera"
  entity=n
  ext=".png"
  filename=paste(path,filename1,entity,ext)
  png(file=filename)
  print(Taagepera.plot) 
  plot(Taagepera.plot)
  dev.off()
  
}


# Identify districts with changing district magnitude ---------------------

library(reshape)
check.district.mag <- ddply(mun.d, .(district.id), summarize, mean.district.magnitude=mean(district.magnitude))
mun.d$mean.district.magnitude <- check.district.mag$mean.district.magnitude[match(mun.d$district.id, check.district.mag$district.id)]
mun.d$district.mag.deviation <- mun.d$mean.district.magnitude-mun.d$district.magnitude
#almost all districts have a non-constant district magnitude; why?

check.district.mag <- mun.d[ , c("year", "district.id", "district.name", "district.magnitude")]
#write.table(check.district.mag, "Check.district.mag",  sep="\t")   # was exported to Excle; harmonization of party names made manually; + coalition composition and ethnicity

district.mag <- reshape(check.district.mag, idvar=c("district.id", "district.name"), timevar="year", direction="wide")


# Gini of Seats in Municipal Council --------------------------------------

#Get number of parties with seats
mun.seats  <- subset(mun, seats>0)                                 #only those parties that have gained a seat
mun.seats.d  <- ddply(mun.seats, .(year, district.id, district.name), summarize, 
                      gini.seats=round(gini(seats), 4),            #gini seats based all parties (those who didn't enter council doesn't make munch sense)
                      district.magnitude=sum(seats),
                      party.seats.numbers=length(party)) 

#merge data based with all parties & those only in council
mun.d <- merge(mun.d, mun.seats.d, by=c("year", "district.id"), all.x=TRUE)
mun.d$district.magnitude.Y <-  mun.d$district.name.Y<- NULL
mun.d <- rename(mun.d, c(district.magnitude.x="district.magnitude", district.name.x="district.name"))
mun.d <- mun.d[,c("year", "district.id", "district.name","votes.district", "gini.votes", "party.numbers", "district.magnitude", "gini.seats", "party.seats.numbers")]
mun.d <- mun.d[order(mun.d$district.id),]

#votes and seats per district disaggregated by ethnicity; all parties (incl those who are not in council)

mun.d.eth <- ddply(mun, .(year, district.id, district.name ,ethnicity),     #based on all parties (also those not in Council)
                   summarize, 
                   votes=sum(votes),
                   seats=sum(seats))

mun.d.eth.1<- reshape(mun.d.eth, idvar=c("district.id", "year", "district.name"), timevar="ethnicity", direction="wide")
mun.d <- merge(mun.d, mun.d.eth.1, by=c("year", "district.id", "district.name"), all.x=TRUE)
mun.d <- mun.d[order(mun.d$district.id),]

mun.d$votes.B.rel=round(mun.d$votes.B/mun.d$votes.district*100, 2)
mun.d$votes.C.rel=round(mun.d$votes.C/mun.d$votes.district*100, 2)
mun.d$votes.S.rel=round(mun.d$votes.S/mun.d$votes.district*100, 2)
mun.d$votes.M.rel=round(mun.d$votes.M/mun.d$votes.district*100, 2)
mun.d$votes.nk.rel=round(mun.d$votes.nk/mun.d$votes.district*100, 2)
mun.d$votes.R.rel=round(mun.d$votes.R/mun.d$votes.district*100, 2)

mun.d$seats.B.rel=round(mun.d$seats.B/mun.d$district.magnitude*100, 2)
mun.d$seats.C.rel=round(mun.d$seats.C/mun.d$district.magnitude*100, 2)
mun.d$seats.S.rel=round(mun.d$seats.S/mun.d$district.magnitude*100, 2)
mun.d$seats.M.rel=round(mun.d$seats.M/mun.d$district.magnitude*100, 2)
mun.d$seats.nk.rel=round(mun.d$seats.nk/mun.d$district.magnitude*100, 2)
mun.d$seats.R.rel=round(mun.d$seats.R/mun.d$district.magnitude*100, 2)

mun.d <- mun.d[,c("year", "district.id","district.name","votes.district", "gini.votes", "party.numbers", "district.magnitude", "gini.seats", "party.seats.numbers", 
                  "votes.B", "votes.C", "votes.S","votes.M", "votes.nk", "votes.R",
                  "votes.B.rel", "votes.C.rel", "votes.S.rel","votes.M.rel", "votes.nk.rel", "votes.R.rel",
                  "seats.B", "seats.C", "seats.S","seats.M", "seats.nk","seats.R",
                  "seats.B.rel","seats.C.rel","seats.S.rel","seats.M.rel","seats.nk.rel", "seats.R.rel"
                  )]


# Add canton and entity ids to mun.d (critical) ---------------------------

y <- merge(mun.d, harmonized.names, by="district.id")
y$district.name.y  <- y$district.id.mean <- y$comment <- NULL
y <- rename(y, c(district.name.x="district.name"))
y <- merge(y, Map, by.x="map_id3", by.y="ID_3")
y$PID <- y$ID_0 <- y$ISO <- y$NAME_0 <- y$NAME_3 <- y$NL_NAME_3 <- y$check <- y$VARNAME_3 <- y$TYPE_3 <- y$ENGTYPE_3 <- NULL

mun.d <- y


# PLOT - GINI of COUNCIL SEATS within DISTRICT ----------------------------


for(n in unique(mun.d$NAME_1)){  
  
  gini.seats.tmp <- subset(mun.d, NAME_1==n)
  
  gini.seats.plot <- ggplot(data=gini.seats.tmp, aes(x=as.factor(year), y=gini.seats, group=district.id, color=district.id)) + 
    geom_line(aes(color=district.id)) + 
    geom_point()+
    ggtitle(paste("Concentration of Seats in Municipal Council","\n"))+
    labs(x="year", y="Gini Seats")+
    theme(legend.position="none")+
    facet_wrap(~ NAME_1 + NAME_2, ncol=2)
  
  path=path
  filename1="GiniSeats"
  entity=n
  ext=".png"
  filename=paste(path,filename1,entity,ext)
  png(file=filename)
  print(gini.seats.plot) 
  plot(gini.seats.plot)
  dev.off()
  
}

# Wasted Votes per District -----------------------------------------------

mun.votes.no.seats <- subset(mun, seats=0)
mun.wasted <- ddply(mun[mun$seats==0,], .(year, district.id, district.name), summarize, votes.wasted=sum(votes))

mun.d <- merge(mun.d, mun.wasted, by=c("year", "district.id", "district.name"), all.x=TRUE)
mun.d$votes.wasted.rel <- round(mun.d$votes.wasted/mun.d$votes.district*100, 2)


# PLOT WASTED VOTES -------------------------------------------------------


for(n in unique(mun.d$NAME_1)){  
  
  wasted.votes.tmp<- subset(mun.d, NAME_1==n)
  
  wasted.votes.plot <- ggplot(data=wasted.votes.tmp, aes(x=as.factor(year), y=votes.wasted.rel, group=district.id)) + 
    geom_line(aes(color=district.id)) + 
    geom_point(aes(color=district.id))+
    ggtitle(paste("% of Votes wasted","\n"))+
    labs(x="year", y="% votes wasted")+
    theme(legend.position="none")+
    geom_text(data=wasted.votes.tmp[wasted.votes.tmp$year==2012,], aes(label=district.id), size=2.5, hjust=-1, vjust=0)+
    facet_wrap(~ NAME_1 + NAME_2, ncol=2)
  
  path=path
  filename1="VotesWasted"
  entity=n
  ext=".png"
  filename=paste(path,filename1,entity,ext)
  png(file=filename)
  print(wasted.votes.plot) 
  plot(wasted.votes.plot)
  dev.off()
  
}


# Vote Share of MAJOR THREE parties over time -----------------------------

major3 <- subset(mun, party=="Stranka Demokratske Akcije (SDA)" | party=="Srpska Demokratska Stranka (SDS)"| party=="Hrvatska Demokratska Zajednica (HDZ)")
major3.district <- ddply(major3, .(year, district.id, district.name), summarize, sum.major3=sum(votes.district.relat))
major3.year <- ddply(major3.district, .(year), summarize, mean.major3=mean(sum.major3), sd.major3=sd(sum.major3))


# PLOT vote share of MAJOR THREE ------------------------------------------


major3.plot <- ggplot(major3.year, aes(year, mean.major3, shape=as.factor(year))) + 
               geom_line() + geom_point() +
               labs(x="year", y="aggregated vote share; mean over districts", fill="aggregated voteshare of major three")+
               ggtitle("Aggregated voteshare on municipal level for SDA, HDZ, and SDS")  
               major3.plot <- major3.plot+geom_ribbon(aes(ymax=mean.major3+sd.major3,ymin=mean.major3-sd.major3), alpha=0.5)
               major3.plot <- major3.plot+scale_y_continuous(limits=c(0,100))
                
               path=path
               filename1="Major3 - voteshare - municipal level1"
               ext=".png"
               filename=paste(path,filename1,ext)
               png(file=filename, width=740, height=507, res=72)

                print(major3.plot) 
                plot(major3.plot)
                dev.off() 



# COMPOSITION of all municipal votes according to ethnicity ---------------

composition <- ddply(mun, .(year, ethnicity), summarize, sum.votes.ethnicity=sum(votes))
composition <- ddply(composition, .(year), transform, votes.year=sum(sum.votes.ethnicity))
composition$voteshare.ethnicity=round(composition$sum.votes.ethnicity/composition$votes.year, 4)
composition <- ddply(composition, .(year), transform, voteshare.ethnicity=sum.votes.ethnicity/sum(sum.votes.ethnicity))


# PLOT COMPOSITION --------------------------------------------------------

composition.plot <- ggplot(composition, aes(x=as.factor(year), y=sum.votes.ethnicity, fill=ethnicity ,color=ethnicity)) +
    geom_bar(bandwidth=1, stat="identity", group="ethnicity", position="fill") +
    labs(x="year", y="% of total municipal votes") +
    ggtitle("Composition of all municipal votes per ethnicity")
   
png(file="graphs/BiH/CompositionMunicipalVotesEthnicity")

print(composition.plot) 
plot(composition.plot)
dev.off() 



# Number of Parties per Ethnicity per District (also parties not in Council) PLOT MISSING-----------------------------------------

# also Parties which did not qualify for Council; number of parties per ethnicity per district
parties.ethnicity <- ddply(mun, .(year, district.id, district.name, ethnicity), summarize, freq.party=length(party))
parties.ethnicity[is.na(parties.ethnicity)]<-0 
parties.ethnicity <- reshape(parties.ethnicity, idvar=c("district.id", "district.name","year"), timevar="ethnicity", direction="wide")

mun.d <- merge(mun.d, parties.ethnicity, by=c("year", "district.id", "district.name"), all.x=TRUE)
mun.d <- mun.d[order(mun.d$district.id),]


# only Parites in Council; number of parties per ethnicity per district (PLOT MISSING)
parties.ethnicity <- subset(mun, seats>0)
parties.ethnicity <- ddply(parties.ethnicity, .(year, district.id, district.name, ethnicity), summarize, freq.party.seats=length(party))
parties.ethnicity <- reshape(parties.ethnicity, idvar=c("district.id", "year", "district.name"), timevar="ethnicity", direction="wide")

mun.d <- merge(mun.d, parties.ethnicity, by=c("year", "district.id", "district.name"), all.x=TRUE)
mun.d <- mun.d[order(mun.d$district.id),]

mun.d[is.na(mun.d)]<-0 

mun.d <- mun.d[,c("year", "district.id", "district.name","map_id3",
                  "votes.district", "gini.votes",  
                  "district.magnitude", "gini.seats",
                  
                  "votes.B", "votes.C", "votes.S","votes.M", "votes.nk", "votes.R",
                  "votes.B.rel", "votes.C.rel", "votes.S.rel", "votes.M.rel", "votes.nk.rel", "votes.R.rel",
                  
                  "seats.B", "seats.C", "seats.S","seats.M", "seats.nk", "seats.R",
                  "seats.B.rel","seats.C.rel","seats.S.rel","seats.M.rel","seats.nk.rel", "seats.R.rel",
                  
                  "party.numbers", 
                  "freq.party.B", "freq.party.C", "freq.party.S", "freq.party.M","freq.party.nk", "freq.party.R",
                  
                  "party.seats.numbers", 
                  "freq.party.seats.B", "freq.party.seats.C","freq.party.seats.S","freq.party.seats.M","freq.party.seats.nk","freq.party.seats.R")]




# Gini - Coefficients / Party Nationalization Score ---------------------

#how many districts for each year? 
districts.by.year <- ddply(mun, .(year), summarize, district.num=length(unique(district.id)))

#how many parties for each year
parties.by.year <- ddply(mun, .(year), summarize, party.num=length(unique(party)))

district.unique <- length(unique(mun$district.id)) #for all years
district.unique

party.unique <- length(unique(mun$party)) #for all years
party.unique

#for each year a matrix with covers all distircts of this year for every party
library(vcd)
require(utils)

for (t in unique(mun$year))  
{
  mun.t<- subset(mun, year==t)
  grid <- as.data.frame(expand.grid(district.id=unique(mun.t$district.id),party=unique(mun.t$party)))
  grid$year <- t
  assign(paste("mun.matrix.",t, sep=""), as.data.frame(grid))       #renames subsetted dataframes
} 

mun.mat <- rbind(mun.matrix.1997, mun.matrix.2000, mun.matrix.2004, mun.matrix.2008, mun.matrix.2012)  #binds all matrices for different years together
mun.mat.filled <- merge(mun.mat, mun, by=c("year", "district.id", "party"), all.x=TRUE)

#identify & drop duplicates (emerged due to the merging)
mun.mat.filled$unique.id2 <- as.numeric(interaction(mun.mat.filled$year, mun.mat.filled$district.id, mun.mat.filled$party, drop=TRUE))  
# mun.mat.filled[duplicated(mun.mat.filled$unique.id2),]                                   # shows duplicates
mun.mat.filled <- mun.mat.filled[!duplicated(mun.mat.filled$unique.id2),]                  # removing duplicates in the harmonization file

#add district name, ethnicity, coalition
mun.mat.filled$district.name <- harmonized.names$district.name[match(mun.mat.filled$district.id, harmonized.names$district.id)]
mun.mat.filled$ethnicity <- harmonized.df$ethnicity[match(mun.mat.filled$party, harmonized.df$name.unified1)]  #name.unified since party names are already unified; not oriignal party names anylonger
mun.mat.filled$coalition <- harmonized.df$coalition[match(mun.mat.filled$party, harmonized.df$name.unified1)]

#create (again) district.magnitude, votes.district, votes.district.relat 

mun.mat.filled[is.na(mun.mat.filled)] <- 0      #sets those fields which were NA (since they didn't exist) to zero
mun.mat.filled <- ddply(mun.mat.filled, .(year, district.id), transform, district.magnitude=sum(seats)) 
mun.mat.filled <- ddply(mun.mat.filled, .(year, district.id), transform, votes.district=sum(votes))
mun.mat.filled$votes.district.relat   <- round(mun.mat.filled$votes/mun.mat.filled$votes.district, 4)



# #standard (absolute vote) -----------------------------------------------

mun.mat.filled <- ddply(mun.mat.filled, .(year, party), transform, pns.standard=(round(1-gini(votes), 4)))
pns.standard <- ddply(mun.mat.filled, .(year, party), summarize, pns.standard=mean(pns.standard))
pns.standard  <- pns.standard[order(pns.standard$pns.standard),]  #good basis for plot
pns.standard.na <- pns.standard[!complete.cases(pns.standard),]   #these are parties which had zero votes in total; delete
pns.standard <- na.omit(pns.standard)                             #dropos observatons which had zero votes 

# #modified (relative vote share) -----------------------------------------

mun.mat.filled <- ddply(mun.mat.filled, .(year, party), transform, pns.modified=(round(1-gini(votes.district.relat), 4)))
pns.modified <- ddply(mun.mat.filled, .(year, party), summarize, pns.modified=mean(pns.modified))
pns.modified <- pns.modified[order(pns.modified$party, pns.modified$year),] #good basis for plot
pns.modified.na <- pns.modified[!complete.cases(pns.modified),] #some na.s (those who didn't gain a single vote; division by zero)
pns.modified <- na.omit(pns.modified)                           #drops observatons which had zero votes 


# #intra-ethnic (based oly on distircts where ethnicity is competing --------

#inclusion of specific district into 'ethnic territory" is based on share of seats of all parties of ethnicity x in the specific district


mun.mat.filled1 <- merge(mun.mat.filled, mun.d, by=c("district.id", "year"))                                      #merging brings in info needed to selecton on ethnicity and seats
mun.mat.filled1 <- rename(mun.mat.filled1, c(district.name.x="district.name", 
                                             votes.district.x="votes.district",
                                             district.magnitude.x="district.magnitude"))
mun.mat.filled1$district.name.y <- mun.mat.filled1$votes.district.y <- mun.mat.filled1$district.magnitude.y <- NULL

pns.ethnic <- data.frame(party=character(0), ethnicity=character(0), year=integer(0), pns.ethnic=integer(0))      #empty data.frame created to available inside loop
row.names(pns.ethnic) <- NULL

for (e in unique(mun.mat.filled1$ethnicity))
       {
      
      mun.mat.filled1$tmp.seats.rel <- mun.mat.filled1[[paste0('seats.', e,'.rel')]]                                    #creates temp column which gets value of relative seats of ethnic group; needed for loop
      
      mun.mat.filled1.x <- subset(mun.mat.filled1, tmp.seats.rel>=20 & ethnicity==e)                                     #all districts in which a e.g. croat parties have in total more than 5 % of the seats
      mun.mat.filled1.x <- ddply(mun.mat.filled1.x, .(year, party), transform, pns.ethnic=1-gini(votes.district.relat))   #relative vote share is used ("modified pns")
      
      pns.ethnic1 <- ddply(mun.mat.filled1.x, .(year, party, ethnicity), summarize, pns.ethnic=round(mean(pns.ethnic), 4))        #summarize to get better party overview
      
      pns.ethnic <- rbind(pns.ethnic, pns.ethnic1)                                                                      #appends results for different ethnic groups to one dataframe
      row.names(pns.ethnic) <- NULL
      
       }

mun.mat.filled1$tmp.seats.rel <- NULL
pns.ethnic <- pns.ethnic[order(pns.ethnic$party, pns.ethnic$year),]
pns.ethnic.na <- pns.ethnic[!complete.cases(pns.ethnic),]
pns.ethnic.na <- pns.ethnic.na[order(pns.ethnic.na$party),]                                                       #if the threshold for selecting ethnic territory is 0; results is the same as for the pns.modified; also same NAs

# sapply(mun.mat.filled1, function(x) sum(is.na(x)))                                                              # more elegant way to check for missing values

# compile different PNS in one table --------------------------------------

pns <- merge(pns.standard, pns.modified, by=c("year", "party"))
pns  <- merge(pns, pns.ethnic, by=c("year", "party"), all.x=TRUE)
pns <- pns[order(pns$party, pns$year),]

tmp <- pns[!complete.cases(pns$pns.standard),]


#select most important parties
pns.most <- pns[which(pns$party=="Bosanskohercegovacka Patriotska Stranka (BPS)" | 
                   pns$party=="BOSS - Bosanska Stranka" | 
                   pns$party=="Demokratska Narodna Zajednica (DNZ)" | 
                   pns$party=="Demokratski Narodni Savez (DNS)" |  
                   pns$party=="Hrvatska Demokratska Zajednica (HDZ)" |  
                   pns$party=="Hrvatska Krscanska Demokratska Unija (HKDU)" |  
                   pns$party=="Hrvatska Seljacka Stranka (HSS)" |  
                   pns$party=="Hrvatska Stranka Prava (HSP)" |  
                   pns$party=="Liberalno Demokratska Stranka Bih (LDS)" |  
                   pns$party=="Savez Nezavisnih Socijaldemokrata (SNSD)" |  
                   pns$party=="Savez Za Bolju Buducnost (SBB)" |  
                   pns$party=="Savez Za Demokratsku Srpsku (SzDS)" |  
                   pns$party=="Socijaldemokratska Partija (SDP)" |  
                   pns$party=="Srpska Demokratska Stranka (SDS)" |  
                   pns$party=="Demokratska Stranka (DS)" |               
                   pns$party=="Srpska Radikalna Stranka (SRS)" |   
                   pns$party=="Srpska Patriotska Stranka (SPAS)" |   
                   pns$party=="Srpska Narodna Stranka  (SNS)" |   
                   pns$party=="Stranka Za Bosnu I Hercegovinu (SzBiH)" | 
                   pns$party=="Stranka Demokratske Akcije (SDA)" | 
                   pns$party=="Srpska Narodna Stranka (SNS)" ),]



# AVERAGE & SD VOTESHARE for every Party --------------------------------------

average.voteshare <- ddply(mun, .(year, party), summarize, average.voteshare=mean(votes.district.relat), sd.voteshare=sd(votes.district.relat))


# # PLOT AVERAGE VOTESHARE ------------------------------------------------

# average.voteshare.tmp <- subset(average.voteshare, party==p)
# 
# average.voteshare.plot <- ggplot(average.voteshare.tmp, aes(year, average.voteshare))+
#   geom_line()+geom_point()+
#   labs(x="year", y="mean share of votes in municipalities", fill="mean voteshare in district") #+
# #   ggtitle("Mean voteshare in municipalities")  #taken out since not needed when combining graphs
# average.voteshare.plot <- average.voteshare.plot+geom_ribbon(aes(ymax=average.voteshare+sd.voteshare, ymin=average.voteshare-sd.voteshare), alpha=0.5)
# average.voteshare.plot <- average.voteshare.plot+scale_y_continuous(limits=c(-10,50))
# 
# path="graphs/BiH/"
# filename1="Average Votesshare - Municipal level"
# ext=".png"
# filename=paste(path,filename1,ext)
# png(file=filename, width=740, height=507, res=72)
# 
# print(average.voteshare.plot) 
# plot(average.voteshare.plot) 
# dev.off() 

# PLOT different PNS over time ---------------------------------------------

row.names(pns.most) <- NULL
pns.most.melt <- melt(pns.most, id=c("year", "party", "ethnicity"))
pns.most.melt <- rename(pns.most.melt, c(variable="PNS.index", value="PNS.score"))
require(ggplot2)
library(grid)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

for (p in unique(pns.most.melt$party))
{
        pns.most.melt.tmp <- subset(pns.most.melt, party==p)
        
        pns.plot <- ggplot(pns.most.melt.tmp, aes(year, PNS.score, colour=PNS.index)) + 
               geom_line(aes(group = PNS.index)) + geom_point() +
               labs(x="year", y="PNS score", fill="%different PNS indices")+
               ggtitle(paste("PNS Scores -", p, "\n Municipal Elections","\n", sep=" "))+
               theme(plot.title = element_text(lineheight=.8, face="bold"))+
               theme(legend.justification=c(0,0), legend.position=c(0,.5))
                
               pns.plot <- pns.plot+scale_y_continuous(limits=c(0,1))
               plot(pns.plot)
                  
        average.voteshare.tmp <- subset(average.voteshare, party==p)
                
        average.voteshare.plot <- ggplot(average.voteshare.tmp, aes(year, average.voteshare))+
                  geom_line()+geom_point()+
                  labs(x="year", y="averag. % of votes in municip.", fill="mean voteshare in district") #+
                #   ggtitle("Mean voteshare in municipalities")  #taken out since not needed when combining graphs
                average.voteshare.plot <- average.voteshare.plot+geom_ribbon(aes(ymax=average.voteshare+sd.voteshare, ymin=average.voteshare-sd.voteshare), alpha=0.5)
                average.voteshare.plot <- average.voteshare.plot+scale_y_continuous(limits=c(-10,50))
                plot(average.voteshare.plot)
        
        path=path
        filename1="PNS.indices.combined"
        party=p
        ext=".png"
        filename=paste(path,filename1,party,ext)
        png(file=filename, width=740, height=507, res=72)
        
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 1)))
        print(pns.plot, vp = vplayout(1:2, 1))
        print(average.voteshare.plot, vp = vplayout(3,1))
        
        dev.off() 
}


# PNS.ethnic for all parties ----------------------------------------------

pns.ethnic1 <- subset(pns.most.melt, PNS.index=="pns.ethnic")
pns.ethnic1 <- rename(pns.ethnic1, c(PNS.index="PNS.ethnic"))

pns.ethnic.plot <- ggplot(pns.ethnic1, aes(factor(year), PNS.score, color=party))+
  geom_line(aes(group=party))+
  geom_point()+
  ggtitle("PNS ethnic scores")+
  geom_text(data=pns.ethnic1[pns.ethnic1$year==2012,], aes(label=party), size=4, hjust=0.50, vjust=0)+
  theme(legend.position="none")
  
  plot(pns.ethnic.plot)
  print(pns.ethnic.plot)

  path="Z:/Documents/ro - ceu/R/graphs/draft/"
  filename1="PNS.ethnic"
  ext=".png"
  filename=paste(path,filename1,ext)
  ggsave(file=filename, unit="cm")

  plot(pns.ethnic.plot)
  print(pns.ethnic.plot)

  dev.off()
  
  

# Party SYSTEM nationalizations score (see Jones, Mainwaring (2003)) ------------------------------------

Vote.absolute.national=ddply(mun, .(year, party), summarize, vote.absolute.national=sum(votes))  
Vote.absolute.national <- Vote.absolute.national[order(Vote.absolute.national$party), ]
Vote.absolute.national <- ddply(Vote.absolute.national, .(year), transform, vote.total.year=sum(vote.absolute.national))
Vote.absolute.national$vote.share.national <- round((Vote.absolute.national$vote.absolute.national/Vote.absolute.national$vote.total.year), 4)

psns <- merge(Vote.absolute.national, pns, by=c("party", "year"))
psns <- psns[order(-psns$vote.share.national),]

psns$standard.x.share <- psns$vote.share.national*psns$pns.standard  
psns$modified.x.share <- psns$vote.share.national*psns$pns.modified                 #why is the standard and modified psns only different by level (parallel lines)?
psns.x <- ddply(psns, .(year), summarize, psns.standard=sum(standard.x.share), psns.modified=sum(modified.x.share))



# psns.y <- ddply(psns, .(year), summarize, psns.standard=sum(vote.share.national*pns.standard), psns.modified=sum(vote.share.national*pns.modified)) # identical result


# # # PSNS for ethnic party systems -----------------------------------------
##### Basis for the national vote share is not the entire country, but only the territory of the "ethnic party system"

voteshare.ethnic1<- data.frame(party=character(0), ethnicity=character(0), year=integer(0), voteshare.ethnic=integer(0))      #empty data.frame created to available inside loop
row.names(voteshare.ethnic1) <- NULL

for (e in unique(mun.mat.filled1$ethnicity))
{
  mun.mat.filled1$tmp.seats.rel <- mun.mat.filled1[[paste0('seats.', e,'.rel')]]                                                    # creates temp variable which gets value of relative seats of ethnic group; needed for loop
  tmp.x <- subset(mun.mat.filled1, tmp.seats.rel>20 & ethnicity==e)      # must be same threshold as when calculating PNS                                                              # all districts in which a e.g. croat parties have in total more than 5 % of the seats
  
  y <- ddply(tmp.x, .(year, party), summarize, votes.party=sum(votes))   # relative vote share is used ("modified pns")
  y <- ddply(y, .(year), transform, vote.total.year=sum(votes.party)) 
 
  y$voteshare.ethnic <- y$votes.party / y$vote.total.year
  
  
  voteshare.ethnic1 <- rbind(voteshare.ethnic1, y)                       # appends results for different ethnic groups to one dataframe
  row.names(voteshare.ethnic1) <- NULL
  
}

psns <- merge(psns, voteshare.ethnic1, by=c("year", "party"), x.all=TRUE)
psns$x <- psns$vote.total.year.y <- psns$name.unified2 <- NULL

psns$ethnic.x.share <- psns$voteshare.ethnic*psns$pns.ethnic           # some parties don't have an ethnic pns since they were existing only in a territory which was not part of the 'ethnic territory'

newdata <- na.omit(psns$pns.ethnic) 

psns.x.e <- ddply(psns, .(year, ethnicity), summarize, psns.ethnic=sum(ethnic.x.share, na.rm=TRUE))    #na.rm excludes missing values; for those parties with no pns
psns.x.e <- reshape(psns.x.e, idvar=("year"), timevar="ethnicity", direction="wide")
row.names(psns.x.e) <- NULL

psns <- merge(psns.x, psns.x.e, by=c("year"))


# PLOT PSNS over time -----------------------------------------------------

psns.melt <- melt(psns, id="year")
psns.melt <- rename(psns.melt, c(variable="PSNS.index", value="PSNS.value"))

psns.plot <- ggplot(psns.melt, aes(year, PSNS.value, colour=PSNS.index)) + 
  geom_line(aes(group = PSNS.index)) + geom_point() +
  labs(x="year", y="PSNS score", fill="%different PSNS indices")+
  ggtitle("PSNS Scores - Municipal Elections")  
  psns.plot <- psns.plot+scale_y_continuous(limits=c(0,1))

  path="Z:/Documents/ro - ceu/R/graphs/draft/"
  filename1="psns.plot"
  ext=".png"
  filename=paste(path,filename1,ext)
  ggsave(file=filename, unit="cm")




# PEDERSEN Index for Municipalities (Electoral Volatility) -----------------------------------

diff2 <- function(x)diff(c(0,x))

pedersen <- mun.mat.filled[,c("year","district.id","district.name","party","votes.district.relat")]

pedersen <- subset(pedersen, pedersen$votes.district.relat>0)

pedersen <- pedersen[order(pedersen$district.id, pedersen$party, pedersen$year),]
pedersen1 <- ddply(pedersen, .(district.id, party), transform, relat.vote.change=diff2(votes.district.relat))
pedersen1$abs.vote.change <- abs(pedersen1$relat.vote.change)
pedersen1 <- ddply(pedersen1, .(district.id, year), transform, sum.relat.vote.change1=sum(relat.vote.change[relat.vote.change > 0]))   #only aggreate positive changes (see Pedersen formula)

pedersen2 <- aggregate(relat.vote.change~year+district.id+district.name,      #equivalent to sum(relat...[relat.vote.chaange>0])
          sum,
          data=subset(pedersen1,relat.vote.change>0))

pedersen2 <- ddply(pedersen2, .(district.id), transform, first.year=as.numeric(interaction(year, district.id)))  #creates an identifier for every year; allows to delete first year

pedersen2 <- subset(pedersen2, first.year!=1)  #takes out the first year which by definition has a volatility of 100



# x=subset(pedersen1, party=="Demokratska Narodna Zajednica (DNZ)" | party=="BOSS - Bosanska Stranka")
# x=subset(x, district.id==1)
# x <- x[order(x$district.id, x$party, x$year),]
# x1 <- ddply(x, .(district.id, year), transform, sum.relat.vote.change1=sum(relat.vote.change[relat.vote.change > 0]))




# # PLOT PEDERSEN volatility ----------------------------------------------

x <- merge(pedersen2, harmonized.names, by="district.id")
x$comment <- x$district.name.y <- x$district.id.mean <- NULL
x <- rename(x, c(district.name.x="district.name"))
x <- merge(x, Map, by.x="map_id3", by.y="ID_3")
x$PID <- x$ID_0 <- x$NAME_3 <- x$check <- x$NL_NAME_3 <- x$VARNAME_3 <- x$ISO <- x$NAME_0 <- x$TYPE_3 <- x$ENGTYPE_3<- NULL

pedersen2 <- x

for(n in unique(pedersen2$NAME_1)){  

  pedersen2.tmp <- subset(pedersen2, NAME_1==n)
  
  pedersen.plot <- ggplot(data=pedersen2.tmp, aes(x=as.factor(year), y=relat.vote.change, group=district.id, color=district.id)) + 
                    geom_line(aes(color=district.id)) + 
                    geom_point()+
                    ggtitle(paste("Electoral Volatility","\n"))+
                    labs(x="year", y="% volatility")+
                    theme(legend.position="none")+
                    facet_wrap(~ NAME_1 + NAME_2, ncol=2)

                    path=path
                    filename1="Volatility"
                    entity=n
                    ext=".png"
                    filename=paste(path,filename1,entity,ext)
                    png(file=filename)
                    print(pedersen.plot) 
                    plot(pedersen.plot)
                    dev.off()
  
                    }

#   canton.plot <- ggplot(data.tmp, aes(x=as.factor(year), y=seats, fill=party, color=party)) +
#   geom_bar(bandwidth=1, stat="identity", group="party", position="fill") +
#   labs(x="year", y="% of seats") +
#   ggtitle(paste("% of Seats for Canton Assembly","\n","Canton:", c,"\n", sep=" "))+                      
#   theme(legend.position="bottom", plot.title=element_text(face="bold"), legend.text=element_text(size=8))+
#   guides(fill=guide_legend(nrow=4, title.position="top", keywidth=0.5, keyheight=0.2, size=5))               



# "Imapct" of CANTON dominance on municipalities ----------------------------

mun$map_id3 <- 0
mun$map_id3 <- harmonized.names$map_id3[match(mun$district.id, harmonized.names$district.id)]

mapx <- Map[,c("ID_2", "ID_3", "NAME_2")]
mun.x  <-  merge(mun, mapx, by.x="map_id3", by.y="ID_3")
mun.x <- rename(mun.x, c(NAME_2="canton.name", ID_2="map_id2"))

mun.x$canton.id <- 0
for(id in 1:nrow(data)){                                                #brings canton.id into mun.x
  mun.x$canton.id[mun.x$map_id2 %in% data$map_id2[id]] <- data$canton.id[id]
}


# Multiplot Function ------------------------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#  PLOT CANTON vs MUNICIPALITY COMPOSITION   --------------------------------------------------------

  library(scales)
  library(ggplot2)
  library(grid)

for (c in 201:210) {                                # 2 - 11 are map_id2 numbers for Cantion 1 to 10;
  
  # c=207   #for test purpose

  #how many rows are needed for combined plot (if canton plot is in a row alone)
  #   o <- unique(mun.x[,c('canton.id','district.id')])
  #   row.names(o) <- NULL
  #   o <- subset(o, canton.id==c)
  #   x <- nrow(o)
  #   o1 <- ((nrow(o)+2)/2)
  
  data.tmp <- subset(data, canton.id==c & seats>0)
  data.tmp <- data.tmp[order(data.tmp$ethnicity),]
      
  canton.plot <- ggplot(data.tmp, aes(x=as.factor(year), y=seats, fill=party, color=party)) +
    geom_bar(bandwidth=1, stat="identity", group="party", position="fill") +
    labs(x="year", y="% of seats") +
    ggtitle(paste("% of Seats for Canton Assembly","\n","Canton:", c,"\n", sep=" "))+                      
    theme(legend.position="bottom", plot.title=element_text(face="bold"), legend.text=element_text(size=8))+
    guides(fill=guide_legend(nrow=4, title.position="top", keywidth=0.5, keyheight=0.2, size=5))                                       
  
    path="graphs/BiH/"
    filename1="CompositionCanton"
    Canton=c
    ext=".pdf"
    filename=paste(path,filename1,Canton,ext)
    pdf(file=filename)
    
    #    print(canton.plot) 
    #    plot(canton.plot)
    dev.off()
#      
    for (d in unique(mun.x$district.id)) {  
    
      # d=125
      
      mun.x.tmp <- subset(mun.x, canton.id==c & district.id==d & seats>0)
      mun.x.tmp <- mun.x.tmp[order(mun.x.tmp$ethnicity), ]
        
        if (nrow(mun.x.tmp)> 0) { 
        
            municipality.plot <- ggplot(mun.x.tmp, aes(x=as.factor(year), y=seats, fill=party, color=party)) +
              geom_bar(bandwidth=1, stat="identity", group="party", position="fill") +
              labs(x="year", y="% of seats") +
              ggtitle(paste("% of Seats for Municipal Council","\n","Municipality:", d, "Canton:", c, "\n", sep=" "))+
            #  ggtitle(paste("% of Seats for Municipal Council","\n","Canton:", c, "\n", sep=" "))+
              theme(legend.position="bottom", plot.title=element_text(face="bold"), legend.text=element_text(size=8))+
              guides(fill=guide_legend(nrow=3, title.position="top", keywidth=0.5, keyheight=0.2, size=5)) #+
              #facet_wrap(~ district.id, ncol=2)
              
              path="Z:/Documents/ro - ceu/R/graphs/draft/"
              filename1="CompositionMunicipality"
              Canton=c
              Municipality=d
              ext=".pdf"
              filename=paste(path,filename1,Canton,Municipality,ext)                  
              pdf(file=filename)
             
              #  png(file=filename, width=740, height=507, res=72)
              #  print(municipality.plot) 
              #  plot(municipality.plot)
              dev.off() 
            
              path="Z:/Documents/ro - ceu/R/graphs/draft/"
              filename1="Canton"
              filename2="Municipality"
              filename3="combined"
              canton=c
              district=d
              ext="png"
              filename=paste(path,filename1,c, filename2,d, filename3,ext)
              png(file=filename) #, width=740, height=507, res=72)
              
              grid.newpage()
              pushViewport(viewport(layout = grid.layout(4,2)))
              print(canton.plot, vp = vplayout(1:2,1:2))
              print(municipality.plot, vp = vplayout(3:4,1:2))
              dev.off()
            
            #   multiplot(canton.plot, municipality.plot, rows=2)
            #   library(gridExtra)
            #   grid.arrange(canton.plot, municipality.plot, ncol=2, main="Comparision")  #allows adding title
            dev.list()
            }
          }
}



# Map template ------------------------------------------------------------

theme_custom <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      plot.title= element_text(size=rel(1.5), face="bold"),
      axis.line=element_line(size=rel(0)),
      legend.title=element_text(size=rel(1), face="plain"),
      legend.position="bottom",
      panel.background = element_rect(fill="white"),
      plot.background = element_rect(fill="white")
    )   
}




# MAP - MUNICIPAL VOTESHARE --------------------------
library(tidyr)
#require("plyr")
library(dplyr)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
library(maps)
library(ggplot2)
library(tmap)

# install.packages("devtools")
# devtools::install_github("tidyverse/ggplot2")

library(sf)


# >> data set with election results ---------------------------------------

df.thesis <- df.final %>%
  select("district.id","district.name","map_id3","year","party","votes")%>%
  group_by(year, district.name)%>%
  mutate(votes.rel=round(votes/sum(votes)*100,2))%>%
  mutate(ID_3=as.numeric(map_id3))%>%
  ungroup()

SDA2004 <-  df.thesis %>%
    filter(grepl("SDA", party))%>%
    filter(!grepl("Aktivnosti", party))%>%
    filter(year==2004)

SDA <-  df.thesis %>%
  filter(grepl("SDA", party))%>%
  filter(!grepl("Aktivnosti", party))


df.thesis <- df.thesis %>%
  mutate(elect.list=case_when(grepl("SDA", party) & !grepl("Aktivnosti", party) ~ "SDA"))


# >> Create dataframe containing all combinations of year x party  --------

#library(tidyr)

df.combis <- df.thesis %>%
  select(party, year, ID_3)%>%
  expand(party, year, ID_3)

length(unique(df.complete$year)) #5
length(unique(df.complete$party)) #859
length(unique(df.complete$ID_3)) #141
  
#141*859*5 = 605595 // expand produces 640140


# >>> merge expanded combinations with observed ---------------------------
#https://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key
df.full <- dplyr::left_join(df.combis, df.thesis, by=c("party"="party", 
                                                 "year"="year",
                                                 "ID_3"="ID_3"))



# >> select parties and include coalitions --------------------------------

df.full <-  df.full %>% 
  mutate(
    elect.list=case_when(grepl("SDA", party) & !grepl("Aktivnosti", party) ~ "SDA",
                         grepl("HDZ", party) & !grepl("1990", party) ~ "HDZ",
                         grepl("SzBiH",party) | grepl("Stranka za Bosnu", party) ~ "SzBiH",
                         grepl("SNSD", party) ~ "SNSD",
                         grepl("SDS", party) ~ "SDS",
                         grepl("SDP", party) ~ "SDP"))


# >> import shape file with sf package ----------------------------------------------------
#library(sf)
#http://strimas.com/r/tidy-sf/
BiH.shape.sf <- sf::st_read("BiHShapeFile/BIH_adm3.shp", quiet = TRUE)
class(BiH.shape.sf) #sf object
glimpse(BiH)



# BiH.shape<- readShapePoly("BiHShapeFile/BIH_adm3.shp")
# gpclibPermit()
# BiH.shape<-  fortify(BiH.shape, region="ID_3")     


# >>> print empty shape file ----------------------------------------------

empty.shape <- BiH.shape %>%
  ggplot(.,aes(long, lat, group=group))+
  geom_polygon(colour="black", fill="grey")+
  coord_equal()+
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10),
        panel.background=element_rect(fill="white"))

print(empty.shape)


# >>>> empty shape file with sf package -----------------------------------

empty.shape.sf <- 
  ggplot(BiH.shape.sf)+
  geom_sf(fill="grey",colour="black")+
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10),
        panel.background=element_rect(fill="white"))

print(empty.shape.sf)



# >> Number of districts --------------------------------------------------

library(dplyr)
n.ids <- df.thesis %>%
  select(year, id)%>%
  distinct()%>%
  group_by(year)%>%
  dplyr::summarise(n.id=n())

n.ids.shape <- BiH.shape %>%
  select(id)%>%
  distinct()%>%
  count() #141 district ids


# >> Merge dataset with shapefile -----------------------------------------

df.shape <- dplyr::left_join(BiH.shape.sf, SDA2004, by="ID_3")
#with sf package dplyr command possible

# >> print shape file with data -------------------------------------------


# >>> SDA 2004 ------------------------------------------------------------


SDA2004.map <- 
  ggplot(df.shape)+
  geom_sf(aes(fill=votes.rel),colour="black")+
  theme(legend.position="bottom",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10),
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="white", high="darkgreen", limits=c(0,100))

print(SDA2004.map)



# >>> SDA all years -------------------------------------------------------

df.shape <- dplyr::left_join(BiH.shape.sf, data, by="ID_3")

SDA.map <- df.shape%>%
  filter(year==2000|year==2004)%>%
  ggplot()+
  geom_sf(aes(fill=votes.rel),colour="black", na.rm = TRUE)+
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10),
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="blue", high="red", limits=c(0,100))+
  facet_wrap(~year, drop=TRUE)

print(SDA.map)

# >>>> Merge dataset with shapefile all combinations -----------------------------------------

df.shape <- dplyr::left_join(BiH.shape.sf, df.full, by="ID_3")

data.map <- df.shape%>%
  filter(!is.na(elect.list))%>%
  group_by(year, ID_3, elect.list)%>%
  #slice(which.max(votes.rel))%>%
  summarise(votes.rel=max(votes.rel, na.rm = TRUE))#%>%
  #mutate(votes.rel.cut=cut(votes.rel, seq(0,100,10)))

data.map$votes.cut <- cut(data.map$votes.rel, seq(0,100,10))
data.map$elect.list <- as.factor(data.map$elect.list)  
levels(data.map$elec)

## Reordering data.map$elect.list
data.map$elect.list <- factor(data.map$elect.list, levels=c("SDA", "SzBiH", "SNSD", "SDS", "HDZ", "SDP"))


# >>> BOS ------------------------------------------------------------------

library(ggplot2)
data.map.plot.BOS <- data.map %>%  
  filter(elect.list=="SDA" | elect.list=="SzBiH") %>%
  ggplot() +
  geom_sf(aes(fill=votes.rel), colour="darkgrey", size=.01)+
  coord_sf(datum = NA) + 
    labs(title="Municipal Elections: Percentage of votes per party",
       #caption="Data: Central Election Commission BiH",
       subtitle="Coalitions included; Grey areas: Party did not run.")+
  theme_minimal()+
  theme(legend.position="left",
        legend.title=element_blank(),
        legend.text = element_text(size=6),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.margin=unit(c(0,0,-1,0), "cm"),
        # axis.ticks = element_blank(),
        # plot.caption=element_text(size=8),
        legend.key.height = unit(0.5,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.size=unit(0.5, "cm"),
        strip.text.y=element_text(size= 10, angle=360),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
      #  panel.border=element_blank(),
        panel.border = element_rect(color = "white", fill = NA, size = 1), 
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="white", high="#2E8B57", limits=c(0,100), na.value="lightgrey",
                      labels=c("   0","   25","   50","   70", "  100"))+
  facet_grid(elect.list~year)

print(data.map.plot.BOS)

# >> Croat Plot ----------------------------------------------------------

data.map.plot.CRO <- data.map %>%  
  filter(elect.list=="HDZ") %>%
  ggplot() +
  geom_sf(aes(fill=votes.rel), colour="darkgrey", size=.01)+
  coord_sf(datum = NA) + 
    # labs(title="Municipal Elections: Percentage of votes per party",
  #      subtitle="Coalitions included",
  #      caption="Data: Central Election Commission BiH")+
  theme_minimal()+
  theme(legend.position="left",
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        legend.text = element_text(size=6),
        legend.key.height = unit(0.5,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.size=unit(0.5, "cm"),
        plot.margin=unit(c(-4,0,-4,0), "cm"),
        # axis.ticks = element_blank(),
        # plot.caption=element_text(size=8),
        strip.text.y=element_text(size= 10, angle=360),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        #  panel.border=element_blank(),
        panel.border = element_rect(color = "white", fill = NA, size = 1), 
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="white", high="#1874CD", limits=c(0,100), 
                      labels=c("   0","   25","   50","   70", "  100"),
                      na.value="lightgrey")+
  facet_grid(elect.list~year)

#"#2E8B57", "#1874CD", "#B22222"

print(data.map.plot.CRO)


# >>> Serb ----------------------------------------------------------------


data.map.plot.SER <- data.map %>%  
  filter(elect.list=="SDS" | elect.list=="SNSD") %>%
  ggplot() +
  geom_sf(aes(fill=votes.rel), colour="darkgrey", size=.01)+
  coord_sf(datum = NA) + 
    # labs(title="Municipal Elections: Percentage of votes per party",
  #      subtitle="Coalitions included",
  #      caption="Data: Central Election Commission BiH")+
  theme_minimal()+
  theme(legend.position="left",
        legend.text = element_text(size=6),
        legend.title=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        legend.key.height = unit(0.5,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.size=unit(0.5, "cm"),
        plot.margin=unit(c(-1,0,-1,0), "cm"),
        # axis.ticks = element_blank(),
        # plot.caption=element_text(size=8),
        strip.text.y=element_text(size= 10, angle=360),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        #  panel.border=element_blank(),
        panel.border = element_rect(color = "white", fill = NA, size = 1), 
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="white", high="#B22222", limits=c(0,100), na.value="lightgrey",
                      labels=c("   0","   25","   50","   70", "  100"))+
  facet_grid(elect.list~year)

#"#2E8B57", "#1874CD", "#B22222"

print(data.map.plot.SER)

# >> Civic ----------------------------------------------------------------
data.map.plot.CIV <- data.map %>%  
  filter(elect.list=="SDP") %>%
  ggplot() +
  geom_sf(aes(fill=votes.rel), colour="darkgrey", size=.01)+
  coord_sf(datum = NA) + 
     labs(caption="Data: Central Election Commission BiH")+
  # title="Municipal Elections: Percentage of votes per party",
  #      subtitle="Coalitions included",
  theme_minimal()+
  theme(legend.position="left",
        legend.title=element_blank(),
        legend.text = element_text(size=6),
        plot.caption=element_text(size=6),
        axis.title=element_blank(),
        axis.text=element_blank(),
        legend.key.height = unit(0.5,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.size=unit(0.5, "cm"),
        plot.margin=unit(c(-1,0,1,0), "cm"),
        strip.text.y=element_text(size= 10, angle=360),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border = element_rect(color = "white", fill = NA, size = 1), 
        panel.background=element_rect(fill="white"))+
  scale_fill_gradient(low="white", high="darkorange", limits=c(0,100), na.value="lightgrey", 
                      name="% of votes",
                      labels=c("   0","   25","   50","   70", "  100"))+  #space between legend key and text
  facet_grid(elect.list~year)

print(data.map.plot.CIV)

# >> combined plot --------------------------------------------------------
library(gridExtra)

library(ggpubr)
combined.graph <- ggarrange(data.map.plot.BOS, 
                            data.map.plot.CRO,
                            data.map.plot.SER,
                            data.map.plot.CIV,
                            heights=c(60,20,50,30),
                            widths=1,
                            ncol=1, nrow=4, align="v")

#remove space between combined plots
#https://stackoverflow.com/questions/15556068/removing-all-the-space-between-two-ggplots-combined-with-grid-arrange

plot(combined.graph)

folder <-"Z:/Documents/ro - ceu/R/graphs/draft/"
time <- format(Sys.time(),"%Y%m%d-%H%M%S")
filename <-"-MunicipalMap.png"
ggsave(paste(folder,time,filename, sep=""), width=16, height=24, unit="cm")




BiH.shape$id <- as.numeric(BiH.shape$id)

#BiH.shape <- full_join(BiH.shape, SDA, by = c("id" = "map_id3"))

BiH.shape <- plyr::join(BiH.shape, SDA, by="id")

BiH.shape <- BiH.shape[order(BiH.shape$order),]

voteshare.map <- ggplot(BiH.shape, aes(long, lat, 
                                       group=group, 
                                       fill=votes.rel, 
                                       colour=party))+
  geom_polygon(colour="grey", size=0.25)+
  coord_equal()+
  labs(fill= "Municipal Voteshare",
       title="Percentage of votes in municipal elections",
       subtitle="",
       caption="Data: Central Election Commission BiH")+
  theme(legend.position="bottom",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10))+
  scale_fill_gradient(low="white", high="darkgreen", limits=c(0,100))+
  facet_grid(.~year)

print(voteshare.map)








#>>>>>>>>>>>>>>>>>>>>>>>>>>>
new.df <- x <- mun.mat.filled1[,c("district.id", "year", "map_id3", "party", "votes.district.relat", "seats.district.relat", "seats", "map_id3")]
class(new.df)

x <- ddply(x, .(year, party), transform, sum.votes.district.relat=sum(votes.district.relat))   #votes.district.relat since absolute votes would be biased towards larger cities
x$relat.voteshare <- x$votes.district.relat/x$sum.votes.district.relat*100                     

y <- ddply(x, .(year, party), transform, seats.year=sum(seats))
x <- subset(y, seats.year>10)                                 #take only those parties which have at least one municipal seats per year; otherwise the merging for the map takes too long






library(maps)
library(ggplot2)

BiH.shape<- readShapePoly("BiHShapeFile/BIH_adm3.shp")

gpclibPermit()
BiH.shape<-  fortify(BiH.shape, region="ID_3")     
BiH.shape <- merge(BiH.shape, x, by.x="id", by.y="map_id3")  
BiH.shape <- BiH.shape[order(BiH.shape$order),]


voteshare.map <- ggplot(BiH.shape, aes(long, lat, 
                                       group=group, 
                                       fill=votes.district.relat, 
                                       colour=party))+
  geom_polygon(colour="grey", size=0.25)+
  coord_equal()+
  labs(fill= "Municipal Voteshare",
       title="Percentage of votes in municipal elections",
       subtitle="",
       caption="Data: Central Election Commission BiH")+
  #           ggtitle(paste("Municipal Elections - voteshare\n", p, "\n", sep=""))+
  theme(legend.position="bottom",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 10))+
  scale_fill_gradient(low="white", high="darkgreen", limits=c(0,1))+
  facet_grid(party~year)

print(voteshare.map)






for (p in unique(BiH.shape$party))     {
          

  
  
  
  

        BiH.shape.tmp <- subset(BiH.shape, party=="Savez Nezavisnih Socijaldemokrata (SNSD)" | 
                                  party=="Stranka Demokratske Akcije (SDA)" | 
                                  party=="Hrvatska Demokratska Zajednica (HDZ)")

        BiH.shape.tmp$party <- as.character( BiH.shape.tmp$party)
        BiH.shape.tmp$party[BiH.shape.tmp$party=="Savez Nezavisnih Socijaldemokrata (SNSD)"] <- "SNSD"
        BiH.shape.tmp$party[BiH.shape.tmp$party=="Stranka Demokratske Akcije (SDA)"] <- "SDA"
        BiH.shape.tmp$party[BiH.shape.tmp$party=="Hrvatska Demokratska Zajednica (HDZ)"] <- "HDZ"
        BiH.shape.tmp$party <- as.factor( BiH.shape.tmp$party)

        p="Savez Nezavisnih Socijaldemokrata (SNSD)"
        
        voteshare.map <- ggplot(BiH.shape.tmp, aes(long, lat, group=group, fill=votes.district.relat, colour=party))+
          geom_polygon(colour="grey", size=0.25)+
          coord_equal()+
          labs(fill= "Municipal Voteshare",
               title="Percentage of votes in municipal elections",
               subtitle="",
               caption="Data: Central Election Commission BiH")+
#           ggtitle(paste("Municipal Elections - voteshare\n", p, "\n", sep=""))+
          theme(legend.position="bottom",
                axis.title=element_blank(),
                axis.text=element_blank(),
                axis.ticks = element_blank(),
                strip.text.y = element_text(size = 10))+
          scale_fill_gradient(low="white", high="darkgreen", limits=c(0,1))+
          facet_grid(party~year)
        
          # voteshare.map <- voteshare.map+theme_custom()
          # voteshare.map <- voteshare.map+scale_fill_gradient(low="grey", high="green", limits=c(0,100))
            
          print(voteshare.map)
        #  dev.off()   
        
  
        print(voteshare.map)
        
  BiH.shape.tmp <- subset(BiH.shape, party==p)        
          
        relat.voteshare.map <- ggplot(BiH.shape.tmp, aes(long, lat, group=group, fill=relat.voteshare))+
          geom_polygon()+
          coord_equal()+
          labs(x="Easting (m)", y="Northing (m)",fill= "Distribution of relative voteshares")+
          ggtitle(paste("Municipal Elections - Distribution of relat. Vote share\n", p, sep=""))+
          theme(legend.position="bottom")+
          scale_fill_gradient(low="grey", high="blue", limits=c(0,20))+
          facet_wrap(~year, ncol=2)
        
        path="graphs/BiH/"
        filename1="Map - MunicipalElections - Relat. Voteshare -"
        party=p
        ext=".pdf"
        filename=paste(path,filename1,party,ext)
        pdf(file=filename)
        
        # voteshare.map <- voteshare.map+theme_custom()
        # voteshare.map <- voteshare.map+scale_fill_gradient(low="grey", high="green", limits=c(0,100))
        
        print(relat.voteshare.map) 
        plot(relat.voteshare.map)
        dev.off()   
     
} 










# Map - % of votes for each ethnicity in each district  ------------------------------------------

MapLink<-read.csv("BiH/BiH - district names unification - map id.csv")

mun.d$map_id3 <- MapLink$map_id3[match(mun.d$district.id, MapLink$district.id)]

mun.d <- mun.d[,c("year", "district.id", "map_id3","district.name",
                  "votes.district", "gini.votes",  
                  "district.magnitude", "gini.seats",
                  
                  "votes.B", "votes.C", "votes.S","votes.M", "votes.nk", "votes.R",
                  "votes.B.rel", "votes.C.rel", "votes.S.rel", "votes.M.rel", "votes.nk.rel", "votes.R.rel",
                  
                  "seats.B", "seats.C", "seats.S","seats.M", "seats.nk", "seats.R",
                  "seats.B.rel","seats.C.rel","seats.S.rel","seats.M.rel","seats.nk.rel", "seats.R.rel",
                  
                  "party.numbers", 
                  "freq.party.B", "freq.party.C", "freq.party.S", "freq.party.M","freq.party.nk", "freq.party.R",
                  
                  "party.seats.numbers", 
                  "freq.party.seats.B", "freq.party.seats.C","freq.party.seats.S","freq.party.seats.M","freq.party.seats.nk","freq.party.seats.R")]

library(maps)
library(ggplot2)

BiH3<- readShapePoly("BiHShapeFile/BIH_adm3.shp")

gpclibPermit()
BiH3_geom <- fortify(BiH3, region="ID_3")     
BiH3_geom <- merge(BiH3_geom, mun.d, by.x="id", by.y="map_id3")  # why actully by.x="id"?
BiH3_geom <- BiH3_geom[order(BiH3_geom$order),]

BiH3_geom.tmp=BiH3_geom                    #subset(BiH3_geom, year==t)  

for (e in unique(mun$ethnicity))
{
     
#     for (t in unique(BiH3_geom$year))            #commnented; with face_wrap not for each year; but consolidated
#       {
        
        BiH3_geom.tmp$tmp.seats.rel <- BiH3_geom.tmp[[paste0('seats.', e,'.rel')]]  
            
        map.seats.rel<- ggplot(BiH3_geom.tmp, aes(long, lat, group=group, fill=tmp.seats.rel))+ geom_polygon()+ coord_equal() +
            labs(x="Easting (m)", y="Northing (m)",fill= "% of Municipal Seats")+ 
            ggtitle(paste("Municipal Elections", "\n % of Municipal Seats", "\nEthnicity:", e, sep=" "))+
            facet_wrap(~year)                                          #puts all graphs into one document
          
            path="graphs/BiH/"
            filename1="Map - Share of Municipal Seats"
            ext=".png"
            ethnicity=e
            year=t
            filename=paste(path,filename1,year, ethnicity, ext)
            png(file=filename)
            
            map.seats.rel <- map.seats.rel+theme_custom()
            map.seats.rel <- map.seats.rel+scale_fill_gradient(low="grey", high="green", limits=c(0,100))
            
            print(map.seats.rel ) 
            plot(map.seats.rel )
            dev.off()   
        
        
        BiH3_geom.tmp$tmp.votes.rel <- BiH3_geom.tmp[[paste0('votes.', e,'.rel')]]  #rel.seats; could be extended to all other collumns
                      
        map.votes.rel<- ggplot(BiH3_geom.tmp, aes(long, lat, group=group, fill=tmp.votes.rel))+ geom_polygon()+ coord_equal() +
            labs(x="Easting (m)", y="Northing (m)",fill= "% of Municipal Votes")+ 
            ggtitle(paste("Municipal Elections", "\n % of Municipal Votes", "\nEthnicity:", e, sep=" "))+
            facet_wrap(~year)                                          #puts all graphs into one document
          
            path="graphs/BiH/"
            filename1="Map - Share of Municipal Votes"
            ext=".png"
            ethnicity=e
            year=t
            filename=paste(path,filename1,year, ethnicity, ext)
            png(file=filename)
            
            map.votes.rel <- map.votes.rel+theme_custom()
            map.votes.rel <- map.votes.rel+scale_fill_gradient(low="grey", high="green", limits=c(0,100))
            
            print(map.votes.rel) 
            plot(map.votes.rel)
            dev.off()   
           
        BiH3_geom.tmp$tmp.freq.party <- BiH3_geom.tmp[[paste0('freq.party.', e)]]  
        
        map.freq.party<- ggplot(BiH3_geom.tmp, aes(long, lat, group=group, fill=tmp.freq.party))+ geom_polygon()+ coord_equal() +
          labs(x="Easting (m)", y="Northing (m)",fill= "Number of Parties")+ 
          ggtitle(paste("Municipal Elections", "\n Number of Parties of Ethnicity", e, sep=""))+
          facet_wrap(~year)                                         
        
          path="graphs/BiH/"
          filename1="Map - Number of Parties"
          ext=".png"
          ethnicity=e
          year=t
          filename=paste(path,filename1,year, ethnicity, ext)
          png(file=filename)
          
          map.freq.party<- map.freq.party+theme_custom()
          map.freq.party <- map.freq.party+scale_fill_gradient(low="grey", high="green", limits=c(0,15))
          
          print(map.freq.party) 
          plot(map.freq.party)
          dev.off()   
        
        BiH3_geom.tmp$tmp.freq.party.seats <- BiH3_geom.tmp[[paste0('freq.party.seats.', e)]]  
        
        map.freq.party.seats<- ggplot(BiH3_geom.tmp, aes(long, lat, group=group, fill=tmp.freq.party.seats))+ geom_polygon()+ coord_equal() +
          labs(x="Easting (m)", y="Northing (m)",fill= "Number of Parties with Seats")+ 
          ggtitle(paste("Municipal Elections", "\n Number of Parties of Ethnicity", e,"with Seats", sep=""))+
          facet_wrap(~year)                                          
        
          path="graphs/BiH/"
          filename1="Map - Number of Parties with Seats"
          ext=".png"
          ethnicity=e
          year=t
          filename=paste(path,filename1,year, ethnicity, ext)
          png(file=filename)
          
          map.freq.party.seats<- map.freq.party.seats+theme_custom()
          map.freq.party.seats <- map.freq.party.seats+scale_fill_gradient(low="grey", high="green", limits=c(0,10))
          
          print(map.freq.party) 
          plot(map.freq.party)
          dev.off()   
 
#         }  #commented since all years are put into one graph; keep for later
    }

print(map.seats.rel) 
plot(map.seats.rel)
print(map.votes.rel) 
plot(map.votes.rel)
print(map.freq.party) 
plot(map.freq.party)
print(map.freq.party) 
plot(map.freq.party)
dev.off()   
