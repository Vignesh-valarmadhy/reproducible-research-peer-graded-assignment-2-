# reproducible-research-peer-graded-assignment-2-
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
Loading Storm Data
stormdata <- read.csv("StormData.csv.bz2", header = TRUE, sep = ",")
storm <- stormdata[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
str(storm)
## 'data.frame':    902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
Cleaning the Data
2 variables requires a transformation into the correct values, Property Damage (PROPDMG) and Crop Damage (CROPDMG). This is done by converting the exponent data (PROPDMGEXP and CROPDMGEXP) into numerical values and mutliplying this by the values in PROPDMG and CROPDMG.

library(plyr)
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
storm$PROPDMGEXP <- mapvalues(storm$PROPDMGEXP, from = c("K", "M","", "B", "m", "+", "0", "5", "6", "?", "4", "2", "3", "h", "7", "H", "-", "1", "8"), to = c(10^3, 10^6, 1, 10^9, 10^6, 0,1,10^5, 10^6, 0, 10^4, 10^2, 10^3, 10^2, 10^7, 10^2, 0, 10, 10^8))
storm$PROPDMGEXP <- as.numeric(as.character(storm$PROPDMGEXP))
storm$PROPDMGTOTAL <- (storm$PROPDMG * storm$PROPDMGEXP)/1000000000

storm$CROPDMGEXP <- mapvalues(storm$CROPDMGEXP, from = c("","M", "K", "m", "B", "?", "0", "k","2"), to = c(1,10^6, 10^3, 10^6, 10^9, 0, 1, 10^3, 10^2))
storm$CROPDMGEXP <- as.numeric(as.character(storm$CROPDMGEXP))
storm$CROPDMGTOTAL <- (storm$CROPDMG * storm$CROPDMGEXP)/1000000000

storm$DAMAGETOTAL <- storm$PROPDMGTOTAL + storm$CROPDMGTOTAL
detach(package:plyr)
The event type will need to be cleaned with like types grouped together

storm_type <- storm %>%
  mutate(evtypegrp = 
   ifelse(grepl("LIGHTNING|LIGNTNING", EVTYPE), "LIGHTNING",
    ifelse(grepl("HAIL", EVTYPE), "HAIL",
     ifelse(grepl("RAIN|FLOOD|WET|FLD", EVTYPE), "RAIN",
      ifelse(grepl("SNOW|WINTER|WINTRY|BLIZZARD|SLEET|COLD|ICE|FREEZE|AVALANCHE|ICY", EVTYPE), "WINTER",
       ifelse(grepl("TORNADO|FUNNEL", EVTYPE), "TORNADO",
        ifelse(grepl("WIND|HURRICANE", EVTYPE), "WINDS",
         ifelse(grepl("STORM|THUNDER|TSTM|TROPICAL +STORM", EVTYPE), "STORM",
          ifelse(grepl("FIRE", EVTYPE), "FIRE",
           ifelse(grepl("FOG|VISIBILITY|DARK|DUST", EVTYPE), "FOG",
            ifelse(grepl("WAVE|SURF|SURGE|TIDE|TSUNAMI|CURRENT|SWELL", EVTYPE), "WAVE",
             ifelse(grepl("HEAT|HIGH +TEMP|RECORD +TEMP|WARM|DRY", EVTYPE), "HEAT",
              ifelse(grepl("VOLCAN", EVTYPE), "VOLCANO",
               ifelse(grepl("DROUGHT", EVTYPE), "DROUGHT",
               "OTHER")))))))))))))

  )
Results
Create a summary data frame containing for the results of the 4 different outcomes

eventsum<-storm_type %>%
  group_by(evtypegrp)%>%
  summarize(damage=sum(DAMAGETOTAL), property=sum(PROPDMGTOTAL), crops=sum(CROPDMGTOTAL), fatallities=sum(FATALITIES), injuries=sum(INJURIES))
The most harmful types of events with respect to population health
Create tables with the top 10 event type for population health and plot the results

fatallities<-head(eventsum[order(eventsum$fatallities, decreasing=TRUE),],10)
injuries<-head(eventsum[order(eventsum$injuries, decreasing=TRUE),],10)


ggplot(fatallities, aes(evtypegrp,fatallities, fill=fatallities))+
  geom_bar(stat = "identity")+
  xlab("Event Type")+ ylab("Number of Fatalities")+
  ggtitle("Total Fatalities By Event Type")+
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))
 
ggplot(injuries, aes(evtypegrp,injuries, fill=injuries))+
  geom_bar(stat = "identity")+
  xlab("Event Type")+ ylab("Number of Injuries")+
  ggtitle("Total Injuries By Event Type")+
  theme(axis.text.x = element_text(angle=90)) +
  expand_limits(y=c(0,6000))
  
  
  damage <-head(eventsum[order(eventsum$damage, decreasing=TRUE),],10)
property <- damage %>% mutate(damage_type="Property", damage_amount=property)
crops <- damage %>% mutate(damage_type="Crops", damage_amount=crops)
damage_10 <- rbind(property,crops)

ggplot(damage_10, aes(evtypegrp, damage_amount, fill=factor(damage_type))) +
  geom_bar(stat = "identity") + 
  ylab("Economical damage 1950 - 2011") +
  xlab("Event") +
  scale_fill_discrete(name = "Damage") +
  ggtitle ("Total Economical Damage by Event") +
  theme(axis.text=element_text(size=6))
  
