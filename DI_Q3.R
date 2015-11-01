# SafeSFMap Proposal  - Exploratory Analysis and Visualization
# by A. Olow

# load libraries
library(RColorBrewer)
library(chron)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(gridExtra)

# # load dataset
# json_file <- "https://data.sfgov.org/resource/gxxq-x39z.json"
# data <- fromJSON(json_file)

# Load crime data
crime <- read.csv("/Users/aolow/Box\ Sync/Documents/R/DataIncubator/Crime/crime.csv")

# Clean data 

# subset for crimes of violence alone - directly contributing to street safety for a passer-by
pedestrian_unsafe <- c("ASSAULT", "DISORDERLY CONDUCT", "DRIVING UNDER THE INFLUENCE", 
                       "DRUG/NARCOTIC", "DRUNKENNESS", "LARCENY/THEFT", "LOITERING", "ROBBERY", "SEX OFFENSES, FORCIBLE",
                       "STOLEN PROPERTY")
crime2 <- crime[crime$Category %in% pedestrian_unsafe,]

# Reduce category number by higher level classification
crime2$Category[crime2$Category =="STOLEN PROPERTY"] <- "LARCENY/THEFT"
crime2$Category[crime2$Category %in% c("DRUNKENNESS","DRUG/NARCOTIC", "LOITERING") ] <- "DISORDERLY CONDUCT" # should double check loitering in that category

# Translate timestamp into time of times of day
crime2$Times <- times(paste0(crime2$Time, ":00"))
breaks <- c(0, 5, 12, 18, 21, 24) / 24 
labels <- c("night", "morning", "afternoon", "evening", "night")
crime2$TimeOfDay <- cut(crime2$Times, breaks, labels, include.lowest = TRUE)
crime2 <- droplevels(crime2)

# Months/Years Extraction 
crime2$Date2 <- as.Date(crime2$Date, '%m/%d/%Y')
crime2$Year <- format(crime2$Date2, '%Y')
crime2$Month <- format(crime2$Date2, '%m')

# 2015 incomplete >> remove
crime2 <- crime2[!crime2$Year==2015,]

###########
# Visualize

# Get map of SF from Google Maps
SF <- get_map(location = c(lon = -122.4437, lat = 37.76), zoom = 12, maptype = 'roadmap')
SF_MAP = ggmap(SF) + 
  scale_x_continuous(limits = c(-122.52, -122.35), expand = c(0, 0)) +
  scale_y_continuous(limits = c(37.7, 37.82), expand = c(0, 0))
#SF_MAP

### FIGURE 1

# TOP PANEL - Increase in crime over time per districts

annual <- aggregate(crime2$IncidntNum~crime2$Category*crime2$Year, data=crime2, function(x) length(x))
names(annual) <- c("Category", "Year", "IncidntNum")

p_annual <- ggplot(annual, aes(x = Year, y = IncidntNum, fill=Category)) + 
  geom_bar(stat = "identity", position = 'dodge') + theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  theme(legend.position='top',
        plot.title = element_text(face='bold', size=18))+
  geom_text(aes(y = (IncidntNum + 15), label = IncidntNum, angle=45), col='grey30', size=3.5, position = position_dodge(width=1))+
  labs(x="", y="Average Number of Incidents", title="Incidence of San Francisco Crimes per Year")


# simple prediction total pedestrian-endangering crimes per neighborhood in 2015
annual2 <- aggregate(crime2$IncidntNum~crime2$PdDistrict*crime2$Year, data=crime2, function(x) length(x))
names(annual2) <- c("PdDistrict", "Year", "IncidntNum")

areas <- unique(annual2$PdDistrict)
fits <- list()

for(i in 1:length(areas)){
  fits[[i]] <- predict(lm(IncidntNum ~ as.numeric(Year), data=annual2[annual2$PdDistrict==areas[i],]), 
                       newdata= data.frame(Year = 2015), interval="prediction")
}

preds <- as.data.frame(t(matrix(unlist(fits), nrow=3, ncol=10)))
names(preds) <- c("fit", "lwr", "upr")
preds$PdDistrict <- areas

p2015 <- ggplot(preds, aes(x = PdDistrict, y = fit)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  geom_errorbar(aes(ymin=lwr,ymax=upr),
                position="dodge", width=0.1, size=0.3) +
  scale_fill_brewer(palette="Set1") +
  coord_flip()+
  theme(legend.position='top',
        plot.title = element_text(face='bold', size=18))+
  labs(x="", y="Predicted Number of Incidents 2015", title="Projected Pedestrian-Endangering Incidents 2015")


# MAP

map2014 <- SF_MAP + geom_point(data = crime2[crime2$Year=="2014",], aes(x = X, y = Y, color = Category), 
                               size=2, alpha = 0.8) + scale_colour_brewer(palette="Set1", guide=F) + theme_bw()+
  labs(x="", y="", title="San Francisco Pedestrian Endangering Crimes 2014")+ 
  theme(plot.title = element_text(face='bold', size=18), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

Fig1 <- grid.arrange(p_annual, arrangeGrob(map2014, p2015, ncol = 2), nrow=2, heights=c(1,1))



#### FIGURE 2

# TOP PANEL 
daytime <- aggregate(crime2$IncidntNum~crime2$Category*crime2$DayOfWeek*crime2$TimeOfDay, data=crime2, function(x) length(x))
names(daytime) <- c("Category", "DayOfWeek", "TimeOfDay", "IncidntNum")


p_week <- ggplot(daytime, aes(x = DayOfWeek, y = IncidntNum, fill=Category)) + 
  geom_bar(stat = "identity")+ theme_bw() +
  facet_wrap(~TimeOfDay, ncol=4) +
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position='top',
        plot.title = element_text(face='bold', size=18),
        strip.text.x = element_text(size = 12, face='bold')) +
  labs(x="", y="Average Number of Incidents / Year", 
       title="Daytime Distribution of San Francisco Crimes [Avg / Year] \n")

# BOTTOM PANEL

####
# total crimes per type per district
by_district <- aggregate(crime2$IncidntNum~crime2$Category*crime2$PdDistrict*crime2$TimeOfDay, data=crime2, function(x) length(x))
names(by_district) <- c("Category", "PdDistrict", "TimeOfDay", "IncidntNum")

by_district$TimeOfDay<- factor(by_district$TimeOfDay, levels = c("morning", "afternoon", "evening", "night"))

p_by_district <- ggplot(by_district, aes(x = PdDistrict, y = IncidntNum, fill=Category)) + 
  facet_grid(~TimeOfDay) +
  geom_bar(stat = "identity")+ theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text.x = element_text(size = 12, face='bold')) +
  scale_fill_brewer(palette="Set1", guide=F) + 
  coord_flip() +
  labs(x="", y="Average Number of Incidents / Year")


## Add a panel of daytime maps

morning <- SF_MAP + geom_point(data = crime2[crime2$TimeOfDay=="morning",], aes(x = X, y = Y, color = Category), 
                               size=1.5, alpha = 0.5) + scale_colour_brewer(palette="Set1", guide=FALSE) + theme_bw()+
  labs(x="", y="", title="Morning [Avg / Year] \n")+ theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

afternoon <- SF_MAP + geom_point(data = crime2[crime2$TimeOfDay=="afternoon",], aes(x = X, y = Y, color = Category), 
                                 size=1.5, alpha = 0.5) + scale_colour_brewer(palette="Set1", guide=FALSE) + theme_bw()+
  labs(x="", y="", title="Afternoon [Avg / Year] \n") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

evening <- SF_MAP + geom_point(data = crime2[crime2$TimeOfDay=="evening",], aes(x = X, y = Y, color = Category), 
                               size=1.5, alpha = 0.5) + scale_colour_brewer(palette="Set1", guide=FALSE) + theme_bw()+
  labs(x="", y="", title="Evening [Avg / Year] \n") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

night <- SF_MAP + geom_point(data = crime2[crime2$TimeOfDay=="night",], aes(x = X, y = Y, color = Category), 
                             size=1.5, alpha = 0.5) + scale_colour_brewer(palette="Set1", guide=FALSE) + theme_bw()+
  labs(x="", y="", title="Night [Avg / Year] \n") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())


### ARRANGE PANELS FIG 2
Fig2 <- grid.arrange(p_week, p_by_district, arrangeGrob(morning, afternoon, evening, night, ncol = 4), nrow=3, heights=c(1.2,1,1.3))


