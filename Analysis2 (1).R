library(RCurl)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyverse)

data <- "airlinedata.json" 
flight_data<-jsonlite::fromJSON(data) #Read jsondata and create a dataset

View(flight_data) #View the DataSet

df<- data.frame(flight_data$Flight.date,flight_data$Day.of.Month,flight_data$Destination.City,flight_data$Origin.City,flight_data$Destination.State,flight_data$Origin.State,+
                  flight_data$dlong,flight_data$dlat,flight_data$olong,flight_data$olat,+
                  flight_data$Flight.Distance,flight_data$Flight.time.in.minutes,flight_data$Scheduled.Departure.Hour,flight_data$Departure.Delay.in.Minutes,flight_data$Arrival.Delay.in.Minutes,flight_data$Flight.cancelled,flight_data$Partner.Code,flight_data$Partner.Name, +
                  flight_data$Age,flight_data$Gender,flight_data$Year.of.First.Flight,flight_data$Flights.Per.Year,flight_data$Total.Freq.Flyer.Accts,flight_data$Loyalty,flight_data$Type.of.Travel,flight_data$Airline.Status,flight_data$Class,flight_data$Shopping.Amount.at.Airport,flight_data$Eating.and.Drinking.at.Airport,flight_data$Price.Sensitivity,flight_data$Likelihood.to.recommend,flight_data$freeText)
View(df)
getwd()

#Renaming columns to make it more user-friendly
colnames(df)[1] <-"Date"
colnames(df)[2] <-"Day_of_Month"
colnames(df)[3] <-"Destination_city"
colnames(df)[4] <-"Origin_city" 
colnames(df)[5] <-"Destination_state"
colnames(df)[6] <-"Origin_state"

colnames(df)[7] <-"dlong" 
colnames(df)[8] <-"dlat"
colnames(df)[9] <-"olong"
colnames(df)[10] <-"olat"
colnames(df)[11] <-"F_Distance"
colnames(df)[12] <-"F_time(mins)"
colnames(df)[13] <-"Scheduled_Departure(Hours)"
colnames(df)[14] <-"Departure_delay(mins)"
colnames(df)[15] <-"Arrival_delay(mins)"
colnames(df)[16] <-"F_cancelled"
colnames(df)[17] <-"Partner_Code" 
colnames(df)[18] <-"Partner_Name" 
colnames(df)[19] <-"Age" 
colnames(df)[20] <-"Gender"
colnames(df)[21] <-"First_flight_year" 
colnames(df)[22] <-"F_per_year" 
colnames(df)[23] <-"Freq_flyer_accts(total)" 
colnames(df)[24] <-"loyalty" 
colnames(df)[25] <-"Type_of_travel" 
colnames(df)[26] <-"Airline_status" 
colnames(df)[27] <-"Class_of_travel" 
colnames(df)[28] <-"Shopping_at_airport"
colnames(df)[29] <-"Eat_drink_at_airport" 
colnames(df)[30] <-"Price_sensitivity"
colnames(df)[31] <-"Likelihood_to_recommend"
colnames(df)[32] <-"Comments"

View(df)

# Calculating the Net Promoter Score

New_flight<- df
n <- nrow(New_flight)
n
for(i in 1:n)
  if (New_flight$Likelihood_to_recommend[i]>=0 && New_flight$Likelihood_to_recommend[i]<7) {
    New_flight$NPS[i]="Detractor"
  } else if (New_flight$Likelihood_to_recommend[i]>=7 && New_flight$Likelihood_to_recommend[i]<=8) {
    New_flight$NPS[i]="Passives"
  } else {
    New_flight$NPS[i]="Promoter"
  }
df$NPS <- New_flight$NPS

Prom<-sum(New_flight$NPS=='Promoter')
Detr<-sum(New_flight$NPS=='Detractor')
NPS=(Prom-Detr)/n*100
NPS

#FINDING AVERAGE NUMBER OF FLIGHTS FOR EACH AGE CATEGORY.. RD

for(i in 1:n)
  if (New_flight$Age[i]>=15 && New_flight$Age[i]<=24) {
    New_flight$AgeCategory[i]="Youth"
    New_flight$AgeDummy[i]="1"
  } else if (New_flight$Age[i]>25 && New_flight$Age[i]<=64) {
    New_flight$AgeCategory[i]="Adults"
    New_flight$AgeDummy[i]="2"
  } else {
    New_flight$AgeCategory[i]="Seniors"
    New_flight$AgeDummy[i]="3"
  }


newdf<-New_flight%>%
  group_by(AgeCategory)%>%
  summarise(mean=mean(F_per_year))
View(newdf)

# NPS of each Flight

air<-unique(New_flight$Partner_Name)
View(air)
y<-length(air)
y
air$NPS<-""
for(i in 1:y)
{
  s<-NPSfun(air[i])
  print(s)
}

View(s)
NPSfun<-function(x)
{
  df<-filter(New_flight,Partner_Name==air[i])
  Prom<-sum(df$NPS=='Promoter')
  Detr<-sum(df$NPS=='Detractor')
  n<-nrow(df)
  NPS1=(Prom-Detr)/n*100
  return(NPS1)
}
airNPS<-c(11.9859,0.3121748,-57.93269,2.018843,19.27711,16.09848,
          10.79545,12.5,-19.26121,17.06865,14.34263,28.84615,
          -11.11111,46.15385)
View(df)
airlines<-data.frame(air,airNPS)
View(airlines)

g <- ggplot(airlines, aes(x = airlines$air ,y= airlines$airNPS)) #Define Data and Y axis
g <- g + geom_col(stat="identity",fill="White",color="steel blue") #Aesthetics of visualization
g <- g + ggtitle("Airline VS NPS plot")#Sets a title to the graph
g <- g +labs(x="Airline",y= "Net Promoter Score") +coord_flip() 
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g #Display the graph


#Percentage NPS per flight.

air<-unique(New_flight$Partner_Name)
View(air)
y<-length(air)
y
air$NPS<-""
colnames(air)
m <- matrix(ncol=3,nrow=y)
for(i in 1:y)
{
  s<-NPSfun(air[i])
  m[i,1]<- s$promPercent
  m[i,2] <- s$PassivePercent
  m[i,3] <- s$PassivePercent
  
}
View(m)

View(s)
a
NPSfun<-function(x)
{
  df<-filter(New_flight,Partner_Name==air[i])
  Prom<-sum(df$NPS=='Promoter')
  Detr<-sum(df$NPS=='Detractor')
  n<-nrow(df)
  PromPercent<-Prom/n*100
  DetrPercent<-Detr/n*100
  Passive<-100-PromPercent-DetrPercent
  rec<-list("promPercent" = PromPercent,"PassivePercent"=Passive,"detrPercent"=DetrPercent)
  return(rec)
}

