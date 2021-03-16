#Analysis1
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

#Finding NA's column-wise.

NaDf <- data.frame(colSums(is.na(df)))
#So three columns has missing values.

#NA Interpolation
library(imputeTS)

dfOrdered <- df[order(df$Origin_city,decreasing = FALSE),]
View(dfOrdered)

dfOrdered <- na_interpolation(dfOrdered)
dfOrdered$Index <- row.names(dfOrdered)
dfOrdered$Index<-as.numeric(gsub(" ","",dfOrdered$Index)) #Remove space and convert string to number
dfOrdered <- dfOrdered[order(dfOrdered$Index,decreasing = FALSE),]
View(dfOrdered)
df <- dfOrdered
df <- df[,-33:-33]

View(df) #Interpolated NA values as per the origin cities 
#COUNTS

# Origin City
orgiS <- data.frame(table(df$Origin_state))
summary(orgiS$Freq)
sum(orgiS$Freq)
View(orgiS)

age <- data.frame(table(df$Age))
winsor.mean(age,k=80,na.rm=TRUE)
View(age)
dev.off()

g <- ggplot(age, aes(x = age$Var1 ,y= age$Freq)) #Define Data and Y axis
g <- g + geom_col() #Aesthetics of visualization
g <- g + ggtitle("Age") #Sets a title to the graph
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g #Display the graph


df2 <- df
df2 <- df2[df2$Age<80,]
View(df2)
age <- data.frame(table(df2$Age))
View(age)
dev.off()

g <- ggplot(age, aes(x = age$Var1 ,y= age$Freq)) #Define Data and Y axis
g <- g + geom_col() #Aesthetics of visualization
g <- g + ggtitle("Age") #Sets a title to the graph
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g #Display the graph

Destinations <-unique(df$Destination_city)
length(Destinations)
#198 Destintions

Origin <- unique(df$Origin_city)
length(Origin)
#198 Origin 

Partners <- unique(df$Partner_Name)
length(Partners)
#14 Partners

TravelTypes <- unique(df$Type_of_travel)
#Mileage, Business, Personal
length(TravelTypes)
#3 Types

#Travel type and airline status
Ttypes_count <- df %>%
  group_by(df$Type_of_travel,df$Airline_status) %>%
  summarise(count=n())
View(Ttypes_count)

h <- ggplot(Ttypes_count, aes(x=reorder(Ttypes_count$`df$Airline_status`,-Ttypes_count$count),fill = (Ttypes_count$`df$Type_of_travel`) ,y= Ttypes_count$count)) #Define Data and Y axis
h <- h + geom_col() #Aesthetics of visualization
h <- h + ggtitle("Airline Status VS Count of Customers") #Sets a title to the graph
h <- h + labs(y="Count of Customers",x="Airline Status",fill="Travel Type")
h <- h + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
h #Display the graph

h <- ggplot(Ttypes_count, aes(x=reorder(Ttypes_count$`df$Airline_status`,-Ttypes_count$count),fill = (Ttypes_count$`df$Type_of_travel`) ,y= Ttypes_count$count)) #Define Data and Y axis
h <- h + geom_bar(position = "dodge",stat="identity") #Aesthetics of visualization
h <- h + ggtitle("Airline Status VS Count of Customers") #Sets a title to the graph
h <- h + labs(y="Count of Customers",x="Airline Status",fill="Travel Type")
h <- h + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Rotate the lables on x axis by 90 degrees
h #Display the graph



AirlineStatus <- unique(df$Airline_status)
#Blue, Silver, Gold, Platinum
length(AirlineStatus)
a <- df[which(df$Airline_status== "Blue" ),]
b <- df[which(df$Airline_status== "Silver" ),]
c <- df[which(df$Airline_status== "Gold" ),]
d <- df[which(df$Airline_status== "Platinum" ),]



bluePlot <- ggplot(a, aes(x=a$Type_of_travel))
bluePlot

Date_count <- df %>%
  group_by(df$Date) %>%
  summarise(count=n())
View(Ttypes_count)
g <- ggplot(Date_count, aes(x = Date_count$`df$Date`, y= Date_count$count)) #Define Data and Y axis
g <- g + geom_line(color="Yellow",) #Aesthetics of visualization
g <- g + ggtitle("Distribution of Detractors(Per day basis)") #Sets a title to the graph
g <- g + labs(x="Date",y="Customer Count") 
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g
#Dates 1/1/14 - 3/9/14
Date_count <- df %>%
  group_by(df$Date) %>%
  filter(NPS == "Passives")%>%
  summarise(count=n())
View(Date_count)
summary(Date_count)
install.packages("lubridate")

library(ggplot2)
plot(table(df$Date))
nf <- data.frame(table(df$Date))
View(nf)
summarize(nf)
#Min.   : 69.0  
#1st Qu.:105.2  
#Median :113.5  
#Mean   :114.2  
#3rd Qu.:125.0  
#Max.   :153.0  

#Travel Type

table(df$Type_of_travel)
#Business Travel: 6323
#Mileage Tickets: 829
#Personal Travel: 3130

#Regional Airplane Companies
View(df)
regional <- data.frame(table(df$Partner_Name))

rp <- ggplot(regional, aes(x=reorder(regional$Var1,Freq),y=regional$Freq))
rp <- rp + geom_col(color="yellow")
rp <- rp + ggtitle("Regional Airplanes Plot")#Sets a title to the graph
rp <- rp + labs(x="Airlines",y="Customers Flown")
rp <- rp + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
rp #Display the graph
Detractor_count <- df %>%
  filter(df$Partner_Name=="Sigma Airlines Inc.")#%>%
View(Detractor_count)

NBADet <- Detractor_count %>%
    filter(NPS=="Detractor")
CountNBADet <- NBADet %>%
    group_by(NPS) %>%
  summarise(count=n())
#  summarise(count=n())
View(Detractor_count)
summary(regional$Freq)
regional2 <- regional[regional$Freq>271.2,]

rp2 <- ggplot(regional2, aes(x=reorder(regional2$Var1,Freq),y=regional2$Freq))
rp2 <- rp2 + geom_col(color="yellow",fill="purple")
rp2 <- rp2 + ggtitle("Regional Airplanes Plot above 25th %ile") #Sets a title to the graph
rp2 <- rp2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
rp2 #Display the graph



#Cheapseats Airlines Inc.         Cool&Young Airlines Inc.         EnjoyFlying Air Services 
#2229                              104                              528 
#FlyFast Airways Inc.                  FlyHere Airways           FlyToSun Airlines Inc. 
#1137                              251                              332 
#GoingNorth Airlines Inc. Northwest Business Airlines Inc.           OnlyJets Airlines Inc. 
#153                             1248                              352 
#Oursin Airlines Inc.         Paul Smith Airlines Inc.              Sigma Airlines Inc. 
#961                              539                             1584 
#Southeast Airlines Co.                West Airways Inc. 
#851                               13 

#Eliminate West Airways

#Gender

table(df$Gender)
#Female   Male 
#5804   4478

# Destination City
dest <- data.frame(table(df$Destination_city))
View(dest)
summary(dest$Freq)
sum(dest$Freq)

# Origin City
orgi <- data.frame(table(df$Origin_city))
summary(orgi$Freq)
sum(orgi$Freq)
View(orgi)

# Destination State
destS <- data.frame(table(df$Destination_state))
View(destS)
summary(destS$Freq)
sum(destS$Freq)

# Origin City
orgiS <- data.frame(table(df$Origin_state))
summary(orgiS$Freq)
sum(orgiS$Freq)
View(orgiS)

age <- data.frame(table(df$Age))
winsor.mean(age,k=80,na.rm=TRUE)
View(age)
dev.off()

g <- ggplot(age, aes(x = age$Var1 ,y= age$Freq)) #Define Data and Y axis
g <- g + geom_col() #Aesthetics of visualization
g <- g + ggtitle("Age") #Sets a title to the graph
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g #Display the graph


df2 <- df
df2 <- df2[df2$Age<80,]
View(df2)
age <- data.frame(table(df2$Age))
View(age)
dev.off()

g <- ggplot(age, aes(x = age$Var1 ,y= age$Freq)) #Define Data and Y axis
g <- g + geom_col() #Aesthetics of visualization
g <- g + ggtitle("Age") #Sets a title to the graph
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #Rotate the lables on x axis by 90 degrees
g #Display the graph
