
#*******************************************************************
#****I was trying to use predictive analysis for Na values that day so below was the code used.Did not complete it. You can try few packages*****.
#install.packages("missForest")
#install.packages("Hmisc")
#library(Hmisc)
#library(missForest)
#install.packages("mi")
#library(mi)
#data("New_Flight")
#New_flight.mis <- prodNA(New_flight, noNA = 0.1)
#summary(New_flight.mis)
#New_flight.mis$imputed_1 <- with(New_flight.mis, impute("Departure_delay(mins)", mean))
#New_flight.mis$imputed_2 <- with(New_flight.mis, impute("Departure_delay(mins)", 'random'))
#impute_arg <- aregImpute(~"Departure_delay(mins)" + "F_time(mins)" +"Scheduled_Departure(Hours)" +"Likelihood_to_recommend" +
                           #"loyalty", data = New_flight.mis, n.impute = 4)
#mi_data<-mi(New_flight.mis,seed=335)
#summary(mi_data)
#******************************************************************
#____________________________________________________________________
#Did that day
v <- lm(formula=flight_data$Loyalty ~ flight_data$Flights.Per.Year,data=flight_data)
summary(v)
cor(flight_data$Loyalty,flight_data$Flights.Per.Year)
str(flight_data$Type.of.Travel)
#______________________________________________________________________
#             SENTIMENTAL ANALYSIS
#_____________________________________________________________________


#install.packages("tidytext")
library(tidytext)
library(stringr)
library(tidytext)
library(dplyr)
library(glue)
library(readr)
 

getSenti<- function(comm)
{ 
  # stick together the path to the file & 1st file name
  fileName <- comm
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  View(fileName)
  fileText<-fileName
  # remove any dollar signs (they're special characters in R)
  
  View(fileText)
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  View(tokens)
  #install.packages("tidyr")
  #library(tidyr)
  # get the sentiment from the first text: 
  library(magrittr)
  tokens %>%
    inner_join(get_sentiments("bing"))%>%
    count(sentiment)%>%
    spread(sentiment, n, fill = 0)%>%
    mutate(sentiment=positive-negative)%>%
    count(sentiment)
    View(tokens)
  return(tokens)
}  

New_flight$Senticol<-0

for(row in 1:5)
                    { 
                      comm<-New_flight[row,"Comments"]
                      New_flight[row,"Senticol"]<-getSenti(comm)
                      paste(sentiment,collapse = "n")
}
View(New_flight)
#_______________________________________________________________________
############################## FOR TESTING THE CODE #########################################
fileName <- New_flight$Comments
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)
View(fileName)
fileText<-fileName
# remove any dollar signs (they're special characters in R)

View(fileText)
# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
View(tokens)
#install.packages("tidyr")
library(tidyr)
# get the sentiment from the first text: 
tokens %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment)%>%
  spread(sentiment, n, fill = 0)%>%
  mutate(sentiment = positive - negative)%>%
 count(sentiment)
  
  
  
  # pull out only sentiment words
  #count(sentiment) #%>% # count the # of positive & negative words
  #spread(sentiment, n, fill = 0) #%>% # made data wide rather than narrow
  #mutate(sentiment = positive - negative) 
  
  # # of positive words - # of negative owrds
tokens[[3]]
tibble.print=tokens[3,]
options(tibble.width=tokens[3,])
senti<-data_frame(tokens,)


ggplot(senti, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") 
#######################################################################









#Linear Regression

#______________________________________________________________________
#             AGE
#_____________________________________________________________________

# Check whether all data in age is correct
New_flight$First_flight_age <- New_flight$Age - (2012 - New_flight$First_flight_year)
# Create a new column that shows the first flight age of all passengers

auz <- New_flight %>%
  filter(First_flight_age <= 0)
auzn <- nrow(auz)
auzn
# Check if there's any customer's age is under 0
# auzn = 0, thus there's no logical mistake with age data

ffa <- New_flight %>%
  filter(First_flight_age <= 0)
ffan <- nrow(ffa)
ffan
# Check if there's any First_flight_age is under 0, which means a wrong data
# ffan = 0, so there's no logical mistakes in data of age

# Group customers into different age range
for(i in 1:n)
  if (New_flight$Age[i] <= 35) {
    New_flight$Age_Group[i]="Low"
  } else if (New_flight$Age[i] > 35  && New_flight$Age[i] <= 65) {
    New_flight$Age_Group[i]="Medium"
  } else {
    New_flight$Age_Group[i]="High"
  }

table(New_flight$Age_Group)
# There are 1568 high Age_Group customers, 5665 medium Age_Group customers, and 3049 low Age_Group customers

LAC <- New_flight %>%
  filter(Age_Group == "Low")
MAC <- New_flight %>%
  filter(Age_Group == "Medium")
HAC <- New_flight %>%
  filter(Age_Group == "High")
# Create and seperate different data frame based on Age_Group

round(prop.table(table(New_flight$Age_Group, New_flight$Likelihood_to_recommend)),3)

# Examine different age group with numeric columns using stepback linear model method

# Low age customers
LAC_LR1 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + `F_time(mins)` + F_Distance + `Scheduled_Departure(Hours)` + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Shopping_at_airport + Eat_drink_at_airport, data = LAC)
summary(LAC_LR1)
# Filter out columns not important
LAC_LR2 <- lm(Likelihood_to_recommend ~ Age + Price_sensitivity + loyalty + F_Distance + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Eat_drink_at_airport, data = LAC)
summary(LAC_LR2)
LAC_Number_of_Important_Fac <- length(LAC_LR2$coefficients) - 1
LAC_Number_of_Important_Fac
# There're 8 important factors to predict Likelihood to recommend for low age customers

# Find out whether these important factors affect positively or negatively to likelihood to recommend
LAC_Affect <- sign(coefficients(LAC_LR2))
# Delete the first row intercept coefficient
LAC_Affect <- LAC_Affect[-1]
Factor <- names(LAC_Affect)
# Create a new data frame using two above list
LAC_IF <- data.frame(Factor, LAC_Affect)

# Create a plot so we can easily inspect the sign of affect each factor poses
ggplot(LAC_IF) +
  aes(x = Factor, y = LAC_Affect) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Affect of Important Factor for LAC")

# For low age customers, by interpreting important factors,
# Age UP -> likelihood to recommend UP
# Price_sensitivity UP -> likelihood to recommend DOWN
# loyalty UP -> likelihood to recommend DOWN
# F_Distance UP -> likelihood to recommend UP
# arrival delay UP -> likelihood to recommend DOWN
# F_per_year UP -> likelihood to recommend DOWN
# Frequent flyer account UP -> likelihood to recommend DOWN
# Eat_drink_at_airport UP -> likelihood to recommend UP

# Medium age customers
MAC_LR1 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + `F_time(mins)` + F_Distance + `Scheduled_Departure(Hours)` + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Shopping_at_airport + Eat_drink_at_airport, data = MAC)
summary(MAC_LR1)
# Filter out columns not important
MAC_LR2 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + F_Distance + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Eat_drink_at_airport, data = MAC)
summary(MAC_LR2)
MAC_Number_of_Important_Fac <- length(MAC_LR2$coefficients) - 1
MAC_Number_of_Important_Fac
# There're 10 important factors to predict Likelihood to recommend for medium age customers

# Find out whether these important factors affect positively or negatively to likelihood to recommend
MAC_Affect <- sign(coefficients(MAC_LR2))
# Delete the first row intercept coefficient
MAC_Affect <- MAC_Affect[-1]
Factor_M <- names(MAC_Affect)
# Create a new data frame using two above list
MAC_IF <- data.frame(Factor_M, MAC_Affect)

# Create a plot so we can easily inspect the sign of affect each factor poses
ggplot(MAC_IF) +
  aes(x = Factor_M, y = MAC_Affect) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Affect of Important Factor for MAC")

# Age UP -> likelihood to recommend DOWN
# First_flight_age UP -> likelihood to recommend UP
# Price_sensitivity UP -> likelihood to recommend DOWN
# loyalty UP -> likelihood to recommend DOWN
# F_Distance UP -> likelihood to recommend UP
# Departure delay UP -> likelihood to recommend UP
# arrival delay UP -> likelihood to recommend DOWN
# F_per_year UP -> likelihood to recommend DOWN
# Frequent flyer account UP -> likelihood to recommend DOWN
# Eat_drink_at_airport UP -> likelihood to recommend UP

# High age customers
HAC_LR1 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + `F_time(mins)` + F_Distance + `Scheduled_Departure(Hours)` + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Shopping_at_airport + Eat_drink_at_airport, data = HAC)
summary(HAC_LR1)
# Filter out columns not important
HAC_LR2 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + `Scheduled_Departure(Hours)` + `Arrival_delay(mins)` + F_per_year, data = HAC)
summary(HAC_LR2)
#Filter out columns not important
HAC_LR3 <- lm(Likelihood_to_recommend ~ Age + `Scheduled_Departure(Hours)` + `Arrival_delay(mins)` + F_per_year, data = HAC)
summary(HAC_LR3)
HAC_Number_of_Important_Fac <- length(HAC_LR3$coefficients) - 1
HAC_Number_of_Important_Fac
# There're 4 important factors to predict Likelihood to recommend for high age customers

# Find out whether these important factors affect positively or negatively to likelihood to recommend
HAC_Affect <- sign(coefficients(HAC_LR3))
# Delete the first row intercept coefficient
HAC_Affect <- HAC_Affect[-1]
Factor_H <- names(HAC_Affect)
# Create a new data frame using two above list
HAC_IF <- data.frame(Factor_H, HAC_Affect)

# Create a plot so we can easily inspect the sign of affect each factor poses
ggplot(HAC_IF) +
  aes(x = Factor_H, y = HAC_Affect) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Affect of Important Factor for HAC")

# For high age customers, by interpreting important factors, 
# Age UP -> likelihood to recommend DOWN
# Scheduled_Departure UP -> likelihood to recommend DOWN
# arrival delay UP -> likelihood to recommend DOWN
# F_per_year UP -> likelihood to recommend DOWN

############################### Combine the affect of important factors of each age group #############################

# Owing that different age groups have different number of customers, multiply the coefficient of linear models and sum the weighted values up using data frame

Factors <- c("Age", "First_flight_age", "Price_sensitivity", "loyalty", "F_time(mins)", "F_Distance", "Scheduled_Departure(Hours)", "Departure_delay(mins)", "Arrival_delay(mins)", "F_per_year", "Freq_flyer_accts(total)", "Shopping_at_airport", "Eat_drink_at_airport")
Weighted_value <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
FWV <- data.frame(Factors, Weighted_value)

# Input weighted value of LAC into FWV
LAC_A <- coefficients(LAC_LR2)
LAC_A <- LAC_A[-1]
LAC_WA <- table(New_flight$Age_Group)[2] * LAC_A
FWV[1,2] <- FWV[1,2] + LAC_WA[1]
FWV[3,2] <- FWV[3,2] + LAC_WA[2]
FWV[4,2] <- FWV[4,2] + LAC_WA[3]
FWV[6,2] <- FWV[6,2] + LAC_WA[4]
FWV[9,2] <- FWV[9,2] + LAC_WA[5]
FWV[10,2] <- FWV[10,2] + LAC_WA[6]
FWV[11,2] <- FWV[11,2] + LAC_WA[7]
FWV[13,2] <- FWV[13,2] + LAC_WA[8]

# Input weighted value of MAC into FWV
MAC_A <- coefficients(MAC_LR2)
MAC_A <- MAC_A[-1]
MAC_WA <- table(New_flight$Age_Group)[3] * MAC_A
FWV[1,2] <- FWV[1,2] + MAC_WA[1]
FWV[2,2] <- FWV[2,2] + MAC_WA[2]
FWV[3,2] <- FWV[3,2] + MAC_WA[3]
FWV[4,2] <- FWV[4,2] + MAC_WA[4]
FWV[6,2] <- FWV[6,2] + MAC_WA[5]
FWV[8,2] <- FWV[8,2] + MAC_WA[6]
FWV[9,2] <- FWV[9,2] + MAC_WA[7]
FWV[10,2] <- FWV[10,2] + MAC_WA[8]
FWV[11,2] <- FWV[11,2] + MAC_WA[9]
FWV[13,2] <- FWV[13,2] + MAC_WA[10]

# Input weighted value of HAC into FWV
HAC_A <- coefficients(HAC_LR3)
HAC_A <- HAC_A[-1]
HAC_WA <- table(New_flight$Age_Group)[1] * HAC_A
FWV[1,2] <- FWV[1,2] + HAC_WA[1]
FWV[7,2] <- FWV[7,2] + HAC_WA[2]
FWV[9,2] <- FWV[9,2] + HAC_WA[3]
FWV[10,2] <- FWV[10,2] + HAC_WA[4]

# Sort the data and output the top 5 weighted value of important factors based on coefficient and number of customers
FWV$Weighted_value <- round(FWV$Weighted_value, 3)
FWV <- FWV[order(abs(FWV$Weighted_value), decreasing = TRUE),]
Strongest_Overall_Factors_Affecting_Age <- head(FWV, 5)
Strongest_Overall_Factors_Affecting_Age

# By inspecting the top 5 important factors, we can tell that these strongest factors are all negative values.
# In order to increase the likelihood to recommend value, airline can keep these factors low.






#______________________________________________________________________
#             GENDER
#_____________________________________________________________________


# First, get to know about the distribution of customers based on gender
table(New_flight$Gender)
prop.table(table(New_flight$NPS, New_flight$Gender))

MC <- New_flight %>%
  filter(Gender == "Male")
FC <- New_flight %>%
  filter(Gender == "Female")

# Examine different gender with numeric columns using stepback linear model method

# Male Customers
MC_LR1 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + `F_time(mins)` + F_Distance + `Scheduled_Departure(Hours)` + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Shopping_at_airport + Eat_drink_at_airport, data = MC)
summary(MC_LR1)
MC_LR2 <- lm(Likelihood_to_recommend ~ Age + Price_sensitivity + F_Distance + `Arrival_delay(mins)` + F_per_year + Shopping_at_airport + Eat_drink_at_airport, data = MAC)
summary(MC_LR2)
MC_LR3 <- lm(Likelihood_to_recommend ~ Age + Price_sensitivity + F_Distance + `Arrival_delay(mins)` + F_per_year + Eat_drink_at_airport, data = MAC)
summary(MC_LR3)
MC_Number_of_Important_Fac <- length(MC_LR3$coefficients) - 1
MC_Number_of_Important_Fac
# There are 6 important factors that enables us to predict likelihood of recommend for male customers

# Find out whether these important factors affect positively or negatively to likelihood to recommend
MC_Affect <- sign(coefficients(MC_LR3))
# Delete the first row intercept coefficient
MC_Affect <- MC_Affect[-1]
Factor_MC <- names(MC_Affect)
# Create a new data frame using two above list
MC_IF <- data.frame(Factor_MC, MC_Affect)

# Create a plot so we can easily inspect the sign of affect each factor poses
ggplot(MC_IF) +
  aes(x = Factor_MC, y = MC_Affect) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Affect of Important Factor for Male Customers")

# For male customers, by interpreting important factors, 
# Arrival delay UP -> likelihood to recommend DOWN
# Age UP -> likelihood to recommend DOWN
# Eat drink at airport UP -> likelihood to recommend UP
# F_per_year UP -> likelihood to recommend DOWN
# F_Distance UP -> likelihood to recommend UP
# Price_sensitivity UP -> likelihood to recommend DOWN

# Female Customers
FC_LR1 <- lm(Likelihood_to_recommend ~ Age + First_flight_age + Price_sensitivity + loyalty + `F_time(mins)` + F_Distance + `Scheduled_Departure(Hours)` + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Shopping_at_airport + Eat_drink_at_airport, data = FC)
summary(FC_LR1)
FC_LR2 <- lm(Likelihood_to_recommend ~ Age + Price_sensitivity + F_Distance + `Departure_delay(mins)` + `Arrival_delay(mins)` + F_per_year + `Freq_flyer_accts(total)` + Eat_drink_at_airport, data = FC)
summary(FC_LR2)
FC_Number_of_Important_Fac <- length(FC_LR2$coefficients) - 1
FC_Number_of_Important_Fac
# There are 8 important factors that enables us to predict likelihood of recommend for female customers

# Find out whether these important factors affect positively or negatively to likelihood to recommend
FC_Affect <- sign(coefficients(FC_LR2))
# Delete the first row intercept coefficient
FC_Affect <- FC_Affect[-1]
Factor_FC <- names(FC_Affect)
# Create a new data frame using two above list
FC_IF <- data.frame(Factor_FC, FC_Affect)

# Create a plot so we can easily inspect the sign of affect each factor poses
ggplot(FC_IF) +
  aes(x = Factor_FC, y = FC_Affect) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Affect of Important Factor for Female Customers")

# For female customers, by interpreting important factors, 
# Arrival delay UP -> likelihood to recommend DOWN
# Age UP -> likelihood to recommend DOWN
# Eat drink at airport UP -> likelihood to recommend UP
# F_per_year UP -> likelihood to recommend DOWN
# F_Distance UP -> likelihood to recommend UP
# Price_sensitivity UP -> likelihood to recommend DOWN
# Departure delay UP -> likelihood to recommend UP
# Freq flyer accounts UP -> likelihood to recommend DOWN

############################### Combine the affect of important factors of each gender #############################

# Owing that different gender have different number of customers, multiply the coefficient of linear models and sum the weighted values up using data frame

Factors <- c("Age", "First_flight_age", "Price_sensitivity", "loyalty", "F_time(mins)", "F_Distance", "Scheduled_Departure(Hours)", "Departure_delay(mins)", "Arrival_delay(mins)", "F_per_year", "Freq_flyer_accts(total)", "Shopping_at_airport", "Eat_drink_at_airport")
Weighted_value_gender <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
GF <- data.frame(Factors, Weighted_value_gender)

# Input weighted value of male customers into GF
MCA <- coefficients(MC_LR3)
MCA <- MCA[-1]
MCAA <- table(New_flight$Gender)[2] * MCA
GF[1,2] <- GF[1,2] + MCAA[1]
GF[3,2] <- GF[3,2] + MCAA[2]
GF[6,2] <- GF[6,2] + MCAA[3]
GF[9,2] <- GF[9,2] + MCAA[4]
GF[10,2] <- GF[10,2] + MCAA[5]
GF[13,2] <- GF[13,2] + MCAA[6]

# Input weighted value of male customers into GF
FCA <- coefficients(FC_LR2)
FCA <- FCA[-1]
FCAA <- table(New_flight$Gender)[1] * FCA
GF[1,2] <- GF[1,2] + FCAA[1]
GF[3,2] <- GF[3,2] + FCAA[2]
GF[6,2] <- GF[6,2] + FCAA[3]
GF[8,2] <- GF[8,2] + FCAA[4]
GF[9,2] <- GF[9,2] + FCAA[5]
GF[10,2] <- GF[10,2] + FCAA[6]
GF[11,2] <- GF[11,2] + FCAA[7]
GF[13,2] <- GF[13,2] + FCAA[8]

# Sort the data and output the top 5 weighted value of important factors based on coefficient and number of customers
GF$Weighted_value_gender <- round(GF$Weighted_value_gender, 3)
GF <- GF[order(abs(GF$Weighted_value_gender), decreasing = TRUE),]
Strongest_Overall_Factors_Affecting_Gender <- head(GF, 5)
Strongest_Overall_Factors_Affecting_Gender

# By inspecting the top 5 important factors, we can tell that these strongest factors are all negative values.
# In order to increase the likelihood to recommend value, airline can keep these factors low.









#______________________________________________________________________
#             Worst Airline
#_____________________________________________________________________

############################## Define worst airline as highest detractor ratio

AL_D <- New_flight %>%
  filter(NPS == "Detractor") %>%
  group_by(Partner_Name) %>%
  summarise(count = n())
AL_D

AL_PS <- New_flight %>%
  filter(NPS == "Passives") %>%
  group_by(Partner_Name) %>%
  summarise(count = n())
AL_PS

AL_PM <- New_flight %>%
  filter(NPS == "Promoter") %>%
  group_by(Partner_Name) %>%
  summarise(count = n())
AL_PM

AL_NPS <- data.frame(AL_D, AL_PS, AL_PM)
View(AL_NPS)
rownames(AL_NPS) <- AL_NPS[,1]
AL_NPS <- AL_NPS[,-1]
AL_NPS <- AL_NPS[,-2]
AL_NPS <- AL_NPS[,-3]
colnames(AL_NPS)[1] <- "Detractor"
colnames(AL_NPS)[2] <- "Passives"
colnames(AL_NPS)[3] <- "Promoters"
AL_NPS$Tt_Customer <- AL_NPS$Detractor + AL_NPS$Passives + AL_NPS$Promoters

AL_NPS$D_Ratio <- round(AL_NPS$Detractor/AL_NPS$Tt_Customer, 3)
DR <- AL_NPS[order(AL_NPS$D_Ratio, decreasing = TRUE),]
WA_NPS <- rownames(DR[1,])

WAL <- New_flight %>%
  filter(Partner_Name == WA_NPS) %>%
  filter(NPS == "Detractor")

# heat map of origin state
WAL_O <- WAL %>%
  group_by(Origin_state) %>%
  summarise(count=n())
WAL_O$Origin_state <- tolower(WAL_O$Origin_state)

us <- map_data("state")#Assign default state data to "us"

stateCenter <- data.frame(state=tolower(state.name),x=state.center$x,y=state.center$y)


statecount <- merge(stateCenter, WAL_O, by.x = c("state"), by.y = c("Origin_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"))

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Origin state of worst airline defined as highest detractor rate")
wap_nps

# heat map of destination state
WAL_D <- WAL %>%
  group_by(Destination_state) %>%
  summarise(count=n())
WAL_D$Destination_state <- tolower(WAL_D$Destination_state)

statecount <- merge(stateCenter, WAL_D, by.x = c("state"), by.y = c("Destination_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"))

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Destination state of worst airline defined as highest detractor rate")
wap_nps











############################## Define worst airline as highest flight cancel rate

Yes <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(F_cancelled == "Yes") %>%
  summarise(count = n())

No <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(F_cancelled == "No") %>%
  summarise(count = n())

Cdf <- merge(Yes, No, by = "Partner_Name")
colnames(Cdf)[2] <- "Cancelled"
colnames(Cdf)[3] <- "Not_cancelled"
Cdf$Tt_Customer <- Cdf$Cancelled + Cdf$Not_cancelled
Cdf$Cancel_rate <- Cdf$Cancelled/Cdf$Tt_Customer
rownames(Cdf) <- Cdf$Partner_Name
Cdf <- Cdf[,-1]

Cdf1 <- Cdf[order(Cdf$Cancel_rate, decreasing = TRUE),]
WA_Cancel <- rownames(Cdf1)[1]

WAC <- New_flight %>%
  filter(Partner_Name == WA_Cancel)

# heat map of origin state
WAC_O <- WAC %>%
  group_by(Origin_state) %>%
  summarise(count = n())
WAC_O$Origin_state <- tolower(WAC_O$Origin_state)

statecount <- merge(stateCenter, WAC_O, by.x = c("state"), by.y = c("Origin_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)
  
wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Origin state of worst airline defined as highest flight cancel rate")
wap_nps

# heat map of destination state
WAC_D <- WAC %>%
  group_by(Destination_state) %>%
  summarise(count = n())
WAC_D$Destination_state <- tolower(WAC_D$Destination_state)

statecount <- merge(stateCenter, WAC_D, by.x = c("state"), by.y = c("Destination_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Destination state of worst airline defined as highest flight cancel rate")
wap_nps










############################## Define worst airline as highest flight departure delay rate

yd <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(`Departure_delay(mins)` > 0) %>%
  summarise(count = n())

nd <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(`Departure_delay(mins)` <= 0) %>%
  summarise(count = n())

Ddf <- merge(yd, nd, by = "Partner_Name")
rownames(Ddf) <- Ddf$Partner_Name
Ddf <- Ddf[,-1]
colnames(Ddf)[1] <- "Delay"
colnames(Ddf)[2] <- "Not_delay"
Ddf$Tt_Customers <- Ddf$Delay + Ddf$Not_delay
Ddf$Delay_ratio <- Ddf$Delay/Ddf$Tt_Customers

Ddf1 <- Ddf[order(Ddf$Delay_ratio, decreasing = TRUE),]
WA_Delay <- rownames(Ddf1)[1]

WAD <- New_flight %>%
  filter(Partner_Name == WA_Delay)

# heat map of origin state

WAD_O <- WAD %>%
  group_by(Origin_state) %>%
  summarise(count = n())
WAD_O$Origin_state <- tolower(WAD_O$Origin_state)

statecount <- merge(stateCenter, WAD_O, by.x = c("state"), by.y = c("Origin_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Origin state of worst airline defined as highest delay rate")
wap_nps

# heat map of destination state

WAD_D <- WAD %>%
  group_by(Destination_state) %>%
  summarise(count = n())
WAD_D$Destination_state <- tolower(WAD_D$Destination_state)

statecount <- merge(stateCenter, WAD_D, by.x = c("state"), by.y = c("Destination_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Destination state of worst airline defined as highest delay rate")
wap_nps










############################## Define worst airline as highest 1~3 in LTR ratio

lltr <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(Likelihood_to_recommend <= 3) %>%
  summarise(count = n())
nlltr <- New_flight %>%
  group_by(Partner_Name) %>%
  filter(Likelihood_to_recommend > 3) %>%
  summarise(count = n())
ltr <- merge(yd, nd, by = "Partner_Name")
rownames(ltr) <- ltr$Partner_Name
ltr <- ltr[,-1]
colnames(ltr)[1] <- "Low_LTR"
colnames(ltr)[2] <- "Not_low_LTR"
ltr$Tt_Customers <- ltr$Low_LTR + ltr$Not_low_LTR
ltr$Low_LTR_ratio <- ltr$Low_LTR/ltr$Tt_Customers

ltr1 <- ltr[order(Ddf$Delay_ratio, decreasing = TRUE),]
WA_LTRL <- rownames(ltr1)[1]

WALL <- New_flight %>%
  filter(Partner_Name == WA_LTRL)

# heat map of origin state

WALL_O <- WALL %>%
  group_by(Origin_state) %>%
  summarise(count = n())
WALL_O$Origin_state <- tolower(WALL_O$Origin_state)

statecount <- merge(stateCenter, WALL_O, by.x = c("state"), by.y = c("Origin_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Origin state of worst airline defined as highest low Likelihood_to_recommend rate")
wap_nps

# heat map of destination state

WALL_D <- WALL %>%
  group_by(Destination_state) %>%
  summarise(count = n())
WALL_D$Destination_state <- tolower(WALL_D$Destination_state)

statecount <- merge(stateCenter, WALL_D, by.x = c("state"), by.y = c("Destination_state"))
t3 <- merge(statecount, us, by.x = c("state"), by.y = c("region"), all = TRUE)

wap_nps <- ggplot(t3,aes(map_id = state))
wap_nps <- wap_nps+ geom_map(map = us,aes(x=long,y=lat, fill = count))
wap_nps <- wap_nps + geom_point(aes(x = x, y = y, size = count))
wap_nps <- wap_nps + coord_map()
wap_nps <- wap_nps + ggtitle("Destination state of worst airline defined as highest low Likelihood_to_recommend rate")
wap_nps



