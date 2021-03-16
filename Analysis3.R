View(df)
#Number of Flights cancled

Number <- which(df$F_cancelled=='Yes')
length(Number)
#98 Flights were cancelled 

#Apriori


library(arules) #Invoke Library   
library(arulesViz) #Invoke Library 
df$ShoppingAirportCategorical <- ifelse(df$Shopping_at_airport>0,'Yes','No') 
#Converting numerical variable to categorical variable
df$EatingAirportCategorical <- ifelse(df$Eat_drink_at_airport>0,'Yes','No') 
#Converting numerical variable to categorical variable
df$LoyaltyCategorical <- ifelse(df$loyalty>0,'Loyal','Not Loyal') 
#Converting numerical variable to categorical variable
df$DelayFlightCategorical <- ifelse(df$`Departure_delay(mins)`>0,'Delayed','No Delay') 
#Converting numerical variable to categorical variable
df$ArrivalDelayCategorical <- ifelse(df$`Arrival_delay(mins)`>0,'Arr Delayed', 'Arr No Delay') 
#Converting numerical variable to categorical variable
df$PriceSensitivityCategorical2 <- ifelse(df$Price_sensitivity<2,'Not Sensitive','Sensitive') 
#Converting numerical variable to categorical variable
n <-nrow(df) 
#Store number of rows in the data set 
for(i in 1:n) #Converting numerical variable to categorical variable
  if (df$Price_sensitivity[i]>=0 && df$Price_sensitivity[i]<=2) {
    df$PriceSensitivityCategorical[i]="Not Sensitive"
  } else if (df$Price_sensitivity[i]>2 && df$Price_sensitivity[i]<=3) {
    df$PriceSensitivityCategorical[i]="Less Sensitive"
  } else {
    df$PriceSensitivityCategorical[i]="Sensitive"
  }
View(df) #Display data frame 
table(df$PriceSensitivityCategorical) 
# Analysing Price Sensetivity 
#Less Sensitive  Not Sensitive      Sensitive 
#128          10145              9 
table(df$PriceSensitivityCategorical2) # Comparing the two price sensitivities

#Not Sensitive     Sensitive 
#7225          3057

dfcat <- df[,c('Partner_Name','Gender','Type_of_travel','Airline_status','Class_of_travel','ShoppingAirportCategorical','EatingAirportCategorical','PriceSensitivityCategorical','PriceSensitivityCategorical2','LoyaltyCategorical','NPS')]
#Creating a new dataframe consisting of categorical variables
View(dfcat) #Display the Data set

dfcatX <- as(dfcat,"transactions") #Creating a transactions matrix for Apriori algorithm
inspect(dfcatX) #Inspect the Transactional Matrix
itemFrequencyPlot(dfcatX) #Plots the item frequency 

#Promoter Analysis 
rulesetPromoter <- apriori(dfcatX, 
                   parameter=list(support=0.015,confidence=0.60),
                   appearance = list(default="lhs", rhs=("NPS=Promoter"))) #Applying apriori algorithm by keeping Promoter on RHS
summary(rulesetPromoter) #Summary of Ruleset
inspectDT(rulesetPromoter) #Interactive ruleset interface

#Detractor Analysis 
rulesetDetractor <- apriori(dfcatX, 
                           parameter=list(support=0.015,confidence=0.60),
                           appearance = list(default="lhs", rhs=("NPS=Detractor"))) #Applying apriori algorithm by keeping Detractor on RHS
summary(rulesetDetractor) #Summary of RuleSet
inspectDT(rulesetDetractor) #Interactive ruleset interface


#North West Business Airlines Apriori

dfBA <- dfcat[dfcat$Partner_Name == 'Northwest Business Airlines Inc.',] 
#Creating a data frame consisting only values of Northwest Business Airline Inc. 
View(dfBA) #Display the dataframe 

dfBAX <- as(dfBA,"transactions") #Creating a transactional matrix 

rulesetNBADetractor <- apriori(dfBAX, 
                   parameter=list(support=0.015,confidence=0.60),
                   appearance = list(default="lhs", rhs=("NPS=Detractor"))) #Applying apriori algorithm by keeping Detractor on RHS
inspectDT(rulesetNBADetractor) #Interactive ruleset interface

# Detractors VS Airlines

dfDecractor <- df[,c('Partner_Name','NPS')] 

table(dfDecractor)

dfDecractor$Partner_Name <- dfDecractor[!(dfDecractor$Partner_Name == "West Airways Inc." | dfDecractor$Partner_Name == "Cool&Young Airlines Inc." | dfDecractor$Partner_Name == "GoingNorth Airlines Inc." |  dfDecractor$Partner_Name == "FlyHere Airways"),]
View(dfDecractor)
table()
g <- plot(table(dfDecractor))
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g
plot()

#Blue and Personal VS Promoter and decrators 
View(dfcat)

blueAS <- dfcat %>%
  filter(Airline_status=="Blue") 
#Creating a data frame of only blue Airline Status

blueAS <- blueAS %>%
  filter(Type_of_travel== "Personal Travel")
#Creating a data frame of Blue airline status and Personal Travel
View(blueAS)

dfblueAS <- as(blueAS,"transactions") #Creating a transactional matrix
rulesetDStatB <- apriori(dfblueAS, 
                    parameter=list(support=0.015,confidence=0.60),
                    appearance = list(default="lhs", rhs=("NPS=Detractor"))) 
#Applying apriori algorithm by keeping Detractor on RHS
summary(rulesetDStatB) #Summary of the ruleset
  
inspectDT(rulesetDStatB) #Interactive ruleset interface
rulesetPStatB <- apriori(dfblueAS, 
                    parameter=list(support=0.001,confidence=0.1),
                    appearance = list(default="lhs", rhs=("NPS=Promoter"))) 
#Applying apriori algorithm by keeping Promoter on RHS
summary(rulesetPStatB) #Summary of the ruleset
inspectDT(rulesetPStatB) #Interactive ruleset interface

plot(table(blueAS$Type_of_travel))
