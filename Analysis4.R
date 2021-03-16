#Ordinal Logistic Regression

library(MASS)#we need this library to perform the ordinal logistic regression
m <- polr(as.factor(NPSnum) ~ Price_sensitivity+Class_of_travelnum, data = New_flight, Hess=TRUE)
#It calculates the ordinal logistic regression for which we get all the p-values
#significant so we can interpret that they are good linear predictors.
summary(m)

(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2 #this helps to 
#find the p-value
(ctable <- cbind(ctable, "p value" = p)) #this adds another column p value 
#to the original result

#Decision Tree
install.packages("kernlab")
library(kernlab)
View(New_flight)
NumMatrix$New_flight.F_time_mins<-as.numeric(NumMatrix$New_flight.F_time_mins)
NumMatrix$New_flight.F_Distance<-as.numeric(NumMatrix$New_flight.F_Distance)
NumMatrix$New_flight.Arrival_delay_mins<-as.numeric(NumMatrix$New_flight.Arrival_delay_mins)
NumMatrix$New_flight.Departure_delay_mins<-as.numeric(NumMatrix$New_flight.Departure_delay_mins)
NumMatrix$New_flight.loyalty<-as.numeric(NumMatrix$New_flight.loyalty)
NumMatrix$New_flight.Shopping_at_airport<-as.numeric(NumMatrix$New_flight.Shopping_at_airport)
NumMatrix$New_flight.Eat_drink_at_airport<-as.numeric(NumMatrix$New_flight.Eat_drink_at_airport)
NumMatrix$New_flight.Age<-as.numeric(NumMatrix$New_flight.Age)
NumMatrix$New_flight.F_per_year<-as.numeric(NumMatrix$New_flight.F_per_year)
NumMatrix$New_flight.Price_sensitivity<-as.numeric(NumMatrix$New_flight.Price_sensitivity)
NumMatrix$New_flight.Likelihood_to_recommend<-as.factor(as.numeric(NumMatrix$New_flight.Likelihood_to_recommend))
NumMatrix$New_flight.Class_of_travelnum<-as.numeric(NumMatrix$New_flight.Class_of_travelnum)

colnames(trainData)[1] <-"Class_of_travel"
colnames(trainData)[2] <-"Shopping"
colnames(trainData)[3] <-"Eating"
colnames(trainData)[4] <-"F_time"
colnames(trainData)[5] <-"F_distance"
colnames(trainData)[6] <-"Arrival_delay"
colnames(trainData)[7] <-"Departure_delay"
colnames(trainData)[8] <-"Loyalty"
colnames(trainData)[9] <-"Age"
colnames(trainData)[10] <-"F_per_year"
colnames(trainData)[11] <-"Price_Sensitivity"
colnames(trainData)[12] <-"LTR"

colnames(NumMatrix)[1] <-"Class_of_travel"
colnames(NumMatrix)[2] <-"Shopping"
colnames(NumMatrix)[3] <-"Eating"
colnames(NumMatrix)[4] <-"F_time"
colnames(NumMatrix)[5] <-"F_distance"
colnames(NumMatrix)[6] <-"Arrival_delay"
colnames(NumMatrix)[7] <-"Departure_delay"
colnames(NumMatrix)[8] <-"Loyalty"
colnames(NumMatrix)[9] <-"Age"
colnames(NumMatrix)[10] <-"F_per_year"
colnames(NumMatrix)[11] <-"Price_Sensitivity"
colnames(NumMatrix)[12] <-"LTR"



View(trainData)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree = rpart(formula=LTR ~ F_time + Price_Sensitivity + F_per_year + Loyalty + Departure_delay +Age, data=trainData)
#We tried multiple values for creating a decision tree and finally it gave 
#as an optimized result.

tree = rpart(formula=LTR ~ F_time + Price_SensitivitY + Loyalty, data=trainData)
#we tried using 3 values and creating a decision tree from these values 
#but we didn't get an appropriate output to interpret so we dropped the idea.

prp(tree)

#Correlation Matrix

cor(NumMatrix,use = "complete.obs")
#It finds out a correlation matrix between all the attributes in the NumMatrix
#with respect to each other
#NumMatrix$New_flight.Likelihood_to_recommend<-NumMatrix$New_flight.Likelihood_to_recommend
View(NumMatrix)

#SVM
NumMatrix<-data.frame(New_flight$Class_of_travelnum,New_flight$Shopping_at_airport,New_flight$Eat_drink_at_airport,New_flight$F_time_mins,New_flight$F_Distance,New_flight$Arrival_delay_mins,New_flight$Departure_delay_mins,New_flight$loyalty,New_flight$Age,New_flight$F_per_year,New_flight$Price_sensitivity,New_flight$Likelihood_to_recommend)
#It creates a dataframe which has all the numeric attributes which can be 
#used further for data analysis
randIndex<-sample(1:dim(NumMatrix)[1])
summary(randIndex)
length(randIndex)
cutPoint2_3 <- floor(2 * dim(NumMatrix)[1]/3)
cutPoint2_3
colSums(is.na(NumMatrix))
trainData<-NumMatrix[randIndex[1:cutPoint2_3],]
testData <-NumMatrix[randIndex[(cutPoint2_3+1):dim(NumMatrix)[1]],]
svmOutput<-ksvm(New_flight.Likelihood_to_recommend~.,data=trainData,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
svmOutput
View(testData)
#testData$New_flight.Likelihood_to_recommend<-as.factor(testData$New_flight.Likelihood_to_recommend)

str(svmpred)
str(testData$New_flight.Likelihood_to_recommend)
#hist(alpha(svmOutput)[[1]])

#install.packages("e1071")
#install.packages("caret")
length(testData)
length(trainData)
library("caret")
library("e1071")
#testData$New_flight.Likelihood_to_recommend<-as.factor(testData$New_flight.Likelihood_to_recommend)
svmpred<-predict(svmOutput,testData)
str(svmpred)
str(testData$New_flight.Likelihood_to_recommend)
confusionMatrix(svmpred,testData$New_flight.Likelihood_to_recommend) #js


length(svmpred)
length(testData$New_flight.Likelihood_to_recommend)
str(testData)
str(trainData)

