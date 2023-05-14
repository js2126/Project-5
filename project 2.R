#dataset came from https://www.kaggle.com/datasets/nehalbirla/vehicle-dataset-from-cardekho?select=Car+details+v3.csv

data <- read.csv("car_data.csv")
head(data)
library("ggplot2")
table(data$name) #car names with multiple quantity

data$mileage <- gsub(' kmpl','',data$mileage)
data$mileage <- gsub(' km/kg','',data$mileage)
data$max_power <- gsub(' bhp','',data$max_power)
data$engine <- gsub(' CC','',data$engine)


#Is petrol or diesel more valuable?
data_2 <- data[,c("year","fuel","selling_price")]
data_3 <- aggregate(selling_price ~ year+fuel, data_2, mean)

options(scipen = 999)
ggplot(data_3, aes(x=year,y=selling_price,fill=fuel)) +
  geom_bar(position="dodge",stat="identity") +
  xlab("Year") +
  ylab("Average Selling Price") + 
  labs("Average Selling Price by Year for Fuel Type")
#figure 1
#we see that average prices go up a lot when cars are made in 2010 onwards, diesel the steepest

diesel_data <- data_2[,c("year","fuel")]
table(diesel_data) #there are a good amount of diesel cars in the data so diesel do on average sell for more compared to petrol
#diesel cars sell at a higher amount than petrol, newer cars seem to make the most money, for petrol its consistent from 2016


#Is manual or automatic more valuable?
data_4 <- data[,c("year","transmission","selling_price")]
data_5 <- aggregate(selling_price ~ year+transmission, data_4, mean)

options(scipen=999)
ggplot(data_5, aes(x=year,y=selling_price,fill=transmission)) +
  geom_bar(position="dodge",stat="identity") +
  xlab("Year") +
  ylab("Average Selling Price") + 
  labs(title = "Average Selling Price by Year for Transmission Type")
#figure 2
#We can see that automatic goes up in value the most the newer the model is
#manual cars go up the newer the car is but not as fast
#automatic cars have a higher value

#does the number of previous owners effect the selling price?
data_6 <- data[,c("year","owner","selling_price")]
data_7 <- aggregate(selling_price ~ year+owner, data_6, mean)

options(scipen=999)
ggplot(data_7, aes(x=year,y=selling_price, fill=owner)) +
  geom_bar(position="dodge",stat="identity") +
  xlab("Year") +
  ylab("Average Selling Price") +
  labs(title = "Average Selling Price by Year for Ownership Type")
#figure 3
#Ownership type for cars made before 2010 makes no difference
#After 2010 car models, the first owner starts to make a difference, the biggest difference between first owner and other ownership types is most recent cars made
#Second ownership has a little effect, however overall second+ ownership has no effect on value


#What is the correlation between km driven, how fuel efficient the car is, engine, max power, year and price?
#install.packages("corrplot")
library("corrplot")

data_8 <- data[,c("km_driven","mileage","engine","max_power","year","selling_price","fuel","transmission")]

data_8$fuel <- gsub('Diesel','1',data_8$fuel)
data_8$fuel <- gsub('Petrol','2',data_8$fuel)
data_8$fuel <- gsub('LPG','3',data_8$fuel)
data_8$fuel <- gsub('CNG','4',data_8$fuel)

data_8$transmission <- gsub('Manual','1',data_8$transmission)
data_8$transmission <- gsub('Automatic','2',data_8$transmission)

data_8[,1:8] <- sapply(data_8[,1:8], as.numeric)
print(str(data_8))
data_8 <- na.omit(data_8)

print(sapply(data_8, is.numeric))
data_8a <- cor(data_8[,c(1,2,3,4,5,6)])

corrplot(data_8a, is.corr = FALSE, method = 'ellipse') #figure 4
head(data_8a)
#Interpretation of data:
#there is a negative correlation between kilometers drove and selling price, so more driving in kilometers the lower the selling price
#weak negative correlation between mileage and selling price, as the mileage increases, the selling price goes down
#as the engine power increases, the value of the car goes up
#max power is strongly positive correlated with selling price, so the higher max power, the higher the selling price
#as year increases, the selling price increases, which means newer cars sell at a higher price
#the max power and engine are strongly positive correlated, so as max power increases, the engine power increases, these cars also tend to be younger as year and max power are low positive correlated

model_1 <- lm(formula = selling_price ~ year+max_power+engine+mileage+km_driven+fuel+transmission, data=data_8)
summary(model_1) #p value is less than 0.05% for all except engine #figure 5

model_2 <- lm(formula = selling_price ~ year+max_power+mileage+km_driven+fuel+transmission, data=data_8)
summary(model_2) #all p-values are less than 0.01% so the independent variables affect the change in selling price
#R-squared is 0.6669, so 66.69% of the variation of the variables can be explained by the regression model
#figure 6


plot(model_2,1) #this model is not linear so it is not valid #figure 7


#quadratic transformation
data_10 <- sqrt(data_8)
model_4 <- lm(formula = selling_price ~year+max_power+mileage+km_driven+fuel+transmission,data=data_10)
summary(model_4) #figure 8
plot(model_4,1) #worse than log transformation #figure 9

#Reciprocal transformation
data_11 <- 1/data_8
data_11 <- data_11[!is.infinite(rowSums(data_11)),]
model_5 <- lm(formula = selling_price ~ year+max_power+mileage+km_driven+fuel+transmission,data=data_11)
summary(model_5) #figure 10
plot(model_5,1) #worse than log transformation #figure 11

#log transformation
data_9 <- log(data_8)
data_9 <- data_9[!is.infinite(rowSums(data_9)),]
model_3 <- lm(formula = selling_price ~ year+max_power+engine+mileage+km_driven+fuel+transmission, data=data_9)
summary(model_3) #figure 12
plot(model_3,1) #this is better, accurately predicts 85.71% of the variation of the variables
#figure 13

qqnorm(data_9$selling_price, main="Normal Q-Q Plot")
qqline(data_9$selling_price, lwd=2) #most points are around the line
#now get coefficients and build the multiple linear regression formula, then done
#figure 14


head(data)
#testing with index 3 car sale
print(-1626.57223+214.52326*log(2006)+0.89971*log(78)+0.50877*log(1497)+0.21058*log(17.7)-0.07179*log(140000)-0.19429*log(2)+0.45977*log(1))
exp(11.90003) #147271 selling price estimate, actual was 158000 so was fairly accurate
