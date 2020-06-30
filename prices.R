############################################################
####                          Final project                ####
###                                                     ####
####                                                    ####
############################################################

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

library(caret)

#Read House price dataset
hprice.df <- read.csv("D:/Nihel/Marquette university/Spring 2019/Business Analytics/Final exam Project/kc_house_data.csv")
head(hprice.df)
str(hprice.df)

#Reshaping our data by extracting 3 columns from the date column which having Year, Month and Day

library(reshape2)
library(ggplot2)
library(tidyverse)
library(lubridate)

hprice.df<-hprice.df %>% 
  mutate(id=as.factor(id)) %>% 
  mutate(Date=str_replace_all(hprice.df$date,"T0{1,}","")) %>% 
  select(Date,everything(),-date,-id)
hprice.df<-hprice.df %>% 
  mutate(Date=ymd(Date)) %>% 
  separate(Date,c("Year","Month","Day"))
head(hprice.df)

#Exploratory Data Analysis
#Check how many houses were sold in 2014 and  2015
options(warn=-1)
library(ggthemes)
hprice.df %>% 
  ggplot(aes(Year,fill=Year))+geom_histogram(stat="count")+
  ggtitle("The number of houses that were sold in 2014 and 2015")+
  theme_economist()

#compare the average prices in these two years
options(warn=-1)
options(scipen=999)
hprice.df %>% 
  ggplot(aes(Year,price,fill=Year))+geom_boxplot()+
  theme_economist()+ggtitle("Average prices in 2014 and 2015")+
  scale_fill_manual(values=c("dodgerblue","snow"))

#Trends in the Data
#Sales Trend in the Year 2015
hprice.df %>% 
  filter(Year==2015) %>% 
  ggplot(aes(Month,price,fill=Month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2015")+
  theme_economist()

#Sales Trend in the Year 2014
hprice.df %>% 
  filter(Year==2014) %>% 
  ggplot(aes(Month,price,fill=Month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2014")+
  theme_economist()

#Price plot
library(ggplot2)
library(tidyverse)

#Distribution of Price
hprice.df %>%
  
  ggplot(aes(x = price)) +    
  geom_histogram(alpha = 0.8) +
  stat_bin(bins = 30, fill="orange")
labs(x= 'Price',y = 'Count', title = "Distribution of Price")+ theme_bw() #Distribution of house prices was right skewed, so lets apply log() and then plot the distribution

# log price distribution
#Most of house price lies between 5.4 to 6 million.
hprice.df<-hprice.df %>% 
  mutate(log_price=log10(price))
ggplot(hprice.df,aes(x=log_price))+geom_histogram(fill="orange",binwidth=0.10) 

# Price density plot
ggplot(hprice.df,aes(price))+geom_density() #We see here that before log transformation we had positively skewed distribution.

# Log price density
ggplot(hprice.df,aes(log(price)))+geom_density() #Post transformation prices are normally distributed around the mean.

# Check how the number of bedrooms affect the price
#Median price is a better indication than average price of a property's value and the local real estate market.
#Real estate professionals refer to the median price rather than the average price because it is less affected by outliers or properties that skew the perceived values in a particular housing market.
#A house that sells for far more or far less than most houses in the area will skew an average price.

hprice.df %>%
  group_by(bedrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bedrooms = reorder(bedrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = bedrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#FF6666") +
  geom_text(aes(x = bedrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bedrooms', 
       y = 'Median Price', 
       title = '# of Bedrooms VS Median Price') +
  coord_flip() + 
  theme_bw()

# Check how the number of bathrooms affect the price
hprice.df %>%
  group_by(bathrooms) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bathrooms = reorder(bathrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  ggplot(aes(x = bathrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#E69F00") +
  geom_text(aes(x = bathrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bathrooms', 
       y = 'Median Price', 
       title = '# of Bathrooms VS Median Price') +
  coord_flip() + 
  theme_bw()

#Check the grade VS Median price
hprice.df %>%
  group_by(grade) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(grade = reorder(grade,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = grade,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#0072B2") +
  geom_text(aes(x = grade, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'grade', 
       y = 'Median Price', 
       title = 'grade VS Median Price') +
  coord_flip() + 
  theme_bw()
#Waterfront VS Median price
hprice.df %>%
  group_by(waterfront) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(waterfront = reorder(waterfront,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = waterfront,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#F0E442") +
  
  labs(x = 'waterfront', 
       y = 'Median Price', 
       title = 'Waterfront VS Median Price') +
  
  theme_bw()

#condition VS Median Price
hprice.df %>%
  group_by(condition) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(condition = reorder(condition,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = condition,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#F0E442") +
  
  labs(x = 'condition', 
       y = 'Median Price', 
       title = 'Condition VS Median Price') +
  
  theme_bw()

#yr_built VS Median Price
hprice.df %>%
  group_by(yr_built) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_built = reorder(yr_built,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = yr_built,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#CC79A7") +
  geom_text(aes(x = yr_built, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year built', 
       y = 'Median Price', 
       title = 'Year built VS Median Price') +
  coord_flip() + 
  theme_bw()

#yr_renovated VS Median Price
hprice.df %>%
  group_by(yr_renovated) %>%
  summarise(PriceMedian = median(price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(yr_renovated = reorder(yr_renovated,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = yr_renovated,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "#009E73") +
  geom_text(aes(x = yr_renovated, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'year renovated', 
       y = 'Median Price', 
       title = 'Year renovated VS Median Price') +
  coord_flip() + 
  theme_bw()

#Maps of the houses
#Houses near the coast are costlier.
#Most of the houses are in the range 250 thousand to 500 thousands. The next highest categories are
#500 to 750 thousand
#0 to 250 thousand
#750 thousand to 1 million
#1 million to 2 million
#and the least is above 2 million

library(leaflet)
hprice.df$PriceBin<-cut(hprice.df$price, c(0,250e3,500e3,750e3,1e6,2e6,999e6))

center_lon = median(hprice.df$long,na.rm = TRUE)
center_lat = median(hprice.df$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345","red"), 
                       hprice.df$PriceBin)



leaflet(hprice.df) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             color = ~factpal(PriceBin))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
  
  addLegend("bottomright", pal = factpal, values = ~PriceBin,
            title = "House Price Distribution",
            opacity = 1)

#Price Bins Count
hprice.df %>%
  mutate(PriceBin = as.factor(PriceBin)) %>%
  group_by(PriceBin) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  mutate(PriceBin = reorder(PriceBin,Count)) %>%
  arrange(desc(Count)) %>%
  
  ggplot(aes(x = PriceBin,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = "#0072B2") +
  geom_text(aes(x = PriceBin, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'PriceBin', 
       y = 'Count', 
       title = 'Count of house price distribution') +
  coord_flip() + 
  theme_bw()

# creating a correlation plot after exclusing Year, Month, Day, PriceBin and Log_price
library(GGally)
ggcorr(hprice.df %>% select(-Year,-Month, -Day, -PriceBin, -log_price), name = "corr",
       label = TRUE, hjust = 1, label_size = 2.5, angle = -45, size = 3) 

#Remove not important variables for building the models
hprice.df$Year  = NULL
hprice.df$Month  = NULL
hprice.df$Day  = NULL
hprice.df$log_price  = NULL
hprice.df$PriceBin  = NULL

#Feature selection using Boruta method
#Replcae the zero values with NA for yr_renovated and sqft_basement as they contain a lot of not available data
hprice.df$yr_renovated[hprice.df$yr_renovated == 0] <- NA
hprice.df$sqft_basement[hprice.df$sqft_basement == 0] <- NA

#Remove NA values
hprice.df[hprice.df == ""] <- NA
hprice.df <- hprice.df[complete.cases(hprice.df),]

#implement Boruta package
library(ranger)
library(Boruta)
set.seed(111)
hprice.df.boruta <- Boruta(price~., data =hprice.df , doTrace = 2)
print(hprice.df.boruta)
plot(hprice.df.boruta)

#Final desicion 
final.boruta <- TentativeRoughFix(hprice.df.boruta)
print(final.boruta)
plot(final.boruta)

#Confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

#Create dataframe for the final results
boruta_hprice.df <- attStats(final.boruta)
class(boruta_hprice.df)
print(boruta_hprice.df)

#### DATA MODELLING

#Splitting the dataset
library(caret)

set.seed(123)
trainIndex <- createDataPartition(hprice.df$price, p = 0.8, list = FALSE, times = 1)
hprice.Train <- hprice.df[trainIndex, ]
hprice.Valid <- hprice.df[-trainIndex, ]

#Build baseline models

# Use RMSE as a metric
#Set the metric and Control
control <- trainControl(method='repeatedcv', number=10, repeats=3)
metric <- 'RMSE'

# Linear Regression (LR)
set.seed(101)
fit.lm <- train(price~., data=hprice.Train, method='lm', metric=metric, 
                preProc=c('center', 'scale'), trControl=control)

# Generalized Linear Regression (GLM)
set.seed(101)
fit.glm <- train(price~., data=hprice.Train, method='glm', metric=metric,
                 preProc=c('center', 'scale'), trControl=control)

# Penalized Linear Regression (GLMNET)
set.seed(101)
fit.glmnet <- train(price~., data=hprice.Train, method='glm',
                    metric=metric, preProc=c('center', 'scale'),
                    trControl=control)

#Random forest Model
set.seed(101)
fit.rf <- train(price~., data=hprice.Train, method='rf',
                    metric=metric, preProc=c('center', 'scale'),
                    trControl=control)

# Support Vector Machines (SVM) 
set.seed(101)
fit.svm <- train(price~., data=hprice.Train, method='svmRadial',
                 metric=metric, preProc=c('center', 'scale'), 
                 trControl=control)

# #XGB model
set.seed(101)
fit.xgbTree<- train(price~., data=hprice.Train, method='xgbTree', metric=metric,
                 preProc=c('center', 'scale'), trControl=control)

# Compare the results of these algorithms
hprice.results <- resamples(list(lm=fit.lm, glm=fit.glm,
                                 glmnet=fit.glmnet, rf=fit.rf,
                                 svm=fit.svm, xgb=fit.xgbTree))

# Summary and Plot of Results
summary(hprice.results)
dotplot(hprice.results,main="Model Training Results")

#Building lm model
#Feature selection using the Step BIC criterion
null.model <- lm(price ~ 1 , data = hprice.Train)
full.model <- lm(price ~ . , data = hprice.Train)
BIC.model  <- step( null.model, scope = list( lower = null.model, upper = full.model), k = log(nrow(hprice.df)),
                    direction = "both")
summary(BIC.model)
#lm model with te most significant variables
fit.lm.model<- lm(price ~ sqft_living+lat+view+grade+yr_built+waterfront+bedrooms+
                    bathrooms+zipcode+long+sqft_above+condition+yr_renovated+
                    sqft_lot15+sqft_living15, data = hprice.Train)
summary(fit.lm.model)

#predict the model
fit.lm.model.pred <-predict(fit.lm.model, hprice.Valid)
postResample(fit.lm.model.pred, hprice.Valid$price)

# first 5 actual and predicted records
data.frame(actual = hprice.Valid$price[1:5], predicted = fit.lm.model.pred[1:5])

plot(fit.lm.model$fitted.values, fit.lm.model$residuals)


# plot lift chart
library(gains)
gain <- gains(hprice.Valid$price, fit.lm.model.pred, groups=10)

plot(c(0,gain$cume.pct.of.total*sum(hprice.Valid$price))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(hprice.Valid$price))~c(0, dim(hprice.Valid)[1]), lty=2)

##Build second models using important variables by Boruta

#Random forest using the selected variable by Boruta(excluding bedrooms, condition, yr_build and yr_renovated)

#Model 2
library(randomForest)
set.seed(101)
fit2.rf <- randomForest(price~bathrooms+sqft_living+sqft_lot+floors+waterfront+view+grade+
      sqft_above+sqft_basement+zipcode+lat+long+sqft_living15+sqft_lot15, data=hprice.Train,
      preProc=c('center', 'scale'),trControl=control, importance = TRUE)

# Predict training with Random forest
price_Prediction_rf = predict(fit2.rf, newdata = hprice.Train)
postResample(hprice.Train$price,price_Prediction_rf)

# Predict validaton with Random forest
price_Prediction_rf = predict(fit2.rf, newdata = hprice.Valid)
postResample(hprice.Valid$price,price_Prediction_rf)

## variable importance plot
varImpPlot(fit2.rf, type = 1)

#Model 3 using the important variables of random forest

library(randomForest)
set.seed(101)
fit3.rf <- randomForest(price~sqft_living+grade+ sqft_above+lat+sqft_living15,
                        data=hprice.Train,
                        preProc=c('center', 'scale'),trControl=control, importance = TRUE)
# Predict training with Random forest
price_Prediction_rf = predict(fit3.rf, newdata = hprice.Train)
postResample(hprice.Train$price,price_Prediction_rf)


# Predict validaton with Random forest
price_Prediction_rf = predict(fit3.rf, newdata = hprice.Valid)
postResample(hprice.Valid$price,price_Prediction_rf)

