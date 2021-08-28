Introduction

In this dataset, there are 1460 observations with 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa. Among explanatory variables, there are 37 integer variables, such as Id, MSSubClass, LotFrontage, and 43 factor variables, such as MSZoning, Street, LotShape. Descriptive analysis and quantitative analysis will use subsets of it depending on models.



#IMPOT DATA
library(ggplot2) # Data visualization
library(readr) 
library(dplyr)
library(plyr)
library(gridExtra)


house<-read.csv("D:\\edub\\PROJECT\\HOUSE.csv")

#check missing data
# list rows of data that have missing values 
missing_row <- house[!complete.cases(house),]
head(missing_row)

nrow(missing_row)

# show all variable names
name <- names(house)
name

#Here, we select these important variables by creating a vector that contains variable names
select_var <- c('Id','MSZoning','Utilities', 'Neighborhood','BldgType','HouseStyle',
                'OverallQual','OverallCond','YearBuilt', 'ExterQual','ExterCond',
                'BsmtQual','BsmtCond','TotalBsmtSF','Heating','HeatingQC', 
                'CentralAir','Electrical','GrLivArea','BedroomAbvGr','KitchenAbvGr',
                'KitchenQual','TotRmsAbvGrd','Functional','Fireplaces','FireplaceQu',
                'GarageArea','GarageQual','GarageCond','OpenPorchSF','PoolArea',
                'Fence','MoSold','YrSold','SaleType','SaleCondition','SalePrice') 




# construct subset of house dataset that is used for prediction
select_house <- house[,select_var]
head(select_house)

summary(select_house)

#summary of target variable
summary(select_house$SalePrice)

# Draw a higtogram to figure out the distribution of SalePrice
options(scipen=10000)
ggplot(select_house, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))

##From the histogram above, the distribution of our target variable,SalePrice is skewed to right. Thus, a log term of SalePrice should be generated for linear regression. Here, we name it lSalePrice.

#log term of SalePrice
select_house$lSalePrice <- log(select_house$SalePrice)


# Draw a higtogram to figure out the distribution of log SalePrice

ggplot(select_house, aes(x = lSalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 0.05) +
  ggtitle("Figure 2 Histogram of log SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))


##After fixing, lSalePrice is normally distributed. 

# count house by MSZoning
options(repr.plot.width=5, repr.plot.height=4)
ggplot(select_house, aes(x = MSZoning, fill = MSZoning )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure 3 Distribution of MSZoning")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

# Distribution of MSZoning
table(select_house$MSZoning)


##From the graph and table above, it is obvious that most of houses in this dataset are built in the area of Residential Low Density(1151 houses), and follows by Residential Medium Density(218 houses). Few houes are built in Commercial, Floating Village and Residential High Density.



#How does housing price look like in each category?
#boxplot of SalePrice by MSZoning
#add average value of SalePrice as green point
ggplot(select_house, aes(x=MSZoning, y=SalePrice, fill=MSZoning)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="blue", fill="blue")+
  theme(legend.position="none")+
  ggtitle("Figure 4 Boxplot of SalePrice by MSZoning")+
  theme(plot.title = element_text(hjust = 0.5))


##The graph above shows the distribution of SalePrice by MSZoning. The sales in "Floating Village Residential" area have the highest average sale price, and then followed by "Residential Low Density". While "Commercial" sales have the lowest average sale price.

# a table here to count houses in each catetory and also show maximum and minimum SalePrice.
ddply(house, .(BldgType), summarize,Total = length(BldgType),Max_price=max(SalePrice),Min_price=min(SalePrice))

# historgram of housing price by BldgType 
ggplot(select_house, aes(SalePrice)) +
  geom_histogram(aes(fill = BldgType), position = position_stack(reverse = TRUE), binwidth = 20000) +
  coord_flip() + ggtitle("Figure 5 Histogram of SalePrice") +
  ylab("Count") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position=c(0.9,0.8), legend.background = element_rect(fill="grey90",
                                                                                                            size=0.5, linetype="solid", 
                                                                                                            colour ="black"))



##For houses with type of Single-family Detached, most of their prices are within the range from 50000 to 300000
##For Two-family Conversion, Duplex, Townhouse End Unit and Townhouse Inside Unit, most of house prices are ranging from 75000 to 210000
##The highest and lowest house price both come to Single-family Detached house type

#The distribution of SalePrice by OverallQual
ggplot(select_house, aes(x = SalePrice,fill = as.factor(OverallQual))) +
  geom_histogram(position = "stack", binwidth = 10000) +
  ggtitle("Figure 6 Histogram of SalePrice") +
  ylab("Count") +
  xlab("Housing Price") + 
  scale_fill_discrete(name="OverallQual")+
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.9,0.7), legend.background = element_rect(fill="grey90",size=0.5, linetype="solid",colour ="black")) 
             
##Most houese are with OverallQuall of 4,5,6 and 7, equivalent to "Below Average", "Average", "Above Average" and "Good"
##The higher rate of overall quality, the higher house sale price
##For each rate level of overall quality, the distribution of house price is almost symmetric


# convert factor to numeric
select_house$ExterCond2 <- as.numeric(factor(select_house$ExterCond, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_house$HeatingQC2 <- as.numeric(factor(select_house$HeatingQC, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_house$CentralAir2 <- as.numeric(factor(select_house$CentralAir, 
                                              levels = c("N", "Y"),
                                              labels = c(0,1) ,ordered = TRUE))


#select variables that be used for model buidling and heat map
model <- c('SalePrice', 
               'OverallQual','OverallCond','YearBuilt','ExterCond2',
               'TotalBsmtSF','HeatingQC2', 
               'CentralAir2','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces',
               'GarageArea','OpenPorchSF','PoolArea',
               'YrSold')
heat <- select_house[,model]


#plot correlation heatmap for SalePrice
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(heat, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "BLACK", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Figure 7 Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))

##In this graph, Red indicates perfect positive correlation and black indicates perfect negative correlation. As we can see, there are several variables should be paid attention to: GarageArea, Fireplaces, TotRmsAbvGrd, GrLivArea, HeatingQC, TotalBsmtSF and YearBuild.

#analyze the correlation between SalePrice and numeric variables, including GrLivArea,TotalBsmtSF, TotRmsAbvGrd, GarageArea.

# scatter plot of GrLiveArea
p1 <- ggplot(select_house, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Figure 8 Scatter plot of SalePrice and GrLivArea") +
  theme(plot.title = element_text(hjust = 0.4))

# scatter plot of TotalBsmtSF
p2 <- ggplot(select_house, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Figure 9 Scatter plot of SalePrice and TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.4))

#scatter plot of TotRmsAbvGrd
p3 <- ggplot(select_house, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Figure 10 Scatter plot of SalePrice and TotRmsAbvGrd") +
  theme(plot.title = element_text(hjust = 0.4))

#scatter plot of GarageArea
p4 <- ggplot(select_house, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Figure 11 Scatter plot of SalePrice and GarageArea") +
  theme(plot.title = element_text(hjust = 0.4))


grid.arrange(p1, p2,p3,p4)

##GrLivArea, TotalBsmtSF, TotRmsAbvGrd, and GarageArea are positively correlated with SalePrice, which means with the increase of GrLivArea, TotalBsmtSF, TotRmsAbvGrd and GarageArea, the SalePrice also increases.
##TotalBsmtSF has more concentrated distribution than others




#MODEL FITTING

#Linear regression
#choose variables and transfer SalePrice into log term

model_lin <- select_house[, model]
model_lin$lSalePrice <- log(model_lin$SalePrice)

set.seed(10000)
house.index <- sample(c(1:dim(model_lin)[1]), dim(model_lin)[1]*0.8)
model_lin_house = model_lin[house.index,]
model_lin_valid <- model_lin[-house.index,]

#run regression
#use lm() to run linear regression of SalePrice on all variables in model dataset
linreg <- lm(lSalePrice~.-SalePrice, data = model_lin_house)
summary(linreg)



#forecast and check for model accuracy
library(forecast)
#use predict() to make prediction on a new set
pred1 <- predict(linreg,model_lin_valid,type = "response")
residuals <- model_lin_valid$lSalePrice - pred1
linreg_pred <- data.frame("Predicted" = pred1, "Actual" = model_lin_valid$lSalePrice, "Residual" = residuals)
accuracy(pred1, model_lin_valid$lSalePrice)

# classification tree
library(rpart)
library(rpart.plot)

class.tree <- rpart(lSalePrice~.-SalePrice,
                    data = model_lin_house,control = rpart.control(cp = 0.01))

plotcp(class.tree)
printcp(class.tree)


#Random Forest
library(randomForest)
RF <- randomForest(lSalePrice ~.-SalePrice, data = model_lin_house, 
                   importance =TRUE,ntree=500,nodesize=7, na.action=na.roughfix)


#prediction
rf.pred <- predict(RF, newdata=model_lin_valid )
accuracy(rf.pred, model_lin_valid$lSalePrice)


plot(rf.pred, model_lin_valid$lSalePrice, main = "Figure 9 Predicted vs. Actual log SalePrice") 
abline(0,1)

