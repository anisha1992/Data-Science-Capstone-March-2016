#Pratice on Capstone Data Set - EDA
####Importing Data set from text file: 
read.csv("~/Practice Internet Ad/Ad Practice/ad-dataset (2)/ad.data", header=FALSE)

###Summary Stats
summary(ad)

### Get the shape of the data
## Get the "shape" of the data
dim(ad)
head(ad)
tail(ad)
str(ad)

###Variable labels (df dataset)
install.packages("readxl")
library(readxl)
df <- read_excel("Variable name .xlsx")

##Transposing from column to row 
View(t(df))

var.names <- t(df)

##Attaching Varible Labels to the dataset 
colnames(ad) <- var.names 


### Converting variables into relevant vectors:
## Variable 1:4 from factor to numeric
install.packages("plyr")
install.packages("dplyr")
library(plyr)
library(dplyr)

#(Delete this possibly)
indx <- sapply(ad, is.factor)
ad[indx] <- lapply(ad[indx], function(x) as.numeric(as.character(x)))

##Subsetting the dataset to keep out the dependent variables from all the conversions
newdata <- ad[c(-1559)]
indx <- sapply(newdata, is.factor)
newdata[indx] <- lapply(newdata[indx], function(x) as.numeric(as.character(x)))

##Adding the dependent variable back in 
newdata$ad_detected <- ad$`ad/nonad`

##Converting integers to factors 
## variable 4:1558 from integer to factor

indx2 <- sapply(newdata, is.integer)
ad[indx2] <- lapply(newdata[indx2], as.factor)


## Checking str(ad)
str(newdata)

## Convert "local" variable from numeric to facto
as.factor(newdata$`local `)
newdata$`local `<- as.factor(newdata$`local `)


##Find a threshold value for when to not use a specific column? 
### remove any columns that may have more than 5% values missing

final_data <- newdata[,colSums(is.na(newdata)) < 163]

###check how many NA are left 
sum(is.na(final_data))

### Removing rows with NA data -- 
ad_data2 <- final_data[rowSums(is.na(final_data)) < 1,]

## "ad_data2" is the dataset used in the model after wrangling is completed and before LASSO

## Logistic (LASSO)
### Creating the test and train samples
test <- sample(nrow(ad_data2),0.3*nrow(ad_data2))
data.train <- ad_data2[-test,]
data.test <- ad_data2[test,] 


x <- model.matrix(ad_detected~.,data=data.train)[,-1]
y <- data.train$ad_detected
x.test <- model.matrix(ad_detected~., data=data.test)[,-1]
y.test <- data.test$ad_detected

install.packages("glmnet")

##cross validation for lambda 
library(glmnet)
grid = 10^seq(10,-2,length=100)
lasso.train <- glmnet(x,y,family="binomial", alpha=1, lambda=grid)
dim(coef(lasso.train))

##determining minimum lambda value
set.seed(123)
cv.out = cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv.out)
bestlam = cv.out$lambda.min
#bestlam = cv.out$lambda.1se



### Training Accuracy
train.lasso = predict(lasso.train, s=bestlam, newx=x, type="class")
table(y, train.lasso) 

### Test Accuracy
lasso.pred = predict(lasso.train, s=bestlam, newx=x.test, type="class")
table(y.test,lasso.pred) 
## 4 is the false positive -- which we want to control. we dont want them to be taking ads out. 

###performing regularization to get coefficients 
out = glmnet(x,y,alpha=1,lambda=grid,family="binomial")
lasso.coef = predict(lasso.train,type="coefficients",s=bestlam)[1:1556,] ####THE 1556 represents the number of variables. 
lasso.coef
lasso.coef[lasso.coef!=0] ## This should give you our model itself. Which variables are relevant. The ones with 0 as coefficients will be removed
length(lasso.coef[lasso.coef!=0])

###Remaining Variables? 57 -- down from 1556 
