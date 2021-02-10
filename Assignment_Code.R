#packages
library(dplyr)
library(tree)
library(boot)

#reading data
data<-read.csv('C:/Users/sonal/Desktop/WSU/2020 Spring/Data Science/Assignment/2019.csv')

head(data)

#checking if all the independent variables are numeric or not
sapply(data,class)

#data cleaning

#replacing 0s with NA
data[data == 0] <- NA

View(data)

#replacing NAs with the mean of the column
data$GDP.per.capita[is.na(data$GDP.per.capita)] <- round(mean(data$GDP.per.capita, na.rm = TRUE))
data$Social.support[is.na(data$Social.support)] <- round(mean(data$Social.support, na.rm = TRUE))
data$Healthy.life.expectancy[is.na(data$Healthy.life.expectancy)] <- round(mean(data$Healthy.life.expectancy, na.rm = TRUE))
data$Freedom.to.make.life.choices[is.na(data$Freedom.to.make.life.choices)] <- round(mean(data$Freedom.to.make.life.choices, na.rm = TRUE))
data$Generosity[is.na(data$Generosity)] <- round(mean(data$Generosity, na.rm = TRUE))
data$Perceptions.of.corruption[is.na(data$Perceptions.of.corruption)] <- round(mean(data$Perceptions.of.corruption, na.rm = TRUE))

View(data)

#adding new column
data_new <- data %>%
  mutate(Score.Level = if_else(data$Score > 5.99, 'H', 'L'))

View(data_new)

summary(data_new)

#Relationship
par(mfrow=c(2,3))
plot(data_new$Score~data_new$GDP.per.capita,main='Score vs GDP',xlab='GDP',ylab='Happiness Score',col=2)
plot(data_new$Score~data_new$Social.support,main='Score vs Social Support',xlab='Social Support',ylab='Happiness Score',col=3)
plot(data_new$Score~data_new$Healthy.life.expectancy,main='Score vs Life Expectancy',xlab='Life Expectancy',ylab='Happiness Score',col=4)
plot(data_new$Score~data_new$Freedom.to.make.life.choices,main='Score vs Freedom',xlab='Freedom',ylab='Happiness Score',col=5)
plot(data_new$Score~data_new$Generosity,main='Score vs Generosity',xlab='Generosity',ylab='Happiness Score',col=6)
plot(data_new$Score~data_new$Perceptions.of.corruption,main='Score vs Corruption',xlab='Corruption',ylab='Happiness Score',col=7)

#Correlation
cor(data_new$Score,data_new$GDP.per.capita) #0.7964624
cor(data_new$Score,data_new$Social.support) #0.7727645
cor(data_new$Score,data_new$Healthy.life.expectancy) #0.7709061
cor(data_new$Score,data_new$Freedom.to.make.life.choices) #0.5667418
cor(data_new$Score,data_new$Generosity) #0.07582369
cor(data_new$Score,data_new$Perceptions.of.corruption) #0.3856131

#Mutliple Linear Regression
lin.reg=lm(data_new$Score~data_new$GDP.per.capita+data_new$Social.support+data_new$Healthy.life.expectancy+data_new$Freedom.to.make.life.choices+data_new$Generosity+data_new$Perceptions.of.corruption)
summary(lin.reg)

#Remove Generosity and Corruptions
lin.reg2=lm(data_new$Score~data_new$GDP.per.capita+data_new$Social.support+data_new$Healthy.life.expectancy+data_new$Freedom.to.make.life.choices)
summary(lin.reg2)

#overall accuracy
anova(lin.reg2)
qf(0.95,4,151) #2.431562

#Residuals
predict(lin.reg2)
resid(lin.reg2)

par(mfrow=c(1,2))

#plotting the residuals against the predicted values
plot(predict(lin.reg2),resid(lin.reg2), xlab = "Fitted Values", ylab = "Residuals",col='skyblue')

#histogram of the residuals
hist(resid(lin.reg2), main = paste("Histogram of Residuals"), xlab = "Residuals",col='skyblue')

#residual plots
par(mfrow=c(2,2))
plot(lin.reg2,lwd=2,col='skyblue',pch=1)

#####################################################################

#Cross Validation

data_cv=data_new[,3:9]

m1=glm(Score~GDP.per.capita,data=data_cv)
m2=glm(Score~GDP.per.capita+Social.support,data=data_cv)
m3=glm(Score~GDP.per.capita+Social.support+Healthy.life.expectancy,data=data_cv)
m4=glm(Score~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices,data=data_cv)
m5=glm(Score~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Generosity,data=data_cv)
m6=glm(Score~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Generosity+Perceptions.of.corruption,data=data_cv)

#training, testing, cv dataset
#Validation-set approach 60/40
#set.seed(2)
tr.id <- sample(1:nrow(data_cv),nrow(data_cv)*.6)
#train dataset
train_cv <- data_cv[tr.id , ]
#validation dataset
validation_cv <- data_cv[-tr.id , ]
dim(train_cv) # 93 7
dim(validation_cv) #63 7

#error estimates

#create initial error vector with values 0's
train_error <- rep(0,6)
#create initial error vector with values 0's
validation_error <- rep(0,6)

m1t = glm(m1, subset = tr.id) #fit model1 for training data
score_hat1 <- predict (m1t ,data_cv) #predict happiness score
train_error[1] <- mean((data_cv$Score-score_hat1)[tr.id]^2) #calculate training error
validation_error[1] <- mean((data_cv$Score-score_hat1)[-tr.id]^2) #calculate validation error

m2t = glm(m2, subset = tr.id) #fit model1 for training data
score_hat2 <- predict (m2t ,data_cv) #predict happiness score
train_error[2] <- mean((data_cv$Score-score_hat2)[tr.id]^2) #calculate training error
validation_error[2] <- mean((data_cv$Score-score_hat2)[-tr.id]^2) #calculate validation error

m3t = glm(m3, subset = tr.id) #fit model3 for training data
score_hat3 <- predict (m3t ,data_cv) #predict happiness score
train_error[3] <- mean((data_cv$Score-score_hat3)[tr.id]^2) #calculate training error
validation_error[3] <- mean((data_cv$Score-score_hat3)[-tr.id]^2) #calculate validation error

m4t = glm(m4, subset = tr.id) #fit model4 for training data
score_hat4 <- predict (m4t ,data_cv) #predict happiness score
train_error[4] <- mean((data_cv$Score-score_hat4)[tr.id]^2) #calculate training error
validation_error[4] <- mean((data_cv$Score-score_hat4)[-tr.id]^2) #calculate validation error

m5t = glm(m5, subset = tr.id) #fit model5 for training data
score_hat5 <- predict (m5t ,data_cv) #predict happiness score
train_error[5] <- mean((data_cv$Score-score_hat5)[tr.id]^2) #calculate training error
validation_error[5] <- mean((data_cv$Score-score_hat5)[-tr.id]^2) #calculate validation error

m6t = glm(m6, subset = tr.id) #fit model6 for training data
score_hat6 <- predict (m6t ,data_cv) #predict happiness score
train_error[6] <- mean((data_cv$Score-score_hat6)[tr.id]^2) #calculate training error
validation_error[6] <- mean((data_cv$Score-score_hat6)[-tr.id]^2) #calculate validation error

#loocv
loocv_error=rep(0,6) 
loocv_error[1]=cv.glm(data_cv,m1)$delta[1]
loocv_error[2]=cv.glm(data_cv,m2)$delta[1]
loocv_error[3]=cv.glm(data_cv,m3)$delta[1]
loocv_error[4]=cv.glm(data_cv,m4)$delta[1]
loocv_error[5]=cv.glm(data_cv,m5)$delta[1]
loocv_error[6]=cv.glm(data_cv,m6)$delta[1]


#kfold
cv_errorKF= rep (0,6)
#set.seed(2) #seed can be any value
cv_errorKF[1] <- cv.glm(data_cv,m1, K=10)$delta[1]
cv_errorKF[2] <- cv.glm(data_cv,m2, K=10)$delta[1]
cv_errorKF[3] <- cv.glm(data_cv,m3, K=10)$delta[1]
cv_errorKF[4] <- cv.glm(data_cv,m4, K=10)$delta[1]
cv_errorKF[5] <- cv.glm(data_cv,m5, K=10)$delta[1]
cv_errorKF[6] <- cv.glm(data_cv,m6, K=10)$delta[1]


#mse values
train_error
validation_error
cv_errorKF
loocv_error

#plotting
model <- c(1:6)
plot(model, train_error, type = "b", xlab = "Model", ylab = "MSE", col = 2, main = "Comparison",ylim=c(0.25,0.6))
{ lines(validation_error, type = "b", col = 3)
  lines(cv_errorKF, type = "b", col = 9)
  lines(loocv_error, type = "b", col = 5)
}
cols=c(2,3,9,5)
legend(x = 4, y = 0.6,cex=0.5, c("Train", "Validation", "LOOCV", "10-fold"), fill=cols)


#####################################################################

#Decision Tree

#keeping only the decision variables and score level
data_tree=data_new[,c(-1:-3)]
View(data_tree)

#converting character variable to factor variable
data_tree$Score.Level = as.factor(data_tree$Score.Level)
sapply(data_tree,class)

#fitting a tree on training data
set.seed(2)
train.dt = sample(1:nrow(data_tree), nrow(data_tree)/1.25)
test.dt=data_tree[-train.dt,]
length(train.dt) #124
length(test.dt) #7
tree.data=tree(Score.Level~.,data_tree,subset=train.dt)
summary(tree.data)
#0.05645 = 7 / 124  misclassified
0.05645*100

#plotting
plot(tree.data)
text(tree.data, pretty=0, cex=0.6)

#cross validation
cv.tree.data=cv.tree(tree.data,FUN = prune.misclass)
plot(cv.tree.data$size, cv.tree.data$dev,type="b",xlim=c(1,10),ylim=c(15,45))
#7

#pruning
prune.tree.data=prune.tree(tree.data,best=6)
plot(prune.tree.data)
text(prune.tree.data, pretty=0, cex=0.75)

summary(prune.tree.data)
#0.1129 = 14 / 124

#testing model accuracy
tree_predicted<-predict(tree.data, test.dt, type="class")

#confusion matrix
table(tree_predicted,test.dt$Score.Level)

#calculating misclassifiction rate
matrix<-table(tree_predicted,test.dt$Score.Level)
misrate<-((matrix[1,2]+matrix[2,1])/sum(matrix))
paste("Misclassification error rate is ",misrate)
# 0.15625

#########################################################

#Principle Component Analysis

#keeping only the independent variables
pca.data=data_new[,4:10]
head(pca.data)

#renaming for simplification purpose
colnames(pca.data)[c(1:6)]=c("GDP","SS","Life","Free","Gener","Corrup")

#mean and variance
sapply(pca.data[,1:6],mean)
sapply(pca.data[,1:6],var)

#PCA
obj = prcomp(pca.data[,1:6], scale. = TRUE) 
names(obj)
obj$rotation
summary(obj)
par(mfrow=c(1,2))

#screeplot
screeplot(obj,col='lightgreen',main='',ylim=c(0,3))

#biplot
biplot(obj,cex=0.75)

#proportion of variance

#sd of each pc
obj$sdev

#calculate var of each pc
obj.var = obj$sdev^2

#proportional variance explained by each pc
pve = obj.var/sum(obj.var)

#plot pve
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')

