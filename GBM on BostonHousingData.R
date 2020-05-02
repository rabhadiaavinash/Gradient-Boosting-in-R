# GBM Boosting

# to fit boosted regression tree to Boston dataset
#We run gbm() with the option
#distribution="gaussian" since this is a regression problem; if it were a binary
#classification problem, we would use distribution="bernoulli".


# n.trees=5000 indicates that we want 5000 trees
# interaction.depth=4 limits the depth of each tree.


library (gbm)
library(mlbench)

set.seed (1)

data("BostonHousing") # under mlbench
data <- BostonHousing
str(data)
View(data)

ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
ind
train <- data[ind==1,]
train
test <- data[ind==2,]
test

boost.boston <- gbm(medv ~ .,data=train , distribution= "gaussian", n.trees =5000 , interaction.depth =4)
boost.boston


summary (boost.boston )

#We see that lstat and rm are by far the most important variables
# partial dependence plot for these 2 var

# These plots
# illustrate the marginal effect of the selected variables on the response after
# integrating out the other variables

par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

# medv are incresing with rm and decreasing with lstat


yhat.boost=predict (boost.boston ,newdata = test,n.trees= 5000)
yhat.boost


mean(( yhat.boost - test$medv)^2)
# 13.20149


# boosting with shrinkage parameter(lambda)

?gbm


boost.boston2 <- gbm(medv ~ .,data=train , distribution= "gaussian", n.trees =5000 , interaction.depth =4,
                     shrinkage = 0.2,
                     verbose = F
                     )

boost.boston2

yhat.boost2=predict (boost.boston2 ,newdata = test,n.trees= 5000)
yhat.boost2


mean(( yhat.boost2 - test$medv)^2)

# 10.19668




