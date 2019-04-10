# big data bread R code
##### QUESTION FOR TAMMY: (Line 20) Why do we need to EXCLUDEEE zip code from tree predictor values????


# importing workingData (grocery stores, and median income of zipcodes) into a dataframe
groceryStores <- read.csv("workingData.csv", header=T, na.strings="?")
fix(groceryStores)
summary(groceryStores)
summary(groceryStores$Zip.Code)
groceryStores

################################################
# Classification Trees
################################################

# make it into a classification tree
install.packages("tree")
library(tree)

# classification tree for predicting CLASSIFICTION of grocery stores (high/medium/low)
tree.classificationGroceryStores=tree(Word.Classification ~. -Grocery.Store.Name -Address -Zip.Code -Number.Classification, groceryStores)
summary(tree.classificationGroceryStores)
plot(tree.classificationGroceryStores)
text(tree.classificationGroceryStores)

# a perfect classifier should have a deviance of 0... but our deviance is 84.69369,
# ... so classification tree is probably not the way to go
deviance(tree.classificationGroceryStores)

# classification tree for predicting MEAN income per zipcode
tree.meanIncomePerZipcode=tree(Mean.Income.for.Zipcode ~. -Grocery.Store.Name -Address -Zip.Code -Nummber.Classification, groceryStores)
summary(tree.meanIncomePerZipcode)
plot(tree.meanIncomePerZipcode)
text(tree.meanIncomePerZipcode)

# ... nOT GOOD 
yikes <- deviance(tree.meanIncomePerZipcode)

# attempt to predict zipcode ... doesn't work well
tree.zipcode=tree(Zip.Code ~. -Grocery.Store.Name -Address -Number.Classification, groceryStores)
summary(tree.zipcode)
plot(tree.zipcode)
text(tree.zipcode)
summary(groceryStores$Zip.Code)

################################################
# Bayes Classifier
################################################

install.packages("e1071")
library(e1071)
help(naiveBayes)

# make a training set of a random sample that's half the size of the groceryStores data
set.seed(2)
train=sample(1:nrow(groceryStores), nrow(groceryStores)/2) #need to play around with "train" data...

bay.c <- naiveBayes(Word.Classification ~. -Grocery.Store.Name -Address -Zip.Code -Number.Classification, groceryStores, subset=train)
summary(bay.c)
bay.c


################################################
# Regression Trees (with bagging!)
################################################
install.packages("randomForest")
library(randomForest)

# create a test-set
grocery.test=groceryStores[-train,"Mean.Income.for.Zipcode"]

# predict MEAN INCOME PER ZIPCODE with regression trees
set.seed(1)
bag.grocery=randomForest(Mean.Income.for.Zipcode ~. -Grocery.Store.Name -Address -Zip.Code, data=groceryStores, subset=train, mtry=3, importance=TRUE)
bag.grocery
yhat.bag = predict(bag.grocery, newdata=groceryStores[-train,])

train
groceryStores[-train,]

plot(yhat.bag, grocery.test)
abline(0, 1)
mean((yhat.bag-grocery.test)^2) # mean squared error is 22-million LOL
yhat.bag
grocery.test
##### saved regression tree as LOL_regressionTreeNoThankYou_Yikes.pdf

# Median income per zipcode provides the most increase in MSE
importance(bag.grocery)
# Median income per zipcode ALSO is the most "pure" in nodes
varImpPlot(bag.grocery)

################################################
# Boosting things (Regression tree)
################################################

#### YIKES ---- only Multnomah county data == NOT ENOUGH DATA for boooooosting!!!!
#install.packages("gbm")
#library(gbm)
#set.seed(1)
#boost.grocery=gbm(Mean.Income.for.Zipcode ~. -Address -Grocery.Store.Name, data=groceryStores[train,], distribution="gaussian", n.trees=20, interaction.depth=4)
#summary(boost.grocery)


################################################
# Regression lines (with multiple regression!!!!!)
################################################

lm.fit=lm(Mean.Income.for.Zipcode ~ . -Address -Grocery.Store.Name -Zip.Code -Number.Classification, data=groceryStores)
lm.fit
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)


################################################
# Let'z K-meanz cluster dis
#
################################################
set.seed(2)
x <- groceryStores[,c(2, 6, 7, 8)]

km.out=kmeans(x, 3, nstart=20)
km.out$cluster
km.out$centers

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", pch=20, cex=2)

km.out$iter

###### TRY K-MEANS CLUSTERING 
### change "classification" value columns (aka "High" to 1; "Medium" to 2; "Low" to 3)




################################################
# Hierarchical clusteringggg
# .... what does this meann!!!!????? it kind of worked
################################################
hc.complete=hclust(dist(groceryStores), method="complete")
hc.average=hclust(dist(groceryStores), method="average")
hc.single=hclust(dist(groceryStores), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average,main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single,main="Single Linkage", xlab="", sub="", cex=.9)


################################################
# Principal components --- reduces the dimensionality of our dataset
################################################

groceryStoreIncomeAndPop <- groceryStores[,c(6, 7, 8)]

summary(groceryStoreIncomeAndPop)

apply(groceryStoreIncomeAndPop, 2, mean)
apply(groceryStoreIncomeAndPop, 2, var)

pr.out=prcomp(groceryStoreIncomeAndPop, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
pr.out$x

biplot(pr.out, scale=0)
