# big data bread R code
##### QUESTION FOR TAMMY: (Line 20) Why do we need to EXCLUDEEE zip code from tree predictor values????


# importing workingData (grocery stores, and median income of zipcodes) into a dataframe
groceryStores <- read.csv("moreWorkingData.csv", header=T, na.strings="?")
fix(groceryStores)
summary(groceryStores)
summary(groceryStores$ZipCode)
groceryStores

highAndLowGroceryStores <- groceryStores[groceryStores$NumberClassification != 2, ]
fix(highAndLowGroceryStores)


################################################
# Classification Trees
################################################

# make it into a classification tree
install.packages("tree")
library(tree)

# classification tree for predicting CLASSIFICTION of grocery stores (high/medium/low)
tree.classificationGroceryStores=tree(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, highAndLowGroceryStores)
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
tree.zipcode=tree(ZipCode ~ WordClassification + MeanIncomeForZipcode + PopulationOfZipcode, highAndLowGroceryStores)
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
train=sample(1:nrow(highAndLowGroceryStores), 100) #need to play around with "train" data...
groceryStores.test=highAndLowGroceryStores[-train,]

# NEED TO CHECK "TRAINING" DATA

bay.c <- naiveBayes(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, highAndLowGroceryStores, subset=train)
summary(bay.c)
bay.c

results <- predict(bay.c, groceryStores.test)
table(results,groceryStores.test$WordClassification)

################################################
# Regression Trees (with bagging!)
################################################
install.packages("randomForest")
library(randomForest)

train=sample(1:nrow(highAndLowGroceryStores), 101) #need to play around with "train" data...

# create a test-set
grocery.test=highAndLowGroceryStores[-train,"MeanIncomeForZipcode"]

# predict MEAN INCOME PER ZIPCODE with regression trees
set.seed(1)
bag.grocery=randomForest(MeanIncomeForZipcode ~ PopulationOfZipcode + WordClassification, data=highAndLowGroceryStores, subset=train, mtry=2, importance=TRUE)
bag.grocery
yhat.bag = predict(bag.grocery, newdata=highAndLowGroceryStores[-train,])
#yhat.bag = head(yhat.bag, -1)

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
# NOT USING THIS --- because not relevant to groceryStores really... and it's not making robust models...
################################################

lm.fit=lm(MeanIncomeForZipcode ~ PopulationOfZipcode, data=groceryStores)
lm.fit
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(MeanIncomeForZipcode ~ PopulationOfZipcode, groceryStores)
abline(lm.fit)
coef(lm.fit)
confint(lm.fit)

summary(lm.fit)

################################################
# Let'z K-meanz cluster dis
#
################################################
set.seed(2)
x <- groceryStores[,c(1, 3, 6, 7, 8)]

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
#
# ... not clean (especially with cutting)
################################################
hc.complete=hclust(dist(highAndLowGroceryStores), method="complete")
hc.average=hclust(dist(highAndLowGroceryStores), method="average")
hc.single=hclust(dist(highAndLowGroceryStores), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average,main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single,main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 3)
cutree(hc.average, 3)
cutree(hc.single, 3)

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

biplot(pr.out, scale=1)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=1)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve

# plot the (regular) proportion of variance (per Principal Component)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')

# plot the cumulative proportion of variance
plot(cumsum(pve), xlab="Principle Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')



