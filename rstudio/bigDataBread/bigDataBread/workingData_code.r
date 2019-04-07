# big data bread R code
##### QUESTION FOR TAMMY: (Line 20) Why do we need to EXCLUDEEE zip code from tree predictor values????


# importing workingData (grocery stores, and median income of zipcodes) into a dataframe
groceryStores <- read.csv("workingData.csv", header=T, na.strings="?")
fix(groceryStores)
summary(groceryStores)
groceryStores

################################################
# Classification Trees
################################################

# make it into a classification tree
install.packages("tree")
library(tree)

# classification tree for predicting CLASSIFICTION of grocery stores (high/medium/low)
tree.classificationGroceryStores=tree(Classification ~. -Grocery.Store.Name -Address -Zip.Code, groceryStores)
summary(tree.classificationGroceryStores)
plot(tree.classificationGroceryStores)
text(tree.classificationGroceryStores)

# a perfect classifier should have a deviance of 0... but our deviance is 84.69369,
# ... so classification tree is probably not the way to go
deviance(tree.classificationGroceryStores)

# classification tree for predicting MEAN income per zipcode
tree.meanIncomePerZipcode=tree(Mean.Income.for.Zipcode ~. -Grocery.Store.Name -Address -Zip.Code, groceryStores)
summary(tree.meanIncomePerZipcode)
plot(tree.meanIncomePerZipcode)
text(tree.meanIncomePerZipcode)

# ... nOT GOOD 
yikes <- deviance(tree.meanIncomePerZipcode)


################################################
# Bayes Classifier
################################################

install.packages("e1071")
library(e1071)
help(naiveBayes)

# make a training set of a random sample of 45 from groceryStores data
set.seed(2)
train=sample(1:nrow(groceryStores), 45) #need to play around with "train" data......

bay.c <- naiveBayes(Classification ~. -Grocery.Store.Name -Address -Zip.Code, groceryStores, subset=train)
summary(bay.c)
bay.c



