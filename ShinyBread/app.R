#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# for classification trees...
install.packages("tree")
library(tree)

# for bayes classfier...
install.packages("e1071")
library(e1071)
help(naiveBayes)

groceryStores <- read.csv("moreWorkingData.csv", header=T, na.strings="?")
highAndLowGroceryStores <- groceryStores[groceryStores$NumberClassification != 2, ]



# Define UI for application that displays various models of the data
ui <- fluidPage(
  
  headerPanel('~~~~~ Let\'s Get This Bread! ~~~~~'),
  titlePanel('An analysis of grocery stores, income, and population for certain zip-codes in the Portland, Vancouver, and Seattle Metro areas.'),
  helpText("Our research provides useful metrics for grocery store quality, location, and concentration. Some metrics that one might find interesting include how a particular zip-code area’s household incomes influence the kind of grocery stores built in that area or how the density of an area may provide more variety in the types of grocery stores present."),
  helpText("Below you will find various models that show our predictions of how much influence that a grocery store has in their respective zip-code areas."),
  helpText("REFERENCE: Data was obtained from United States Census Bureau (for grocery store locations) and American Community Survey (for income and population per zipcode)."),
  headerPanel('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
  
  fluidRow(
    
    # Classification tree stuff  ~~~~~~~~~~~~~~~
    column(10,
           titlePanel("Classification Tree"),
           helpText("These types of decision trees are helpful with figuring out how to predict the classification of different grocery stores.",
                    "The predictions for each classification is based on the income and population for each relevant zip-code that our collection of grocery stores appear in."),
           helpText("To note, high-level grocery stores include stores like Whole Foods Market and New Seasons, medium-level grocery stores include Fred Meyer and Safeway, and low-level grocery stores include Walmart and Grocery Outlet."),
           helpText("We included the options to choose either to look at the datasets for all classifications or for only high-and-low classifications due to the immense number of medium-level grocery stores in our overall dataset (which may cause the model to be less accurate)."),
           helpText("You also have the option to choose either mean or median incomes per zipcode as your predictor variable for the tree (along with the population per zipcode)."),
           sidebarPanel(
              selectInput("classificationTreeChoice", 
                         "Choose dataset: ", 
                         list("All Grocery Stores (High, Medium, and Low-level)", 
                              "Only High and Low-level Grocery Stores")
              ),
              selectInput("classificationMeanOrMedianIncome",
                          "Choose mean or median income: ",
                          list("Mean income per zipcode",
                               "Median income per zipcode")
              )
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("classificationTreePlot")
           )
    ),
    
    # Kmeans stuff ~~~~~~~~~~~~~~~
    column(10, 
           titlePanel("K-means Clustering"),
           helpText("K-means is a greedy clustering algorithm that, given a dataset of features, will produce K sets of observations.",
                    "In our case, we are able to observe the similarities between different grocery stores based on the incomes and populations per zipcode."),
           helpText("Furthermore, we can see the wide range of populations and incomes per zip-code for each of the grocery stores that are in our dataset."),
           helpText("You have the option to choose to analyze the data with either mean or median income per zip-code (along with the population per zip-code).",
                    "Additionally, you can choose the number of clusters that you would like to use for analyzing the data."),
           sidebarPanel(
             selectInput('xcol', 'Choose X Variable:', list("MeanIncomeForZipcode", "MedianIncomeForZipcode")),
             selectInput('ycol', 'Choose Y Variable:', list("PopulationOfZipcode")),
             sliderInput("clusters",
                         "Number of clusters:",
                         min = 1,
                         max = 10,
                         value = 5) 
           ),
           mainPanel(
             plotOutput('kmeansPlot')
           )
    ),
    
    # Bayes classifier stuff ~~~~~~~~~~~~~~~
    column(10,
           titlePanel("Bayes Classifier (and Confusion Table)"),
           helpText("We can use Bayes Theorem to create a classifier. You can calculate the probabilities of the class being assigned X based on the probabilities",
                    "of each feature conditioned on the class being X multiplied together and finally multiplied by the probability of class being X."),
           helpText("Essentially, this information will help us see how well mean/median income and population per zip-code will predict the classification-level of each grocery store.",
                    "If you take a look at the \"Conditional Properties\" section, then you'll see the range of income and population values that are predicted for high, medium, and low-level grocery stores."),
           helpText("Additionally, the confusion table (shown on the right) describes the accuracy of our Bayes classifier model."),
           helpText("Again, you can choose to look at the dataset for all classifications or for only high-and-low classifications due to the immense number of medium-level grocery stores in our overall dataset (which may cause the model to be less accurate)."),
           helpText("You also have the option to choose to look at mean or median incomes per zip-code (along with the population per zip-code)."),
           sidebarPanel(
             selectInput("bayesClassifierChoice",
                         "Choose dataset: ",
                         list("All Grocery Stores (High, Medium, and Low-level)", 
                              "Only High and Low-level Grocery Stores")
             ),
             selectInput("bayesIncome",
                         "Choose mean or median income:",
                         list("Mean income per zipcode",
                              "Median income per zipcode")
             )
           ),
           
           mainPanel(
             fluidRow(
               splitLayout(cellWidths = c("86%", "45%"), 
                           verbatimTextOutput(outputId = "bayesClassifier"), 
                           verbatimTextOutput(outputId = "confusionTable")
               )
             )
           )
    ),
    
    # Principal component stuff  ~~~~~~~~~~~~~~~
    column(10,
           titlePanel("Principal Components"),
           helpText("The loadings (which are the 3 blue arrows) for the first two principal components can show how different the various predictor variables are based on their principal component score on the left-most biplot.",
                    "As we can see in the left biplot, the loadings (which are the weights of the principal components) for mean and median incomes are very similar to each other, while the loading for population is different from the income predictors."),
           helpText("The middle plot describes the variance for each principal component, which allows us to see how much of the total variance is explained by each principal component.",
                    "The middle graph shows how the variances are decreasing for each principal component.",
                    "So if we were to keep the principal components that explain at least 50% of the variance in the data, we would most likely keep only the first principal component into our consideration for analyzing the data."),
           helpText("The right-most plot describes the cumulative proportion of variance, which shows how all the principal components are explaining all the variance (since the grpah's line is increasing)."),
           
           mainPanel(
             fluidRow(
               splitLayout(cellWidths = c("80%", "50%", "50%"), 
                           plotOutput('pcaOriginalPlot'),
                           plotOutput('pcaPlot1'), 
                           plotOutput('pcaPlot2')
               )
             )
           )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Kmeans stuff ~~~~~~~~~~~~~~~
  
  selectedData <- reactive({
    groceryStores[, c(input$xcol, input$ycol)]
    #groceryStores[, c(7, 8)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
    # kmeans(groceryStores, input$clusters)
  })
  
  output$kmeansPlot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         #xlab = "",
         #ylab = "",
         pch = 20, cex = 3
    )
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  
  
  # Classification tree stuff  ~~~~~~~~~~~~~~~
  classificationChosenData <- reactive({
    if (input$classificationTreeChoice == "All Grocery Stores (High, Medium, and Low-level)") {
      groceryStores
    } else {
      highAndLowGroceryStores
    }
  })
  
  output$classificationTreePlot <- renderPlot({
    if (input$classificationMeanOrMedianIncome == "Mean income per zipcode") {
      tree.classificationGroceryStores=tree(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, classificationChosenData())
    } else {
      tree.classificationGroceryStores=tree(WordClassification ~ MedianIncomeForZipcode + PopulationOfZipcode, classificationChosenData())
    }
    
    plot(tree.classificationGroceryStores)
    text(tree.classificationGroceryStores)
  })

    
  # Bayes classifier stuff  ~~~~~~~~~~~~~~~
  
  # make a training set of a random sample that's half the size of the groceryStores data
  
  bayesChosenData <- reactive({
    if (input$bayesClassifierChoice == "All Grocery Stores (High, Medium, and Low-level)") {
      groceryStores
    } else {
      highAndLowGroceryStores
    }
  })
  
  output$bayesClassifier <- renderPrint({
    set.seed(2)
    train=sample(1:nrow(bayesChosenData()), 100) #need to play around with "train" data...
    groceryStores.test=bayesChosenData()[-train,] # NEED TO CHECK "TRAINING" DATA
    
    if (input$bayesIncome == "Mean income per zipcode") {
      bay.c <- naiveBayes(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, bayesChosenData(), subset=train)
    } else {
      bay.c <- naiveBayes(WordClassification ~ MedianIncomeForZipcode + PopulationOfZipcode, bayesChosenData(), subset=train)
    }
    
    bay.c
  })
  
  
  output$confusionTable <- renderPrint({
    set.seed(2)
    train=sample(1:nrow(bayesChosenData()), 100) #need to play around with "train" data...
    groceryStores.test=bayesChosenData()[-train,] # NEED TO CHECK "TRAINING" DATA
    
    if (input$bayesIncome == "Mean income per zipcode") {
      bay.c <- naiveBayes(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, bayesChosenData(), subset=train)
    } else {
      bay.c <- naiveBayes(WordClassification ~ MedianIncomeForZipcode + PopulationOfZipcode, bayesChosenData(), subset=train)
    }
    
    results <- predict(bay.c, groceryStores.test)
    
    table(results,groceryStores.test$WordClassification)
  })
  

  
  
  # Principal component stuff  ~~~~~~~~~~~~~~~
  
  groceryStoreIncomeAndPop <- groceryStores[,c(6, 7, 8)]
  
  pr.out=prcomp(groceryStoreIncomeAndPop, scale=TRUE)
  
  output$pcaOriginalPlot <- renderPlot({
    # create a biplot of the 3 principal components (mean income, median income, and population)
    biplot(pr.out, scale=1)
  })
  
  pr.var=pr.out$sdev^2
  pve=pr.var/sum(pr.var)
  
  output$pcaPlot1 <- renderPlot({
    # plot the (regular) proportion of variance (per Principal Component)
    plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
  })
  
  output$pcaPlot2 <- renderPlot({
    # plot the cumulative proportion of variance
    plot(cumsum(pve), xlab="Principle Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
  })
  
    
    
}#end of server function
  
  


# Run the application 
shinyApp(ui = ui, server = server)

