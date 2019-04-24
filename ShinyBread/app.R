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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  headerPanel('Let\'s Get This Bread!'),
  mainPanel("Please take a look at some of the models that we have built!"),
  fluidRow(
    
    # Classification tree stuff  ~~~~~~~~~~~~~~~
    column(10,
           titlePanel("Classification Tree"),
           helpText("choose stuff"),
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
           helpText("choose stuff for kmeans..."),
           sidebarPanel(
             selectInput('xcol', 'X Variable', list("MeanIncomeForZipcode", "MedianIncomeForZipcode")),
             selectInput('ycol', 'Y Variable', list("PopulationOfZipcode")),
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
           helpText("Maybe put like a helperText here describing how Confusion Table describes accuracy of our dataset"),
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
             verbatimTextOutput(outputId = "confusionTable")
           )
    ),
    
    # Principal component stuff  ~~~~~~~~~~~~~~~
    column(10,
           titlePanel("Principal Components"),
           helpText("Left graph describes proportion of variance (per Principal Component), and right graph describes cumulative proportion of variance."),
           
           mainPanel(
             fluidRow(
               splitLayout(cellWidths = c("70%", "70%"), 
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
  
  output$confusionTable <- renderPrint({
    set.seed(2)
    train=sample(1:nrow(bayesChosenData()), 100) #need to play around with "train" data...
    groceryStores.test=bayesChosenData()[-train,]
    
    # NEED TO CHECK "TRAINING" DATA
    
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

