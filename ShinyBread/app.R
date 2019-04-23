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




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  headerPanel('K means'),
  fluidRow(
    # Kmeans stuff ~~~~~~~~~~~~~~~
    column(10, 
      sidebarPanel(
        selectInput('xcol', 'X Variable', names(workingData[,c(7)])),
        selectInput('ycol', 'Y Variable', names(workingData[,c(8)]),
                   selected=names(workingData)[[2]]),
        sliderInput("clusters",
                   "Number of clusters:",
                   min = 1,
                   max = 10,
                   value = 5) 
      ),
      mainPanel(
        plotOutput('plot1')
      )
    ),
    
    # Classification tree stuff  ~~~~~~~~~~~~~~~
    column(10,
       sidebarPanel(
         selectInput("lalala", "Choose lalala: ", list(1, 2, 3))
       ),
       
       # Show a plot of the generated distribution
       mainPanel(
         plotOutput("swagPlot")
       )
    ),
    
    # Bayes classifier stuff ~~~~~~~~~~~~~~~
    column(10,
      sidebarPanel(
        selectInput("ohohoh", "Maybe put like a helperText here describing how Confusion Table describes accuracy of our dataset: ", list(1, 2, 3))
      ),
      
      mainPanel(
        verbatimTextOutput(outputId = "confusionTable")
        #tableOutput("confusionTable")
      )
    ),
    
    # Principal component stuff  ~~~~~~~~~~~~~~~
    column(10,
       sidebarPanel(
         selectInput("hehehe", "I love principal components!: ", list(1, 2, 3))
       ),
       
       mainPanel(
         fluidRow(
           splitLayout(cellWidths = c("50%", "50%"), plotOutput('pcaPlot1'), plotOutput('pcaPlot2'))
         )
       )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Kmeans stuff ~~~~~~~~~~~~~~~
  groceryStores <- read.csv("moreWorkingData.csv", header=T, na.strings="?")
  
  selectedData <- reactive({
    #groceryStores[, c(input$xcol, input$ycol)]
    groceryStores[, c(7, 8)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
    # kmeans(groceryStores, input$clusters)
  })
  
  output$plot1 <- renderPlot({
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
  highAndLowGroceryStores <- workingData
  
  output$swagPlot <- renderPlot({
    
    tree.classificationGroceryStores=tree(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, groceryStores)
    summary(tree.classificationGroceryStores)
    plot(tree.classificationGroceryStores)
    text(tree.classificationGroceryStores)
    
  })

    
  # Bayes classifier stuff  ~~~~~~~~~~~~~~~
  
  # make a training set of a random sample that's half the size of the groceryStores data
  set.seed(2)
  train=sample(1:nrow(highAndLowGroceryStores), 100) #need to play around with "train" data...
  groceryStores.test=highAndLowGroceryStores[-train,]
  
  # NEED TO CHECK "TRAINING" DATA
  
  bay.c <- naiveBayes(WordClassification ~ MeanIncomeForZipcode + PopulationOfZipcode, highAndLowGroceryStores, subset=train)
  summary(bay.c)
  bay.c
  
  results <- predict(bay.c, groceryStores.test)
  #bayesConfusionTable <- table(results,groceryStores.test$WordClassification)
  #table(results,groceryStores.test$WordClassification)
  
  output$confusionTable <- renderPrint(
    table(results,groceryStores.test$WordClassification)
  )
  
  
  
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

