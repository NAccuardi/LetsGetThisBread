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
    )#,
    
    # Bayes classifier stuff ~~~~~~~~~~~~~~~
    #column(10,
    #
    #              
    #)
    
    
    
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

    
  
  
    
    
}#end of server function
  
  




# Run the application 
shinyApp(ui = ui, server = server)

