#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lets get this bread!"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("graph", h3("Model Selection: "),
                     c("K means" = 1,
                       "classification tree" = 2,
                       "regression tree" = 3,
                       "hierarchical clustering" = 4,
                       "random forest" =5),
                       selected = 1),
         
         
         
         conditionalPanel(
           condition = "input.graph == '1'",
         sliderInput("clusters", "Cluster Count", min=1, max=10, value=3)
         ),
         
         conditionalPanel(
           condition = "input.graph == '2'",
           sliderInput("breakCount", "graph 2", min=1, max=1000, value=20)
         ),
         
         conditionalPanel(
           condition = "input.graph == '3'",
           sliderInput("breakCount", "graph 3", min=1, max=1000, value=40)
         ),
         
         conditionalPanel(
           condition = "input.graph == '4'",
           sliderInput("breakCount", "graph 4", min=1, max=1000, value=80)
         )
     ),   #end of sidebar
      
      

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  plot1 <- reactive({
    # kmeans
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
  
  plot2 <- reactive({
    # classification tree
  })
  
  plot3 <- reactive({
    # regression tree
  })
  
  plot4 <- reactive({
    # hierarchical clustering
  })
  
  plot5 <- reactive({
    # random forest
  })
  
  # Return the requested graph
  graphInput <- reactive({
    switch(input$graph,
           "Plot1" = plot1(),
           "Plot2" = plot2(),
           "Plot3" = plot3(),
           "Plot4" = plot4(),
           "plot5" = plot5()
    )
  })
  
  output$selected_graph <- renderPlot({ 
    graphInput()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

