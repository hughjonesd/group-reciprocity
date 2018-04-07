#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("simulations.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Evolution of Group Reciprocity"),
   
   # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("G",
            "Number of groups:",
            min = 6,
            max = 24,
            value = 6,
            step = 2
          ),
      sliderInput("gsize",
        "Size of groups:",
        min = 8,
        max = 60,
        value = 8,
        step = 4
      ),
      sliderInput("periods",
        "Number of periods in a generation:",
        min = 1,
        max = 100,
        value = 10
      ),
      sliderInput("memory",
        "Length of group reciprocators' memory:",
        min = 1,
        max = 100,
        value = 10
      ),
      sliderInput("b",
        "Benefit of being helped:",
        min = 1,
        max = 100,
        value = 10
      )
    ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("generations_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  plot_values <- list(generation = integer(0), prop_GR = numeric(0))
  
  myfun <- function(pop, gen, stats) {
    isolate({
      plot_values$generation <- c(plot_values$generation, gen %% 100)
      plot_values$prop_GR    <- c(plot_values$prop_GR, sum(pop$strat == 'GR')/nrow(pop))
    })
    output$generations_plot <<- renderPlot({
      plot(plot_values$generation, plot_values$prop_GR, xlim = c(1, 100), ylim = c(0, 1),
        type = "l")
    })
  }
  
  observe({
    simulate(N = input$gsize * input$G, G = input$G, memory = input$memory,
          periods = input$periods, stop_at_generations = TRUE,
          b = input$b, generations = 1, report = 1, shiny_plotter = myfun)
    # need to have this run a single generation and update
    invalidateLater(20)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

