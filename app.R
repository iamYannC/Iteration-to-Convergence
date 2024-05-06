itra <- function(simula,minr,maxr,ssize){
  iterations <-rep(0,simula)
  final_value <- rep(0, simula,ssize)
  for (i in 1:simula) {
    x <- sample(minr:maxr,ssize,F)

    while (length(table(x)) != 1) {

      iterations[i] <- iterations[i] + 1
      x <- sample(x, ssize, T)
    }
    final_value[i] <- x[1]
  }
  return(list(i=iterations,v=final_value))
}


library(shiny);library(tidyverse);library(glue)


# Define UI for application that draws a histogram
ui <- navbarPage(

                 # Application title
                 header=h1("Iteration to Convergence Simulation",align="center"),
                 br(),
h4("This simulator generates the distribution of iterations"),
h4("untill reaching convergence on a single value for a given sample size."),
br(),br(),
footer=h4("Algorithm by Prof. Yoav Kessler"),
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("sim",
                                 "Number of simulations:",
                                 min = 10,
                                 max = 1e4,step = 100,
                                 value = 100),
                     sliderInput("s.size",
                                 "Size of the initial sample:",
                                 min = 2,
                                 max = 300,step = 1,
                                 value = 10),
                     verbatimTextOutput("info"),
                     verbatimTextOutput("ds")

                   ),

                   # Show a plot of the generated distribution
                   mainPanel(
                     plotOutput("iter"),
                     plotOutput("val")
                   )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  iterations <- c()  # number of iterations until convergenc
  it <- reactive({
    itra(input$sim,1,400,input$s.size)

  })

  output$iter <- renderPlot({


    hist(it()[[1]],breaks=100,
         main=paste("Number of iterations till convergence"),
         sub=paste("Number of simulations:",input$sim,sep=" "),
         xlab=paste("Mean = ",round(mean(it()[[1]]),3),sep=""))


  })
  output$val <- renderPlot({


    hist(it()[[2]],breaks=100,
         main=paste("Converged value"),
         sub=paste("Number of simulations:",input$sim,sep=" "),
         xlab=paste("Mean value = ",round(mean(it()[[2]]),3),sep=""))


  })
  output$info <- renderPrint({
    glue(
   "Number of simulations: {input$sim}
    Sample size: {input$s.size}
    Mean converged value: {round(mean(it()[[2]]),3)}
    ")
  })

  output$ds <- renderPrint({
    glue(
    "SD of the iterations: {round(sd(it()[[1]]),3)}
    Mean iterations: {round(mean(it()[[1]]),3)}
    MD iterations: {round(median(it()[[1]]),3)}
    MO iterations: {round(as.numeric(names(sort(table(it()[[1]]),T)[1])),3)}
    Max/Min iterations: {max(it()[[1]])}/{min(it()[[1]])}

    ")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
