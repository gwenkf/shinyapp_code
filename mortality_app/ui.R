library(shiny)

fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Summary"),
      tabPanel("Plots",
               fluidRow(
                 column(6,
                        sliderInput(inputId = "selYear", label = h3("Year Range"), min = 1999, 
                                    max = 2015, value = c(2012, 2015), step = 1, round = TRUE, sep = "", ticks = TRUE, dragRange = TRUE)
                 )
               ),
               plotOutput("avgYrRace")
      )
    )
  )
)

