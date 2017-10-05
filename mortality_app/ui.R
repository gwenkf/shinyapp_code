library(shiny)

fluidPage(
  titlePanel(h1("Interesting App Name")),
    mainPanel(
      tabsetPanel(
        tabPanel("Death Rates Across US States",
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "selYear", label = h3("Please Select A Year:"), min = 1999, 
                                      max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 400)
                   )
                 ),
          sidebarPanel(
            radioButtons("mapRates_button", label = h3("Rates"),
                         choices = list("Crude Mortality Rate" = 1, "Annual Mortality Rate" = 2), 
                         selected = 1)
          ),
          mainPanel(
            htmlOutput("geochart") 
        )
        ),
        tabPanel("Overview",
                 fluidRow(
                   h2("Interesting Title"),
                   p("TYPE OUT INTRODUCTION HERE")
                 ),
                 plotOutput("avgdeaths_cause"),
                 plotOutput("annualmortrate"),
                 plotOutput("annualmortrate_Gender")
        ),
        tabPanel("Who is most affected?",
                   fluidRow(
                     column(6,
                     checkboxGroupInput("checkCoD", label = h3("Select Cause of Death:"),
                                        choices = c("Drug-Induced", "Alcohol-Induced"), selected = c("Drug Induced"))
                     )
                    ),
        mainPanel(
          htmlOutput("raceRatesCoD")
        )
        )
      )
    )
)

