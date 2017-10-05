library(shiny)

fluidPage(
  titlePanel(h1("Interesting App Name")),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   h2("Interesting Title"),
                   p("TYPE OUT INTRODUCTION HERE")
                 ),
                 plotOutput("avgdeaths_cause"),
                 plotOutput("annualmortrate"),
                 plotOutput("annualmortrate_Gender")
                 ),
        tabPanel("Death Rates Across US States",
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "selYear", label = h3("Year"), min = 1999, 
                                      max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE)
                   )
                 ),
                 h3(textOutput("Year")),
                 htmlOutput("geochart")
        ),
        tabPanel("Who is most affected?",
                   sidebarPanel(
                     checkboxGroupInput("checkStates", label = h3("Select States"),
                                        choices = state_names, selected = c("New York", "Connecticut", "Vermont", "New Hampshire", "New Jersey", "Pennsylvania", "Massachusetts", "Maine", "Rhode Island", "Maryland", "Delaware"))
                   ),
                 mainPanel(
                  htmlOutput("statesRates") 
                 )
                 )
      )
    )
)

