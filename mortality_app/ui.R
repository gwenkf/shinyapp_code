library(shiny)

fluidPage(
  titlePanel(h1("Interesting App Name")),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   h2("Overview of Project"),
                   column(9, p("write some kind of interesting description of research questions and initial findings here!"))
                 ),
          mainPanel(
            fluidRow(
            column(12,plotOutput("avgdeaths_cause"), plotOutput("annualmortrate"))
            )
        )
        ),
        tabPanel("Death Rates Across US States",
                 fluidRow(
                   h2("Interesting Title"),
                   p("TYPE OUT INTRODUCTION HERE")
                 ),
                 mainPanel(
                   sliderInput(inputId = "selYear", label = h3("Please Select A Year:"), min = 1999, 
                               max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500),
                   htmlOutput("geochart")
                 )
        ),
        tabPanel("Who is most affected?",
                   fluidRow(
                     column(6,
                            radioButtons("CoDbutton", label = h3("Select a Cause of Death:"),
                                         choices = list("Drug-Induced" = 1, "Alcohol-Induced" = 2), 
                                         selected = 1),
                            sliderInput(inputId = "slideYear", label = h3("Please Select A Year:"), min = 1999, 
                                        max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500)
                     )
                    ),
        mainPanel(
          htmlOutput("raceRatesCoD"),
          plotOutput("annualmortrate_Gender"),
          plotOutput("annualmortrate_Age")
        )
        )
      )
    )
)

