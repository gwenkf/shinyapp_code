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
        tabPanel("Racial Breakdown",
                   fluidRow(
                     column(6,
                            radioButtons("racebutton", label = h3("Select Race/Ethnicity:"),
                                         choices = list("Black or African American", "American Indian or Alaska Native",
                                                        "Asian or Pacific Islander", "White", "Hispanic or Latino"), 
                                         selected = "Black or African American"),
                            sliderInput(inputId = "slideYear", label = h3("Please Select A Year:"), min = 1999, 
                                        max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500)
                     )
                    ),
        mainPanel(
          plotOutput("annualmortrate_Age"),
          plotOutput("DrugVAlc_raceYear"),
          plotOutput("AllDeaths_Race")
        )
        ),
        tabPanel("Breakdown by Gender",
          fluidRow(
            column(6,
                   radioButtons("genderbutton", label = h3("Select Sex:"),
                                choices = list("Female", "Male"), 
                                selected = "Female"),
                   sliderInput(inputId = "slideYearGen", label = h3("Please Select a Year"), min = 1999,
                               max = 2015, value = c(2015), step=1, round = TRUE, sep = "", ticks = TRUE, width = 500)
                   )
          ),
        mainPanel(
          plotOutput("annualmortrate_Gender"),
          plotOutput("DrugVAlc_GenYear"),
          plotOutput("Age_GenYear")
        )
        )
      )
    )
)

