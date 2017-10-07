library(shiny)
library(data.table)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(googleVis)
library(tidyr)
library(dplyr)
library(scales)
library(plotly)
require(datasets)

Totals_cause = fread("./data/STdrugalcdeath_cause1599.csv", header = FALSE, stringsAsFactors = FALSE, 
                     col.names = c("State","State_code","Year","Year_code","DrugAlc_induced_causes","DrugAlc_induced_code",
                                   "Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
Totals_cause = Totals_cause[-c(1),]
Totals_cause = na.omit(Totals_cause)


GenderAge = fread("./data/STdrugalcdeath_genderage1599.csv", header = FALSE, stringsAsFactors = FALSE, 
                  col.names = c("State","State_code","Year","Year_code", "Gender","Gender_code", "Age_grps","Age_grpsCode",
                                "DrugAlc_induced_causes","DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
GenderAge = GenderAge[-c(1),]
GenderAge = na.omit(GenderAge)


RaceAge = fread("./data/STdrugalcdeath_raceage1599.csv", header = FALSE, stringsAsFactors = FALSE,
                col.names = c("State","State_code","Year","Year_code", "Race","Age_grps","Age_grpsCode","DrugAlc_induced_causes","DrugAlc_induced_code",
                              "Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
RaceAge = RaceAge[-c(1),]
RaceAge = RaceAge[Race != "Not Stated"&Race != "Not Hispanic or Latino", , ]
RaceAge = na.omit(RaceAge)

RaceGender = fread("./data/STdrugalcdeath_racegender1599.csv", header = FALSE, stringsAsFactors = FALSE,
                   col.names = c("State","State_code","Year","Year_code", "Gender","Gender_code", "Race","DrugAlc_induced_causes",
                                 "DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
RaceGender = RaceGender[-c(1),]
RaceGender = RaceGender[Race != "Not Stated"&Race != "Not Hispanic or Latino", , ]
RaceGender = na.omit(RaceGender)

CauseLabel = list(title = "Cause of Death", size = 20, color = "#17202A")
PrctLabel = list(title = "Percentage of Deaths", size = 20, color = "#17202A")
AgeLabel = list(title = "Ten Year Age Groups", size = 20, color = "#17202A")
YearLabel = list(title = "Year", size = 20, color = "#17202A")
TotalLabel = list(title = "Number of Deaths", size = 20, color = "#17202A")


ui = fluidPage(
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
                   column(12,plotlyOutput("avgdeaths_cause"), plotOutput("annualmortrate"))
                 )
               )
      ),
      tabPanel("Percentage of Deaths Across US States",
               fluidRow(
                 h2("Interesting Title"),
                 p("TYPE OUT INTRODUCTION HERE")
               ),
               mainPanel(
                 sliderInput(inputId = "selYear", label = h3("Please Select A Year:"), min = 1999, 
                             max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500),
                 radioButtons("causebutton", label = h3("Select a Cause of Death:"),
                              choices = list("Drug-Induced" = "Drug-Induced Causes", "Alcohol-Induced" = "Alcohol-Induced Causes",
                                             "Both" = "Total"), 
                              selected = "Total"),
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
                 plotlyOutput("annualmortrate_Age"),
                 plotlyOutput("DrugVAlc_raceYear"),
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
                 plotlyOutput("DrugVAlc_GenYear"),
                 plotlyOutput("Age_GenYear")
               )
      )
    )
  )
)


server = function(input, output) {
  output$avgdeaths_cause = renderPlotly({
    avg_totalCause = (Totals_cause %>% filter(DrugAlc_induced_causes == "Drug-Induced Causes"|DrugAlc_induced_causes == "Alcohol-Induced Causes")
                      %>% group_by(DrugAlc_induced_causes) %>% summarise(avgdeaths = sum(as.numeric(Deaths))))
    (plot_ly(x = avg_totalCause$DrugAlc_induced_causes, y = avg_totalCause$avgdeaths, name = "Total Drug & Alcohol Related Deaths in the USA, 1999-2015",
            type = "bar", color = I("#DC7633"), width = 1) %>% layout(xaxis = CauseLabel, yaxis = TotalLabel))
  })
  
  output$annualmortrate = renderPlot({
    deathsTotalYr = (Totals_cause %>% filter(DrugAlc_induced_causes == "Total"&Year != "") %>% group_by(Year) 
    %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365))
    (ggplot(data=deathsTotalYr, aes(x=Year, y=rate, group=1)) + geom_line(color="#1c9099",size=1) 
      + geom_point(color="#1c9099", size=2, shape = 23, fill="#1c9099")
      + theme_base() + ylab("Death Rate") + ggtitle("Annual Mortality by Drug and Alcohol Use, 1999-2015"))
  })
  
  raceInput = reactive({
    input$racebutton
  })
  YearRace_Input = reactive({
    input$slideYear
  })
  
  filter_RaceYr = reactive({
    RaceAge %>% filter(Race == raceInput()&Year == YearRace_Input())
  })
  
  output$annualmortrate_Age = renderPlotly({
    edit_data = as.data.frame(filter_RaceYr())
    edit_data = edit_data %>% group_by(Age_grps) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population))*1000)/365)*100))
    (plot_ly(x = edit_data$Age_grps, y = edit_data$rate, name = "Percentage of Deaths by Age Group",
            type = "bar", color = I("#DC7633"), width = 1))
  })
  
  output$DrugVAlc_raceYear = renderPlotly({
    plot4 = as.data.frame(filter_RaceYr())
    plot4 = plot4 %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population))*1000)/365)*100))
    plot_ly(x = plot4$DrugAlc_induced_causes, y = plot4$rate, name = "Percentage of Drug & Alcohol Related Deaths",
            type = "bar", color = I("#DC7633"), width = 1)
  })
  
  output$AllDeaths_Race = renderPlot ({
    plot5 = as.data.frame(RaceAge)
    plot5 = (plot5 %>% filter(Race == raceInput()) %>% group_by(Year) 
    %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365))
    (ggplot(data = plot5, aes(x = Year, y = rate, group = 1)) + geom_point(size = 2, shape = 23, color = "#1c9099", fill="#1c9099") 
      + geom_line(color = "#1c9099") + theme_base() + ggtitle("Annual Mortality by Drug & Alcohol Use") + xlab("Year")
      + ylab("Mortality Rate")) 
  })
  
  YearGen_Input = reactive ({
    input$slideYearGen
  })
  
  Gen_Input = reactive({
    input$genderbutton
  })
  
  filterGen = reactive({
    GenderAge %>% filter(Year == YearGen_Input()&Gender == Gen_Input())
  })
  
  output$annualmortrate_Gender = renderPlot({
    plotDeathsGender = (GenderAge %>% filter(Gender == Gen_Input()) %>% group_by(Year) 
    %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    (ggplot(data=plotDeathsGender, aes(x=Year, y=rate, group=1)) + geom_point(size=2, shape=23, color="#1c9099", fill = "#1c9099") 
      + geom_line(size=1, color="#1c9099") 
      + ylab("Death Rate") + theme_base() + ggtitle("Annual Mortality Rates by Gender, 1999-2015"))
  })
  
  output$DrugVAlc_GenYear = renderPlotly({
    DrugValc_gender = as.data.table(filterGen())
    DrugValc_gender = DrugValc_gender %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100)
    plot_ly(x = DrugValc_gender$DrugAlc_induced_causes, y = DrugValc_gender$rate, name = "Percentage of Drug & Alcohol Related Deaths",
            type = "bar", color = I("#DC7633"), width = 1)
  })
  
  output$Age_GenYear = renderPlotly({
    Gender_age = as.data.table(filterGen())
    Gender_age = (Gender_age %>% group_by(Age_grps) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    plot_ly(x = Gender_age$Age_grps, y = Gender_age$rate, name = "Percentage of Deaths by Age Group", type = "bar",
            color = I("#DC7633"), width = 1)
  })
  
  YearInput = reactive({
    year = input$selYear
    Totals_cause %>% filter(Year == year)
  })
  
  CauseInput = reactive({
    input$causebutton
  })
  
  output$geochart = renderGvis({
    states_year = as.data.frame(YearInput())
    states_year = (states_year %>% filter(DrugAlc_induced_causes == CauseInput()) %>% group_by(State) 
    %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    gvisGeoChart(states_year,"State", "rate",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=900, height=700,
                              colorAxis="{colors:['#FDEDEC', '#E74C3C']}"
                 ))     
  })
}

shinyApp(ui = ui, server = server)