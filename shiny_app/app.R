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
  titlePanel(h1("Exploring Mortality Rates by Drug and Alcohol Use in the United States")),
  mainPanel(
    tabsetPanel(
      tabPanel("Project Overview",
               fluidRow(
                 column(12, h4("The prevalance of drug and alcohol abuse is one of the largest pulic health issues here in the United States, particularly
                    as we find ourselves in the midst of what many are calling an 'opioid epidemic.' Drug and Alcohol dependency is an indiscriminate disease,
                    affecting people of all backgrounds across diverse communities. Using mortality data disseminated by the Center for Disease Control, 
                    I have set out to discover some of the common trends behind drug and alcohol related mortality in the United States between the years of
                    1999-2015. All of the information provided in the CDC dataset was obtained from death certificates where the immediate cause of death
                    was cited as either drug or alcohol-related. Provided along with that information was the race/ethnicity, gender and age group to which
                    the deceased belonged. Please refer to the interactive graphs wihin any of the four tabs to take a closer look at 
                    which populations are dying due to drug and alcohol use, and where they are dying."))
               ),
               mainPanel(
                 fluidRow(
                          splitLayout(cellWidths = c("95%", "105%"), plotlyOutput("avgdeaths_cause"), plotOutput("annualmortrate")))
               )
      ),
      tabPanel("Percentage of Deaths Across US States",
               fluidRow(
                 h2("Mapping Death Rates Across the US"),
                 h4("The map below depicts the percentage of deaths that have occurred in each US state due to drug and alcohol use. The annual mortality rate
                   (deaths per 1,000 persons per year) for each state was converted to a percentage and can be viewed across the years 1999 to 2015 using the slider widget and
                   according to the immediate cause of death of the deceased using the radio buttons.")
               ),
               mainPanel(
                 radioButtons("causebutton", label = h3("Select a Cause of Death:"),
                              choices = list("Drug-Induced" = "Drug-Induced Causes", "Alcohol-Induced" = "Alcohol-Induced Causes",
                                            "Both" = "Total"), selected = "Total"),
                 splitLayout(cellWidths = c(600, "170%"), sliderInput(inputId = "selYear", label = h3("Please Select A Year:"), min = 1999, 
                             max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500),
                 htmlOutput("geochart"))
               )
      ),
      tabPanel("Racial Breakdown",
               fluidRow(
                 h4("How do annual mortality rates differ across racial and ethnic categories?"),
                 column(6,
                        radioButtons("racebutton", label = h3("Select Race/Ethnicity:"),
                                     choices = list("Black or African American", "American Indian or Alaska Native",
                                                    "Asian or Pacific Islander", "White", "Hispanic or Latino"), 
                                     selected = "Black or African American")),
                 column(6,
                        sliderInput(inputId = "slideYear", label = h3("Please Select A Year:"), min = 1999, 
                                    max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE,width = 500))
                 ),
               mainPanel(
                 splitLayout(cellWidths = c("110%", "100%", "100%"), 
                 plotOutput("AllDeaths_Race"),
                 plotlyOutput("DrugVAlc_raceYear"),
                 plotlyOutput("annualmortrate_Age"))
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
                 column(12,
                 plotOutput("annualmortrate_Gender"),
                 plotlyOutput("DrugVAlc_GenYear"),
                 plotlyOutput("Age_GenYear"))
               )
      )
    )
  )
)

server = function(input, output) {
  output$avgdeaths_cause = renderPlotly({
    avg_totalCause = (Totals_cause %>% filter(DrugAlc_induced_causes == "Drug-Induced Causes"|DrugAlc_induced_causes == "Alcohol-Induced Causes")
                      %>% group_by(DrugAlc_induced_causes) %>% summarise(avgdeaths = sum(as.numeric(Deaths))))
    (plot_ly(x = avg_totalCause$DrugAlc_induced_causes, y = avg_totalCause$avgdeaths, 
             name = "Total Drug & Alcohol Related Deaths in the USA, 1999-2015",
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
            type = "bar", color = I("#DC7633"), width = 1) %>% layout(xaxis = AgeLabel, yaxis = PrctLabel))
  })
  
  output$DrugVAlc_raceYear = renderPlotly({
    plot4 = as.data.frame(filter_RaceYr())
    plot4 = plot4 %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population))*1000)/365)*100))
    (plot_ly(x = plot4$DrugAlc_induced_causes, y = plot4$rate, name = "Percentage of Drug & Alcohol Related Deaths",
            type = "bar", color = I("#DC7633"), width = 1) %>% layout(xaxis = CauseLabel, y = PrctLabel))
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
    (plot_ly(x = DrugValc_gender$DrugAlc_induced_causes, y = DrugValc_gender$rate, name = "Percentage of Drug & Alcohol Related Deaths",
            type = "bar", color = I("#DC7633"), width = 1) %>% layout(xaxis = CauseLabel, yaxis = PrctLabel))
  })
  
  output$Age_GenYear = renderPlotly({
    Gender_age = as.data.table(filterGen())
    Gender_age = (Gender_age %>% group_by(Age_grps) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    (plot_ly(x = Gender_age$Age_grps, y = Gender_age$rate, name = "Percentage of Deaths by Age Group", type = "bar",
            color = I("#DC7633"), width = 1) %>% layout(xaxis = AgeLabel, yaxis = PrctLabel))
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