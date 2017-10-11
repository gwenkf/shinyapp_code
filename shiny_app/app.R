library(shiny)
library(data.table)
library(ggplot2)
library(ggthemes)
library(googleVis)
library(tidyr)
library(dplyr)
library(plotly)
library(shinythemes)
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

CauseLabel = list(title = "Cause of Death", size = 13, color = "#17202A")
PrctLabel = list(title = "Percentage of Deaths", size = 13, color = "#17202A", exponentformat = "E")
AgeLabel = list(title = "Ten Year Age Groups", size = 13, color = "#17202A")
YearLabel = list(title = "Year", size = 13, color = "#17202A")
TotalLabel = list(title = "Number of Deaths", size = 13, color = "#17202A")
RateLabel = list(title = "Mortality Rate", size = 13, color = "#17202A", exponentformat = "E")
Font = list(family = "Serif", size = 13, color = "black")


ui = fluidPage(theme = shinytheme("cosmo"),
  titlePanel(h1(tags$b("Exploring Mortality Rates by Drug and Alcohol Use in the United States"))),
  mainPanel(
    tabsetPanel(
      tabPanel(tags$b("Project Overview"),
               br(),
               br(),
               fluidRow(
                 splitLayout(cellWidths = c(690, 700), plotlyOutput("avgdeaths_cause"), plotlyOutput("annualmortrate"))
               ),
               fluidRow(
                 tags$hr(),
                 h4("The prevalance of drug and alcohol abuse is one of the biggest public health concerns in the United States, particularly
                    as we find ourselves in the midst of what many are calling an 'opioid epidemic.' Drug and Alcohol dependency is an indiscriminate disease,
                    affecting people of all backgrounds across diverse communities. Using mortality data disseminated by the Center for Disease Control, 
                    I have set out to discover some of the common trends behind drug and alcohol related mortality in the United States between the years of
                    1999-2015. All of the information provided in the CDC dataset was obtained from death certificates where the immediate cause of death
                    was cited as either drug or alcohol-related. Provided along with that information was the race/ethnicity, gender and age group to which
                    the deceased belonged.")), br(), 
                    column(12, h4("The charts above demonstrate the annual mortality rate (deaths per 1,000 persons per year) by drug and alcohol use for each year between 1999 and 2015 as well as the total number of deaths 
                    that occurred over that period of time by the cause of death. Although the rates suggest that only a very small portion of the US population is dying 
                    due to these causes, it is evident that the numbers are increasing each year. For a closer look at the populations and areas most affected, refer to 
                    the interactive charts in any of the following three tabs.")
               ),
               mainPanel(
               )
      ),
      tabPanel(tags$b("Mapping Death Rates Across the US"),
               fluidRow(
                 h4("The map below depicts the percentage of deaths that have occurred in each US state due to drug and alcohol use. The annual mortality rate
                   for each state was converted to a percentage and can be viewed across the years 1999 to 2015 and according to the immediate cause of death of the deceased using the radio buttons and slider bar.
                    Geographical differences in terms of the prevalence of drug and/or alcohol related deaths can be observed across the years.")
               ), tags$hr(),
               mainPanel(
                 splitLayout(cellWidths = c(500,400), radioButtons("causebutton", label = h3("Select a Cause of Death:"),
                              choices = list("Drug-Induced" = "Drug-Induced Causes", "Alcohol-Induced" = "Alcohol-Induced Causes",
                                            "Both" = "Total"), selected = "Total"),
                 sliderInput(inputId = "selYear", label = h3("Select A Year:"), min = 1999, 
                             max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE)),
                 htmlOutput("geochart")
               )
      ),
      tabPanel(tags$b("Racial Breakdown"),
               fluidRow(
                 column(12,h4("How do annual mortality rates differ across racial and ethnic categories? How much do rates of mortality differ between age groups?
                    The following graphs visualize the trends in mortality rates across age groups and racial categories. The rates and percentages
                    visualized below are extremely small yet, the data allow us to better characterize the popluations that are dying
                    as a result of drug and alcohol use. The highest death rates overall are observed within the 'American Indian or Alaska Native' and 'White'
                    categories. In 2015 the American Indian/Alaska Native population experienced a higher percentage of alcohol-related deaths than drug-related deaths,
                    while the opposite is true for the White population. Further information on any particular Age or Racial group can be accessed using the slider bar and radio
                    buttons below."))
                 ), tags$hr(),
               mainPanel(
                 splitLayout(cellWidths = c(700, 300, 600), plotlyOutput("AllDeaths_Race"),
                             (radioButtons("racebutton", label = h3("Select Race/Ethnicity:"),choices = list("Black or African American", "American Indian or Alaska Native",
                              "Asian or Pacific Islander", "White", "Hispanic or Latino"), selected = "Black or African American")),
                             (sliderInput(inputId = "slideYear", label = h3("Select A Year:"), min = 1999, 
                                          max = 2015, value = c(2015), step = 1, round = TRUE, sep = "", ticks = TRUE))),
                 br(),
                 splitLayout(cellWidths = c(700, 700), 
                 plotlyOutput("DrugVAlc_raceYear"),
                 plotlyOutput("annualmortrate_Age")),
                 tags$br()
               )
      ),
      tabPanel(tags$b("Breakdown by Sex"),
               fluidRow(
                 h4("Differing rates of drug and alcohol related deaths between men and women are depicted below. It is interesting to see
                    that drug-related deaths seem to be more common for both men and women, although the percentage of men dying from either cause
                    is higher than that of females. Further information on any particular sex or age group can be accessed using the slider bar and
                    radio buttons below.")
               ), tags$hr(),
               mainPanel(
                 splitLayout(cellWidths = c(700, 300, 600) ,plotlyOutput("annualmortrate_Gender"), 
                             (radioButtons("genderbutton", label = h3("Select Sex:"), choices = list("Female", "Male"), 
                            selected = "Female")), (sliderInput(inputId = "slideYearGen", label = h3("Select a Year"), 
                            min = 1999, max = 2015,value = c(2015), step=1, round = TRUE, sep = "", ticks = TRUE))),
                 br(),
                 splitLayout(cellWidths = c(700, 700),plotlyOutput("DrugVAlc_GenYear"),
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
             type = "bar", marker = list(color = c("#2874A6")),width = 1) %>% layout(xaxis = CauseLabel, yaxis = TotalLabel, 
              title = "Total Drug & Alcohol Related Deaths in the USA, 1999-2015", font=Font))
  })
  
  output$annualmortrate = renderPlotly({
    deathsTotalYr = (Totals_cause %>% filter(DrugAlc_induced_causes == "Total"&Year != "") %>% group_by(Year) 
    %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365))
    (plot_ly(x = deathsTotalYr$Year, y = deathsTotalYr$rate, type = 'scatter', mode = 'lines', color = c("red")) 
      %>% layout(xaxis = YearLabel, yaxis = RateLabel, title = "Annual Mortality Rate by Drug & Alcohol Use, 1999-2015", font=Font))
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
    edit_data = edit_data %>% group_by(Age_grps, DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population))*1000)/365)*100))
    edit_data = spread(data = edit_data, key = DrugAlc_induced_causes, value = rate)
    (plot_ly(x = edit_data$Age_grps, y = edit_data$`Alcohol-Induced Causes`, name = "Alcohol-Induced", showlegend = TRUE,
            type = "bar", width = 1) %>% add_trace(y = edit_data$`Drug-Induced Causes`, name = "Drug-Induced")
      %>% layout(xaxis = AgeLabel, yaxis = PrctLabel, title = "Percentage of Deaths by Age Group", font=Font, barmode = "stack"))
  })
  
  output$DrugVAlc_raceYear = renderPlotly({
    plot4 = as.data.frame(filter_RaceYr())
    plot4 = plot4 %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population))*1000)/365)*100))
    (plot_ly(x = plot4$DrugAlc_induced_causes, y = plot4$rate, name = "Percentage of Drug & Alcohol Related Deaths",
            type = "bar", marker = list(color = c("#2874A6")), width = 1) %>% layout(xaxis = CauseLabel, y = PrctLabel, 
            title = "Percentage of Drug & Alcohol Related Deaths", font=Font))
  })
  
  output$AllDeaths_Race = renderPlotly ({
    plot5 = as.data.frame(RaceAge)
    plot5 = (plot5 %>% filter(Race == raceInput()) %>% group_by(Year) 
    %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365))
    (plot_ly(x = plot5$Year, y = plot5$rate, type = 'scatter', mode = 'lines', color = c("red")) %>% layout(xaxis = YearLabel,
    yaxis = RateLabel, title = "Annual Mortality by Drug & Alcohol Use", font=Font))
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
  
  output$annualmortrate_Gender = renderPlotly({
    plotDeathsGender = (GenderAge %>% filter(Gender == Gen_Input()) %>% group_by(Year) 
    %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365))
    (plot_ly(x = plotDeathsGender$Year, y = plotDeathsGender$rate, type = 'scatter', mode = 'lines', color = c("red")) %>% layout(xaxis = YearLabel,
      yaxis = RateLabel, title = "Annual Mortality by Drug & Alcohol Use", font=Font))
  })
  
  output$DrugVAlc_GenYear = renderPlotly({
    DrugValc_gender = as.data.table(filterGen())
    DrugValc_gender = DrugValc_gender %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100)
    (plot_ly(x = DrugValc_gender$DrugAlc_induced_causes, y = DrugValc_gender$rate, name = "Percentage of Drug & Alcohol Related Deaths",
      type = "bar", marker = list(color = c("#2874A6")), width = 1) 
      %>% layout(xaxis = CauseLabel, yaxis = PrctLabel, title = "Percentage of Drug & Alcohol Related Deaths", font = Font))
  })
  
  output$Age_GenYear = renderPlotly({
    Gender_age = as.data.table(filterGen())
    Gender_age = (Gender_age %>% group_by(Age_grps, DrugAlc_induced_causes) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    Gender_age = spread(data = Gender_age, key = DrugAlc_induced_causes, value = rate)
    (plot_ly(x = Gender_age$Age_grps, y = Gender_age$`Alcohol-Induced Causes`, name = "Alcohol-Induced", type = "bar", showlegend = TRUE,
      width = 1) %>% add_trace(y = Gender_age$`Drug-Induced Causes`, name = "Drug-Induced") 
      %>% layout(xaxis = AgeLabel, yaxis = PrctLabel, title = "Percentage of Deaths by Age Group", font = Font, barmode="stack"))
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
    %>% summarise(Percentage = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100))
    gvisGeoChart(states_year,"State", "Percentage",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=900, height=700,
                              colorAxis="{colors:['#FDEDEC', '#E74C3C']}"
                 ))     
  })
  
}

shinyApp(ui = ui, server = server)