library(shiny)
require(datasets)

function(input, output) {
    output$avgdeaths_cause = renderPlot({
      plot1 = avg_totalCause
      (ggplot(data=plot1, aes(x=DrugAlc_induced_causes, y=avgdeaths, label=avgdeaths)) 
       + geom_col(fill="#e34a33", width = .5) + theme_base()
       + geom_label() + ylab("Average Number of Deaths") + xlab("Cause of Death") 
       + ggtitle("Average Number of Drug & Alcohol Related Deaths in the USA, 1999-2015"))
    })
    
    output$annualmortrate = renderPlot({
      plot2 = deathsTotalYr
      (ggplot(data=deathsTotalYr, aes(x=Year, y=annual_rate, group=1)) + geom_line(color="#1c9099",size=1) 
        + geom_point(color="#1c9099", size=1.5)
        + theme_base() + ylab("Death Rate") + ggtitle("Annual Mortality by Drug and Alcohol Use, 199-2015"))
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
    
    output$annualmortrate_Age = renderPlot({
      edit_data = as.data.frame(filter_RaceYr())
      edit_data = edit_data %>% group_by(Age_grps) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data=edit_data, aes(x=Age_grps, y=rate, fill=Age_grps)) + geom_col(position = "dodge")
        + theme_base())
    })
    
    output$DrugVAlc_raceYear = renderPlot({
      plot4 = as.data.frame(filter_RaceYr())
      plot4 = plot4 %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = plot4, aes(x = DrugAlc_induced_causes, y = rate, fill = DrugAlc_induced_causes))
        + geom_col())
    })
    
    output$AllDeaths_Race = renderPlot ({
      plot5 = as.data.frame(RaceAge)
      plot5 = plot5 %>% filter(Race == raceInput()) %>% group_by(Year) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = plot5, aes(x = Year, y = rate, group = 1)) + geom_point() + geom_line() + theme_base()) 
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
      plotDeathsGender = GenderAge %>% filter(Gender == Gen_Input()) %>% group_by(Year) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data=plotDeathsGender, aes(x=Year, y=rate, group=1)) + geom_point(size=1.5) + geom_line(size=1) 
        + ylab("Death Rate") + theme_base() + ggtitle("Annual Mortality Rates by Gender, 1999-2015"))
    })
    
    output$DrugVAlc_GenYear = renderPlot({
      DrugValc_gender = as.data.table(filterGen())
      DrugValc_gender = DrugValc_gender %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = DrugValc_gender, aes(x=DrugAlc_induced_causes, y=rate, fill=DrugAlc_induced_causes)) + geom_col() + theme_base())
    })
    
    output$Age_GenYear = renderPlot({
      Gender_age = as.data.table(filterGen())
      Gender_age = Gender_age %>% group_by(Age_grps) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = Gender_age, aes(x=Age_grps, y=rate, fill=Age_grps)) + geom_col() + theme_base())
    })
    
    YearInput = reactive({
      year = input$selYear
      Totals_cause %>% filter(Year == year)
    })
    
    output$geochart = renderGvis({
      states_year = as.data.frame(YearInput())
      states_year = states_year %>% group_by(State) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      gvisGeoChart(states_year,"State", "rate",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width=900, height=700,
                                colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                   ))     
    })
}