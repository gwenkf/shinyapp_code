library(shiny)
require(datasets)

function(input, output) {
    output$avgdeaths_cause = renderPlot({
      avg_totalCause = (Totals_cause %>% filter(DrugAlc_induced_causes == "Drug-Induced Causes"|DrugAlc_induced_causes == "Alcohol-Induced Causes")
                        %>% group_by(DrugAlc_induced_causes) %>% summarise(avgdeaths = sum(as.numeric(Deaths))))
      (ggplot(data=avg_totalCause, aes(x=DrugAlc_induced_causes, y=avgdeaths, label=avgdeaths)) 
       + geom_col(fill="#e34a33", width = .5) + theme_base()
       + geom_label() + ylab("Total Number of Deaths") + xlab("Cause of Death") 
       + ggtitle("Total Number of Drug & Alcohol Related Deaths in the USA, 1999-2015"))
    })
    
    output$annualmortrate = renderPlot({
      deathsTotalYr = Totals_cause %>% filter(DrugAlc_induced_causes == "Total") %>% group_by(Year) %>% summarise(rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)
      (ggplot(data=deathsTotalYr, aes(x=Year, y=rate, group=1)) + geom_line(color="#1c9099",size=1) 
        + geom_point(color="#1c9099", size=2, shape = 23, fill="#1c9099")
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
      (ggplot(data=edit_data, aes(x=Age_grps, y=rate, fill=Age_grps)) + geom_col(position = "dodge", width=.5)
        + theme_base() + scale_fill_brewer(palette = "Dark2"))
    })
    
    output$DrugVAlc_raceYear = renderPlot({
      plot4 = as.data.frame(filter_RaceYr())
      plot4 = plot4 %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = plot4, aes(x = DrugAlc_induced_causes, y = rate, fill = DrugAlc_induced_causes))
        + geom_col() + scale_fill_brewer(palette = "Dark2"))
    })
    
    output$AllDeaths_Race = renderPlot ({
      plot5 = as.data.frame(RaceAge)
      plot5 = plot5 %>% filter(Race == raceInput()) %>% group_by(Year) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = plot5, aes(x = Year, y = rate, group = 1)) + geom_point(size = 2, shape = 23, color = "#1c9099", fill="#1c9099") 
        + geom_line(color = "#1c9099") + theme_base()) 
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
      (ggplot(data=plotDeathsGender, aes(x=Year, y=rate, group=1)) + geom_point(size=2, shape=23, color="#1c9099", fill = "#1c9099") + geom_line(size=1, color="#1c9099") 
        + ylab("Death Rate") + theme_base() + ggtitle("Annual Mortality Rates by Gender, 1999-2015"))
    })
    
    output$DrugVAlc_GenYear = renderPlot({
      DrugValc_gender = as.data.table(filterGen())
      DrugValc_gender = DrugValc_gender %>% group_by(DrugAlc_induced_causes) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = DrugValc_gender, aes(x=DrugAlc_induced_causes, y=rate, fill=DrugAlc_induced_causes)) + geom_col() + theme_base()
        + scale_fill_brewer(palette = "Dark2"))
    })
    
    output$Age_GenYear = renderPlot({
      Gender_age = as.data.table(filterGen())
      Gender_age = Gender_age %>% group_by(Age_grps) %>% summarise(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population)))
      (ggplot(data = Gender_age, aes(x=Age_grps, y=rate, fill=Age_grps)) + geom_col() + theme_base() + scale_fill_brewer(palette= "Dark2"))
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
      states_year = states_year %>% filter(DrugAlc_induced_causes == CauseInput()) %>% group_by(State) %>% summarise(rate = ((sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365)*100)
      gvisGeoChart(states_year,"State", "rate",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width=900, height=700,
                                colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                   ))     
    })
}