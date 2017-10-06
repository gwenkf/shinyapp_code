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
    
    YearCoD = reactive({
      InputYear = input$slideYear
      edit_data = as.data.frame(raceCoDInput())
      edit_data = edit_data[Year == InputYear,]
    })
    
    raceCoDInput = reactive({
      buttonCause = input$CoDbutton
      if (buttonCause == "Drug-Induced") {
        return (droplevels(subset(GenderAge, DrugAlc_induced_causes == "Drug-Induced Causes")))
      } else {
        return (droplevels(subset(GenderAge, DrugAlc_induced_causes == "Alcohol-Induced Causes")))
      }
    })
    
    output$annualmortrate_Gender = renderPlot({
      plot3 = deaths_Gender
      (ggplot(data=deaths_Gender, aes(x=Year, y=annualrate, group=Gender, color=Gender)) + geom_point(size=1.5) + geom_line(size=1) 
        + scale_color_manual(values = c("#1c9099", "#e34a33"))
        + ylab("Death Rates") + theme_base() + ggtitle("Annual Mortality Rates by Gender, 1999-2015"))
    })
    
    output$annualmortrate_Age = renderPlot({
      edit_data = as.data.table(YearCoD())
      edit_data = edit_data[, .(DeathsPrct = ((sum(as.numeric(Deaths))/((sum(as.numeric(Population)))*1000))/365)*100), by = .(Year, Age_grps)]
      plot4 = as.data.frame(edit_data)
      (ggplot(data=edit_data, aes(x=Year, y=DeathsPrct, fill=Age_grps)) + geom_col(position = "dodge")
        + theme_base())
    })
    
    YearInput = reactive({
      year = input$selYear
      droplevels(subset(byST_annualrate, Year %in% year))
    })
    output$geochart = renderGvis({
      states_year <- as.data.frame(YearInput())
      gvisGeoChart(states_year,"State", "rate",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width=900, height=700,
                                colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                   ))     
    })
}