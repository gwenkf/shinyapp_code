library(shiny)
require(datasets)

function(input, output) {
    output$avgdeaths_cause = renderPlot({
      plot1 = avg_totalCause
      (ggplot(data=plot1, aes(x=DrugAlc_induced_causes, y=avgdeaths, label=avgdeaths)) 
       + geom_col(fill="#e34a33", width = .5) + theme_base()
       + geom_label() + ylab("Average Number of Deaths") + xlab("Cause of Death"))
    })
    
    output$annualmortrate = renderPlot({
      plot2 = deathsTotalYr
      (ggplot(data=deathsTotalYr, aes(x=Year, y=annual_rate, group=1)) + geom_line(color="#1c9099",size=1) 
        + geom_point(color="#1c9099", size=1.5)
        + theme_base() + ylab("Death Rate"))
    })
    
    output$annualmortrate_Gender = renderPlot({
      plot3 = deaths_Gender
      (ggplot(data=deaths_Gender, aes(x=Year, y=annualrate, group=Gender, color=Gender)) + geom_point(size=1.5) + geom_line(size=1) 
        + scale_color_manual(values = c("#1c9099", "#e34a33"))
        + ylab("Death Rates") + theme_base())
    })
    
    myYear <- reactive({
      input$selYear
    })
    output$year <- renderText({
      paste("Democratic share of the presidential vote in", myYear())
    })
    output$geochart <- renderGvis({
      myData <- byST[Year == myYear, , ]
      gvisGeoChart(myData,
                   "State", "rate",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width=600, height=400,
                                colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                   ))     
    })
    dataStatesInput = reactive({
      chStates = input$checkStates
      droplevels(subset(deaths_racetest, State %in% chStates))
      
    })
    output$statesRates = renderGvis({
      state_data = as.data.frame(dataStatesInput())
      linechrt = gvisLineChart(state_data, xvar="Year")
      plot(linechrt)
    })
}