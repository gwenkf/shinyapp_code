library(shiny)

function(input, output) {

    output$avgYrRace = renderPlot({
    yearRange = seq(input$selYear[1], input$selYear[2])
    plot2 = avg_deathsCause[which(avg_deathsCause$Year %in% yearRange),]
    (ggplot(data=plot2, aes(x=Year, y=avg, group=DrugAlc_induced_causes, color=DrugAlc_induced_causes)) 
      + geom_point(size=1) + geom_line(size=.5) 
      + scale_color_brewer(palette = "Dark2", labels = c("Alcohol-Induced", "Drug-Induced"), guide_legend(title="Cause of Death")) + theme_base() + ylab("Average Deaths Per Year"))
  })
}