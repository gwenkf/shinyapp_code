library(data.table)
library(ggplot2)
library(maps)
library(leaflet)
library(ggmap)
library(ggthemes)
library(googleVis)
library(treemap)
library(usmap)
library(tidyr)

#State files
setwd("/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/mortalitydata/")
RaceAge = read.csv("STdrugalcdeath_raceage1599.csv", stringsAsFactors = FALSE, header = FALSE, col.names = c("State","State_code","Year","Year_code", "Race","Age_grps","Age_grpsCode","DrugAlc_induced_causes","DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
RaceAge = RaceAge[-c(1),]
RaceAge = as.data.table(RaceAge)
RaceAge$Year = as.factor(RaceAge$Year)
RaceAge = RaceAge[Race != "Not Stated"&Race != "Not Hispanic or Latino", , ]


GenderAge = read.csv("STdrugalcdeath_genderage1599.csv", stringsAsFactors = FALSE, header = FALSE,
                     col.names = c("State","State_code","Year","Year_code", "Gender","Gender_code", "Age_grps","Age_grpsCode","DrugAlc_induced_causes","DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
GenderAge = GenderAge[-c(1),]
GenderAge = as.data.table(GenderAge)
GenderAge = na.omit(GenderAge)

RaceGender = read.csv("STdrugalcdeath_racegender1599.csv", stringsAsFactors = FALSE, header = FALSE, 
                      col.names = c("State","State_code","Year","Year_code", "Gender","Gender_code", "Race","DrugAlc_induced_causes","DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
RaceGender = RaceGender[-c(1),]
RaceGender = as.data.table(RaceGender)
RaceGender = na.omit(RaceGender)
RaceGender$Year = as.factor(RaceGender$Year)
RaceGender = RaceGender[Race != "Not Stated"&Race != "Not Hispanic or Latino", , ]

Totals_cause = read.csv("STdrugalcdeath_cause1599.csv", stringsAsFactors = FALSE, header = FALSE, 
                        col.names=c("State","State_code","Year","Year_code","DrugAlc_induced_causes","DrugAlc_induced_code","Deaths","Population","Crude_rate","Rate_LowerConfInt","Rate_UpperConfInt","Rate_StErr"))
Totals_cause = Totals_cause[-c(1),]
Totals_cause = as.data.table(Totals_cause)
Totals_cause$Year = as.factor(Totals_cause$Year)
na.omit(Totals_cause)

#GRAPHS
deathsTotalYr = Totals_cause[DrugAlc_induced_causes == "Total"&Year != "", .(avgdeaths = mean(sum(as.numeric(Deaths))),sumdeaths = sum(as.numeric(Deaths)), annual_rate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365), by = .(Year)]
na.omit(deathsTotalYr)
(ggplot(data=deathsTotalYr, aes(x=Year, y=annual_rate, group=1)) + geom_line(color="#1c9099",size=1) + geom_point(color="#1c9099", size=1.5)
  + theme_base() + ylab("Death Rate"))

avg_totalCause = Totals_cause[DrugAlc_induced_causes != "Total"&Year != "", .(avgdeaths=mean(sum(as.numeric(Deaths)))), by = DrugAlc_induced_causes]
(ggplot(data=avg_totalCause, aes(x=DrugAlc_induced_causes, y=avgdeaths, label=avgdeaths)) + geom_col(fill="#e34a33", width = .5) + theme_base()
  + geom_label() + ylab("Average Number of Deaths"))


deaths_Race = RaceGender[, .(annualrate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365), by = .(State, Year, Race)]
deaths_Race = na.omit(deaths_Race)
(ggplot(data=deaths_Race, aes(x=Year, y=rate, group=Race, color=Race)) + geom_point(size=1.5) + geom_line(size=1) + scale_color_brewer(palette = "Dark2") + ylab("Death Rate") + theme_base())

deaths_racetest = spread(deaths_Race, Race, annualrate, fill=NA)


Line1 <- gvisLineChart(deaths_racetest, xvar="Year", yvar=c("American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American", "Hispanic or Latino", "White"))
plot(Line1)

deaths_Gender = RaceGender[, .(sumdeaths = sum(as.numeric(Deaths)), annualrate = (sum(as.numeric(Deaths))/(sum(as.numeric(Population)))*1000)/365), by = .(Year, Gender)][order(-Year)]
(ggplot(data=deaths_Gender, aes(x=Year, y=annualrate, group=Gender, color=Gender)) + geom_point(size=1.5) + geom_line(size=1)
  + scale_color_manual(values = c("#de2d26", "#fc9272")) + ylab("Death Rates"))

states = Totals_cause[ , .(State), by = State]
state_names = states$State

#MAP
byST = RaceGender[, .(rate = sum(as.numeric(Deaths))/sum(as.numeric(Population))), by = .(State,Year)]
byST2015$State <- tolower(byST2015$State)
byST$Year = as.numeric(byST$Year)

require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(byST, "State", "rate",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)
