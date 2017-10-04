library(data.table)
library(ggplot2)
library(maps)
library(leaflet)
library(ggmap)
library(ggthemes)
library(googleVis)
library(treemap)

#loading files and cleaning
setwd("/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/mortalitydata/")
files = list.files(pattern="drugalcoholdeath*")
# First apply read.csv, then rbind
handle_each <- function(x){
  year <- as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))
  temp <- read.csv(x, header = FALSE,  col.names = c("County","County_code","Year", "Year_code", "Gender", "Gender_code", "Race", "DrugAlc_induced_causes", "DrugAlc_causes_code", "Deaths", "Population", "Crude_rate", "Crude_rate_lowerConfInt", "Crude_rate_upperConfInt", "Crude_rate_StErr"))
  temp$year <- year
  return(temp)
}
drug_alc_deaths = do.call(rbind, lapply(files, handle_each))
which(drug_alc_deaths$County == "County")
drug_alc_deaths = drug_alc_deaths[-c(1,862,1744,2699,3754,4883,6060,7305,8668,10055,11513,12955,14438,16091,17759,19494),]
drug_alc_deaths = as.data.table(drug_alc_deaths, keeprows= TRUE)

deaths_hispAge = read.csv("drugalcdeath_hispage1599.csv", header = FALSE, col.names = c("County",	"County_code", 	"Year",	"Year_code",	"Race",	"TenYr_AgeGrps",	"AgeGrps_code",	"DrugAlc_induced_causes",	"DrugAlc_casuses_code",	"Deaths",	"Population",	"Crude_rate",	"Crude_rate_lowerConfInt",	"Crude_rate_upperConfInt", "Crude_rate_StErr"))
deaths_hispAge = deaths_hispAge[-c(1),]

deaths_raceAge = read.csv("drugalcdeath_raceage1599.csv", header = FALSE, col.names = c("County",	"County_code", 	"Year",	"Year_code",	"Race",	"TenYr_AgeGrps",	"AgeGrps_code",	"DrugAlc_induced_causes",	"DrugAlc_casuses_code",	"Deaths",	"Population",	"Crude_rate",	"Crude_rate_lowerConfInt",	"Crude_rate_upperConfInt", "Crude_rate_StErr"))
deaths_raceAge = deaths_raceAge[-c(1),]

deaths_ALLraceAge = rbind(deaths_raceAge,deaths_hispAge)
deaths_ALLraceAge = as.data.table(deaths_ALLraceAge)

#Analsysis
drugalc_deathsYR = (deaths_ALLraceAge[Year==1999|Year==2000|Year==2001|Year==2002|Year==2003|Year==2004|Year==2005|Year==2006|Year==2007
                                   |Year==2008|Year==2009|Year==2010|Year==2011|Year==2012|Year==2013
                                   |Year==2014|Year == 2015, , ])

drugValc_us = drugalc_deathsYR[ , .(sumdeaths = sum(as.numeric(Deaths)), rate = (sum(as.numeric(Deaths)))/(sum(as.numeric(Population)))), by = .(Year,DrugAlc_induced_causes)]
(ggplot(drugValc_us, aes(x=Year, y=sumdeaths, fill=DrugAlc_induced_causes)) + geom_col(color="black") + theme_base() + scale_fill_brewer(palette = "Set1", labels = c("Alcohol-Induced", "Drug-Induced"))
  + ylab("Total Deaths") + guides(fill=guide_legend(title = "Cause of Death")))

(ggplot(drugValc_us, aes(x=Year, y=rate, group=DrugAlc_induced_causes, color=DrugAlc_induced_causes)) + geom_point(size=1.5) + geom_line(size=1) + theme_base()
  + ylab("Death Rate") + scale_color_brewer(palette = "Dark2", labels = c("Alcohol-Induced", "Drug-Induced"),guide_legend(title="Cause of Death")))

deaths_Race = deaths_ALLraceAge[ , .(sumdeaths = sum(as.numeric(Deaths)), rate = (sum(as.numeric(Deaths)))/(sum(as.numeric(Population)))), by = .(Year, Race, DrugAlc_induced_causes)][order(-Year)]
(ggplot(data=deaths_Race, aes(x=Year, y=rate, group=Race, color=Race)) + geom_point(size=1.5) + geom_line(size=1) + scale_color_brewer(palette = "Dark2") + ylab("Death Rate") + theme_base())

deaths_age = deaths_ALLraceAge[ , .(sumdeaths = sum(as.numeric(Deaths)), rate = (sum(as.numeric(Deaths)))/(sum(as.numeric(Population)))), by = .(Year, TenYr_AgeGrps)][order(-Year)]
(ggplot(data=deaths_age,aes(x=Year, y=rate, group=TenYr_AgeGrps, color=TenYr_AgeGrps)) + geom_point() + geom_line())

avg_deathsYr = deaths_ALLraceAge[ , .(avg = mean(as.numeric(Deaths))), by = .(Year,Race)]
(ggplot(data=avg_deathsYr, aes(x=Year, y=avg, group=Race, color=Race)) + geom_point(size=1.5) + geom_line(size=1) + theme_base() +
      scale_color_brewer(palette = "Dark2") + ylab("Average Deaths Per Year"))

avg_deathsCause = deaths_ALLraceAge[ , .(avg = mean(as.numeric(Deaths))), by = .(Year, DrugAlc_induced_causes)]
(ggplot(data=avg_deathsCause, aes(x=Year, y=avg, group=DrugAlc_induced_causes, color=DrugAlc_induced_causes)) 
        + geom_point(size=1) + geom_line(size=.5) 
        + scale_color_brewer(palette = "Dark2", labels = c("Alcohol-Induced", "Drug-Induced"), guide_legend(title="Cause of Death")) + theme_base() + ylab("Average Deaths Per Year"))


tree_race15 = deaths_ALLraceAge[Year == 2015, .(DeathsN = sum(as.numeric(Deaths))), by = .(Race,DrugAlc_induced_causes)]
treemap(tree_race15, index = c("DrugAlc_induced_causes","Race" ,"DeathsN"), vSize = c("DeathsN"), vColor = c("DrugAlc_induced_causes"), title="Proportion of Drug and Alcohol Induced Deaths by Race/Ethnicity")


#MAP
library(rgdal) 
setwd("/Users/gfern/Downloads")

counties = readOGR(dsn="cb_2015_us_county_5m", layer="cb_2015_us_county_5m")
plot(counties)

deathrates2015 = drug_alc_deaths[Year == 2015, .(County_code, Deaths), ]
deathrates2015p = as.data.frame(deathrates2015)

county_map = map("county")
fips_codes = 
  
library(choroplethr)
library(choroplethrMaps)
deathrates2015$value = deathrates2015$Deaths
county.fips



 data(countyMapEnv)
View(countyMapEnv)
us_county = map("county")
state = map("state", fill=TRUE, plot=FALSE)
leaflet(state) %>% addPolygons()

ggplot() + geom_map(data=, map=usaMapEnv, aes(x=long, y=lat))
head(county.fips)

leaflet(us_county, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>% addPolylines(color = "black",weight=1)

county_choropleth(deaths_county)
