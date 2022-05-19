---
title: "**Visualisation of Time Series Data**"
author: "William D'Alessandro"
runtime: shiny
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true 
    number_sections: true
---
```{r, echo = FALSE, include=FALSE}
##Setting up required packages, loading the data in
library(vroom)
library(tidyverse)
library(wbstats)
library(countrycode)
library(scales)
library(ggpmisc)
library(plotly)
library(ggpubr)
library(reshape2)
library(rnaturalearthdata)
library(data.table) 
library(rnaturalearth)
library(RColorBrewer)
library(sf)
library(ggvis)
library(shiny)
library(rgeos)

Covid19DeathsTotal <-  vroom("https://raw.githubusercontent.com/Williamd40/DataForRWorkshops/main/Workshops/Workshop4/time_series_covid19_deaths_global.csv ")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
```



# Covid Data Analysis:

## Initial Visualisation

```{r, include = FALSE}
# First I wanted to see the raw data and get an idea of how to transform it into something use-able
Covid19DeathsTotal
```
```{r, include=FALSE}
# From this I can infer that the data is in wide format, so I need to convert to long using:

## I need to assign the long version of my data to a new variable, so I am able to still have access to the original files


Covid19DeathsTotalLong <- Covid19DeathsTotal %>%
  
  ## I am only interested in the columns that give me useful information
  ## Therefore I can omit the columns Lat and Long for now
  ## I took the values and names of then columns PAST the column 'Long'
  pivot_longer(cols = -c("Province/State":Long),
               names_to = "Date",
               values_to = "Deaths")  %>%
  select(c('Country/Region',Date, Deaths))

## Correcting the format of the date column
Covid19DeathsTotalLong$Date_Corrected <- as.Date(Covid19DeathsTotalLong$Date, format = "%m/%d/%y") 

## Removing the original Date column for ease of looking
Covid19DeathsTotalLong <- select(Covid19DeathsTotalLong, -c(Date))

## Changing column one to a more use-able format
names(Covid19DeathsTotalLong)[1] <- "Country_Region"


## Adding the country codes so I can group the data into more manageable sets
Covid19DeathsTotalLong$code <-  countrycode(Covid19DeathsTotalLong$Country_Region, 
                                      origin = "country.name", 
                                      destination = "iso3c")



```











```{r, include = FALSE}
## To get the total deaths per country
CovidCountryData <- Covid19DeathsTotalLong %>% 
  
  ## This tells R to group data based on the values in the *code*  column
  group_by(code) %>% 
  
  ##  I then want the total deaths of each group
  summarise(Deaths = max(Deaths))

## I then wanted to sort the total deaths so I can analyse and compare the highest five and lowest five.
CovidCountryDataSorted <- CovidCountryData[order(-CovidCountryData$Deaths),]

## This code is redundant, however I left it as an example for myself.
# ## To get the total deaths in the UK
# CovidCountryDataUK <- Covid19DeathsTotalLong %>% 
#   
#   ## This tells R to filter all rows that contain "GBR"
#   filter(code == "GBR")



```

















```{r, include = FALSE}

## This was simply me being curious to see how many countries were in this data, 188 to be exact.
NumberOfCountries <- (unique(Covid19DeathsTotalLong$Country_Region))


```










This graph is plotted as each countries increase in deaths by covid over the time frame within the data frame. The graph is messy, but I can see between April and March that the initial peak began.
```{r, echo=FALSE}

##make the ggplot object
FirstGraphicalAnalysis <- ggplot(data = Covid19DeathsTotalLong, aes(x = Date_Corrected, y = Deaths, "FirstGraphicalAnalysis")) + geom_point(aes(col = Country_Region)) + theme(legend.position = "none")+ ggtitle ("Graphical Analysis of Covid Time Stamp Data, for the whole duration")+  labs(y = "Total Deaths", x = "Date")

FirstGraphicalAnalysis
```




This graph shows the period of 2020-03-20 to 2020-06-01, which is the initial spike in covid deaths.
<br> I can see around 10 countries that had substantially higher death totals then other countries, so I wanted to extract these.
<br> Before that however I wanted to see how the initial spikes looked for all the countries.
```{r, echo=FALSE,message=FALSE,warning=FALSE}

## Plotting a second graph of where the initial peak began

SecondGraphicalAnalysis <- ggplot(data = Covid19DeathsTotalLong, aes(x = Date_Corrected, y = Deaths, "SecondGraphicalAnalysis"))
SecondGraphicalAnalysis + geom_point(aes(col = Country_Region)) + theme(legend.position = "none")+ ggtitle ("Graphical Analysis of Covid Time Stamp Data, for the Initial Peak" ) +                                       
  scale_x_date(limits = as.Date(c("2020-03-20", "2020-06-01"))) +  labs(y = "Total Deaths", x = "Date")

```

From this graph I can see that a few countries had a far quicker initial death rate climb then others; this could be due to a range of reasons, such as but not limited to: 
<br> - [Population size](#POPULATION_HEADER)
<br> - [GDP](#GDP_HEADER)
<br> - Climatic Conditions
<br>
Since I am a resident within the UK, I was especially interested in the stats relating to GBR. I therefore have included a section regarding these figures.
<br> - [UK stats](#UK_HEAD)


<br> From what the graphs showed, I next wanted to investigate which countries where the most affected by Covid.
<br> The highest death total countries:
```{r, echo=FALSE}

##The top ten highest death total countries
HighestDeathTotalCountries <- head(CovidCountryDataSorted, n=10)

##Setting the order of the top ten, so the bar charts make sense later.
HighestDeathTotalCountries$code <- factor(HighestDeathTotalCountries$code, levels = HighestDeathTotalCountries$code[order(HighestDeathTotalCountries$Deaths)])

ThirdGraphicalAnalysis <- ggplot(HighestDeathTotalCountries, aes(code, Deaths, fill = code)) + geom_bar(stat="identity") + ggtitle ("Top 10 Death Totals" ) + coord_flip() +  labs(y = "Total Deaths", x = "Country Code")
                                                                        
ThirdGraphicalAnalysis 




```


```{r, include=FALSE}
## The lowest death total countries:
##The lowest five highest death total countries, just for curiosity
LowestDeathTotalCountries <- tail(CovidCountryDataSorted, n=5)
LowestDeathTotalCountries
```






<br> After seeing these graphs, I wanted to explore what possible reasons there were that caused the certain countries to experience higher death tolls.

 ---
 
## Population size {#POPULATION_HEADER}

```{r, include = FALSE}
## For my interest into the population data, I pulled the populations from 2019-2020 from the package wbstats, converted this to a tibble. FIND OUT WHAT INDICATOR IS. I select the date range that covered the entirety of covid up to the present day
PopulationData <- as_tibble(wb_data(indicator = "SP.POP.TOTL", 
                    start_date = 2017, 
                    end_date = 2022))%>%
  select(c(date,iso3c, SP.POP.TOTL))

PopulationDataFor2021 <- PopulationData %>% 
  
  ##I only want to return data where the date is equal to the maximum value in the column "date", which is 2020.
  filter(date == max(PopulationData$date))



## All countries and all deaths, left join to only keep values present in OUR table. Selecting rows to keep based on code, saying that code = iso3c
HighestDeathTotalCountriesWithPopulationALL<- left_join(CovidCountryData, 
                         PopulationDataFor2021 %>% select(iso3c, "SP.POP.TOTL"),
                         by = c("code" = "iso3c"))



## Same as above, but using a previously defined n = 10 variant of the highest 10 countries
HighestDeathTotalCountriesWithPopulation <- left_join(HighestDeathTotalCountries, 
                         PopulationDataFor2021 %>% select(iso3c, "SP.POP.TOTL"),
                         by = c("code" = "iso3c"))



LowestDeathTotalCountriesWithPopulation <- left_join(LowestDeathTotalCountries, 
                         PopulationDataFor2021 %>% select(iso3c, "SP.POP.TOTL"),
                         by = c("code" = "iso3c"))


## Sorting the data so that the bar charts look better and are easier to read
HighestDeathTotalCountriesWithPopulationSorted <- HighestDeathTotalCountriesWithPopulation[order(-HighestDeathTotalCountriesWithPopulation$SP.POP.TOTL),]

## Logging the death count for ease of reading the graph
HighestDeathTotalCountriesWithPopulationSorted$"DeathToPopulationRatio(-log10)" <- -log10(HighestDeathTotalCountriesWithPopulationSorted$Deaths / HighestDeathTotalCountriesWithPopulationSorted$SP.POP.TOTL)

```
Here I have a table containing countries recent populations in 2020 and their covid death totals, as this is the most recent available data. From highest to lowest. This is the pattern I would expect to see the death totals in, with the largest countries having the highest deaths. This pattern however was not seen. This table also shows the death:population ratio. It should be noted that these figures are out of date now, with non-credible sources estimating that India alone has nearly four-million deaths. 

```{r, echo = FALSE}
HighestDeathTotalCountriesWithPopulationSorted
```

```{r, echo = FALSE}

## Setting the order of the table to be based off population, else ggplot makes it alphabetical
HighestDeathTotalCountriesWithPopulationSorted$code <- factor(HighestDeathTotalCountriesWithPopulationSorted$code, levels = HighestDeathTotalCountriesWithPopulationSorted$code[order(HighestDeathTotalCountriesWithPopulationSorted$SP.POP.TOTL)])



ThirdGraphicalAnalysisWithTotalPopulation <- ggplot(HighestDeathTotalCountriesWithPopulationSorted, aes(SP.POP.TOTL,code,fill = code)) + geom_bar(stat="identity") +  ggtitle ("Top 10 Countries and their total Population at the Time" ) + labs(x = "Total Population", y = "Country Code")

ThirdGraphicalAnalysisWithTotalPopulation

```

From viewing the data, I can see that while India has the highest population, but it doesn't have the highest death total, which is unexpected. This could be due to the under-reporting of death cases, as a result of the high case surge that was experienced within India.

<br> For a side by side comparison of the countries death totals and their total populations, see below:
```{r,echo=FALSE,message=FALSE,warning=FALSE}

## Black and white variants of thew previouslyt made graphs
ThirdGraphicalAnalysisWithTotalPopulationBW <- ggplot(HighestDeathTotalCountriesWithPopulationSorted, aes(SP.POP.TOTL,code)) + geom_bar(stat="identity") +  ggtitle ("Top 10 Countries and their total Population at the Time" ) + labs(x = "Total Population", y = "Country Code")
ThirdGraphicalAnalysisBW <- ggplot(HighestDeathTotalCountries, aes(code, Deaths)) + geom_bar(stat="identity") + ggtitle ("Top 10 Death Totals" ) + coord_flip() +  labs(y = "Total Deaths", x = "Country Code")


## Combining these together, I need to work on understanding faceting.
FigureOne <- ggarrange(ThirdGraphicalAnalysisWithTotalPopulationBW,ThirdGraphicalAnalysisBW,
                    ncol = 1, nrow = 2)
FigureOne

```

Finally I wanted to see the overall correlation between population size and death totals:
```{r, include = FALSE,message=FALSE,warning=FALSE}

##QUESTION: Is there an easier way to plot a y~x and pull the equation? <<<<<<<<<<<<<<<<<<<<<<<<<<


formulaDeathTotalVSPopulation <- lm(SP.POP.TOTL ~ Deaths, data = HighestDeathTotalCountriesWithPopulationALL)

summary(formulaDeathTotalVSPopulation)


## Setting the equation for plotting the line of best fit
formula <-  y~x
## Using the package ggpmisc, I can plot a line of best fit and pull the equation for deaths vs total population
DeathTotalVSPopulation <- ggplot(HighestDeathTotalCountriesWithPopulationALL, aes(Deaths, SP.POP.TOTL)) + ggtitle ("Top 10 Death Totals" ) + coord_flip() +  labs(y = "Total Population", x = "Deaths") +    geom_smooth(method = "lm", se=FALSE, color="black", formula = formula) +
   stat_poly_eq(formula = formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE, label.x = 1, label.y = 1) +         
   geom_point()

summary(formulaDeathTotalVSPopulation)
DeathTotalVSPopulation



```
```{r,echo=FALSE,message=FALSE,warning=FALSE}
DeathTotalVSPopulation
```

<br> The value of R^2 being 0.08 shows that there should be no direct correlation between population size and death total.



















 ---
 

## GDP {#GDP_HEADER}
```{r, include=FALSE}
## This code is extracting the GDP for the top 10 countries I am investigating
my_indicators = c("pop" = "SP.POP.TOTL",
                  "gdp" = "NY.GDP.MKTP.CD")

GDPInformationFull <- wb_data(my_indicators, start_date = 2020, end_date = 2020)


CountriesToChoose <- c(HighestDeathTotalCountriesWithPopulationSorted[,1])

GDPInformationFullNeededInfomation <-  GDPInformationFull %>%
  select(iso3c,gdp) 


HighestDeathTotalCountriesWithPopulationAndGDP <- left_join(CovidCountryData, 
                         PopulationDataFor2021 %>% select(iso3c, "SP.POP.TOTL"),
                         by = c("code" = "iso3c"))



HighestDeathTotalCountriesWithGDPandPop <- left_join(HighestDeathTotalCountriesWithPopulation, 
                         GDPInformationFullNeededInfomation %>% select(iso3c, "gdp"),
                         by = c("code" = "iso3c"))


## Saving a copy of all countries gdp with their deaths
HighestDeathTotalCountriesWithPopulationAndGDPAll <- left_join(CovidCountryData, 
                         GDPInformationFullNeededInfomation %>% select(iso3c, "gdp"),
                         by = c("code" = "iso3c"))



HighestDeathTotalCountriesWithGDPandPop$GDP_per_Person <- HighestDeathTotalCountriesWithGDPandPop$gdp / HighestDeathTotalCountriesWithGDPandPop$SP.POP.TOTL

HighestDeathTotalCountriesWithGDPandPopSorted <- HighestDeathTotalCountriesWithGDPandPop[order(-HighestDeathTotalCountriesWithGDPandPop$GDP_per_Person),]


                                  
```

```{r, include=FALSE}
# Here is a table of the previously mentioned countries, and their respective GDP in 2020:
HighestDeathTotalCountriesWithGDPandPopSorted
```

```{r, echo=FALSE}


HighestDeathTotalCountriesWithGDPandPopSorted$code <- factor(HighestDeathTotalCountriesWithGDPandPopSorted$code, levels = HighestDeathTotalCountriesWithGDPandPopSorted$code[order(HighestDeathTotalCountriesWithGDPandPopSorted$gdp)])


FourthGraphicalAnalysisWithTotalPopulation <- ggplot(HighestDeathTotalCountriesWithGDPandPopSorted, aes(code,gdp,fill = code)) + coord_flip()+  labs(x = "Total Population", y = "Total GDP") +ggtitle ("Top 10 Countries and their total GDP at the Time" ) + geom_bar(stat="identity")
                                                                        
FourthGraphicalAnalysisWithTotalPopulation 


```
<br> From this graph I can infer that while the USA had the highest deaths, it also has the largest GDP out of all the countries. This *should* imply that it would have the best healthcare, but from the previous graphs this statement isn't supported.

This was further confirmed by examining the average GDP (log10) per person for each country:

```{r, echo=FALSE}

HighestDeathTotalCountriesWithGDPandPop$code <- factor(HighestDeathTotalCountriesWithGDPandPop$code, levels = HighestDeathTotalCountriesWithGDPandPop$code[order(HighestDeathTotalCountriesWithGDPandPop$GDP_per_Person)])






FifthGraphicalAnalysisWithTotalPopulation <- ggplot(HighestDeathTotalCountriesWithGDPandPop, aes(code,GDP_per_Person,scale_x_log10(), fill = code )) + geom_bar(stat="identity") + coord_flip() + ggtitle ("Top 10 Countries and their GDP per Person" ) + labs(x= "Country Code", y = "GDP per person")

FifthGraphicalAnalysisWithTotalPopulation 
```




This graph shows how death and GDP correlates. The low R^2 values of 0.52 however shows that only 50% of our variance is explained, but this is far higher then the previous graph. This shows that, as expected, *generally* higher GDP countries faired better against covid.
<br>

```{r, echo = FALSE,message=FALSE,warning=FALSE}
#HighestDeathTotalCountriesWithPopulationAndGDPAll

#formulaDeathTotalVSPopulation <- lm((HighestDeathTotalCountriesWithPopulationAndGDPAll$Deaths)~(HighestDeathTotalCountriesWithPopulationAndGDPAll$gdp))

formula <- y~x

DeathTotalVSPopulation <- ggplot(HighestDeathTotalCountriesWithPopulationAndGDPAll, aes(Deaths, gdp)) + ggtitle ("" ) + coord_flip() +  labs(x = "Deaths", y = "GDP") +    geom_smooth(method = "lm", se=FALSE, color="black", formula = formula) +
   stat_poly_eq(formula = formula, 
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                parse = TRUE, label.x = 0.97, label.y = 0.07) +         
   geom_point() 

DeathTotalVSPopulation
```





---
# UK Statistics {#UK_HEAD}


Next, I was curious to see how the current covid-19 data looked like for the UK. Graphs made use data taken from the current [government covid-19 death toll](https://coronavirus.data.gov.uk/details/deaths).

## Death Trend
<br> 

```{r, echo=FALSE,message=FALSE,warning=FALSE}
## Up to date covid death data for the UK, from https://coronavirus.data.gov.uk/details/deaths
CovidCountryDataUKForDeaths = vroom("https://raw.githubusercontent.com/Williamd40/DataForRWorkshops/main/Workshops/Workshop4/overview_2021-10-18%20(1).csv")

KeepingColumns <- c("date","cumDeaths28DaysByDeathDate","areaName")
CovidCountryDataUKForDeaths <-  CovidCountryDataUKForDeaths[KeepingColumns]
CovidCountryDataUKForDeaths$Date_Corrected <- as.Date(CovidCountryDataUKForDeaths$date, format = "%y/%m/%d") 


##make the ggplot object
UKDeathTrend <- ggplot(data = CovidCountryDataUKForDeaths, aes(Date_Corrected, cumDeaths28DaysByDeathDate, "UKDeathTrend")) + geom_point(aes(col = cumDeaths28DaysByDeathDate)) + ggtitle ("Cumulative Deaths in the UK")+  labs(y = "Total Deaths", x = "Date") + theme(legend.position = "none")

## This adds the question of the line
#+   geom_smooth(method = "lm", se=FALSE, color="black") formula = formula) +
   #stat_poly_eq(formula = formula, 
               # aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                #parse = TRUE) +         
  # geom_point()

UKDeathTrend
```



## Daily Hospital Admissions and Deaths

This graph shows how the amount of daily hospital admissions correlates to the daily death total. For those who may have difficulty distinguishing the colours used, the lower line is Daily Deaths and the higher line is Hospital Cases.

```{r, echo=FALSE,message=FALSE,warning=FALSE}

KeepingColumns <- c("hospitalCases","newDailyNsoDeathsByDeathDate","Date_Corrected")
UKHospitalAndDeathDailyData <-  vroom("https://raw.githubusercontent.com/Williamd40/DataForRWorkshops/main/Workshops/Workshop4/HospitalAndDailyDeathCovidData.csv")




UKHospitalAndDeathDailyData$Date_Corrected <- as.Date(UKHospitalAndDeathDailyData$date, format =  "%d/%m/%y")

UKHospitalAndDeathDailyData <-  UKHospitalAndDeathDailyData[KeepingColumns]
colnames(UKHospitalAndDeathDailyData)[2] <- "Daily_Deaths"




## https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2.html

HospitalCasesDeathsDailyGraph <- ggplot(UKHospitalAndDeathDailyData, aes(x = Date_Corrected)) + 
  geom_line(aes(y = Daily_Deaths, colour = "Daily Deaths")) + 
  geom_line(aes(y = hospitalCases, colour = "Hospital Cases")) + ggtitle ("Daily Hospital Admissions vs Daily Deaths")+  labs(y = "Daily Deaths", x = "Date") + theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    ) + theme(legend.title = element_text(face = "bold"),legend.text = element_text(size = 8, colour = "purple"))


HospitalCasesDeathsDailyGraph

```


## Last 28 days Deaths

This graph is taken from constantly updating covid 19 death data for the last 28 days, hover over a point to find out information.



```{r,echo=FALSE,message=FALSE,warning=FALSE}




Last28daysDeaths <- vroom("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv")



Last28daysDeaths$Date_Corrected <- as.Date(Last28daysDeaths$date, format =  "%d-%m-%y")

## Okay I'm going to attempt to break this down, it is 1 am and I haven't slept. Let's do this.


##this bit is responsible for embedding the graph into a shiny output, so the hovering points actually work
##links with bind_shiny("Last28daysDeathsPlot"), makes the graph intractable
## I believe this create the User Interface (UI), that is the actual interactive piece

ui <- fluidPage(
  ggvisOutput("Last28daysDeathsPlot")
)

##Places all the graph information into Last28daysDeathsPlotActive, acts as the server for shiny

Last28daysDeathsPlotActive <- function(input, output) 
  
  ## Sets x and y values
  ggvis(Last28daysDeaths, ~Date_Corrected, ~newDeaths28DaysByDeathDate) %>% 
  
  ## Adds points to appear on the graph
  layer_points() %>%
  
  ## Adds the X axis title
    add_axis("x", title = "Date", orient = "bottom") %>%
  
   ## Adds the y axis title
    add_axis("y", title = "Daily Death Count") %>%
  
  ## Adds the line to appear on the graph
    layer_lines() %>% 
  
  ## Adds the actual output when you hover over a point
    add_tooltip(function(Last28daysDeaths) { paste0("Death Count: ",Last28daysDeaths$newDeaths28DaysByDeathDate) }) %>%
  
  ## Binds everything to shiny
    bind_shiny("Last28daysDeathsPlot")


shinyApp(ui = ui, server = Last28daysDeathsPlotActive)


```


## All Time hospital Death:Recovered Ratio

```{r,echo=FALSE,message=FALSE,warning=FALSE}

TotalHospitalCasesAndDeaths <-   vroom("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumAdmissions&metric=cumDailyNsoDeathsByDeathDate&format=csv")

TotalHospitalCasesAndDeaths <- na.omit(TotalHospitalCasesAndDeaths)

TotalAdmissionsCasesSum <- max(TotalHospitalCasesAndDeaths$cumAdmissions, na.rm = TRUE)

TotalDeathsSum <- max(TotalHospitalCasesAndDeaths$cumDailyNsoDeathsByDeathDate, na.rm = TRUE)

DeathRate <- (TotalDeathsSum/TotalAdmissionsCasesSum)*100

print(DeathRate)




```






# World map Covid Deaths

World heat-map of Covid Deaths

```{r,include=FALSE,message=FALSE,warning=FALSE}



## https://ourworldindata.org/covid-deaths
WorldInformation <-   vroom("https://raw.githubusercontent.com/Williamd40/DataForRWorkshops/main/Workshops/Workshop4/WorldCovidData.csv")



# extract world data
world <- ne_countries(scale = "medium", returnclass = "sf")

KeepingColumns <-  c("iso_code","total_deaths")

WorldInformationColumnsForMap <- WorldInformation[KeepingColumns]

CountriesAndMaxDeaths <- setDT(WorldInformationColumnsForMap)[ , .SD[which.max(total_deaths)], by = iso_code ]  

names(CountriesAndMaxDeaths)[1] <-  "iso_a3"



WorldDataWithCovid <- merge(CountriesAndMaxDeaths, world)

# as_tibble(CountriesAndMaxDeaths)




# define colors
palette = colorRampPalette(brewer.pal(n=7, name='Purples'))(7)
palette = c("white", palette)
WorldDataWithCovid <- st_as_sf(WorldDataWithCovid)

WorldDataWithCovid$total_deathsESC <- base::cut(WorldDataWithCovid$total_deaths,
                        breaks = c(0,1000,2500,5000,50000, 
                                   250000,
                                   1000000,5000000), 
                        labels = 1:7, right = F, ordered_result = T)


# create map
WorldCovidDeathMap <- ggplot() +
  geom_sf(data = WorldDataWithCovid, aes(fill = total_deathsESC)) +
  scale_fill_manual(values = palette) +
  # customize legend title
  labs(fill = "Covid Death Toll") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "bottom",
        legend.title = element_text(colour="Purple", size=10, 
                                      face="bold"),
legend.background = element_rect(fill="grey", 
                                  size=0.5, linetype="solid",
                                 colour ="white"),
plot.title = element_text(face ="bold.italic", colour = "Black", size=20),
        
panel.background = element_rect("grey", "grey"),
axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"),
plot.background = element_rect(fill = "grey")
) +ggtitle("World Heat Map of Covid Death Totals")




WorldCovidDeathMap

```


```{r,echo=FALSE,message=FALSE,warning=FALSE}
WorldCovidDeathMap
```


























```{r,include=FALSE,message=FALSE,warning=FALSE}
# 
# ## PLEASE IGNORE(AGAIN)
# 
# 
# library(ggvis)
# library(shiny)
# Last28daysDeaths <- vroom("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv")
# 
# 
# 
# Last28daysDeaths$Date_Corrected <- as.Date(Last28daysDeaths$date, format =  "%d/%m/%y")
# 
# Last28daysDeathsGraph <-Last28daysDeaths %>% ggvis(x = ~Date_Corrected, y = ~newDeaths28DaysByDeathDate, fill = ~newDeaths28DaysByDeathDate) %>%
# layer_points() %>%
# add_axis("x", title = "date", orient = "top")
# 
# Last28daysDeathsGraph

```




```{r,include=FALSE,message=FALSE,warning=FALSE}
# 
# ##PLEASE IGNORE THIS ENTIRE SECTION, THIS WAS A TEST THAT I INTEND TO (MAYBE) USE FOR FUTURE MULTI GRAPH PLOTS
# 
# #KeepingColumns <- c("newDeaths28DaysByDeathDate","Date_Corrected")
# 
# #Last28daysDeaths <-  Last28daysDeaths[KeepingColumns]
# 
# library(shiny)
# Last28daysDeaths <- vroom("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv")
# 
# 
# 
# Last28daysDeaths$Date_Corrected <- as.Date(Last28daysDeaths$date, format =  "%d/%m/%y")
# 
# 
# ui <- fluidPage(
#   selectInput("var_y", "Y-Axis", choices = names(Last28daysDeaths)),
#   plotOutput("distPlot", hover = "plot_hover"),
#   uiOutput("dynamic")
# 
# )
# 
# server <- function(input, output) {
# 
#   output$distPlot <- renderPlot({
#     req(input$var_y)
#     ggplot(Last28daysDeaths, aes_string("date", input$var_y)) + 
#       geom_point()
#   })
# 
#   output$dynamic <- renderUI({
#     req(input$plot_hover) 
#     verbatimTextOutput("vals")
#   })
# 
#   output$vals <- renderPrint({
#     hover <- input$plot_hover 
#     # print(str(hover)) # list
#     y <- nearPoints(Last28daysDeaths, input$plot_hover)[input$var_y]
#     req(nrow(y) != 0)
#     y
#   })
# 
# }
# 
# shinyApp(ui = ui, server = server)


```

















