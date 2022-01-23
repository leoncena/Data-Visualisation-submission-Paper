library(tidyverse)
library(ggthemr) # installed by devtools
library(readr)
require(gridExtra)
library(ggridges)
library(treemapify)
library(ggthemes)
library(waffle)
library(ggplot2)
library(reshape2)
library(scales)


setwd("/Users/leoncena/Repos/GitHub/Data-Viz-Paper/Code")
devtools::install_github('Mikata-Project/ggthemr')

# Notes
#Incident rate has to be calculated on our own
# Case Fatality not summable

# Read and clean Covid Data
covid_daily <- read_csv("data/covid/covid_19_daily_reports_01-20-2022.csv") %>% 
    select(Country_Region, Confirmed, Deaths) 
covid_daily[is.na(covid_daily)] <- 0 #Na should be zero to not break calculation
covid_daily <- covid_daily%>%
    group_by(Country_Region) %>%
    summarise_each(funs(sum))

covid_time_series <- read.csv("~/Repos/GitHub/Data-Viz-Paper/Code/data/covid/time_series_covid19_confirmed_01-21-2022.csv") %>% 
    select(-c(Province.State, Lat,Long)) %>%
    group_by(Country.Region) %>%
    summarise_each(funs(sum)) %>%
    select(Country.Region,X1.21.21:X1.21.22)
  
# Read and clean Vaccination Data
blankChar <- read.csv("~/Repos/GitHub/Data-Viz-Paper/Code/data/vacc/vaccine_data_global.csv")$Province_State[1] #for filtering later

vacc_global_data <- read.csv("~/Repos/GitHub/Data-Viz-Paper/Code/data/vacc/vaccine_data_global.csv") %>% 
    filter(Province_State == blankChar) %>%
    select(-Province_State,-UID,-Report_Date_String)


blankChar <- read.csv("~/Repos/GitHub/Data-Viz-Paper/Code/data/vacc/time_series_covid19_vaccine_doses_admin_global.csv")$Province_State[1] #for filtering later
vacc_time_series <- read.csv("~/Repos/GitHub/Data-Viz-Paper/Code/data/vacc/time_series_covid19_vaccine_doses_admin_global.csv") %>%
  filter(Province_State == blankChar) %>%  
  select(-(UID:Admin2),-Lat,-Long_,-Combined_Key,-Province_State)
#vacc_time_series[is.na(vacc_time_series)] <- 0 #Na should not be zero otherwise we get .inf results because of divisions

vacc_global_data$Country_Region = trimws(vacc_global_data$Country_Region) # delete Trailing Spaces
vacc_time_series$Country_Region = trimws(vacc_time_series$Country_Region) # delete Trailing Spaces


# Joining data sets
# Add Vaccination rate as new variable
CovidAndVaccData <- covid_daily %>%
    left_join(vacc_global_data) %>%
    left_join(select(vacc_time_series,Country_Region,Population))%>%
    mutate(vacc_Rate = People_fully_vaccinated/Population ) %>%
    mutate(incident = Confirmed/Population*100000) %>%  ##this is the total incident, not weekly
    mutate(fatality_Rate = Deaths/Confirmed)


# interssant vacc rate und fatality
# ggpairs(CovidAndVaccData, columns = c(2,8,9,11))

## Visualisation
ggthemr("light")

############################################################################################################################################
##################################### Chart 1: Confirmed Cases and vaccination information  ################################################
############################################################################################################################################
ggthemr("light")
amount_of_countries <- 8
#Part 1.1 Top 8 countries with highest amount of cases per country with vaccination information

#Bar Chart
plot11 <- ggplot(data = CovidAndVaccData[order(CovidAndVaccData$Confirmed, decreasing = TRUE)[1:amount_of_countries],], mapping = aes(x=Country_Region, y=Confirmed, fill=vacc_Rate)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Amount of Confirmed Cases", fill = "Vaccination rate") +
  ggtitle("8 countries with most confirmed cases")

#Part 1.2 Top 8 countries with lowest amount of Cases per country with vaccination information
# possibility to set thresholds for confirmed cases and vaccination rate
threshhold_confirmed <- 6000 # can be adjusted, 
threshhold_vaccination_rate <- 0.00 # can be adjusted, should >=0 to prevent errors
# filter thresholds
pdata <- filter(CovidAndVaccData, Confirmed>=threshhold_confirmed, vacc_Rate >=threshhold_vaccination_rate)
#Bar Chart
plot12 <- ggplot(data = pdata[order(pdata$Confirmed, decreasing = FALSE)[1:amount_of_countries],], mapping = aes(x=Country_Region, y=Confirmed, fill=vacc_Rate)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Confirmed Cases", fill = "Vaccination rate") +
  ggtitle("10 countries with least confirmed cases")

#2x1 facette
grid.arrange(plot11, plot12, ncol=2)



############################################################################################################################################
################# Chart 2: Mean-deviation of vaccination rates of the countries with highest/lowest fatality rate  #########################
############################################################################################################################################

ggthemr("flat") # chosen because of better contrast for population difference
amount_n_of_oveserved_countries <- 30

#2.1 We see that countries with high fatality rate tend to have lower vaccination rates
#filter data set to prevent NAs
caseCovid <- filter(CovidAndVaccData, fatality_Rate >=0) %>%
  filter(vacc_Rate >=0)


islegalEntry <- !is.na(CovidAndVaccData$vacc_Rate) & !is.infinite(CovidAndVaccData$vacc_Rate) # only use proper values for calculations
first_n_indices_of_countries_with_highest_fatality_decreasing <- order(caseCovid$fatality_Rate, decreasing = TRUE)[1:amount_n_of_oveserved_countries]

caseCovid <- caseCovid[first_n_indices_of_countries_with_highest_fatality_decreasing,] %>% 
  mutate(
    vacc_mean = vacc_Rate - mean(CovidAndVaccData$vacc_Rate[islegalEntry]),
    Country_Region = reorder(Country_Region, vacc_mean)
  )

# deviation column chart with flipped coordinates
plot21 <- ggplot(data = caseCovid) +
  geom_col(mapping = aes(x = Country_Region, y = vacc_mean, fill=Population)) +
  coord_flip() +
  labs(x = "Country", y = "Deviation from average vaccination rate") + 
  ggtitle("Deviation of the average vaccination rate for the countries with the highest fatality rates")

#2.2 We see that countries with low fatality rate tend to have higher vaccination rates than avg
caseCovid <- filter(CovidAndVaccData, fatality_Rate >=0) %>% #create fitler data set
  filter(vacc_Rate >=0)

islegalEntry <- !is.na(CovidAndVaccData$vacc_Rate) & !is.infinite(CovidAndVaccData$vacc_Rate) # only use proper values for calculations
first_n_indices_of_countries_with_highest_fatality_ascending <- order(caseCovid$fatality_Rate, decreasing = FALSE)[1:amount_n_of_oveserved_countries]

caseCovid <- filter(CovidAndVaccData, fatality_Rate >=0) %>%
  filter(vacc_Rate >=0)

caseCovid <- caseCovid[first_n_indices_of_countries_with_highest_fatality_ascending,] %>% 
  mutate(
    vacc_mean = vacc_Rate - mean(CovidAndVaccData$vacc_Rate[islegalEntry]),
    Country_Region = reorder(Country_Region, vacc_mean)
  )

maxPopulation <-  max(CovidAndVaccData$Population)
plot22 <- ggplot(data = caseCovid) +
  geom_col(mapping = aes(x = Country_Region, y = vacc_mean, fill=Population)) +
  coord_flip() +
  labs(x = "Country", y = "Deviation from average vaccination rate") +
  ggtitle("Deviation of the average vaccination rate for the countries with the lowest fatality rates")


#2x1 facette
grid.arrange(plot21, plot22, ncol=2)



############################################################################################################################################
################# Classification of Vaccination progrwess by country  #########################
############################################################################################################################################

assessVacc <- function(vacc){
  if(is.na(vacc) | is.null(vacc)){return("NA")}
  if(between(vacc,.90,2.)){return("90%+")} 
  if(between(vacc,.80,0.8999)){return("80%+")}
  if(between(vacc,.700,0.7999)){return("70%+")}
  if(between(vacc,.60,0.6999)){return("60%+")}
  if(between(vacc,.60,0.6999)){return("60%+")}
  if(between(vacc,.50,0.5999)){return("50%+")}
  if(between(vacc,.30,0.4999)){return("30%+")}
  if(between(vacc,.10,0.2999)){return("10%+")}
  if(between(vacc,0.0,0.999)){return("below 10%")}
  if(NA){return("NA")}
}
assessVacc_vec <- Vectorize(assessVacc, vectorize.args = "vacc")


treemapdata <- CovidAndVaccData
treemapdata$vacc_cat =assessVacc_vec(treemapdata$vacc_Rate)



############################################################################################################################################
################# Chart 1 but no fill by collour instead by category  #########################
############################################################################################################################################

ggthemr("light")
amount_of_countries <- 8
#Part 1.1 Top 8 countries with highest amount of cases per country with vaccination information

#Bar Chart
plot11 <- ggplot(data = treemapdata[order(treemapdata$Confirmed, decreasing = TRUE)[1:amount_of_countries],], mapping = aes(x=Country_Region, y=Confirmed, fill=vacc_cat)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Amount of Confirmed Cases", fill = "Vaccination rate") +
  ggtitle("8 countries with most confirmed cases")

#Part 1.2 Top 8 countries with lowest amount of Cases per country with vaccination information
# possibility to set thresholds for confirmed cases and vaccination rate
threshhold_confirmed <- 6000 # can be adjusted, 
threshhold_vaccination_rate <- 0.00 # can be adjusted, should >=0 to prevent errors
# filter thresholds
pdata <- filter(treemapdata, Confirmed>=threshhold_confirmed, vacc_Rate >=threshhold_vaccination_rate)
#Bar Chart
plot12 <- ggplot(data = pdata[order(pdata$Confirmed, decreasing = FALSE)[1:amount_of_countries],], mapping = aes(x=Country_Region, y=Confirmed, fill=vacc_cat)) +
  geom_bar(stat = "identity") + 
  labs(x = "Country", y = "Confirmed Cases", fill = "Vaccination rate") +
  ggtitle("10 countries with least confirmed cases")

#2x1 facette
grid.arrange(plot11, plot12, ncol=2)


############################################################################################################################################
################# Chart 3 : Treemap  #########################
############################################################################################################################################
ggthemr_reset()

ggthemr("pale")
tree_treemap <- treemapdata %>%
  group_by(vacc_cat) %>% 
  summarise(no_cat = n())

ggplot(data = tree_treemap, mapping = aes(area = no_cat, fill = vacc_cat, label = paste(vacc_cat,"in ",no_cat, "countries" ))) +
  geom_treemap() +
  geom_treemap_text() +
  theme(legend.position = "none") + 
  theme_economist()



############################################################################################################################################
################# Chart 4 : Bubble  ######################### 
############################################################################################################################################
ggthemr("flat")
covid_cat <- CovidAndVaccData
covid_cat$vacc_cat =assessVacc_vec(treemapdata$vacc_Rate)

plot41 <- ggplot(data = covid_cat, mapping = aes(x = vacc_Rate, y = Confirmed, size = Population)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Vaccination rate", y = "Confirmed Cases", fill = "Population")

plot42 <- ggplot(data = covid_cat, mapping = aes(x = vacc_Rate, y = Deaths, size = Population)) +
  geom_point(alpha = 0.4) + 
  labs(x = "Vaccination rate", y = "Deaths", fill = "Population") 
  

#2x1 facette
grid.arrange(plot41, plot42, ncol=2, top="Bubble chart on vaccination rates, confirmed cases, death rates and the underlying population") 

############################################################################################################################################
################# Chart 5 : Time Series  ##################################################################
############################################################################################
covid_time_series$Country.Region = trimws(covid_time_series$Country.Region) # delete Trailing Spaces
covid_time_series_DE <- filter(covid_time_series, Country.Region == "Germany")

## alltime series
clean_time_series <- melt(data = covid_time_series,
                          id.vars = c("Country.Region"),
                          variable.name = "date",
                          value.name = "confirmed_cases")
clean_time_series$date = sub('.', '', clean_time_series$date)
clean_time_series$date = as.Date(clean_time_series$date, "%m.%d.%Y")

german_time_series <- filter(clean_time_series,Country.Region %in% c("Germany", "Liechtenstein", "Switzerland", "Austria")) # correct date

t1 <- ggplot(data = german_time_series, mapping = aes(x = date, y = confirmed_cases, color = Country.Region)) +
  geom_line() + 
  labs(x = "Date", y = "Confirmed Cases", color="Country with trend line") +
  geom_smooth(method="lm") +
  ggtitle("Amount Confirmed Cases") + 
  scale_y_continuous(name="Amount of confirmed cases", labels = comma)


vacc_time_series_temp = select(vacc_time_series, Country_Region,X2021.01.21:X2022.01.21)

# alltime series for vacc
clean_vacc_time_series <- melt(data = vacc_time_series_temp,
                          id.vars = c("Country_Region"),
                          variable.name = "date",
                          value.name = "vaccinations")
clean_vacc_time_series$date = sub('.', '', clean_vacc_time_series$date)
clean_vacc_time_series$date = as.Date(clean_vacc_time_series$date, "%Y.%m.%d")
clean_vacc_time_series <- filter(clean_vacc_time_series,!is.na(vaccinations))

german_vacc_time_series <- filter(clean_vacc_time_series,Country_Region %in% c("Germany", "Liechtenstein", "Switzerland", "Austria")) # correct date

t2 <- ggplot(data = german_vacc_time_series, mapping = aes(x = date, y = vaccinations, color = Country_Region)) +
  geom_line() + 
  labs(x = "Date", y = "Vaccinations", color="Country with trend line") +
  geom_smooth(method="lm") +
  ggtitle("Amount Vaccinations") + 
  scale_y_continuous(name="Amount of vaccinations", labels = comma)


#2x1 facette
grid.arrange(t1, t2, ncol=2)


