library(readr)
library(IDPmisc)
library(dplyr)
library(shinydashboard)
library(shiny)
library(maps)
library(fiftystater)
library(ggplot2)
library(plotly)

## load the NIH data
# needed for rstudio
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read_csv("data/2007-2017 - NIH Funding.csv")
data$FUNDING <- as.numeric(gsub('[$,]', '', data$FUNDING))

## clean up the NIH data
data_censored <- NaRV.omit(data)

## let's use only data from the US for simplicity
data_censored_USA <- subset(data_censored, COUNTRY=="UNITED STATES")

## let's filter out US territories
data_censored_USA_territories <- subset(data_censored_USA, STATE %in%
                                          c("AS", "GU", "MP", "PR", "UM", "VI"))
data_censored_USA <- setdiff(data_censored_USA, data_censored_USA_territories)


## generate the state data frame
## it seems that we need to first separate the dataframe into lists by year
## aggregate the state data by years
## recombine the resulting data as a dataframe
## aggregate the data by year
nih_usa_years <- split(data_censored_USA, f = data_censored_USA$YEAR)
## aggregate the data by states
nih_usa_states_aggregate <- lapply(nih_usa_years,
                                   FUN = function(year) {
                                     state_data <- aggregate(FUNDING~STATE, year, sum)
                                     state_years <- rep(year$YEAR[1],
                                                        each=length(state_data$STATE))
                                     df <- as.data.frame(state_data)
                                     df$YEAR <- state_years
                                     df
                                   })
## create the data frame
df_nih <- as.data.frame(nih_usa_states_aggregate)
## remove duplicate columns
df_nih <- df_nih[, -seq(3,length(df_nih), 3)]
df_nih <- df_nih[, -seq(3,length(df_nih), 2)]
## add proper names
names(df_nih) <- c("States", names(nih_usa_states_aggregate))
df_nih <- df_nih[order(df_nih$State),]

## normalize the data frame
df_nih_normalize <- as.data.frame(
  lapply(df_nih[-1],
         FUN = function(year) {
           year / sum(year)
         })
)
df_nih_normalize <- cbind(df_nih$States, df_nih_normalize)
## add proper names
names(df_nih_normalize) <- names(df_nih)

## let's add the GDP data
## https://www.bea.gov/iTable/iTable.cfm?reqid=70&step=10&isuri=1&7003=200&7035=-1&7004=sic&7005=1&7006=xx&7036=-1&7001=1200&7002=1&7090=70&7007=-1&7093=levels#reqid=70&step=10&isuri=1&7003=200&7035=-1&7004=naics&7005=1&7006=00000,01000,02000,04000,05000,06000,08000,09000,10000,11000,12000,13000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,40000,41000,42000,44000,45000,46000,47000,48000,49000,50000,51000,53000,54000,55000,56000&7036=-1&7001=1200&7002=1&7090=70&7007=2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007&7093=levels
## data is in the millions ($)
df_gdp <- read_csv("data/GDP.csv", skip = 4)
## remove unneeded parts from the gdp dataframe
df_gdp <- df_gdp[2:52,]
df_gdp <- df_gdp[,-1]
df_gdp <- df_gdp[,-1]
## the gdp data is organized alphabetically
df_gdp$State <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
                    "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR",
                    "PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI", "WY")
## reorganize the gdp data
df_gdp <- cbind(State=df_gdp$State, df_gdp[,-(length(df_gdp))])
df_gdp <- df_gdp[order(df_gdp$State),]

## normalize the data
## normalize the data frame
df_gdp_normalize <- as.data.frame(
  lapply(df_gdp[-1],
         FUN = function(year) {
           year / sum(year)
         })
)
df_gdp_normalize <- cbind(df_gdp$State, df_gdp_normalize)
## add proper names
names(df_gdp_normalize) <- names(df_nih_normalize)

## let's create a combined dataframe of the GDP and NIH data
df_combined <- df_gdp_normalize[-1] + df_nih_normalize[-1]
df_combined_normalize <- as.data.frame(
  lapply(df_combined,
         FUN = function(year) {
           year / sum(year)
         })
)
df_combined_normalize <- cbind(df_gdp_normalize$State, df_combined_normalize)
names(df_combined_normalize) <- names(df_nih_normalize)

## let's look at some ratios
df_gdp_vs_nih <- df_gdp_normalize[-1] / df_nih_normalize[-1]
df_gdp_vs_nih <- cbind(df_gdp_normalize$State, df_gdp_vs_nih)
names(df_gdp_vs_nih) <- names(df_nih_normalize)

## function to generate the required map
genMap <- function(type) {
  nih_map_data <- fifty_states
  nih_map_data$States <- state.abb[match(nih_map_data$id, tolower(state.name))]
  nih_map_data$States[is.na(nih_map_data$State)] <- "DC"
  if (type == "NIH Awards") {
    nih_map_data <- merge(df_nih_normalize, nih_map_data, by="States")
  } else if (type == "State GDP") {
    nih_map_data <- merge(df_gdp_normalize, nih_map_data, by="States")
  } else if (type == "Combined") {
    nih_map_data <- merge(df_combined_normalize, nih_map_data, by="States")
  } else if (type == "Ratio") {
    nih_map_data <- merge(df_gdp_vs_nih, nih_map_data, by="States")
  }
  nih_map_data <- nih_map_data[order(match(nih_map_data$order, fifty_states$order)),]
  return(nih_map_data)
}
