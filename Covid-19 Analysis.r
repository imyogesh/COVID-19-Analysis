library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(reshape2)
library(covid19.analytics)
library(caret)
library(tmap)
library(shinySIR)
library(deSolve)

recent_case_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases.csv"

confirmed.df <- covid19.data(case = 'ts-confirmed')
dim(confirmed.df)
death.df <- covid19.data(case = 'ts-deaths')
dim(death.df)
recovered.df <- covid19.data(case = 'ts-recovered')
dim(recovered.df)
recent.df <- read.csv(recent_case_global_link)
dim(recent.df)

head(confirmed.df)
head(death.df)
head(recovered.df)
head(recent.df)

length(unique(confirmed.df$Country.Region))
length(unique(death.df$Country.Region))
length(unique(recovered.df$Country.Region))
length(unique(recent.df$Country_Region))

## calculating the recent global cases - Confirmed, Deaths, Recovered, Active
recent.df %>% summarise( Confirmed = sum(Confirmed), Deaths = sum(Deaths), 
                         Recovered = sum(Recovered), Active = sum(Active, na.rm = TRUE) )

## aggregate confirmed cases data by country
confirmed_total <- data.frame(Date = as.Date(colnames(confirmed.df[, 5:ncol(confirmed.df)])), 
                              Total = colSums(confirmed.df[, 5:ncol(confirmed.df)], na.rm = TRUE) )

## cumulative confirmed cases by month
plt_confirmed_total <-  ggplot(confirmed_total, aes(x = Date, y = Total))  +
                        geom_line(size = 1.2, colour = lubridate::month(confirmed_total$Date)) +
                        ylab("Cumulative global Confirmed cases") +
                        xlab("Month") +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b")+
                        theme_bw()
 
## using ggplotly to create interactive plot        
ggplotly(plt_confirmed_total)

## calculate the daily rise in world confirmed cases
confirmed_daily <- confirmed_total %>% 
                            mutate(confirmed_diff = Total - lag(Total) ) 

## daily rise in confirmed cases by month
plt_confirmed_daily <-  ggplot(confirmed_daily, aes(x = Date, y = confirmed_diff))  +
                        geom_line(size = 1.2, colour = lubridate::month(confirmed_daily$Date)) +   
                        ylab("Daily rise in Confirmed cases") +
                        xlab("Month") +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                        theme_bw()

## using ggplotly to create interactive plot        
ggplotly(plt_confirmed_daily)

## aggregate death cases data by country
death_total <- data.frame(Date = as.Date(colnames(death.df[, 5:ncol(death.df)])), 
                           Total = colSums(death.df[, 5:ncol(death.df)], na.rm = TRUE) )

## cumulative deathss by month
plt_deaths_total <-  ggplot(death_total, aes(x = Date, y = Total))  +
                     geom_line(size = 1.2, colour = lubridate::month(death_total$Date)) +   
                     ylab("Cumulative global deaths") +
                     xlab("Month") +
                     scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                     theme_bw()

## using ggplotly to create interactive plot
ggplotly(plt_deaths_total)

## calculate the daily rise in world death cases
death_daily <- death_total %>% 
                            mutate(death_diff = Total - lag(Total) ) 

## daily rise in death cases by month
plt_death_daily <-  ggplot(death_daily, aes(x = Date, y = death_diff))  +
                    geom_line(size = 1.2, colour = lubridate::month(death_daily$Date)) +   
                    ylab("Daily rise in Deaths") +
                    xlab("Month") +
                    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                    theme_bw()

## using ggplotly to create interactive plot        
ggplotly(plt_death_daily)

## aggregate confirmed cases data by country
recovered_total <- data.frame(Date = as.Date(colnames(recovered.df[, 5:ncol(recovered.df)])),
                              Total = colSums(recovered.df[, 5:ncol(recovered.df)], na.rm = TRUE))

## cumulative deathss by month
plt_recovered_total <-  ggplot(recovered_total, aes(x = Date, y = Total))  +
                        geom_line(size = 1.2, colour = lubridate::month(recovered_total$Date)) +   
                        ylab("Cumulative global Recovered") +
                        xlab("Month") +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b")  +
                        theme_bw()

## using ggplotly to create interactive plot 
ggplotly(plt_recovered_total)

## calculate the daily rise in world death cases
recovered_daily <- recovered_total %>% 
                            mutate(recovered_diff = Total - lag(Total) ) 

## cumulative deaths by month
plt_recovered_daily <-  ggplot(recovered_daily, aes(x = Date, y = Total))  +
                        geom_line(size = 1.2, colour = lubridate::month(recovered_daily$Date)) +   
                        ylab("Daily rise in Recovered") +
                        xlab("Month") +
                        scale_x_date(date_breaks = "1 month", date_labels = "%b")  +
                        theme_bw()

## using ggplotly to create interactive plot 
ggplotly(plt_recovered_daily)

## aggregate Confirmed, Deaths, Recovered cases daily data 
combined_total <- data.frame( Date = confirmed_total$Date, 
                              Confirmed = confirmed_total$Total, 
                              Deaths = death_total$Total,
                              Recovered = recovered_total$Total )

## using ggplotly to create interactive plot 
plt_combined <-  ggplot(combined_total, aes(x = Date)) +
                 geom_line(aes(y = Confirmed), colour = lubridate::month(combined_total$Date), linetype = 1) +
                 geom_line(aes(y = Recovered), colour = lubridate::month(combined_total$Date), linetype = 3) +
                 geom_line(aes(y = Deaths),    colour = lubridate::month(combined_total$Date), linetype = 4) +
                 scale_x_date(date_breaks = "1 month", date_labels = "%b")+
                 theme_bw()
        
ggplotly(plt_combined)

## aggregate recent Confirmed, Deaths, Recovered cases by country
countrywise <- recent.df %>% group_by(Country_Region) %>% select(2,6,7,8,9)
countrywise_global <-aggregate(countrywise[ ,2:5] , by = list(Country_Region = countrywise$Country_Region), FUN=sum)

## reordering the data based on recent Confirmed cases by country
countrywise_global <- countrywise_global[order(countrywise_global$Confirmed, decreasing = TRUE), ]

## calculate Confirmed, Deaths, Recovered cases by country as a percentage of world
countrywise_global$Active <- countrywise_global$Confirmed - countrywise_global$Deaths - countrywise_global$Recovered
countrywise_global_pct <- countrywise_global %>% 
                                    mutate ( Confirmed_pct = round((Confirmed / sum(Confirmed)) * 100, 2),
                                             Deaths_pct = round((Deaths / sum(Deaths)) * 100, 2), 
                                             Recovered_pct = round((Recovered / sum(Recovered)) * 100, 2),
                                             Active_pct = round((Active / sum(Active)) * 100, 2) )

## identifying top10 countries based on their recent Confirmed cases count
countrywise_top_10 <- melt(countrywise_global[1:10,], id.vars='Country_Region')


## plot the bar plot  for top10 countries based on their recent Confirmed cases count
top_10 <- ggplot(countrywise_top_10, aes(x = reorder(Country_Region, value))) +
                        geom_col(aes(y = round(value/1000000, 2), fill = variable)) +
                        facet_grid( . ~ variable, scales="free_x") +
                        ylab("Count in Million") +
                        xlab("Country") +
                        coord_flip() +
                        theme_bw()
ggplotly(top_10)

## normalizing the data
countrywise_global.norm <- sapply(countrywise_global[, 2:5], scale)
rownames(countrywise_global.norm) <- countrywise_global$Country_Region

## set.seed for the reproducibility purpose
set.seed(42)

# grouped countries based on Confirmed, Recovered & Deaths into 3 clusters
km_3 <- kmeans(countrywise_global.norm, 3)

## assigned the derived clusters to normalized and w/o normalized dataframe
countrywise_global_cluster <- mutate(countrywise_global, Cluster = km_3$cluster)

plt_cluster <- ggplot(countrywise_global_cluster, aes(x= round(Confirmed/1000000, 2), y = round(Recovered/1000000, 2))) +
               geom_point(aes(color = as.factor(Cluster), size = Deaths ) ) +
               facet_grid( . ~ Cluster, scales="free") +
               theme_bw()
                
ggplotly(plt_cluster)

## count of countries under each cluster
count <- ggplot(countrywise_global_cluster, aes(x = Cluster, fill = as.factor(Cluster))) +
         geom_bar() +
         facet_grid(. ~ Cluster) +
         theme_bw()

ggplotly(count)

## rearranging the dataframe for further analysis using melt function
countrywise_global_cluster_box <- melt(countrywise_global_cluster , id.vars=c('Country_Region', 'Cluster'))

## plot the box plot for each cluster 
box <- ggplot(countrywise_global_cluster_box, aes(x = variable, y = value, fill = varibale)) +
       geom_boxplot() +
       facet_grid(. ~ Cluster) +
       theme_bw()

ggplotly(box)

## filter rows where country = India from Confirmed.df 
confirmed_india_daily <- confirmed.df[confirmed.df$Country.Region == "India", 5:ncol(confirmed.df)]
confirmed_india_daily <- as.data.frame(t(as.matrix(confirmed_india_daily)))

## filter rows where country = India from Recovered.df 
recovered_india_daily <- recovered.df[recovered.df$Country.Region == "India", 5:ncol(recovered.df)]
recovered_india_daily <- as.data.frame(t(as.matrix(recovered_india_daily)))

## filter rows where country = India from Death.df
death_india_daily <- death.df[death.df$Country.Region == "India", 5:ncol(death.df)]
death_india_daily <- as.data.frame(t(as.matrix(death_india_daily)))

## Combining aobe three dataframe to obtain final dataframe
india_daily_case <- data.frame( date = as.Date(rownames(confirmed_india_daily)),
                                   confirmed = confirmed_india_daily[,1],
                                   recovered = recovered_india_daily[,1], 
                                   death = death_india_daily[,1])

## to obtain daily increase in Confirmed, Recovered, Death cases
india_daily_rise <- india_daily_case %>% 
                            mutate(confirmed_diff = confirmed - lag(confirmed),
                                   recovered_diff = recovered - lag(recovered),
                                   death_diff = death - lag(death) )

## combine plot of daily rise in cases
plt_india_daily_rise <-  ggplot(india_daily_rise, aes(x = date)) +
        geom_line(aes(y = confirmed_diff), colour = "blue", size = 1) +
        geom_line(aes(y = recovered_diff), colour = "green", size = 1) +
        geom_line(aes(y = death_diff),    colour = "red", size = 1) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b")+
        ylab("Count") +
        xlab("Month") +
        theme_bw()
        
ggplotly(plt_india_daily_rise)

## set.seed for reproducibility purpose
set.seed(2)

## calculate sample size for 70% train & 30% test dataframe
smp_size <- floor(0.7 * nrow(india_daily_rise))

## creating index for train & test dataframe
train_ind <- sample(seq(1, nrow(india_daily_rise), 1), size=smp_size)
test_ind <- sample(-train_ind )

## creating train & test dataframe from sampled indexes
train.df <- india_daily_rise[train_ind,] 
test.df <- india_daily_rise[test_ind, ]

## create linear regression model from training dataframe
myLM_confirmed_india <- lm(confirmed_diff ~ date, data = train.df)
summary(myLM_confirmed_india)

## accuracy of myLM model on train dataframe
confirmed_df <- predict(myLM_confirmed_india, train.df)
actuals_preds  <- data.frame(cbind(actuals=train.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")

## accuracy of myLM model on test dataframe
confirmed_df <- predict(myLM_confirmed_india, test.df)
actuals_preds  <- data.frame(cbind(actuals=test.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")
tail(actuals_preds)

## filter rows where country = US from Confirmed.df 
confirmed_us_daily <- confirmed.df[confirmed.df$Country.Region == "US", 5:ncol(confirmed.df)]
confirmed_us_daily <- as.data.frame(t(as.matrix(confirmed_us_daily)))

## filter rows where country = US from Recovered.df 
recovered_us_daily <- recovered.df[recovered.df$Country.Region == "US", 5:ncol(recovered.df)]
recovered_us_daily <- as.data.frame(t(as.matrix(recovered_us_daily)))

## filter rows where country = US from Death.df 
death_us_daily <- death.df[death.df$Country.Region == "US", 5:ncol(death.df)]
death_us_daily <- as.data.frame(t(as.matrix(death_us_daily)))

## Combining aobe three dataframe to obtain final dataframe
us_daily_case <- data.frame( date = as.Date(rownames(confirmed_us_daily)),
                                   confirmed = confirmed_us_daily[,1],
                                   recovered = recovered_us_daily[,1], 
                                   death = death_us_daily[,1])

## to obtain daily increase in Confirmed, Recovered, Death cases
us_daily_rise <- us_daily_case %>% 
                            mutate(confirmed_diff = confirmed - lag(confirmed),
                                   recovered_diff = recovered - lag(recovered),
                                   death_diff = death - lag(death) )

## combine plot of daily rise in cases
plt_us_daily_rise <-  ggplot(us_daily_rise, aes(x = date)) +
        geom_line(aes(y = confirmed_diff), colour = "blue", size = 1) +
        geom_line(aes(y = recovered_diff), colour = "green", size = 1) +
        geom_line(aes(y = death_diff),    colour = "red", size = 1) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b")+
        ylab("Count") +
        xlab("Month") +
        theme_bw()
        
ggplotly(plt_us_daily_rise)

## set.seed for reproducibility purpose
set.seed(2)

## calculate sample size for 70% train & 30% test dataframe
smp_size <- floor(0.7 * nrow(us_daily_rise))

## creating index for train & test dataframe
train_ind <- sample(seq(1, nrow(us_daily_rise), 1), size=smp_size)
test_ind <- sample(-train_ind )

## creating train & test dataframe from sampled indexes
train.df <- us_daily_rise[train_ind,] 
test.df <- us_daily_rise[test_ind, ]

## create linear regression model from training dataframe
myLM_confirmed_us <- lm(confirmed_diff ~ date, data = train.df)
summary(myLM_confirmed_us)

## accuracy of myLM model on train dataframe
confirmed_df <- predict(myLM_confirmed_us, train.df)
actuals_preds  <- data.frame(cbind(actuals=train.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")

## accuracy of myLM model on test dataframe
confirmed_df <- predict(myLM_confirmed_us, test.df)
actuals_preds  <- data.frame(cbind(actuals=test.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")
tail(actuals_preds)

## filter rows where country = Brazil from Confirmed.df 
confirmed_brazil_daily <- confirmed.df[confirmed.df$Country.Region == "Brazil", 5:ncol(confirmed.df)]
confirmed_brazil_daily <- as.data.frame(t(as.matrix(confirmed_brazil_daily)))

## filter rows where country = Brazil from Recovered.df 
recovered_brazil_daily <- recovered.df[recovered.df$Country.Region == "Brazil", 5:ncol(recovered.df)]
recovered_brazil_daily <- as.data.frame(t(as.matrix(recovered_brazil_daily)))

## filter rows where country = Brazil from Death.df
death_brazil_daily <- death.df[death.df$Country.Region == "Brazil", 5:ncol(death.df)]
death_brazil_daily <- as.data.frame(t(as.matrix(death_brazil_daily)))

## Combining aobe three dataframe to obtain final dataframe
brazil_daily_case <- data.frame( date = as.Date(rownames(confirmed_brazil_daily)),
                                   confirmed = confirmed_brazil_daily[,1],
                                   recovered = recovered_brazil_daily[,1], 
                                   death = death_brazil_daily[,1])

## to obtain daily increase in Confirmed, Recovered, Death cases
brazil_daily_rise <- brazil_daily_case %>% 
                            mutate(confirmed_diff = confirmed - lag(confirmed),
                                   recovered_diff = recovered - lag(recovered),
                                   death_diff = death - lag(death) )

## combine plot of daily rise in cases
plt_brazil_daily_rise <-  ggplot(brazil_daily_rise, aes(x = date)) +
        geom_line(aes(y = confirmed_diff), colour = "blue", size = 1) +
        geom_line(aes(y = recovered_diff), colour = "green",  size = 1) +
        geom_line(aes(y = death_diff),    colour = "red", size = 1) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b")+
        ylab("Count") +
        xlab("Month") +
        theme_bw()
        
ggplotly(plt_brazil_daily_rise)


## set.seed for reproducibility purpose
set.seed(2)

## calculate sample size for 70% train & 30% test dataframe
smp_size <- floor(0.7 * nrow(brazil_daily_rise))

## creating index for train & test dataframe
train_ind <- sample(seq(1, nrow(brazil_daily_rise), 1), size=smp_size)
test_ind <- sample(-train_ind )

## creating train & test dataframe from sampled indexes
train.df <- brazil_daily_rise[train_ind,] 
test.df <- brazil_daily_rise[test_ind, ]

## create linear regression model from training dataframe
myLM_confirmed_brazil <- lm(confirmed_diff ~ date, data = train.df)
summary(myLM_confirmed_brazil)

## accuracy of myLM model on train dataframe
confirmed_df <- predict(myLM_confirmed_brazil, train.df)
actuals_preds  <- data.frame(cbind(actuals=train.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")

## accuracy of myLM model on test dataframe
confirmed_df <- predict(myLM_confirmed_brazil, test.df)
actuals_preds  <- data.frame(cbind(actuals=test.df$confirmed_diff, predicteds=confirmed_df))
cor(actuals_preds$actuals, actuals_preds$predicteds, use = "pairwise.complete.obs")

## Defined SIR model with equation for S, I and R

mySIRS <- function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    # Change in Susceptibles
    dS <- - (beta * S * I)
    
    # Change in Infecteds
    dI <- (beta * S * I) - (gamma * I)
    
    # Change in Recovereds
    dR <- gamma * I 
    
    return(list(c(dS, dI, dR)))
  })
}



## calculate tmax value in times parameter
SIR_india_confirmed <- confirmed.df[confirmed.df$Country.Region == "India", 1:ncol(confirmed.df)]
SIR_india_confirmed_t <- as.data.frame(t(as.matrix(SIR_india_confirmed)))


## Initiating the required parameter of India for SIR model R0 = 1.46
state <- c(S = 1352642280, I = 1, R = 0)
times <- seq(0, (nrow(SIR_india_confirmed_t)-5) , by = 1)
parameters <- c(beta =  1.55E-10, gamma = 1/7)

## calling mySIRS function using ODE(Ordinary Differential Equation) function
out_india <- ode(y = state, times = times, func = mySIRS, parms = parameters)
out_india_df <- as.data.frame(out_india)

## adding actual daily confirmed cases out of India to predicted dataframe
out_india_df$Conf <- as.numeric(SIR_india_confirmed_t[5:nrow(SIR_india_confirmed_t), 1])

## plot predicted I, R value against actual Confirmed cases value 
plt_india_SIR <-  ggplot(out_india_df, aes(x = time)) +
                 geom_line(aes(y = S), colour = "blue", size = 1) +
                 geom_line(aes(y = I), colour = "red", size = 1) +
                 geom_line(aes(y = R), colour = "green", size = 1) +
                 geom_line(aes(y = Conf), colour = "purple", linetype = 2, size = 1) +
                 ylab("Count of cases") +
                 theme_bw()

ggplotly(plt_india_SIR)


## calculate tmax value in times parameter
SIR_US_confirmed <- confirmed.df[confirmed.df$Country.Region == "US", 1:ncol(confirmed.df)]
SIR_US_confirmed_t <- as.data.frame(t(as.matrix(SIR_US_confirmed)))


## Initiating the required parameter of US for SIR model for R0 value 1.62
state <- c(S = 331722709, I = 1, R = 0)
times <- seq(0, (nrow(SIR_US_confirmed_t)-5) , by = 1)
parameters <- c(beta =  6.5E-10, gamma = 1/7)

## calling mySIRS function using ODE(Ordinary Differential Equation) function
out_US <- ode(y = state, times = times, func = mySIRS, parms = parameters)
out_US_df <- as.data.frame(out_US)

## adding actual daily confirmed cases out of US to predicted dataframe
out_US_df$Conf <- as.numeric(SIR_US_confirmed_t[5:nrow(SIR_US_confirmed_t), 1])

## plot predicted I, R value against actual Confirmed cases value 
plt_US_SIR <-  ggplot(out_US_df, aes(x = time)) +
                 geom_line(aes(y = S), colour = "blue") +
                 geom_line(aes(y = I), colour = "red") +
                 geom_line(aes(y = R), colour = "green") +
                 geom_line(aes(y = Conf), colour = "purple", linetype = 2) +
                 ylab("Count of cases") +
                 theme_bw()

ggplotly(plt_US_SIR)

## calculate tmax value in times parameter
SIR_Brazil_confirmed <- confirmed.df[confirmed.df$Country.Region == "Brazil", 1:ncol(confirmed.df)]
SIR_Brazil_confirmed_t <- as.data.frame(t(as.matrix(SIR_Brazil_confirmed)))


## Initiating the required parameter of United States for SIR model for R0 value 1.46
state <- c(S = 212559417, I = 1, R = 0)
times <- seq(0, (nrow(SIR_Brazil_confirmed_t)-5) , by = 1)
parameters <- c(beta =  9.8E-10, gamma = 1/7)

## calling mySIRS function using ODE(Ordinary Differential Equation) function
out_brazil <- ode(y = state, times = times, func = mySIRS, parms = parameters)
out_brazil_df <- as.data.frame(out_brazil)

## adding actual daily confirmed cases out of Brazil to predicted dataframe
out_brazil_df$Conf <- as.numeric(SIR_US_confirmed_t[5:nrow(SIR_Brazil_confirmed_t), 1])

## plot predicted I, R value against actual Confirmed cases value 
plt_Brazil_SIR <-  ggplot(out_brazil_df, aes(x = time)) +
                   geom_line(aes(y = S), colour = "blue") +
                   geom_line(aes(y = I), colour = "red") +
                   geom_line(aes(y = R), colour = "green") +
                   geom_line(aes(y = Conf), colour = "purple", linetype = 2) +
                   ylab("Count of cases") +
                   theme_bw()

ggplotly(plt_Brazil_SIR)

library(tmap)

## import data of country geometry multipolygon
data(World)

countrywise <- recent.df %>% group_by(Country_Region) %>% select(2,6,7,8,9, 17)
countrywise_global_map <-aggregate(countrywise[ ,2:5] , by = list(Country_Region = countrywise$Country_Region, iso_a3 = countrywise$ISO3), FUN=sum)
length(unique(countrywise_global_map$iso_a3))

## merge countrywise_global_map,World on country name
df <- merge(countrywise_global_map, World, by = c("iso_a3"))
df <- df %>% select(1:6,ncol(df))

## mapping Recent Confirmed 
map <- ggplot(df) + 
       geom_sf( aes(geometry = geometry, fill = Confirmed), color="black" ) +
       ggtitle("World Map of Confirmed Covid Cases", subtitle="Total Cases on November 12, 2020") +
       theme_bw()

ggplotly(map)


