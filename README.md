# Happiness-Covid19

## Abstract

Happiness score is a common method to describe the happiness of people, which is an important dependent variable based on many other independent variable. Due to the emergence of Covid-19, the goal is to explore the Covid-19 impact on happiness score. The first step is to  combine the two table into one single table. There are many same variables during Covid-19 period, such as freedom, GDP. This is a sign to combine these four tables together and just adds one another variables--death variable, which represents the database of Covid-19. Try to make the regression model about the happiness score and other variables. Also, take the regional indicators into consideration. The second step is to check the relationship between happiness score and Covid-19, based on the relationships, and draw graphs to find the relationship between happiness score and other variables--the death of different countries. To use box plot to see the difference of happiness score between different years, and then draw the ggplot to see the correlation between different variables. Finally, make the linear regression model of happiness score and other variables. Use the forward regression model to get the fittest regression models. 


## The steps of this project

1. Combine the two table into one single table.

2. Check the relationship between happiness score and covid.

3. Use graphs to find the relationship between happiness score and other variables.

4. Map the death of different countries: explore the death

5. Make the linear regression model of happiness score and other variables.


```{r include=FALSE}
library("dbplyr")
library("tidyverse")
library("lubridate")
library(magrittr)
library(dplyr) 
library("janitor")


h2022<-read.csv("2022.csv")
h2021<-read.csv("2021.csv")
h2020<-read.csv("2020.csv")
h2019<-read.csv("2019.csv")
h2018<-read.csv("2018.csv")
h2017<-read.csv("2017.csv")
h2016<-read.csv("2016.csv")
h2015<-read.csv("2015.csv")
covid<-read.csv("owid-covid-data.csv")

```

```{r include=FALSE}
colnames(h2015)[4] <- "happiness_2015"
colnames(h2016)[4] <- "happiness_2016"
colnames(h2017)[3] <- "happiness_2017"
colnames(h2018)[3] <- "happiness_2018"
colnames(h2019)[3] <- "happiness_2019"
colnames(h2020)[3] <- "happiness_2020"
colnames(h2021)[3] <- "happiness_2021"
colnames(h2022)[3] <- "happiness_2022"

colnames(h2015)[1] <- "Country"
colnames(h2016)[1] <- "Country"
colnames(h2017)[1] <- "Country"
colnames(h2018)[2] <- "Country"
colnames(h2019)[2] <- "Country"
colnames(h2020)[1] <- "Country"
colnames(h2021)[1] <- "Country"
colnames(h2022)[2] <- "Country"

convert_number <- function(x){
    x <- as.character(x)
    x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
    x <- as.numeric(x)
    return(x)
}

h2022$happiness_2022<-convert_number(h2022$happiness_2022)

happy2015 <- h2015[c(1,4)]
happy2016 <- h2016[c(1,4)]
happy2017 <- h2017[c(1,3)]
happy2018 <- h2018[c(2,3)]
happy2019 <- h2019[c(2,3)]
happy2020 <- h2020[c(1,3)]
happy2021 <- h2021[c(1,3)]
happy2022 <- h2022[c(2,3)]
```

## Happiness score differences between each year

```{r include=FALSE}
happy_compare<-list(happy2015,happy2016,happy2017,happy2018,happy2019,happy2020,happy2021,happy2022) %>% reduce(inner_join, by='Country')
str(happy_compare)
head(happy_compare)
```
```{r}
boxplot(happy_compare$happiness_2015,happy_compare$happiness_2016,happy_compare$happiness_2017,happy_compare$happiness_2018,happy_compare$happiness_2019,happy_compare$happiness_2020,happy_compare$happiness_2021,happy_compare$happiness_2022,names=c("2015","2016","2017","2018","2019","2020","2021","2022"),xlab="Year",ylab="Happinesss Score")
```
![download](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download.png)
```{r message=FALSE}
HappinessScore<-c(happy_compare$happiness_2015,happy_compare$happiness_2016,happy_compare$happiness_2017,happy_compare$happiness_2018,happy_compare$happiness_2019,happy_compare$happiness_2020,happy_compare$happiness_2021,happy_compare$happiness_2022)
Year.levels<-c("2015","2016","2017","2018","2019","2020","2021","2022")
r.i=c(117,117,117,117,117,117,117,117)
Year<-as.factor(rep(Year.levels,r.i))
model<-aov(HappinessScore~Year)
if (!require("lsmeans"))
{
  install.packages("lsmeans")
  library(lsmeans)
}
lsmType=lsmeans(model,~Year) 
summary(contrast(lsmType,method="pairwise",adjust="tukey"), infer=c(T,T),levle=0.95,side="two-sided")
```


From the boxplot, there is no significant difference for happiness scores between 2015 to 2022. To further check this assumption quantitatively, we build a test to pairwisely compare the happiness score differences between two years by Tukey' method to double check, the results shows that all the p-value are very large. The happiness scores for all the years do not have significant difference.So we can not tell whether Covid-19 will have influence on happiness score. If it has significant influence on happiness score, then there should be other factors which will neutralize the negative impact from Covid-19(sum of death).


## Check the relationship between happiness score and covid (total deaths) for 2020 and 2021

```{r include=FALSE}

#data cleaning for covid
covid_new<-covid[c(2,3,4,8,9)]
colnames(covid_new)[2]<-"Country"
covid_new<-na.omit(covid_new)
covid_new$year<-substr(covid_new$date,1,4)

covid_new2<-covid_new %>%
    group_by(year,Country) %>%
    summarize(sum_of_deaths = sum(new_deaths))%>%
    as.data.frame()
covid_new<-distinct(covid_new[c(1,2)])
covid_new<-merge(x=covid_new,y=covid_new2,by="Country",all.x=FALSE, all.y=FALSE)
covid_new <- covid_new[-which(covid_new$continent == ""), ]
covid_new <- covid_new[-which(covid_new$year == "2022"), ]
```

```{r echo=FALSE}
# data combination for happiness score and covid(total death for each country for 2020 and 2021)

happy2020_new<-h2020[c(1,3,7,8,9,10,11,12)]
happy2021_new<-h2021[c(1,3,7,8,9,10,11,12)]

happy2020_new$year<-c(rep(2020,153))
happy2021_new$year<-c(rep(2021,149))

colnames(happy2020_new)[2]<-"happiness_score"
colnames(happy2021_new)[2]<-"happiness_score"

colnames(happy2020_new)[3]<-"log.GDP.per.capita"
colnames(happy2021_new)[3]<-"log.GDP.per.capita"

country_full<-list(happy2020_new[1],happy2021_new[1],covid_new[1]) %>% reduce(inner_join, by='Country')
country_full<-distinct(country_full)

happy2020_new<-merge(x=happy2020_new,y=country_full,by="Country",all.x=FALSE, all.y=FALSE)
happy2021_new<-merge(x=happy2021_new,y=country_full,by="Country",all.x=FALSE, all.y=FALSE)

happiness_score<-rbind.data.frame(happy2020_new,happy2021_new)

happiness_score$year<-as.character(happiness_score$year)
happiness_score <- happiness_score %>% inner_join(covid_new, 
            by=c('Country','year'))
colnames(happiness_score)[4]<-"Social.support"
colnames(happiness_score)[5]<-"Healthy.life.expectancy"
colnames(happiness_score)[6]<-"Freedom.to.make.life.choices"
colnames(happiness_score)[7]<-"Generosity"
colnames(happiness_score)[8]<-"Perceptions.of.corruption"

str(happiness_score)
```

Data description: It has 276 observations and 11 variables. The first 10 variables are from the happiness table and the last one is from the Covid table. Also, the Covid table has more than 60 variable. Thus, there are some same variables in both two tables, so keeping one. The reason to keep the different variable, death variable, is that the Covid-19 mostly influences different countries because of death. 

## Exploratory Data Analysis

```{r fig.height=4, fig.width=4}
# basic plot
ggplot(happiness_score, aes(x = log(sum_of_deaths), y= happiness_score, color =year))+geom_point()
ggplot(happiness_score, aes(x = year, y = happiness_score)) +geom_boxplot()
ggplot(happiness_score, aes(x = continent, y = happiness_score)) +geom_boxplot()
happiness_score$log_sum_of_deaths<-log(happiness_score$sum_of_deaths)
```
![download-1](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-1.png)
![download-2](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-2.png)
![download-3](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-3.png)

From the box plot for 2020 and 2021, the happiness scores of two years are slightly different. For the box plot for the relationship between happiness score and log transformation of sum of deaths for 2020 and 2021 separately, there is no obvious pattern.  So we can guess log transformation of sum of deaths should not have significant effect on happiness scores, which indicate that covid-19 does not lead to the decrease of happiness score. 

For the box plot for the relationship between happiness score and continent, it shows differences happiness score for different continent. Countries in Oceania shows highest average happiness scores, and Countries in Europe shows second highest average happiness scores. So continent may be a factor that will affect the happiness score.

From the last graph, the happiness score has the strongly positive correlation with GDP and Social support. Also, the happiness score has the weakly positive correlation with Life expectancy. About the other variables, the happiness score don't have strong correlation.

## Map visualization

```{r eval=FALSE, message=FALSE, include=FALSE}
library(ggthemes)
library(gganimate)
library(maps)

library("gifski")

mapdata<-read.csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
mapdata<-mapdata[c(2,3,4)]
colnames(covid)[3]<-"country"
covid_map<-merge(x=covid[c(3,4,8)],y=mapdata,by="country",all.x=FALSE, all.y=FALSE)
covid_map$total_deaths[covid_map$total_deaths == "NA"] <- 0

covid_map$date<-as.Date(as.POSIXct(covid_map$date,format="%Y-%m-%d",tz = ""))


str(covid_map)

ggplot(data = covid_map) +
  borders("world", colour = "gray90", fill = "gray85") +
  theme_map() + 
  geom_point(aes(x = longitude, y = latitude, size = total_deaths), 
             colour = "#351C4D", alpha = 0.55) +
  ggtitle("Distribution of sum_of_deaths") +
  labs(title = "Date: {frame_time}", size = "total_deaths") +
  transition_time(date) +
  ease_aes("linear")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
library(ggplot2)
library(maps)
library(ggthemes)
mapdata<-map_data("world")
colnames(mapdata)[5]<-"Country"
mapdata<-mapdata[c(1,2,5)]
colnames(covid_new2)[2]<-"country"

t2020<-filter(covid_new2, year == "2020")
t2021<-filter(covid_new2, year == "2021")
library(rworldmap)
death2020 <- joinCountryData2Map(t2020, joinCode="NAME", nameJoinColumn="country")
death2021 <- joinCountryData2Map(t2021, joinCode="NAME", nameJoinColumn="country")
mapCountryData(death2020, nameColumnToPlot="sum_of_deaths", mapTitle="Total deaths in 2020")
mapCountryData(death2021, nameColumnToPlot="sum_of_deaths", mapTitle="Total deaths in 2021")

```
![download-4](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-4.png)
![download-5](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-5.png)
## Linear regression model

```{r, eval=F, echo=T}
intercept<-lm(happiness_score~1,data=happiness_score)
all<-lm(happiness_score~ as.factor(year)+as.factor(continent)+Perceptions.of.corruption+Generosity+Freedom.to.make.life.choices+Healthy.life.expectancy+Social.support+log.GDP.per.capita+log_sum_of_deaths,data = happiness_score)
forward<-step(intercept,direction = "forward",scope=list(upper=all,lower=intercept))
```

```{r}
model<-lm(happiness_score ~ log.GDP.per.capita + Freedom.to.make.life.choices + 
    as.factor(continent) + Social.support + Perceptions.of.corruption + 
    Generosity + Healthy.life.expectancy,data=happiness_score)
summary(model)
```
![download-6](https://github.com/yutongyang0414/Happiness-Covid19/blob/main/Figure/download-6.png)
From the plot and model, it seems like year and sum of death for each each and each country does not have significant relationship with happiness score. 

Using the forward method to find a best regression model. From the best regression model, the happiness score is correlated with GDP, Freedom, continent, social support, corruption, generosity, and life expectancy. Also, the p-value of this model is small enough and the p-values of each variables are very small. Then the R-squared is 0.8007, and it means these variables of this model are correlated.

Although we consider the Covid variable, the death variable, it has less influence on happiness score. If we still want to consider about the Covid-19 influence the happiness score, we may use another factor to explore it. 


## Conclusion

The Covid-19 doesn't influence the happiness score. The happiness score is correlated with GDP, Freedom, continent, social support, corruption, generosity, and life expectancy. There are many backwards for this project. The first one is the death variable can not show all the information of the Covid-19, and we may use more than 5 specified variables to express the Covid-19. The second one is that the values of the different tables are different, so it is important and difficult to combine these values in a single table. This combination might cause some error, so the database of this model is not very sufficient. The way to change it is to look for another Covid table which has the same variables, and it is not very simple for us to find it. The another way is to use both similar variables and named it twice, but this way will increase the number of the variables.

