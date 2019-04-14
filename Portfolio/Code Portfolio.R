##Data Import

#loading the library tidyverse
library(tidyverse)

#Data from R packages - dplyr in this case
data(package = "dplyr")

#Data from CSV in a url
url<-"http://gattonweb.uky.edu/sheather/book/docs/datasets/magazines.csv"
nyc <- read_csv(url)
dim(nyc)

#Writing CSVs - to write a data frame to a CSV
write_csv(magazines, path = "magazines.csv")

#Loading CSV from local computer
getwd() #to help find out the work directory where file is located
mags <- read_csv("magazines.csv")

#Data from excel - read_excel cannot read files from a url, hence they need to be downloaded and 
src <- "http://gattonweb.uky.edu/sheather/book/docs/datasets/GreatestGivers.xls"
lcl <- basename(src)
download.file(url = src, destfile = lcl)
library(readxl)
philanthropists <- read_excel(lcl)

#saving and loading data
save(magazines, file = "magazines.rda", compress = "xz")
load("~/magazines.rda")



##Data wrangling

#An important and often time consuming step of any data analysis is ¡§data wrangling,¡¨ or the process of cleaning up the dataset. This process is often required before any meaningful data exploration can be carried out.
#The dplyr package has a concise set of operations to peform this step. 

#illustrating through gapminder. Loading gapminder. 
library(gapminder)

#pipe operator
1:100 %>% sum
1:10 %>% rnorm(10, mean=.)

#nested and pipe
## pipe
rnorm(1, mean=10) %>%
  mean() %>%
  rnorm(100, mean=.) %>%
  density() %>%
  plot()

#dplyr verbs
#select(): to select variables, or columns of the data frame
#filter(): to select obserations, or rows of the data frame.
#arrange(): to order the observations.
#mutate(): to modify a variable or create a new one.
#summarize(): to summarize the values of a variable

#Another important function is group_by(), which changes the behavior of the other five functions to operate at the group level rather than on the full dataset.

#We will illustrate the five verbs by trying to answer a few interesting questions with the gapminder dataset:
  
#  What were the 5 richest countries in Europe in 1997?
#  What was the total GDP of Japan in 1962?
#  What was the average life expectancy for each continent in 2007?
#  How did the average life expectancy of each continent change each year between 1962 and 1997?

#  What were the 5 richest countries in Europe in 1997?
#To answer this question we need to:
#Select the observations corresponding to European countries in the year 1997;
#select the variables related to gdp and country;
#order the countries by gdp.

gapminder %>%
  filter(continent == "Europe") %>%
  filter(year == 1997) %>%
  select(country, gdpPercap) %>%
  arrange(desc(gdpPercap)) %>%
  head(n = 5)


#Total GDP of Japan in 1962
#To answer this question we need to:
#Select the observations corresponding to Japan in the year 1962;
#multiply per capita GDP by total population.
gapminder %>%
  filter(country == "Japan" & year == 1962) %>%
  mutate(totalGDP = gdpPercap * pop) %>%
  pull(totalGDP)

#Average life expectancy for each continent in 2007
#To answer this question we need to:
#Select the observations corresponding to the year 2007;
#group the data by continent;
#compute the average.
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(averageExp = mean(lifeExp))

#Life expectancy per continent 1962-1997
gapminder %>%
  filter(year >= 1962 & year <= 1997) %>%
  group_by(continent, year) %>%
  summarize(averageExp = mean(lifeExp))


#Data Visualization and Data Analysis

#Explore the trends in life expectancy for each continent over the years. We can use group_by and summarize to compute the average life expectancy per each continent between 1962 and 2007.
gapminder %>%
  group_by(continent, year) %>%
  summarize(averageExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = averageExp, group=continent,
             color=continent)) +
  geom_line()

#plotting for each country instead of each continent
theme_set(theme_classic())
gapminder %>%
  ggplot(aes(x = year, y = lifeExp, group=country,
             color=continent)) +
  geom_line()

#the above plot lead to overplotting, so better to use facers
gapminder %>%
  ggplot(aes(x = year, y = lifeExp, group=country,
             color=continent)) +
  geom_line() +
  facet_wrap(~continent, nrow = 2)

#plotting only the average curve and range for each continent
gapminder %>%
  group_by(continent, year) %>%
  summarize(averageExp = mean(lifeExp), 
            minExp = min(lifeExp),
            maxExp = max(lifeExp)) %>%
  ggplot(aes(x = year, y = averageExp, group=continent,
             fill=continent)) +
  geom_ribbon(aes(ymin = minExp, ymax = maxExp, alpha = 0.5)) +
  geom_line() + theme(legend.position="none") +
  facet_wrap(~continent, nrow = 2)

#relation between 2 variables - life expectancy and GDP for 1962. We can also color the points by continent and have the size of the point proportional to the country¡¦s population.
gapminder %>%
  filter(year == 1962) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, size=pop)) +
  geom_point() + scale_x_log10()

#adding labels instead
gapminder %>%
  filter(year == 1962) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, size=pop, label=country)) +
  geom_label() + scale_x_log10()

