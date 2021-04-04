#1 i
#anlayse Gapminder all
#“for each of the five pairs of countries below, which country do you think has the
#highest child mortality rates? Which pairs do you think are most similar?”.

gapminderallcsv <- read.csv("gapminderall.csv")

library(psych)
library(dplyr)
describe(gapminderallcsv)
inf_mort_00 <- gapminderallcsv %>% 
  mutate(infant_mortality= ifelse(is.na(infant_mortality),0,infant_mortality)) %>% 
  select(country, infant_mortality) %>% filter(country == c("Sri Lanka", "Turkey"))

inf_mort_01 <- gapminderallcsv %>% 
  filter(!is.na(infant_mortality)) %>%   select(country, infant_mortality, year ) 


describe(inf_mort_00)



# number of distinct values
inf_mort_01 %>% summarise(n_distinct(country))

inf_mort_02 <- gapminderallcsv %>% 
  filter(!is.na(infant_mortality)) %>% 
  group_by(country ) %>% summarise(maxYear = max(year), min_im = min(infant_mortality) )

describe(inf_mort_02)
head(inf_mort_02)
tail(inf_mort_02)

inf_mort_03 <- inf_mort_02 %>% 
  filter(country %in% c("Sri Lanka", "Turkey","Thailand", "South Africa","Poland", "Korea, Rep.","Malaysia", "Russia","Pakistan", "Vietnam"))
inf_mort_03 %>% arrange(min_im) 
  
inf_mort_02 %>% filter(country %in% c("Sri Lanka", "Turkey"))

inf_mort_02 %>% filter(country %in% c("Poland", "Korea, Rep."))

inf_mort_02 %>% filter(country %in% c("Malaysia", "Russia"))

inf_mort_02 %>% filter(country %in% c("Pakistan", "Vietnam"))

inf_mort_02 %>% filter(country %in% c("Thailand", "South Africa"))


#Is it reasonable to characterise today’s world into rich western nations with long
#lifespans and small families versus the developing world of Africa, Asia and Latin
#America with shorter lifespans and large families?

library(ggplot2)
library(gridExtra)
library(ggthemes)
library(ggrepel)


gapminder_all_gdpcap <- gapminderallcsv %>% 
  mutate(gdp_per_capita = gdp / population ) %>% 
  filter(!is.na(gdp)) #%>% 
  #select(gdp_per_capita, life_expectancy, fertility, continent, country,year)

#graph 1
max(gapminder_all_gdpcap$year)
gapmin_la <- gapminder_all_gdpcap %>% filter(year %in% c(2011L))

#graph2
gapminderallcsv %>% filter(year %in% c(1960L, 2015L)) %>%  
  ggplot(aes( fertility,life_expectancy,colour=continent)) + geom_point() +  facet_grid(continent~year)


#graph3
gapminderallcsv %>% filter(year %in% c(1960L, 2015L)) %>%  
  ggplot(aes( fertility,life_expectancy,colour=continent)) + geom_point(show.legend = F) +  facet_grid(~year)

#graph 4
fltr_lst <- c(1962,1980,1990,2000,2015)

gapminderallcsv %>% filter(year %in% fltr_lst & continent %in% c("Asia", "Europe")) %>%  
  ggplot(aes( fertility,life_expectancy,colour=continent)) + geom_point() +  facet_grid(.~year)


describe(gapminderallcsv)
describe(gapminder_all_gdpcap)

#part iii
#exploratory data analysis for the linear model of life expectancy
# first analyse correlation of numeric variables, then show scatter plots of pairs of individual variables
# try to find a linear relationship even if you have to log transform some of the expalnatory variables

#Let's start off with a correlation scatter plot matrix of the numeric variables in gapminder_all
cor(gapminderallcsv[, 3:8], use="pairwise.complete.obs") 
#picked columns 3:6 which are the numeric ones 

pairs(gapminderallcsv[, 3:8])

#plos of individual variabels
gapminderallcsv %>% ggplot(aes(infant_mortality, life_expectancy)) +
  geom_point()   
#finding: pretty linear relationship between infant mortality and life expectancy

gapminderallcsv %>% ggplot(aes(infant_mortality, fertility)) +  geom_point()   
#finding 2 fertility maxes out before inf mort does

gapminderallcsv %>% ggplot(aes(life_expectancy, fertility)) +  geom_point()   
# finding less fertile, live longer? correlation or causation

# neither very linear
gapminderallcsv %>% ggplot(aes(life_expectancy, gdp)) +  geom_point()   
gapminder_all_gdpcap %>% ggplot(aes(life_expectancy, gdp_per_capita)) +  geom_point()   

gapminderallcsv %>% ggplot(aes(population, life_expectancy, colour = year)) +  geom_point()   
# finding less fertile, live longer? correlation or causation

#Look for better variables
pairs(gapminder_all_gdpcap[,c(3:8,11)])

gapminder_all_gdpcap %>% ggplot(aes(life_expectancy, gdp_per_capita)) +  geom_point()   

gapminder_all_gdpcap02 <- gapminder_all_gdpcap %>% mutate(log_gdp_pc = log(gdp_per_capita))
cor(gapminder_all_gdpcap02[,c(3:8,11:12)], use="pairwise.complete.obs")
# note the , use="pairwise.complete.obs" strips out the NAs

pairs(gapminder_all_gdpcap02[,c(3:8,11:12)])

gapminder_all_gdpcap02 %>% ggplot(aes(log_gdp_pc,life_expectancy,colour = continent))+geom_point()

#Part 1 part iv
# Create a graph of fertility on the x axis and life expectancy on the y axis by missing
# values and non-missing values using the linear regression and mean missing value
# imputation methods shown in the Learning Guide. 

#basic dot plot
gapminder_all_gdpcap02 %>% ggplot(aes(fertility,life_expectancy))+geom_point()

gapminderallcsv %>% ggplot(aes(fertility,life_expectancy))+geom_point()

# apply imputation learnings as raw file has 187 missing rows

# Missing Values management
library(simputation)
??simputation

## identify NA
#packages to handle NAs
library(naniar)

# naniar:miss_var_summary

miss_var_summary(gapminderallcsv)
# some missing fertility values
# mean missing value
gapminderallcsv_01 <- gapminderallcsv %>% 
  mutate(fertility  = ifelse(is.na(fertility)  ,0      , fertility  ))

gapminderallcsv_01 %>% ggplot(aes(fertility,life_expectancy,colour = continent))+geom_point()

# Use mean this time
gapminderallcsv_02 <- gapminderallcsv %>% 
  mutate(fertility  = ifelse(is.na(fertility)  ,mean(fertility , na.rm=TRUE) , fertility  ))

gapminderallcsv_02 %>% ggplot(aes(fertility,life_expectancy,colour = continent))+geom_point()

gapminderallcsv_03 <- gapminderallcsv %>% 
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_proxy(fertility~mean(fertility,na.rm=TRUE))

gapminderallcsv_03 %>% filter(fertility_NA %in% c("NA") ) %>% 
  ggplot(aes(fertility,life_expectancy,colour = continent))+geom_point()
# Still not so great

## Try linear regression

gapminderallcsv_04 <- gapminderallcsv %>% 
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_lm( fertility       ~ life_expectancy ) %>% 
  impute_lm(infant_mortality ~ life_expectancy + fertility ) %>% 
  impute_lm(gdp ~ life_expectancy + year             )
miss_var_summary(gapminderallcsv_04)

str(gapminderallcsv_04)

gapminderallcsv_04 %>% filter(fertility_NA %in% c("NA") ) %>% 
  ggplot(aes(fertility,life_expectancy,colour = continent))+geom_point()


gapminderallcsv_04 %>% 
  ggplot(aes(fertility,life_expectancy,colour = fertility_NA))+geom_point()




