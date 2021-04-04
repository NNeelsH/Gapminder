#Week 4 Video 2 Data Processing issues
# Missing Values management
library(simputation)
??simputation

## identify NA
#packages to handle NAs
library(naniar)

# naniar:miss_var_summary
miss_var_summary(gapminder_all )
miss_case_summary(gapminder_all)                 

# if don;t want to use packages
sapply(gapminder_all, function(x){sum(is.na(x))})

## Process NAs
#drop NAs
gapminder_naomit <- na.omit(gapminder_all)

# imputation
# 0 -0-0001 mean 

gapminder_zeroNA <- gapminder_all %>% 
  mutate(infant_mortality = ifelse(is.na(infant_mortality),0,infant_mortality),
         gdp        = ifelse(is.na(gdp)        ,0      , gdp  ),
         fertility  = ifelse(is.na(fertility)  ,0      , fertility  ),
         population = ifelse(is.na(population) ,0      , population  ) )
miss_var_summary(gapminder_zeroNA)

# need to remove NAs from mean()
gapminder_meanNA <- gapminder_all %>% 
  mutate(infant_mortality = ifelse(is.na(infant_mortality),mean(infant_mortality,na.rm=TRUE),infant_mortality),
         gdp        = ifelse(is.na(gdp)        ,mean(gdp       , na.rm=TRUE)       , gdp  ),
         fertility  = ifelse(is.na(fertility)  ,mean(fertility , na.rm=TRUE) , fertility  ),
         population = ifelse(is.na(population) ,mean(population, na.rm=TRUE), population  ) )
miss_var_summary(gapminder_meanNA)

# need to remove NAs from median()
gapminder_medianNA <- gapminder_all %>% 
  mutate(infant_mortality = ifelse(is.na(infant_mortality),median(infant_mortality,na.rm=TRUE),infant_mortality),
         gdp        = ifelse(is.na(gdp)        ,median(gdp       , na.rm=TRUE)       , gdp  ),
         fertility  = ifelse(is.na(fertility)  ,median(fertility , na.rm=TRUE) , fertility  ),
         population = ifelse(is.na(population) ,median(population, na.rm=TRUE), population  ) )
miss_var_summary(gapminder_medianNA)

#simputation, bind adds new columns to id NAs called [varname_NA], !NA/NA the nas by cols, add shad: overall label missing/not missing called [any_missing]
gapminder_zeroreplace1 <- gapminder_all %>% 
  bind_shadow(only_miss=TRUE) %>% 
  add_label_shadow() %>% 
  impute_const(infant_mortality~0) %>% 
  impute_const(gdp~0)              %>% 
  impute_const(fertility~0)        %>% 
  impute_const(population~0)
miss_var_summary(gapminder_zeroreplace1)

gapminder_zeroreplace1 %>% select(infant_mortality_NA, gdp_NA,fertility_NA, population_NA, any_missing )

#use base R indexing to find NAs and 0
gapminder_zeroreplace2 <- gapminder_all
gapminder_zeroreplace2[is.na(gapminder_zeroreplace2)]<- 0

#impute all with mean
gapminder_meanreplace <- gapminder_all %>% 
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_mean_all()
miss_var_summary(gapminder_meanreplace)

#naniar, bind adds new columns to id NAs called [varname_NA], !NA/NA the nas by cols, add shad: overall label missing/not missing called [any_missing]
gapminder_medianreplace <- gapminder_all %>% 
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_median_all()
miss_var_summary(gapminder_medianreplace)

# imputing a mix of variable treatments. Constant is arbitrary
gapminder_mixreplace <- gapminder_all %>% 
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_const(fertility~5) %>% 
  impute_proxy(population~mean(population,na.rm=TRUE)) %>% 
  impute_proxy(infant_mortality~median(infant_mortality,na.rm=TRUE)) %>% 
  impute_proxy(gdp~mean(gdp,na.rm=TRUE))
miss_var_summary(gapminder_mixreplace)
#impute_proxy needs a calculation

#  linear imputation
# need to build up variables in the right order as cant lm NAs
miss_var_summary(gapminder_all)
gapminder_lm <- gapminder_all %>% 
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_lm( fertility       ~ life_expectancy ) %>% 
  impute_lm(population       ~ life_expectancy + fertility + year ) %>% 
  impute_lm(infant_mortality ~ life_expectancy + fertility ) %>% 
  impute_lm(gdp ~ life_expectancy + year             )
miss_var_summary(gapminder_lm)

# Algo to impute a missing value

###############################################################################
# Outliers
# do nothing, drop, cap, impute the outlier


# imputing similar to missing values
#can mutatte ifelse
# can filter out outliers

# pmin and pmax to cap outliers
x <- c(1,2,3,100)
pmin(x,5)

y <- c(-100,1,2,3)
pmax(y,0)

# each element subject to min max of limit or itslef


# Correlations - see learning guide

######################################################################
######################################################################
#One-hot Encoding() needed by Python

gapminder_all_onehot <- gapminder_all %>% 
  mutate(europe = ifelse(continent == "Europe", 1, 0)) %>%
  mutate(africa = ifelse(continent == "Africe", 1, 0)) %>% 
  mutate(america = ifelse(continent == "Americas", 1, 0)) %>% 
  mutate(asia = ifelse(continent == "Asia", 1, 0)) %>% 
  mutate(oceania = ifelse(continent == "Oceania", 1, 0)) 
  
  gapminder_all_onehot %>% select(continent,europe,africa,america,asia,oceania)
  
# Lin regressions in R done in background
# dummy encoding done in lin reg
# dummy encoding drops the last one hot cat as not needed
  
## too many variables (high cardinality0 then binning encoding)
  
#binning encoding example
  length(unique(gapminder_all$region))
  
  #example uses domain knowlege as grouping by continents

  #1. define factors for each continent
  #europe <- c("etc")
  # gapminder_all_group <- gapminder_all %>% 
  #   mutate(continent_new = case_when(region %in% factor_Europe ~ "Europe"))
  
  # data driven method
  #look at freq of cat variable of interest group low freq togehter
  
  gapminder_all_lifegroup <- gapminder_all %>% 
    mutate(lifeExprange = case_when(life_expectancy <= 50 ~ "life50under",
                                    life_expectancy > 50  ~ "life50over"  ))
  
  gapminder_table <- gapminder_all_lifegroup %>% select(region,lifeExprange) %>% 
    table()
    #table to summarise
  gapminder_table
  
  # define loe LE as <35% etc
  gapminder_proptable <- prop.table(gapminder_table ,1 )# add 1 so that you get the value of each cell as  a% of row
  gapminder_proptable
  
  #convert the proportions table to a tibble so can process with gapminder
  gapminder_proptibble <- as_tibble(gapminder_proptable)
  
  # do some filtering as life50over the category being recut
  # rename new var = old var name
  gapminder_proptibble2 <- gapminder_proptibble %>% filter(lifeExprange == "life50over") %>% rename(proportion = n)
View(gapminder_proptibble2)  
  
gapminder_all_joined <- left_join(gapminder_all, gapminder_proptibble2, by ="region")
  glimpse(gapminder_all_joined)
  
#set up categories based on numeric criteria
 gapminder_all_v2 <-  gapminder_all_joined %>% mutate(regiongroups = case_when(proportion < 0.6 ~ "lowLERegion" , 
                                                           proportion >= 0.6 & proportion < 0.8 ~ "midLEregion" ,
                                                           proportion >= 0.8 ~ "highLEregion" ))
  
  #one-ot encode new regional variables
 gapminder_all_v3 <-  gapminder_all_v2 %>% mutate(lowLERegion = ifelse(regiongroups =="lowLERegion",1,0)) %>% 
   mutate( midLEregion = ifelse(regiongroups =="midLEregion" ,1,0)) %>%
   mutate(highLEregion = ifelse(regiongroups =="highLEregion",1,0))
 
 #############################
 # high numnber of variables
 # principal component analysis
 
gapminder_all_omit <- na.omit(gapminder_all)
 #now run the numeic variables through pca
 gapminder_allpca <- prcomp(gapminder_all_omit[,c(3,4,6,7,8)], center = TRUE, scale. = TRUE )
 #use summary to id variables to drop
 summary(gapminder_allpca)

  str(gapminder_allpca)
  gapminder_allpcavalues <- as_tibble(gapminder_allpca$x)
  gapminder_allpcavalues
  
  # combine the pcs columns with gapminder dataset
 gapminder_allpca2 <- bind_cols(gapminder_all_omit , gapminder_allpcavalues)
 # select the columns of use for the model
 gapminder_allpca23 <- gapminder_allpca2[,c(5,11:14)]
 
 
 
 ############################
 
 
 
 
 
 