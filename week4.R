#Video One

library(gapminder)

library(dplyr)
library(ggplot2)

library(gridExtra)

library(ggthemes)
library(ggrepel)

str(gapminder)

gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(continent)) +
  geom_bar(colour="steel blue", fill = "steel blue")

?geom_bar

# ggplot has 3 prerequisites, aesthetics , type , formatting
# piping avoids need to specify data in ggplot
# arguemnts inside geombar are nice to haves

# ggplot uses layers that are separated by +

# Numeric Continous
#frq, density, box whisker

gapminder %>% filter(year == 1952 ) %>% 
  ggplot() +
  geom_histogram(aes(gdpPercap))

# aes can go in either gplot or geom


gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(gdpPercap)) +
  geom_histogram(binwidth = 5000)

#geom density good for continuious data
gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(gdpPercap)) +
  geom_density()

#box and whisker
gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(gdpPercap)) +
  geom_boxplot()


# filter out the outlier
# visulaisations to understand the date
gapminder %>% filter(year == 1952 ) %>%  filter(gdpPercap < 30000) %>% 
  ggplot(aes(gdpPercap)) 
  geom_boxplot()

gapminder %>% filter(year == 1952 ) %>%  filter(gdpPercap >= 30000)

# relations between variables

# 2 var box jitter scatter correlation regression
# example 1 geom_boxplot for box plot
gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(continent, lifeExp)) +
  geom_boxplot()

# example 2
gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(lifeExp, group=continent, col=continent)) +
  geom_boxplot()

# group for different box plots by continent, col(our) to colour them differently

# Jitter
# scatterplot for geom_point
gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(continent, lifeExp)) +
  geom_point() +
  geom_jitter(alpha=0.5) #the alpha affects the transparency
#example of two different geoms as layers

gapminder %>% filter(year == 1952 ) %>% 
  ggplot(aes(continent, lifeExp)) +
  geom_jitter(alpha=0.5) #the alpha affects the transparency
#jitter allows points to spread


## two continuous variables: scatter plots
gapminder %>% filter(year == 1952 ) %>%  filter(gdpPercap < 30000) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point()

# correlations
pairs(gapminder[,3:6])
# scatter plot matrix

# line of best fit: geom_smooth
gapminder %>% filter(year == 1952 ) %>%  filter(gdpPercap < 30000) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point() +
  geom_smooth()
# can configure Line of fit

## Compositions and comparisons within a variable
# group colour fill
# Colour to differentiate values
gapminder %>% ggplot(aes(year, lifeExp, col=country)) +
  geom_line(show.legend = F)
  
# Fill to differentiate values
gapminder %>% ggplot(aes(continent, fill=country)) +
  geom_bar(show.legend = F)

gapminder_aus <- gapminder %>% filter(country == "Australia")

gapminder_aus %>% ggplot(aes(year, lifeExp, col=country)) +
  geom_line(show.legend = F)

ggplot() +
  geom_line(data = gapminder    , aes(year, lifeExp, group=country)) +
  geom_line(data = gapminder_aus, aes(year, lifeExp), colour="blue")

#Let Maggie know that can have any colour so long as its red if colour is in aes
# 
ggplot() +
  geom_line(data = gapminder    , aes(year, lifeExp, group=country)) +
  geom_line(data = gapminder_aus, aes(year, lifeExp, colour="blue"))



##Comparison
gapminder %>% filter(country == "Australia") %>% 
  ggplot(aes(year,lifeExp)) +
  geom_line()

# facet grid facet wrap comparisons between groups

# line plots by continent groups for countries
ggplot(gapminder, aes(x=year, y = lifeExp, group = country ))+
  geom_line() +
  facet_grid(.~ continent)
# continent groups are "columns"

# now continent groups are "rows"
ggplot(gapminder, aes(x=year, y = lifeExp, group = country ))+
  geom_line() +
  facet_grid(continent ~ .)

# facet wrap when there is clumping, or because its just better than facet_grid either option

ggplot(gapminder, aes(x=year, y = lifeExp, group = country ))+
  geom_line() +
  facet_wrap(.~ continent)

ggplot(gapminder, aes(gdpPercap, lifeExp, colour=year ))+
  geom_point() +
  facet_wrap(.~ continent)

#gridarrange :: gridextra

gapminder_c1 <- gapminder %>% filter(country == "Australia") %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line()

gapminder_c2 <- gapminder %>% filter(country == "United States") %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line()

gapminder_c3<- gapminder %>% filter(country == "Japan") %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line()

gapminder_c4 <- gapminder %>% filter(country == "United Kingdom") %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line()

#Note the facet scales differ
gapminder_grid <- grid.arrange(gapminder_c1, gapminder_c2, gapminder_c3,gapminder_c4, ncol=4)
gapminder_grid

# can facet wrap these charts so scales are consistent. Better practice than grid.arrange
gapminder_facet <- gapminder %>% filter(country == c("Australia","United States","Japan","United Kingdom")) %>% 
  ggplot(aes(year,lifeExp)) +
  geom_line() +
  facet_grid(.~country) #facet grid cols forces same vertical scale for cfs
gapminder_facet

#Scaling
ggplot(gapminder, aes(x=gdpPercap, y = lifeExp))+
  geom_point(size=0.25) +
  scale_x_log10()

ggplot(gapminder, aes(x=gdpPercap, y = lifeExp))+
  geom_point(size=3) +
  scale_x_continuous(trans = "log10")

#themes and labels
# xlab, ylab layers


gapminder %>% filter(year == 1952 & country %in% c("Australia","United States","Japan","United Kingdom")) %>% 
  ggplot(aes(gdpPercap,lifeExp, label=country)) +
#  geom_text_repel() + #this replaces geom_text and ensures
  geom_point()+
  xlab("GDP per Capita") +
  ylab("Life Expectancy [years]") +
  ggtitle("Plot of life expectancy for select countries")+
#  theme_classic() +
  theme_fivethirtyeight()+
  theme(plot.title = element_text(size=12, face="bold"))
  
# save plot, copy clipboard save as
#plot tab to right, click on export

ggsave("output/gapmider_grid.pdf",gapminder_grid)

