
#q2
ggplot(data = gapminder)


#q5 - q8
chr_url <- "http://www.statsci.org/data/oz/ms212.txt"
pulserate <- read_tsv(chr_url)

summary(pulserate)
pulserate %>% ggplot(aes(Height,Pulse1,col=Gender)) + geom_point()

# Convert 1 to M and 2 to F

pulserate3 <- pulserate %>% mutate(Sex = ifelse(Gender==1, "M" , "F"))

pulserate3 %>% ggplot(aes(Height,Pulse1,col=Sex)) + geom_point(size=5)

pulserate3 %>% ggplot(aes(Height,Pulse1,alpha=Sex)) + geom_point()

pulserate3 %>% ggplot(aes(Height,Pulse1,col="blue")) + geom_point()


#q9 are these hte same?
gapminder %>% ggplot(aes(year, pop, fill = year)) + geom_bar(stat = "identity")
gapminder %>% ggplot(aes(year, pop)) + geom_bar(aes(fill = year), stat = "identity")
gapminder %>% ggplot(aes(year,pop, fill = year)) + geom_col()

#q10
gapminder %>% filter(country %in% c("Australia", "New Zealand")) %>% 
  ggplot(aes(year, pop, colour = country) )+ geom_line( )

#q11
gapminder %>% filter(continent == "Oceania") %>% ggplot(aes(year, pop, group = country)) + geom_line()

gapminder %>% filter(continent == "Oceania") %>% ggplot(aes(year, pop)) + geom_line(aes(group = country)) +
  geom_point()
