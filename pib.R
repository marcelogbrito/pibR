library(modelr)
library(tidyverse)
library(gapminder)
head(gapminder)
dim(gapminder)
nlevels(gapminder$country)
length(unique(gapminder$year))
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)
gapminder %>%
  ggplot(aes(year, gdpPercap, group = country)) +
  geom_line(alpha = 1/3)
gap.dt <- filter(gapminder, year == "2002")

ggplot(gap.dt, aes(x=gdpPercap, y=lifeExp)) +
  geom_point(size=2, shape=23) +
  labs(title="Expectativa de vida x PIB per capita",
       x="PIB per capita", y = "Expectativa d evida")

ggplot(gap.dt, aes(x=gdpPercap, y=lifeExp)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(title="Expectativa de vida x PIB per capita",
       x="PIB per capita", y = "Expectativa d evida")

ggplot(gap.dt, aes(x=log(gdpPercap), y=lifeExp)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm , color="red", se=FALSE) +
  labs(title="Expectativa de vida x PIB per capita",
       x="PIB per capita", y = "Expectativa d evida")

#By modifying the scale of the x-axis to log one, we can bypass the accumulation of data points, making data visualization more friendly to the eye.

#In conclusion, we can clearly see how there is a direct relationship between a country's income and life expectancy, with people in richer countries living longer lives on average.

# separa 70% pra treino e 30% pra teste

data_split = sort(sample(nrow(gap.dt), nrow(gap.dt)*.7))

treino <- gap.dt[data_split,]

teste <- gap.dt[-data_split,]


lm(formula = gdpPercap ~ lifeExp, data = gap.dt)

# salvando modelo
gdp_le_model <- lm(gdpPercap ~ lifeExp, data = treino)
print(gdp_le_model)

summary(gdp_le_model)

# salvando modelo com log na variavel x
gdp_le_log_model <- lm(log(gdpPercap) ~ lifeExp, data = treino)
print(gdp_le_log_model)

summary(gdp_le_log_model)

ggplot(treino, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = "#69b3a2")) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)


ggplot(treino, aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(color = "#69b3a2")) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)


# https://mawds.github.io/r-tidyverse-intro/06-model-fitting/

