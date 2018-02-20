#' # Many Models
#' From Chapter 25 of r4ds

#+ setup, echo = F
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)
#spin("many_models.R")

library(tidyverse)
library(knitr)
library(modelr)
options(na.action = na.warn)
library(gapminder)

#' How does life expectancy change over time for each country?
#+ initial_eda
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

#' First we'll create a model for a single country
#+ nz

nz <- gapminder %>% filter(country == "New Zealand")
nz_mod <- lm(lifeExp ~ year, data = nz)

nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear Trend + ")

nz %>% 
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining Pattern")

#' How do we fit that model to every country?
#+ each_country

#create nested data frame
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

#apply funciton to each country

by_country <- by_country %>%
  mutate(model = purrr::map(data, country_model))

by_country <- by_country %>%
  mutate(resids = purrr::map2(data, model, add_residuals))

resids <- unnest(by_country, resids)

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~continent)