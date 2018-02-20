#' # R4DS Modeling Practice

#+ setup, echo = F
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)
#ezspin_pt(file_name = "model", project_directory = ".", file_folder = "munge", keep_html = F)

library(tidyverse)
library(knitr)
library(modelr)
options(na.action = na.warn)

#' ### Hypothesis generation vs hypothesis confirmation
#' 1. Each observation can either be used for exploration or confirmation, not both.
#' 2. You can use an observation as many times as you like for exploration, but you can only use it once for confirmation.
#' As soon as you use an observation twice, you’ve switched from confirmation to exploration.
#' 
#' We'll be overly optomistic every time if these 2 ideas aren't adhered to.
#' 
#' #### Split data into 3 pieces before we begin:
#' 1. 60%: training / exploration. Fit tons of models
#' 2. 20%: query set. Use to compare models or visuals by hand
#' 3. 20%: Test data. Can only use ONCE to test final model

#'### Simple Model 
#+ simple_model

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

sim1_mod <- lm(y ~ x, dat = sim1)

coef(sim1_mod)

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm')

#'### Visualizing Models
#' Understanding a model by looking at it's predictions
#' 
#' First, get an evenly space grid of values with `modelr::data_grid()`
#+ predictions

grid <- sim1 %>%
  data_grid(x)

grid

# add model predictions
grid <- grid %>%
  add_predictions(sim1_mod)

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(data = grid, aes(x = x, y = pred), color = "red", size = 1)

#'#### Residuals
#+ resid

sim1 <- sim1 %>%
  add_residuals(sim1_mod)

sim1

#' Draw a frequency polygon to understand spread of residuals
#+ resid_graphs

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

sim1_loess_mod <- loess(y ~ x, data = sim1)

grid <- grid %>%
  add_predictions(sim1_loess_mod)

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, aes(x, pred), color = "red")

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE)

#' #### Looking at gather and spread predictions
#+ gather_spread

#sample data
df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)

plot(df)

# 2 different models
m1 <- lm(y ~ x, data = df)
m2 <- lm(y ~ poly(x, 2), data = df)

grid <- data.frame(x = seq(0, 1, length = 10))

grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)

#'#### Categorical Variables
#+ cat_var

ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)

grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#'#### Interactions - continuous and categorical variable
#+ interact
ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

#' Two possible models below
#' `+`` looks at each effect independent of all others. Fitting the interaction and indvidual components with `*``
#+ two_models
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)

grid

# visualize model predictions

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

# look at residuals
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

#' Looking at the residuals, mod2 is clearly superior
#' 
#'#### Interactions - 2 continuous
#+ two_cont
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

# use `seq_range to create a regularly spaced grid`

seq_range(c(0.0123, 0.923423), n = 5)

seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2,n = 5, expand = 0.10)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

#'#### Transformations
#+ trans
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

# Fit 5 models
library(splines)
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

#'### Model Building
#+ load_stuff
library(nycflights13)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#' Why do low quality diamonds have higher prices?
#+ weight

# weight of the diamond is the confounding variable

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

#' Lets fit a model for price to separate out the effect of `carat`
#+ transform
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

# we've made this a linear pattern, which is easier to work with

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

# undo log to see raw predictions
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

# check residuals
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

# re graph motivating plots, just a a function of carat size
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

#'#### A more complicated model
#' Now we are going to create a model that also observe the effects of color, cut and clarity
#+ complicated
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(cut, lcarat = -0.515, color = "G", clarity = "SI1") %>%
  add_predictions(mod_diamond2)

grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

# visualize residuals

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

# look at biggest residuals

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

#' ## Flight Data
#' What affects the number of daily flights?
#+ flight_eda

daily <- flights %>%
  mutate(date = make_date(year, month, day), wday = wday(date, label = TRUE)) %>%
  group_by(date, wday) %>%
  summarise(n = n())

daily

ggplot(daily, aes(date, n)) +
  geom_line()

ggplot(daily, aes(wday, n)) +
  geom_boxplot()

#+ fit_model
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

# look at residuals

daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()

# there are patterns that the model is clearly missing

daily %>% 
  filter(resid < -100)

# lots of holidays

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)

#' #### Seasonal saturday effect
#+ sats

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

# add term to model
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

# overlay model predictons on raw data
# here we see that outliers are really affecting our estimates

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

#' bundle variable computation to avoid errors
#+ var_comp
compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

#'##### time of year: alternative approach
#+ toy
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()