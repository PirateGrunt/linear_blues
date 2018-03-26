library(tidyverse)
knitr::opts_chunk$set(
  echo = FALSE
  , message = FALSE
)
munge_blues <- function(tbl) {
  tbl %>% 
    tidyr::gather(bar, chord, -row) %>% 
    mutate(bar = gsub('bar_', "", bar))
}

plt_blues <- function(tbl) {
  tbl %>% 
    ggplot(aes(bar, row)) +
    geom_tile(aes(fill = chord), color = 'white') + 
    geom_text(aes(label = chord), color = 'white', size = 10) +
    theme_void() + 
    theme(
      legend.position = 'none'
    ) +
    scale_y_reverse() +
    scale_fill_manual(values = c('#0000FF', '#CC8614', '#7914CC'))
}
tbl_blues <- tribble(
  ~row, ~bar_1, ~bar_2, ~bar_3, ~bar_4
  , 1 , 'I', 'I', 'I', 'I'
  , 2 , 'IV', 'IV', 'I', 'I'
  , 3 , 'V', 'V', 'I', 'I'
) %>% 
  munge_blues()
tbl_blues %>% 
  plt_blues()
tribble(
  ~row, ~bar_1, ~bar_2, ~bar_3, ~bar_4
  , 1 , 'I', 'I', 'I', 'I'
  , 2 , 'IV', 'IV', 'I', 'I'
  , 3 , 'V', 'IV', 'I', 'I'
) %>% 
  munge_blues() %>% 
  plt_blues()
tribble(
  ~row, ~bar_1, ~bar_2, ~bar_3, ~bar_4
  , 1 , 'I', 'I', 'I', 'I'
  , 2 , 'IV', 'IV', 'I', 'I'
  , 3 , 'V', 'IV', 'I', 'V'
) %>% 
  munge_blues() %>% 
  plt_blues()
set.seed(123)
sims <- 500
b_0 <- 10
b_1 <- 1.5
tbl_linear <- tibble(
    x = runif(sims, -10, 10)
  , e = rnorm(sims, 0, 2) 
) %>% 
  mutate(
    y = b_0 + b_1 * x + e
  )
tbl_linear %>% 
  ggplot(aes(x, y)) + 
  geom_point()
eval_epsilon <- function(parms) {
  est_epsilon <- tbl_linear %>% 
    mutate(est_epsilon = y - parms[1] - parms[2] * x) %>% 
    pull(est_epsilon)
  est_epsilon^2  %>% mean()
}

fits <- optim(
    par = c(10, 1.5)
  , eval_epsilon
)
fits

eval_epsilon(fits$par)
fit <- lm(y ~ 1 + x, data = tbl_linear)
b_0_fit <- coef(fit)[1]
b_1_fit <- coef(fit)[2]
tbl_linear <- tbl_linear %>% 
  mutate(
    y_adj = y - b_1_fit * x
  )
tbl_linear %>% 
  ggplot(aes(x, y_adj)) + 
  geom_point()
tbl_linear <- tbl_linear %>% 
  mutate(
      y_adj = y_adj - b_0_fit
    , y_adj_2 = y - b_0 - b_1 * x
  )

tbl_linear %>% 
  ggplot(aes(x, y_adj)) + 
  geom_point()
tbl_linear %>% 
  ggplot(aes(x, y)) + 
  geom_point()
tbl_linear %>% 
  ggplot(aes(x, y_adj)) + 
  geom_point()
tbl_linear <- tbl_linear %>% 
  mutate( 
      e_logis = rlogis(sims)
    , latent = b_0 + b_1 * x + e
    , y_logis = as.integer(latent > 0)
  )
tbl_linear %>% 
  ggplot(aes(x, y_logis)) + 
  geom_point()
tbl_linear %>% 
  ggplot(aes(x, y_logis)) + 
  geom_point() + 
  geom_smooth(method = glm,  method.args = list(family = "binomial"))
fit_logistic <- glm(
    y_logis ~ 1 + x
  , data = tbl_linear
  , family = binomial
)

coef(fit_logistic)
sims_poisson <- 5e3
tbl_poisson <- tibble(
  x = sample(100:1000, size = sims_poisson, replace = TRUE)
) %>% 
  mutate(
      eta = b_0 + b_1 * x 
    , y = rpois(sims_poisson, eta)
  )
tbl_poisson %>% 
  ggplot(aes(x, y)) +
  geom_hex()
fit_poisson <- glm(y ~ 1 + x, data = tbl_poisson, family = poisson(link = 'identity'))
summary(fit_poisson)
library(raw)
data("comauto")

tbl_reserve <- comauto %>% 
  filter(DevelopmentYear < 1997) %>% 
  group_by(Company, AccidentYear) %>% 
  arrange(Lag, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(
      PriorPaid = dplyr::lag(CumulativePaid)
    , IncrementalPaid = CumulativePaid - PriorPaid
  ) %>% 
  filter(Lag != 1) %>% 
  mutate(LagFactor = as.factor(Lag))

fit_reserves <- lm(
    CumulativePaid ~ 0 + PriorPaid:LagFactor
  , data = tbl_reserve
)

summary(fit_reserves)
fit_reserves_inc <- lm(
    IncrementalPaid ~ 0 + PriorPaid:LagFactor
  , data = tbl_reserve
)
summary(fit_reserves_inc)
tbl_linear <- tbl_linear %>% 
  mutate(
      exposure = x + rnorm(sims)
    , olep = exposure + rnorm(sims)
    , ep = olep + rnorm(sims)
  )
fit_exposure <- lm(
  y ~ 1 + exposure
  , data = tbl_linear
)
summary(fit_exposure)
fit_olep <- lm(
    y ~ 1 + olep
  , data = tbl_linear
)
summary(fit_olep)
fit_ep <- lm(
    y ~ 1 + ep
  , data = tbl_linear
)
summary(fit_ep)
summary(fit_poisson)
tbl_poisson <- tbl_poisson %>% 
  mutate(noise = rnorm(sims_poisson, 0, 10))

fit_poisson_2 <- glm(
    y ~ 1 + noise
  , data = tbl_poisson
  , family = poisson(link = 'identity'))
summary(fit_poisson_2)
