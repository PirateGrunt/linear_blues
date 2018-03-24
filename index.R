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
    geom_tile(aes(fill = chord), color = 'grey') + 
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
    x = runif(sims, 10, 20)
  , e = rnorm(sims, 0, 1) 
) %>% 
  mutate(
    y = b_0 + b_1 * x + e
  )
tbl_linear %>% 
  ggplot(aes(x, y)) + 
  geom_point()
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
tbl_logistic <- tibble( 
    e = rlogis(sims)
  , x = runif(sims, -10, 10)
) %>% 
  mutate(
      latent = b_0 + b_1 * x + e
    , y = as.integer(latent > 0)
  )
tbl_logistic %>% 
  ggplot(aes(x, y)) + 
  geom_point()
tbl_logistic %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = glm,  method.args = list(family = "binomial"))
sims <- 50e3
tbl_poisson <- tibble(
  x = sample(100:1000, size = sims, replace = TRUE)
) %>% 
  mutate(
      mojo = b_0 + b_1 * x 
    # , mojo = log(mojo)
    , y = rpois(sims, mojo)
  )
tbl_poisson %>% 
  ggplot(aes(x, y)) +
  geom_hex()
