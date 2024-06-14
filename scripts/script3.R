library(tidyverse)
library(brms)

source("https://raw.githubusercontent.com/mark-andrews/ibdar24/main/data/dl_data.R")
source("https://raw.githubusercontent.com/mark-andrews/ibdar24/main/data/sim_data.R")

M_3 <- lm(weight ~ height + gender + age, data = weight_df)
summary(M_3)
round(summary(M_3)$coefficient, 3)
round(confint(M_3), 3)

M_4 <- brm(weight ~ height + gender + age, 
           save_pars = save_pars(all = TRUE),
           data = weight_df)
M_4
# just the regression coefs
round(fixef(M_4), 3)

prior_summary(M_4)
summarise(weight_df, median(weight), mad(weight))

# draw samples from prior over sigma (residual sd)
abs(rt(n = 1e4, df = 3) * 15.7) %>% quantile() %>% round(3)

stancode(M_4) # view stan code produced by brms



# change priors
M_5 <- brm(weight ~ height + gender + age, 
           prior = set_prior("normal(0, 100)"),
           data = weight_df)

M_5

round(fixef(M_5), 3)

new_priors <- c(
  set_prior(prior = 'normal(0, 10)', class = 'b', coef = 'age'),
  set_prior(prior = 'normal(0, 100)', class = 'b', coef = 'gendermale'),
  set_prior(prior = 'normal(0, 10)', class = 'b', coef = 'height'),
  set_prior(prior = 'normal(0, 100)', class = 'Intercept'),
  set_prior(prior = 'normal(0, 10)', class = 'sigma')
)

new_priors2 <- c(
  set_prior(prior = 'normal(0, 10)', class = 'b'),
  set_prior(prior = 'normal(0, 100)', class = 'Intercept'),
  set_prior(prior = 'normal(0, 10)', class = 'sigma')
)

M_6 <- brm(weight ~ height + gender + age, 
           prior = new_priors,
           data = weight_df)

waic(M_6)
loo(M_6)
bayes_factor()

# default priors: 4 predictors; compare to M_4
M_7 <- brm(weight ~ height + gender + age + race, 
           save_pars = save_pars(all = TRUE),
           data = weight_df)

waic(M_4, M_7)

M_8 <- lm(weight ~ height + gender + age + race, 
          data = weight_df)

anova(M_3, M_8)


loo(M_4, M_7)

bayes_factor(M_4, M_7, log = TRUE)


# Beyond the linear models --------------------------------------------------

M_9 <- brm(weight ~ height + gender + age + race, 
           family = student(),
           save_pars = save_pars(all = TRUE),
           data = weight_df)

waic(M_7, M_9)


ggplot(weight_df, aes(x = height, y = weight, colour = gender)) + geom_point()
