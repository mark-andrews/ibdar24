library(tidyverse)
library(brms)

source("https://raw.githubusercontent.com/mark-andrews/ibdar24/main/data/dl_data.R")
source("https://raw.githubusercontent.com/mark-andrews/ibdar24/main/data/sim_data.R")

M_3 <- lm(weight ~ height + gender + age, data = weight_df)
summary(M_3)
round(summary(M_3)$coefficient, 3)
round(confint(M_3), 3)

M_4 <- brm(weight ~ height + gender + age, data = weight_df)
M_4
# just the regression coefs
round(fixef(M_4), 3)

prior_summary(M_4)
summarise(weight_df, median(weight), mad(weight))

# draw samples from prior over sigma (residual sd)
abs(rt(n = 100, df = 3) * 15.7) %>% quantile()
