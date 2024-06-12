library(priorexposure)

m <- 139 # number of heads
n <- 250 # number of coin flips

bernoulli_likelihood(n = n, m = m)

m_prime <- 14
n_prime <- 25

bernoulli_likelihood(n = n_prime, m = m_prime)

# Plot some beta distributions --------------------------------------------

beta_plot(alpha = 3, beta = 5)
beta_plot(alpha = 1, beta = 5)
beta_plot(alpha = 10, beta = 5)
beta_plot(alpha = 20, beta = 10)
beta_plot(alpha = 10, beta = 10)
beta_plot(alpha = 20, beta = 20)
beta_plot(alpha = 2, beta = 2)
beta_plot(alpha = 1, beta = 1)
beta_plot(alpha = 0.5, beta = 0.5)

# if we choose a beta(10, 10) prior then
# the posterior is 
beta_plot(m + 10, n - m + 10)

bernoulli_posterior_plot(n = n, m = m, alpha = 10, beta = 10)
bernoulli_posterior_plot(n = n, m = m, alpha = 10, beta = 10) + xlim(0.35, 0.65)

bernoulli_posterior_plot(n = n_prime, m = m_prime, alpha = 10, beta = 10)

# prior is uniform
beta_plot(1, 1)
bernoulli_posterior_plot(n = n, m = m, alpha = 1, beta = 1) + xlim(0.3, 0.7)

# compare to beta(10, 10) prior
bernoulli_posterior_plot(n = n, m = m, alpha = 10, beta = 10) + xlim(0.3, 0.7)



# mean of the posterior is 
# (m + alpha) / (n + alpha + beta)
# e.g.
(m + 10) / (n + 10 + 10)
bernoulli_posterior_summary(n = n, m = m, alpha = 10, beta = 10)


S <- bernoulli_posterior_summary(n = n, m = m, alpha = 10, beta = 10)

# Approx 95% area under curve range
S$mean + c(-1, 1) * 2 * S$sd

# 95% HPD
get_beta_hpd(m + 10, n - m + 10)

# 95% quantile interval
qbeta(0.025, m + 10, n - m + 10)
qbeta(0.975, m + 10, n - m + 10)



