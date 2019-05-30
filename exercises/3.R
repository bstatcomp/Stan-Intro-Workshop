# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# !!!!! load the data !!!!!

# !!!!! compile the model !!!!!



## Finland --------------------------------------------------------------------

# !!!!! filter Finland July data !!!!!
data_fin <- !!!!!

# we can treat the smallest year as 0 (this is a trick to make regression faster and more stable)
min_year <- min(data_fin$year)
data_fin$year_adjusted <- data_fin$year - min_year

# !!!!! prepare Finland July data for Stan !!!!!

# fit
fit_fin <- sampling(model, data = stan_data,
                    chains = 1, iter = 2000, warmup = 1000)

# examine fit
traceplot(fit_fin)
print(fit_fin)

# extract
extract_fin <- extract(fit_fin)

# plots
df_results_fin <- data.frame(a=extract_fin$a, b=extract_fin$b)

# lines and confidence in lines
ggplot() + 
  geom_point(data=data_fin, aes(x=year_adjusted, y=temperature), alpha=0.4, shape=16) +
  geom_abline(data=df_results_fin, aes(slope=b, intercept=a), alpha=0.02, size=1) +
  labs(x="year") +
  scale_x_continuous(breaks=seq(0, 5), labels=seq(2010, 2015)) +
  ylim(0, 25)

# plot slope density
ggplot(data=df_results_fin, aes(x=b)) +
  geom_density(color=NA, fill="#3182bd", alpha=0.6) +
  xlim(-1.5, 1.5)

# !!!!! probability that T is rising !!!!!

# !!!!! calculate the 95% CI of the relevant parameter, can we conclude anything substantial? !!!!!


## Slovenia -------------------------------------------------------------------
# !!!!! filter Slovenian July data !!!!!
data_si <- !!!!!
  
# we can treat the smallest year as 0 (this is a trick to make regression faster and more stable)
min_year <- min(data_si$year)
data_si$year_adjusted <- data_si$year - min_year

# !!!!! prepare Slovenian July data for Stan !!!!!

# fit
fit_si <- sampling(model, data = stan_data,
                    chains = 1, iter = 2000, warmup = 1000)

# examine fit
traceplot(fit_si)
print(fit_si)

# extract
extract_si <- extract(fit_si)

# plots
df_results_si <- data.frame(a=extract_si$a, b=extract_si$b)

# lines
ggplot() + 
  geom_point(data=data_si, aes(x=year_adjusted, y=temperature), alpha=0.4, shape=16) +
  geom_abline(data=df_results_si, aes(slope=b, intercept=a), alpha=0.01, size=1) +
  labs(x="year") +
  scale_x_continuous(breaks=seq(0, 120, 30), labels=seq(1900, 2020, 30)) +
  ylim(0, 25)

# plot slope density
ggplot(data=df_results_si, aes(x=b)) +
  geom_density(color=NA, fill="#3182bd", alpha=0.6) +
  xlim(0, 0.03)

# !!!!! probability that T is rising !!!!!

# !!!!! calculate the 95% CI of the relevant parameter, can we conclude anything substantial? !!!!!



## predict Slovenian temperature in 2019 --------------------------------------------
# get mean parameter values
a = mean(extract$a)
b = mean(extract$b)
sigma = mean(extract$sigma)

# generate 100000 samples for year 2019 based on mean parameter values
# rnorm generates random samples from the normal distribution
t_2019 = rnorm(100000, mean = a + (2019 - min_year) * b, sd = sigma)

# !!!!! calculate mean and 95% CI of the predicted 2019 temperature !!!!!

# !!!!! calculate the probability that temperature will be over 20 in 2019 !!!!!


## predict Slovenian temperature in 2070 --------------------------------------------
# generate 100000 samples for year 2070 based on mean parameter values
# rnorm generates random samples from the normal distribution
t_2070 = rnorm(100000, mean = a + (2070 - min_year) * b, sd = sigma)

# !!!!! calculate mean and 95% CI of the predicted 2070 temperature !!!!!

# !!!!! calculate the probability that temperature will be over 20 in 2070!!!!!

