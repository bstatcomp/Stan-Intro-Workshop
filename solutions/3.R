# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# load the data
data <- read.csv("../data/temperature.csv", sep=";")

# compile the model
model <- stan_model("linear.stan")


## filter to summer months and fit --------------------------------------------
data <- data %>% filter(month == 7)


## Finland --------------------------------------------------------------------
# filter data
data_fin <- data %>% filter(country == "Finland")

# we can treat the smallest year as 0 (this is a trick to make regression faster and more stable)
min_year <- min(data_fin$year)
data_fin$year_adjusted <- data_fin$year - min_year

# prepare Finland data for Stan
x <- data_fin$year_adjusted
y <- data_fin$temperature
n <- length(y)
stan_data <- list(x = x,
                  y = y,
                  n = length(y))

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
  xlim(-3, 3)

# probability that T is rising
mcse(extract_fin$b > 0)

# based on the 95% CI we cannot conclude anything
quantile(extract_fin$b, probs = c(0.025, 0.975))


## Slovenia -------------------------------------------------------------------
# filter data
data_si <- data %>% filter(country == "Slovenia")

# we can treat the smallest year as 0 (this is a trick to make regression faster and more stable)
min_year <- min(data_si$year)
data_si$year_adjusted <- data_si$year - min_year

# prepare Slovenia data for Stan
x <- data_si$year_adjusted
y <- data_si$temperature
n <- length(y)
stan_data <- list(x = x,
                  y = y,
                  n = length(y))

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

# probability that T is rising
mcse(extract_si$b > 0)

# based on the 95% CI we can quite confidently conclude that the temperature is rising
quantile(extract_si$b, probs = c(0.025, 0.975))


## predict Slovenian temperature in 2019 --------------------------------------------
# get mean parameter values
a = mean(extract_si$a)
b = mean(extract_si$b)
sigma = mean(extract_si$sigma)

# generate 100000 samples for year 2019 based on mean parameter values
# rnorm generates random samples from the normal distribution
t_2019 = rnorm(100000, mean = a + (2019 - min_year) * b, sd = sigma)

# mean and error
mcse(t_2019)

# 95% CI
quantile(t_2019, probs = c(0.025, 0.975))

# probability that T is over 20
mcse(t_2019 > 20)


## predict Slovenian temperature in 2070 --------------------------------------------
# generate 100000 samples for year 2070 based on mean parameter values
# rnorm generates random samples from the normal distribution
t_2070 = rnorm(100000, mean = a + (2070 - min_year) * b, sd = sigma)

# mean and error
mcse(t_2070)

# 95% CI
quantile(t_2070, probs = c(0.025, 0.975))

# probability that T is over 20
mcse(t_2070 > 20)
