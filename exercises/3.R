# libraries
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("data/temperature.csv", sep=";")

# compile the model
model <- stan_model("linear.stan")


## filter to summer months and fit fitting ------------------------------------
data <- data %>% filter(month <= 8 & month >= 6)
min_year <- min(data$year)
data$year_adjusted <- data$year - min_year

x <- data$year_adjusted
y <- data$temperature
n <- length(y)
stan_data <- list(x = x,
                  y = y,
                  n = length(y))

fit <- sampling(model, data = stan_data,
                    chains = 1, iter = 2000, warmup = 1000)

# examine fit
traceplot(fit)
print(fit)

# extract
extract <- extract(fit)


## plot -----------------------------------------------------------------------
df_results <- data.frame(a=extract$a, b=extract$b)

# lines
ggplot() + 
  geom_point(data=data, aes(x=year_adjusted, y=temperature), alpha=0.4, shape=16) +
  geom_abline(data=df_results, aes(slope=b, intercept=a), alpha=0.01, size=1) +
  labs(x="year") +
  scale_x_continuous(breaks=seq(0, 120, 30), labels=seq(1900, 2020, 30)) +
  ylim(0, 25)

# plot slope density
ggplot(data=df_results, aes(x=b)) +
  geom_density(color=NA, fill="#3182bd", alpha=0.6) +
  xlim(0, 0.03)


## temperature in 2019 --------------------------------------------------------
a = mean(extract$a)
b = mean(extract$b)
sigma = mean(extract$sigma)

# draw 10000 samples
t_2019 = rnorm(10000, mean = a + (2019 - min_year) * b, sd = sigma)

# mean and error
mcse(t_2019)

# 95% CI
quantile(t_2019, probs = c(0.025, 0.975))


## temperature in 2060 --------------------------------------------------------
# draw 10000 samples
t_2060 = rnorm(10000, mean = a + (2060 - min_year) * b, sd = sigma)

# mean and error
mcse(t_2060)

# 95% CI
quantile(t_2060, probs = c(0.025, 0.975))
