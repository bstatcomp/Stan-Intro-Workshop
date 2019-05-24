# libraries
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("data/temperature.csv", sep=";")

# compile the model
model <- stan_model("normal.stan")


## data separation and fitting ------------------------------------------------
data <- data %>% filter(month <= 8 & month >= 6)

# old
data_old <- data %>% filter(year > 1980 & year < 2010)
y <- data_old$temperature
n <- length(y)
stan_data <- list(y = y,
                  n = length(y))

fit_old <- sampling(model, data = stan_data,
                        chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_old)
print(fit_old)

# extract
extract_old <- extract(fit_old)

# recent
data_recent <- data %>% filter(year > 2010)
y <- data_recent$temperature
n <- length(y)
stan_data <- list(y = y,
                  n = length(y))

fit_recent <- sampling(model, data = stan_data,
                    chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_recent)
print(fit_recent)

# extract
extract_recent <- extract(fit_recent)


## analyse --------------------------------------------------------------------
df_results <- data.frame(mu=extract_old$mu, group="1980-2010")
df_results <- rbind(df_results, data.frame(mu=extract_recent$mu, group="2010+"))

# plot theta density
ggplot(data=df_results, aes(x=mu, fill=group)) +
  geom_density(color=NA, alpha=0.6) +
  scale_fill_brewer(type="qual") +
  xlab("success rate")

# probability that success on default rim is larger
mcse(extract_old$mu < extract_recent$mu)

# difference in temperature
mcse(extract_recent$mu - extract_old$mu)

# 95% CI
quantile(extract_recent$mu - extract_old$mu, probs = c(0.05, 1))
