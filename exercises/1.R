# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("data/basketball_shots.csv", sep=";")

# compile the model
model <- stan_model("bernoulli.stan")


## default rim, player1 -------------------------------------------------------
default_rim <- data %>% filter(SpecialRim == 0 & PlayerID == 1)
y <- default_rim$Made
stan_data <- list(y = y,
                  n = length(y))

fit_default <- sampling(model, data = stan_data,
                chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_default, inc_warmup=TRUE)
print(fit_default)

# extract
extract_default <- extract(fit_default)
theta_default <- extract_default$theta

# probability that the player hits more than 50% of shots
mcse(theta_default > 0.5)


## smaller rim, player1 -------------------------------------------------------
smaller_rim <- data %>% filter(SpecialRim == 1& PlayerID == 1)
y <- smaller_rim$Made
stan_data <- list(y = y,
                  n = length(y))

fit_smaller <- sampling(model, data = stan_data,
                        chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_smaller, inc_warmup=TRUE)
print(fit_smaller)

# extract
extract_smaller <- extract(fit_smaller)
theta_smaller <- extract_smaller$theta

# probability that the player hits more than 50% of shots
mcse(theta_smaller > 0.5)


## compare --------------------------------------------------------------------
df_results <- data.frame(theta=theta_default, rim="default")
df_results <- rbind(df_results, data.frame(theta=theta_smaller, rim="smaller"))

# plot theta density
ggplot(data=df_results, aes(x=theta, fill=rim)) +
  geom_density(color=NA) +
  scale_fill_brewer(type="qual") +
  xlim(0, 1) +
  xlab("success rate")

# probability that success on default rim is larger
mcse(theta_default > theta_smaller)

# difference in success rate
mcse(theta_default - theta_smaller)

# 95% CI
quantile(theta_default - theta_smaller, probs = c(0.05, 1))
