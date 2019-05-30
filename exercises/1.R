# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# load the data
data <- read.csv("../data/basketball_shots.csv", sep=";")

# compile the model
model <- stan_model("bernoulli.stan")


## default rim, player1 -------------------------------------------------------
# !!!!! filter the data !!!!!


# !!!!! prepare data for Stan, store it in the stan_data variable !!!!!


# fit
fit_default1 <- sampling(model, data = stan_data,
                chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_default1, inc_warmup=TRUE)
print(fit_default1)

# extract parameter values
extract_default1 <- extract(fit_default1)
theta_default1 <- extract_default1$theta

# probability that player 1 hits more than 50% of shots
mcse(theta_default1 > 0.5)


## default rim, player1 -------------------------------------------------------
# !!!!! filter the data !!!!!


# !!!!! prepare data for Stan, store it in the stan_data variable !!!!!


# fit
fit_default2 <- sampling(model, data = stan_data,
                         chains = 1, iter = 1200, warmup = 200)

# extract
extract_default2 <- extract(fit_default2)
theta_default2 <- extract_default2$theta

# probability that player 2 hits more than 50% of shots
mcse(theta_default2 > 0.5)

## smaller rim, player1 -------------------------------------------------------
# !!!!! filter the data !!!!!


# !!!!! prepare data for Stan, store it in the stan_data variable !!!!!


# fit
fit_smaller1 <- sampling(model, data = stan_data,
                        chains = 1, iter = 1200, warmup = 200)

# extract
extract_smaller1 <- extract(fit_smaller1)
theta_smaller1 <- extract_smaller1$theta

# probability that player 1 hits more than 50% of shots on a smaller rim
mcse(theta_smaller1 > 0.5)


## compare player 1 vs 2 -----------------------------------------------------
# join data in a data frame
df_players <- data.frame(theta=theta_default1, player="1")
df_players <- rbind(df_players, data.frame(theta=theta_default2, player="2"))

# plot theta density for rim comparison
ggplot(data=df_players, aes(x=theta, fill=player)) +
  geom_density(color=NA) +
  scale_fill_brewer(type="qual") +
  xlim(0, 1) +
  xlab("success rate")

# probability that success on default rim is larger
mcse(theta_default2 > theta_default1)

# difference in success rate
difference <- theta_default2 - theta_default1
mcse(difference)

# 95% CI
quantile(difference, probs = c(0.025, 0.975))

# histogram
# create a data frame
df_player_diff <- data.frame(difference=difference)
# plot
ggplot(data=df_player_diff, aes(x=difference)) +
  geom_histogram(bins = 40) +
  xlim(-0.5, 0.5)


## compare small rim vs default rim -------------------------------------------
# join data in a data frame
df_rims <- data.frame(theta=theta_default1, rim="default")
df_rims <- rbind(df_rims, data.frame(theta=theta_smaller1, rim="smaller"))

# plot theta density for rim comparison
ggplot(data=df_rims, aes(x=theta, fill=rim)) +
  geom_density(color=NA) +
  scale_fill_brewer(type="qual") +
  xlim(0, 1) +
  xlab("success rate")

# probability that success on default rim is larger
mcse(theta_default1 > theta_smaller1)

# difference in success rate
mcse(theta_default1 - theta_smaller1)

# 95% CI
quantile(theta_default1 - theta_smaller1, probs = c(0.025, 0.975))

# histogram
# create a data frame
df_prim_diff <- data.frame(difference=theta_default1 - theta_smaller1)
# plot
ggplot(data=df_prim_diff, aes(x=difference)) +
  geom_histogram(bins = 80) +
  xlim(-1, 1)
