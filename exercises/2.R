# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)


## preparation ----------------------------------------------------------------
# !!!!! load the data !!!!!


# !!!!! compile the model !!!!!



## data separation and fitting ------------------------------------------------


# !!!!! filter 1970 - 1985 July data !!!!!


# !!!!! prepare 1970 - 1985 July data for Stan !!!!!


# fit
fit_old <- sampling(model, data = stan_data,
                        chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_old)
print(fit_old)

# extract
extract_old <- extract(fit_old)


# !!!!! filter 2000+ July data !!!!!


# !!!!! prepare 2000+ July data for Stan !!!!!


# fit
fit_recent <- sampling(model, data = stan_data,
                    chains = 1, iter = 1200, warmup = 200)

# examine fit
traceplot(fit_recent)
print(fit_recent)

# extract
extract_recent <- extract(fit_recent)


## analysis -------------------------------------------------------------------
# join all data in a data frame
df_results <- data.frame(mu=extract_old$mu, group="1970-1985")
df_results <- rbind(df_results, data.frame(mu=extract_recent$mu, group="2000+"))

# plot theta density
ggplot(data=df_results, aes(x=mu, fill=group)) +
  geom_density(color=NA, alpha=0.6) +
  scale_fill_brewer(type="qual") +
  xlab("success rate")


# !!!!! probability that 1970-1985 temperature is lower !!!!!


# !!!!! difference in temperature !!!!!
difference <- #!!!!!

# !!!!! 95% CI for temperature difference !!!!!


# histogram
# create a data frame
df_difference <- data.frame(difference=difference)
# plot
ggplot(data=df_difference, aes(x=difference)) +
  geom_histogram(alpha=0.6, bins=60) +
  xlab("difference in temperature") +
  xlim(-5, 5)