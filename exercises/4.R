# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)
library(reshape2)


## preparation ----------------------------------------------------------------
# !!!!! load the data !!!!!

# !!!!! compile the model !!!!!


## expand the categorical variable -------------------------------------------
data <- data %>%
  mutate(stateNewYork = as.numeric(state == "NewYork"),
         stateFlorida = as.numeric(state == "Florida"),
         stateCalifornia = as.numeric(state == "California"))

# shorter
#states_prep <- model.matrix(~ State - 1, data=data)
#data <- cbind(data, states_prep)

## fit ------------------------------------------------------------------------
# !!!!! prepare  data for Stan !!!!!

# fit 
fit <- sampling(model, data = stan_data,
                chains = 1, iter = 2000, warmup = 1000)

# examine fit
traceplot(fit)
print(fit)

# extract
extract <- extract(fit)

# extract beta values and rename columns
df_betas <- data.frame(b=extract$b)
colnames(df_betas) <- colnames(X)

## where should we build our HQ -----------------------------------------------
df_location <- df_betas %>%
  select(stateNewYork, stateFlorida, stateCalifornia)
df_location <- melt(df_location)

# densities
ggplot(data=df_location, aes(x=value, fill=variable)) +
  geom_density(color=NA, alpha=0.6) +
  scale_fill_hue() +
  xlab("")


# !!!!! which state is the best, how confident are we in that finding !!!!!

# !!!!! how much more is the company expected to earn by selecting the best state !!!!!


## how should we invest our money ---------------------------------------------
df_investment <- df_betas %>%
  select(research, administration, marketing)
df_investment <- melt(df_investment)

# densities
ggplot(data=df_investment, aes(x=value, fill=variable)) +
  geom_density(color=NA, alpha=0.6) +
  scale_fill_hue() +
  xlab("")

# !!!!! how should the company invest their money !!!!!
