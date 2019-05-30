# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)
library(reshape2)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("../data/50_startups.csv")

# compile the model
model <- stan_model("multiple_linear.stan")


## expand the categorical variable -------------------------------------------
data <- data %>%
  mutate(stateNewYork = as.numeric(state == "NewYork"),
         stateFlorida = as.numeric(state == "Florida"),
         stateCalifornia = as.numeric(state == "California"))

# shorter
#states_prep <- model.matrix(~ State - 1, data=data)
#data <- cbind(data, states_prep)

## fit ------------------------------------------------------------------------
# prepare data for Stan
n <- nrow(data)
X <- data %>%
  select(research, administration, marketing, stateNewYork, stateFlorida, stateCalifornia)
k <- ncol(X)
y <- data$profit
stan_data <- list(n = n,
                  k = k,
                  X = X,
                  y = y)

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


# florida vs ny
florida_ny <- df_betas$stateFlorida - df_betas$stateNewYork
mcse(florida_ny)
quantile(florida_ny, probs = c(0.025, 0.975))

# probability that we are making the right call
mcse(florida_ny > 0)

# florida vs cali
florida_cali <- df_betas$stateFlorida - df_betas$stateCalifornia
mcse(florida_cali)
quantile(florida_cali, probs = c(0.025, 0.975))

# probability that we are making the right call
mcse(florida_cali > 0)


## how should we invest our money ---------------------------------------------
df_investment <- df_betas %>%
  select(research, administration, marketing)
df_investment <- melt(df_investment)

# densities
ggplot(data=df_investment, aes(x=value, fill=variable)) +
  geom_density(color=NA, alpha=0.6) +
  scale_fill_hue() +
  xlab("")

# profitability of research vs marketing
research_marketing <- df_betas$research - df_betas$marketing
mcse(research_marketing)
quantile(research_marketing, probs = c(0.025, 0.975))

# profitability of research vs marketing
research_administration <- df_betas$research - df_betas$administration
mcse(research_administration)
quantile(research_administration, probs = c(0.025, 0.975))

# how should we distribute our money?
sum(df_betas$research) / sum(df_investment$value)
sum(df_betas$marketing) / sum(df_investment$value)
sum(df_betas$administration) / sum(df_investment$value)
