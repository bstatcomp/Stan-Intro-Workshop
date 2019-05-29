# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)
library(reshape2)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("../data/elections.csv", sep=";")

# compile the model
model <- stan_model("elections.stan")


## expand the votes variable --------------------------------------------------
votes <- data %>% select(vote)
votes <- model.matrix(~ vote - 1, data=votes)


## fit ------------------------------------------------------------------------
# prepare data for Stan
n <- nrow(data)
Y <- votes
k <- ncol(Y)
stan_data <- list(n = n,
                  k = k,
                  Y = Y)

#fit
fit <- sampling(model, data = stan_data,
                chains = 1, iter = 2000, warmup = 1000)

# examine fit
traceplot(fit)
print(fit)

# extract
extract <- extract(fit)

# extract beta values and rename columns
df_thetas <- data.frame(theta=extract$theta)
colnames(df_thetas) <- substring(colnames(votes), 5)


## analysis -------------------------------------------------------------------
# CIs for all parties
quantile(df_thetas$DeSUS, probs = c(0.025, 0.975))
quantile(df_thetas$Levica, probs = c(0.025, 0.975))
quantile(df_thetas$LMS, probs = c(0.025, 0.975))
quantile(df_thetas$NSi, probs = c(0.025, 0.975))
quantile(df_thetas$SAB, probs = c(0.025, 0.975))
quantile(df_thetas$SD, probs = c(0.025, 0.975))
quantile(df_thetas$SDS, probs = c(0.025, 0.975))
quantile(df_thetas$SLS, probs = c(0.025, 0.975))
quantile(df_thetas$SMC, probs = c(0.025, 0.975))
quantile(df_thetas$SNS, probs = c(0.025, 0.975))

# densities plot
df_densities <- melt(df_thetas)
ggplot(data=df_densities, aes(x=value)) +
  geom_density(color=NA, fill="black", alpha=0.6) +
  facet_grid(variable ~ .) +
  xlab("percentage") +
  xlim(0, 0.5)

# current parliament
df_current <- NULL
chairs <- c(5, 9, 13, 7, 5, 10, 25, 0, 10, 4)
df_current <- rbind(df_current, chairs)
colnames(df_current) <- colnames(df_thetas)

# chair predictions
df_cp <- NULL
df_more <- NULL
for (i in 1:nrow(df_thetas)) {
  # extract row
  row <- df_thetas[i,]

  # min requirement to get any chairs is 2%
  row[row < 0.02] <- 0
  # renormalize
  row <- row / sum(row)
  # 88 chairs
  row <- round(row * 88)
  # attach row to data frame
  df_cp <- rbind(df_cp, row)
  
  # more chairs?
  more <- row >= df_current
  # attach row to data frame
  df_more <- rbind(df_more, more)
}

# chairs plot
df_chairs <- melt(df_cp)
bins <- max(df_chairs$value)+1
ggplot(data=df_chairs, aes(x=value)) +
  geom_histogram(alpha=0.6, bins=bins) +
  facet_grid(variable ~ .) +
  xlab("chairs")

# probability of more chairs than currently
colSums(df_more) / nrow(df_more)

# probability of SDS, SLS, NSi and SNS coalition (more than 44 chairs)
mcse(df_cp$SDS + df_cp$SLS + df_cp$NSi + df_cp$SNS > 44)

# probability of LMS, SAB, SD, DeSUS and SMC coalition (more than 44 chairs)
mcse(df_cp$LMS + df_cp$SAB + df_cp$SD + df_cp$DeSUS + df_cp$SMC > 44)
