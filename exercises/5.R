# libraries
library(ggplot2)
library(dplyr)
library(rstan)
library(mcmcse)
library(reshape2)


## preparation ----------------------------------------------------------------
# !!!!! load the data !!!!!
data <- !!!!!

# !!!!! compile the model !!!!!


## expand the votes variable --------------------------------------------------
votes <- data %>% select(vote)
votes <- model.matrix(~ vote - 1, data=votes)


## fit ------------------------------------------------------------------------
# !!!!! prepare data for Stan !!!!!

# fit
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
# !!!!! calculate 95% CIs for all parties, or for the ones you are interested in !!!!!

# densities plot
df_densities <- melt(df_thetas)
ggplot(data=df_densities, aes(x=value)) +
  geom_density(color=NA, fill="black", alpha=0.6) +
  facet_grid(variable ~ .) +
  xlab("percentage") +
  xlim(0, 0.5)

# current state in the parliament
df_current <- NULL
chairs <- c(5, 9, 13, 7, 5, 10, 25, 0, 10, 4)
df_current <- rbind(df_current, chairs)
colnames(df_current) <- colnames(df_thetas)

# transform vote predictions into chair predictions
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
  
  # more chairs than previously?
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

# !!!!! probability of SDS, SLS, NSi and SNS coalition (more than 44 chairs) !!!!!

# !!!!! probability of LMS, SAB, SD, DeSUS and SMC coalition (more than 44 chairs) !!!!!
