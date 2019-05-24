# libraries
library(dplyr)
library(rstan)


## preparation ----------------------------------------------------------------
# load data
data <- read.csv("data/temperature.csv")

# load or compile the model
if (!file.exists("normal.compiled")) {
  model <- stan_model("normal.stan")
  saveRDS(model, file = "normal.compiled")
} else {
  model <- readRDS("normal.compiled")  
}


## default rim ----------------------------------------------------------------
data <- data %>% filter(SpecialRim == 0)



# exploratory plot
avg_made <- data %>%
  group_by(ThrowNum)  %>%
  summarize(mean_made=mean(Made))

ggplot(data=avg_made, aes(x=ThrowNum, y=mean_made)) +
  geom_point() +
  ylab("success rate")

x <- data$ThrowNum
y <- data$Made
stan_data <- list(x = x,
                  y = y,
                  n = length(y))

fit <- sampling(model, data = stan_data,
                        chains = 1, iter = 2000, warmup = 1000, control=list(adapt_delta=0.95))

# examine fit
traceplot(fit)
print(fit)

# extract
extract <- extract(fit)



# plot theta density
ggplot(data=df_results, aes(x=theta, fill=rim)) +
  geom_density(color=NA) +
  scale_fill_hue() +
  xlim(0, 1) +
  xlab("success rate")
