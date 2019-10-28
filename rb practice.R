##regressions tutorial 1

x = rnorm(10, mean = 2, sd = 3)
mean(x)   #why is this value not 2, in the tutorial?
sd(x)
print(x)

set.seed(42) #b/c 6*9?
rnorm(10)



###data simulation process
beta_0 = 2
beta_1 = 3
sigma = 1
n_obs = 1000

x_1 = as.numeric(scale(rnorm(n_obs)))


y = beta_0 + beta_1*x_1 + rnorm(n_obs, mean = 0, sd = sigma)

library(tidyverse)

# y: the predicted values
ggplot(data = NULL, aes(x_1, y)) + geom_point()

fit1 = lm(y~x_1) #how well x_1 predicts y

fit1$coefficients
sd(fit1$residuals)
#my coefs values and sd of residuals is *slightly* different than in the pdf. not sure if this is a problem


###Practice Exercise #1
#1a
beta_0a = 5
beta_1a = 7
beta_2a = 5
beta_3a = 3
sigma_a = 1
n_obs_a = 5000

x_1a = as.numeric(scale(rnorm(n_obs_a)))
x_2a = as.numeric(scale(rnorm(n_obs_a)))
x_3a = as.numeric(scale(rnorm(n_obs_a)))

data = data.frame(x_1a, x_2a, x_3a)
head(data)

data$y_a = beta_0a + beta_1a*x_1a + beta_2a*x_2a + beta_3a*x_3a + rnorm(n_obs_a, mean = 0, sd = sigma_a)
head(data)

fit1a = lm(y_a~x_1a+x_2a+x_3a, data = data)
fit1a$coefficients

#writing the function to simulate data
fx_lms = function(n_obs, beta_0, beta_1, beta_2, beta_3, sigma){
  data = data.frame(x_1 = as.numeric(scale(rnorm(n_obs))),
                    x_2 = as.numeric(scale(rnorm(n_obs))),
                    x_3 = as.numeric(scale(rnorm(n_obs))))
  
  data$y = beta_0 + beta_1*x_1 + beta_2*x_2 + beta_3*x_3 + rnorm(n_obs, mean = 0, sd = sigma)
  
  fit = lm(y~x_1+x_2+x_3, data = data)
  
  output <- data.frame(beta_0, beta_0_hat = NA,
                       beta_1, beta_1_hat = NA,
                       beta_2, beta_2_hat = NA,
                       beta_3, beta_3_hat = NA,
                       sigma, sigma_hat = NA)
  
  output[, c("beta_0", "beta_1", "beta_2", "beta_3")] <- c(beta_0, beta_1, beta_2, beta_3)
  output[, c("beta_0_hat", "beta_1_hat", "beta_2_hat", "beta_3_hat")] <- fit$coef
  output$sigma_hat <- sd(fit$resid)
  output
  
}


####skipping the simulations for now. 


mysleep = sleep
levels(mysleep$group) = c("pre", "post")
head(mysleep)

m1 = lm(extra~group, data = mysleep)
summary(m1)


mysleep %>% group_by(group) %>%summarize(mean = mean(extra))

mysleep = mysleep %>% mutate(group.num = as.numeric(group) - 1)
head(mysleep)
view(mysleep)

#Practice Exercise 3
mysleep = mysleep %>% mutate(group.c = scale(as.numeric(group)))
summary(mysleep)
