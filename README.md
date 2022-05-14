# Gaussian-distribution-and-sensitivity-analysis-of-daily-temperature-of-major-cities-in-the-world.

# libraries ----
library(dplyr)
library(ggplot2)
library(statsr)
# importing data ----

df <- read.csv("city_temp_data.csv")

# cleaning data and manipulating ----
df <- df %>% filter(AvgTemperature != -99) %>% 
  mutate(CAvgTemperature = (AvgTemperature - 32)/1.8) %>% select(-c("AvgTemperature"))
df1 <- df %>% filter(City == "Madrid", Year == 2019,
                     Month == 1) 
# calculating mean and std for 2019 january in Madrid
MadridAvgMean <- mean(df1$CAvgTemperature)
MadridAvgSd <- sd(df1$CAvgTemperature)

# creating prior ----
prior <- df %>% filter(Year < 2019, Month == 1, City == "Madrid") %>%
  group_by(Year) %>% summarise(MeanAvgTemp = mean(CAvgTemperature),
                               SdAvgTemp = sd(CAvgTemperature))
# prior mean and std----
prior_mean <- mean(prior$MeanAvgTemp)
prior_sd <- mean(prior$SdAvgTemp)

# Values for the prior mean and prior standard deviation 
prior_mean
prior_sd

# calculating posterior mean and std ----
posterior_mean <- (prior_mean * MadridAvgSd^2 + 
                     nrow(prior) * MadridAvgMean * prior_sd^2) /
  (MadridAvgSd^2 + nrow(prior) * prior_sd^2 )
posterior_sd <- sqrt((MadridAvgSd^2 * prior_sd^2)/(MadridAvgSd^2 + nrow(prior) * prior_sd^2))

# Values for the posterior mean and posterior standard deviation 
posterior_mean
posterior_sd

#  data visualization
p <- seq(-3, 10, 0.01)
prior1 <- dnorm(p, prior_mean, prior_sd)
posterior1 <- dnorm(p, posterior_mean, posterior_sd)
temp <- matrix(c(p,
                 prior1,
                 posterior1), 
               nrow = 3, byrow = TRUE)
rownames(temp) <- c("Model (p)", "Prior P(model)", 
                    "Posterior P(model|data)")
colnames(temp) <- c(seq(1, length(p)))

plot_data <- data.frame(t(temp))

ggplot(plot_data) + 
  geom_bar(aes(x = Model..p., y = Prior.P.model.), stat = "identity", fill = "red", alpha = 0.5 ) +
  geom_bar(aes(x = Model..p., y = Posterior.P.model.data.), stat = "identity", fill = "green", alpha = 0.5 ) + 
   labs(title = "Distribution plot")

#  Calculating the credible intervals ----
prior_n0 <- nrow(prior)
prior_df <- prior_n0 - 1
df_n <- nrow(df1)
n_n <- df_n + prior_n0
v0 <- df_n + prior_df

posterior_mean + qt(c(0.025, 0.0975), v0) * sqrt(posterior_sd / n_n)


# Yearly Analysis for the Sensitivity Analysis
# importing data ----
df <- read.csv("city_temp_data.csv")

# cleaning data and manipulating ----
df <- df %>% filter(AvgTemperature != -99) %>% 
  mutate(CAvgTemperature = (AvgTemperature - 32)/1.8) %>% select(-c("AvgTemperature"))
df1 <- df %>% filter(City == "Madrid", Year == 2019)
MadridAvgMean <- mean(df1$CAvgTemperature)
MadridAvgSd <- sd(df1$CAvgTemperature)

# creating prior ----
prior <- df %>% filter(Year < 2019, City == "Madrid") %>%
  group_by(Year) %>% summarise(MeanAvgTemp = mean(CAvgTemperature),
                               SdAvgTemp = sd(CAvgTemperature))
# prior mean and std----
prior_mean <- mean(prior$MeanAvgTemp)
prior_sd <- mean(prior$SdAvgTemp)

#Values for the prior mean and prior standard deviation
prior_mean
prior_sd

# calculating posterior mean and std ----
posterior_mean <- (prior_mean * MadridAvgSd^2 + 
                     nrow(prior) * MadridAvgMean * prior_sd^2) /
  (MadridAvgSd^2 + nrow(prior) * prior_sd^2 )
posterior_sd <- sqrt((MadridAvgSd^2 * prior_sd^2)/(MadridAvgSd^2 + nrow(prior) * prior_sd^2))

#Values for the prior mean and prior standard deviation
posterior_mean
posterior_sd

#  data visualization
p <- seq(-3, 50, 0.01)
prior1 <- dnorm(p, prior_mean, prior_sd)
posterior1 <- dnorm(p, posterior_mean, posterior_sd)
temp <- matrix(c(p,
                 prior1,
                 posterior1), 
               nrow = 3, byrow = TRUE)
rownames(temp) <- c("Model (p)", "Prior P(model)", 
                    "Posterior P(model|data)")
colnames(temp) <- c(seq(1, length(p)))

plot_data <- data.frame(t(temp))

ggplot(plot_data) + 
  geom_bar(aes(x = Model..p., y = Prior.P.model.), stat = "identity", fill = "red", alpha = 0.5 ) +
  geom_bar(aes(x = Model..p., y = Posterior.P.model.data.), stat = "identity", fill = "green", alpha = 0.5 ) + 
  labs(title = "Distribution plot")

#Credible interval calculations ----
prior_n0 <- nrow(prior)
prior_df <- prior_n0 - 1
df_n <- nrow(df1)
n_n <- df_n + prior_n0
v0 <- df_n + prior_df

posterior_mean + qt(c(0.025, 0.0975), v0) * sqrt(posterior_sd / n_n)



# For all cities yearly ----
# importing data ----
df <- read.csv("city_temp_data.csv")

# cleaning data and manipulating ----
df <- df %>% filter(AvgTemperature != -99) %>% 
  mutate(CAvgTemperature = (AvgTemperature - 32)/1.8) %>% select(-c("AvgTemperature"))
df1 <- df %>% filter( Year == 2019)
AvgMean <- mean(df1$CAvgTemperature)
AvgSd <- sd(df1$CAvgTemperature)

# creating prior ----
prior <- df %>% filter(Year < 2019) %>%
  group_by(Year) %>% summarise(MeanAvgTemp = mean(CAvgTemperature),
                               SdAvgTemp = sd(CAvgTemperature))
#Calculating prior mean and prior standard deviation ----
prior_mean <- mean(prior$MeanAvgTemp)
prior_sd <- mean(prior$SdAvgTemp)

#Values for prior mean and prior standard 
prior_mean
prior_sd

# calculating posterior mean and std ----
posterior_mean <- (prior_mean * AvgSd^2 + 
                     nrow(prior) * AvgMean * prior_sd^2) /
  (AvgSd^2 + nrow(prior) * prior_sd^2 )
posterior_sd <- sqrt((AvgSd^2 * prior_sd^2)/(AvgSd^2 + nrow(prior) * prior_sd^2))

#Values for the posterior mean and standard deviation 
posterior_mean
posterior_sd

#  data visualization
p <- seq(-3, 50, 0.01)
prior1 <- dnorm(p, prior_mean, prior_sd)
posterior1 <- dnorm(p, posterior_mean, posterior_sd)
temp <- matrix(c(p,
                 prior1,
                 posterior1), 
               nrow = 3, byrow = TRUE)
rownames(temp) <- c("Model (p)", "Prior P(model)", 
                    "Posterior P(model|data)")
colnames(temp) <- c(seq(1, length(p)))

plot_data <- data.frame(t(temp))

ggplot(plot_data) + 
  geom_bar(aes(x = Model..p., y = Prior.P.model.), stat = "identity", fill = "red", alpha = 0.5 ) +
  geom_bar(aes(x = Model..p., y = Posterior.P.model.data.), stat = "identity", fill = "green", alpha = 0.5 ) + 
  labs(title = "Distribution plot")

# Calculating the credible interval ----
prior_n0 <- nrow(prior)
prior_df <- prior_n0 - 1
df_n <- nrow(df1)
n_n <- df_n + prior_n0
v0 <- df_n + prior_df

posterior_mean + qt(c(0.025, 0.0975), v0) * sqrt(posterior_sd / n_n)




df <- read.csv("city_temp_data.csv")
df <- df %>% filter(AvgTemperature != -99) %>% 
  mutate(CAvgTemperature = (AvgTemperature - 32)/1.8) %>% select(-c("AvgTemperature"))

posterior <- function(city, month){
  df3 <- df %>% filter(City == city, Year == 2019,
                       Month == month)
  CityAvgMean <- mean(df3$CAvgTemperature)
  CityAvgSd <- sd(df3$CAvgTemperature)
  
  # creating prior ----
  prior <- df %>% filter(Year < 2019, Month == month, City == city) %>%
    group_by(Year) %>% summarise(MeanAvgTemp = mean(CAvgTemperature),
                                 SdAvgTemp = sd(CAvgTemperature))
  # prior mean and std----
  prior_mean <- mean(prior$MeanAvgTemp)
  prior_sd <- mean(prior$SdAvgTemp)
  posterior_mean <- (prior_mean * CityAvgSd^2 + 
                       nrow(prior) * CityAvgMean * prior_sd^2) /
    (CityAvgSd^2 + nrow(prior) * prior_sd^2 )
  posterior_sd <- sqrt((CityAvgSd^2 * prior_sd^2)/(CityAvgSd^2 + nrow(prior) * prior_sd^2))
  list <- list(Mean = posterior_mean, Sd=posterior_sd)
  return(list)
}
posterior("Madrid", 1)




posterior_year <- function(city){
  df4 <- df %>% filter(City == city, Year == 2019)
  CityAvgMean <- mean(df4$CAvgTemperature)
  CityAvgSd <- sd(df4$CAvgTemperature)
  
  # creating prior ----
  prior <- df %>% filter(Year < 2019, City == city) %>%
    group_by(Year) %>% summarise(MeanAvgTemp = mean(CAvgTemperature),
                                 SdAvgTemp = sd(CAvgTemperature))
  # prior mean and std----
  prior_mean <- mean(prior$MeanAvgTemp)
  prior_sd <- mean(prior$SdAvgTemp)
  posterior_mean <- (prior_mean * CityAvgSd^2 + 
                       nrow(prior) * CityAvgMean * prior_sd^2) /
    (CityAvgSd^2 + nrow(prior) * prior_sd^2 )
  posterior_sd <- sqrt((CityAvgSd^2 * prior_sd^2)/(CityAvgSd^2 + nrow(prior) * prior_sd^2))
  list <- list(Mean = posterior_mean, Sd=posterior_sd)
  return(list)
}
posterior_year("Madrid")
