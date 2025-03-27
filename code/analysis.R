###
# code and analysis reported in Griffiths et al. 2025
# Shrinking shrimp - investigating the weight loss of northern shrimp Pandalus borealis following boiling
# data and results of statistical analysis can also be found in Table 1 and 2, and Tables S1 and S2

###
# packages
require(dplyr)
require(readxl)
require(lme4)
require(MuMIn)

sessionInfo()
# MuMIn_1.48.4 lme4_1.1-37  Matrix_1.7-2 readxl_1.4.5 dplyr_1.1.4 

###
# load data
data = read_excel("data/Weight loss data.xlsx", sheet = 1)
head(data)

###
# tidy data for analysis
dat = data.frame(matrix(NA, ncol=11, nrow=NROW(data)))
colnames(dat) = c("trip", "haul", "quarter", "year", 'landed_to_boiled', "boiled_to_packed", 'landed_to_packed', 
                  "start_temperature", "salinity","temperature_change", "boiling_duration")

dat$trip = data$trip
dat$haul = data$haul
dat$quarter = as.factor(data$quarter)
dat$year = as.factor(data$year)
dat$landed_to_boiled = as.numeric(data$delta_boiled)
dat$boiled_to_packed = as.numeric(data$delta_boiled_to_packed)
dat$landed_to_packed = as.numeric(data$delta_packed)
dat$start_temperature = data$boiling_start_temperature
dat$salinity = data$water_salinity
dat$temperature_change = data$delta_boiling_temperature
dat$boiling_duration = data$boiling_duration

###
# linear mixed effect models by comparison

##
# Capture to Boiled
options(na.action = "na.fail")
test1 = dat[,c(1:5,8:11)] # remove not needed columns
test1 = filter(test1, !landed_to_boiled %in% c(NA)) # remove NAs
global_model = lmer(landed_to_boiled ~ quarter + year + start_temperature + salinity + temperature_change + boiling_duration + (1|trip), data = test1) # fit most complicated model
dd <- dredge(global_model) # run dredge function across all possible models 
dd <- subset(dd, delta < 4) # filter for models with an delta AICc less than 4

# fit best fitting model
mod1 = lmer(landed_to_boiled ~ quarter + (1|trip), data = test1)
summary(mod1)
plot(mod1)
qqnorm(residuals(mod1))

# extract fixed effect model estimates
fixef(mod1)

# extract lower and upper confidence intervals for fixed effect estimates
confint(mod1)

# fit simplest model
mod1_simple = lmer(landed_to_boiled ~ (1|trip), data = test1)
summary(mod1_simple)
plot(mod1_simple)
qqnorm(residuals(mod1_simple))

# extract fixed effect model estimates
fixef(mod1_simple)

# extract lower and upper confidence intervals for fixed effect estimates
confint(mod1_simple)

##
# Boiled to packed 
options(na.action = "na.fail")
test2 = dat[,c(1:4,6,8:11)] # remove not needed columns
test2 = filter(test2, !boiled_to_packed %in% c(NA)) # remove NAs
global_model2 = lmer(boiled_to_packed ~ quarter + year + start_temperature + salinity + temperature_change + boiling_duration + (1|trip), data = test2) # fit most complicated model
dd2 <- dredge(global_model2) # run dredge function across all possible models 
dd2 <- subset(dd2, delta < 4) # filter for models with an delta AICc less than 4

# fit best fitting model
mod2 = lmer(boiled_to_packed ~ (1|trip), data = test2)
summary(mod2)
plot(mod2)
qqnorm(residuals(mod2))

# extract fixed effect model estimates
fixef(mod2)

# extract lower and upper confidence intervals for fixed effect estimates
confint(mod2)

##
# Capture to packed 
options(na.action = "na.fail") # remove not needed columns
test3 = dat[,c(1:4,7,8:11)] # remove NAs
global_model3 = lmer(landed_to_packed ~ quarter + year + start_temperature + salinity + temperature_change + boiling_duration + (1|trip), data = test3) # fit most complicated model
dd3 <- dredge(global_model3) # run dredge function across all possible models 
dd3 <- subset(dd3, delta < 4) # filter for models with an delta AICc less than 4

# fit best fitting model
mod3 = lmer(landed_to_packed ~ quarter + (1|trip), data = test3)
summary(mod3)
plot(mod3)
qqnorm(residuals(mod3))

# extract fixed effect model estimates
fixef(mod3)

# extract lower and upper confidence intervals for fixed effect estimates
confint(mod3)

# fit simplest model
mod3_simple = lmer(landed_to_packed ~ (1|trip), data = test3)
summary(mod3_simple)
plot(mod3_simple)
qqnorm(residuals(mod3_simple))

# extract fixed effect model estimates
fixef(mod3_simple)

# extract lower and upper confidence intervals for fixed effect estimates
confint(mod3_simple)
