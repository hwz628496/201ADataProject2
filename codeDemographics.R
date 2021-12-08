setwd('C:/Users/hwz62/Dropbox/~Grad School/07 2021 Fall - Biostats 201A/Data Analysis Project 2')

library(multcomp)
library(vtable)
library(dplyr)
library("PerformanceAnalytics")
library('GGally')
library(vioplot)
library('car')
library(qqplotr)
library(glmnet)
library(leaps)


# load and transform data
tdata <- read.csv('cdi_data.csv')
tdata$crimerate <- 1000*tdata$crimes/tdata$pop
tdata$logarea <- log(tdata$area)
tdata$density <- tdata$area/tdata$pop
tdata$logdensity <- log(tdata$area/tdata$pop)

# outliers?
ddata <- select(tdata, crimerate, area, pop18, pop65)
st(ddata)
boxplot(ddata$crimerate) #a few outliers
boxplot(log(ddata$area))
boxplot(ddata$pop18)
boxplot(ddata$pop65)

# remove outliers
outliers <-c(6,123,70,9)
tdataO<-tdata[-c(6,123,70,9), ]
ddataO <- select(tdataO, crimerate, logarea, pop18, pop65)
ggpairs(ddataO)


# correlations and collinearities 
ddata2 <- select(tdata, crimerate, area, logarea, density, logdensity, pop18, pop65)
ggpairs(ddata2)

hist(ddata$area)
hist(log(ddata$area)) #log-transform area
tdata$logarea <- log(tdata$area)
hist(tdata$density)
hist(tdata$logdensity) #log-transform density

# ANOVA
ANOVAstate <- aov(crimerate ~ state, data = tdata)
summary(ANOVAstate)

# which states are the most crime-ridden?
stateList <-unique(tdata$state)
stateCrime <- data.frame(data.frame(matrix(ncol = 2, nrow = 48))
)
colnames(stateCrime ) <- c('state', 'crime')
stateCrime$state<-stateList
for (i in 1:length(stateList))
{
  stateCrime$crime[i]<-mean(subset(tdata, state == stateList[i])$crimerate)
}


ANOVAregion <- aov(crimerate ~ as.factor(region), data = tdata)
summary(ANOVAregion)









# regressions
areaModel <- lm(crimerate ~ logarea, data=tdata)
densityModel <- lm(crimerate ~ logdensity, data=tdata)
pop18Model <- lm(crimerate ~ pop18, data=tdata)
pop65Model <- lm(crimerate ~ pop65, data=tdata)
allModel <- lm(crimerate ~ logarea+pop18+pop65, data=tdata)
interactModel <- lm(crimerate ~ (logarea+pop18+pop65)^2, data=tdata)
selectedModel <- lm(crimerate ~ logarea+pop18+pop65+logarea*pop18+logarea*pop65, data=tdata)
summary(areaModel) #less dense areas havs slightly less crime
summary(densityModel) #much better predictor than area
summary(pop18Model) #young people increase crime
summary(pop65Model) #old people don't increase crime
summary(allModel) #young people seem to increase crime the most
summary(interactModel) #less density reduces rate of young crime
summary(selectedModel)

# regressions without outliers
areaModelO <- lm(crimerate ~ logarea, data=tdataO)
densityModelO <- lm(crimerate ~ logarea, data=tdataO)
pop18ModelO <- lm(crimerate ~ pop18, data=tdataO)
pop65ModelO <- lm(crimerate ~ pop65, data=tdataO)
allModelO <- lm(crimerate ~ logarea+pop18+pop65, data=tdataO)
interactModelO <- lm(crimerate ~ (logarea+pop18+pop65)^2, data=tdataO)
selectedModelO <- lm(crimerate ~ logarea+pop18+pop65+logarea*pop18+logarea*pop65, data=tdataO)
selectedModelO2 <- lm(crimerate ~ logdensity+pop18+pop65+logdensity*pop18+logdensity*pop65, data=tdataO)
selectedModelO3 <- lm(crimerate ~ logdensity+pop18+logdensity*pop18, data=tdataO)
summary(areaModelO)
summary(densityModelO)
summary(pop18ModelO) 
summary(pop65ModelO) 
summary(allModelO) 
summary(interactModelO) 
summary(selectedModelO2)
summary(selectedModelO3)

# Some conclusions:
# 1. Young people cause crime
# 2. Density matters more than area
# 3. Old people don't cause crime




# population normality?
smp18<-data.frame(pop18=ddata$pop18)
gg18 <- ggplot(data = smp18, mapping = aes(sample = pop18)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
smp65<-data.frame(pop65=ddata$pop65)
gg65 <- ggplot(data = smp65, mapping = aes(sample = pop65)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")















# Ignore below - me playing with automatic variable selection


# Automated variable selection with interactions
y <- tdataO$crimerate
x <- data.matrix(tdataO[,-c(1,2,3,10,17,18)])
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

autoVar <- regsubsets(crimerate ~ pop+pop18+pop65+docs+beds+hsgrad+bagrad+poverty+unemp+pcincome+totalinc+logarea,
                      data = tdataO, nbest = 1, nvmax = 8) #tried with original data, only log-transform area
info<-summary(autoVar)
autoDf<-cbind(info$which, round(cbind(rsq=info$rsq, adjr2=info$adjr2, cp=info$cp, bic=info$bic, rss=info$rss), 3))
autoDfBest<-data.frame(autoDf[8,])

autoVar2 <- regsubsets(crimerate ~ (pop18+pop65+docs+beds+hsgrad+bagrad+poverty+unemp+pcincome+totalinc+logarea+logdensity)^2,
                      data = tdataO, nbest = 1, nvmax = 8, really.big = T) #using density-based variables. Takes long time to run. Inconsistent results?
info2<-summary(autoVar2)
autoDf2<-cbind(info2$which, round(cbind(rsq=info2$rsq, adjr2=info2$adjr2, cp=info2$cp, bic=info2$bic, rss=info2$rss), 3))
autoDfBest<-data.frame(autoDf2[8,]) #version 8 is best on all metrics
finalReg <- lm(crimerate ~ beds+pop65+unemp+poverty+logarea+logdensity+pcincome+totalinc+
               pop65*poverty+pop65*unemp+beds*logarea+beds*logdensity+poverty*pcincome+poverty*totalinc, 
               data=tdataO)
summary(finalReg)

ggpairs(select(tdataO, crimerate, beds, pop65, unemp, poverty, logarea, logdensity, pcincome, totalinc))

