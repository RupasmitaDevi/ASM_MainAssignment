# Question 2a
setwd("C:/Users/Hp/OneDrive/Documents/SEM2/ASM")
df1 <- read.csv("heart_data.csv")

library(rstanarm)
library(bayesplot) ## use this package for plotting results

library("scales")

dim(df1)
apply(df1, 2, mean)
apply(df1, 2, sd)
apply(df1, 2, range)

age <-rescale(df1$Age)
restBloodPressure <-rescale(df1$RestBloodPressure)
sex <-rescale(df1$Sex)
serumCholestoral <- rescale(df1$SerumCholestoral)
fastingBloodSugar <- rescale(df1$FastingBloodSugar)
maxHeartRate <- rescale(df1$MaxHeartRate)
exerciseInduced <- rescale(df1$ExerciseInduced)
slope <- rescale(df1$Slope)
majorVessels <- rescale(df1$MajorVessels)
class <- rescale(df1$Class)

df1$Age <- age
df1$RestBloodPressure <- restBloodPressure
df1$SerumCholestoral <- serumCholestoral
df1$FastingBloodSugar <- fastingBloodSugar
df1$MaxHeartRate <- maxHeartRate
df1$ExerciseInduced <- exerciseInduced
df1$Slope <- slope
df1$Sex <- sex
df1$MajorVessels <- majorVessels
df1$Class <- class

head(df1)

dim(df1)
apply(df1, 2, mean)
apply(df1, 2, sd)
apply(df1, 2, range)

fit_glm <- glm(Class ~ ., data = df1, family = binomial(), ) ## fit model
summary(fit_glm)

pred_heart_disease <- predict(fit_glm, type = "response") ## this is on prob scale
pred_heart_disease
plot(pred_heart_disease ~ df1$Class)
table(pred_heart_disease > 0.5, df1$Class) ## choosing 0.5 as threshold

df <- read.csv("heart_data.csv")
plot(Class ~ ExerciseInduced, data = df) ## is there much evidence of sex having an effect?

plot(Class ~ Age, data = df)
plot(Class ~ RestBloodPressure, data = df)
plot(Class ~ SerumCholestoral, data = df)
plot(Class ~ MaxHeartRate, data = df)

pred <- predict(fit_glm, df)
pred
