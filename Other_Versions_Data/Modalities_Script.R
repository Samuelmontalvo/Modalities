library(readxl)
Modalities <- read_excel("Modalities_Data.xlsx",
                              sheet = "Couri")
View(Modalities)
attach(Modalities)

library(tidyverse)
Modalities <- Modalities %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity,
                 Low="Low",Moderate="Moderate", High="High"))

Baseline <- Modalities %>% filter(Stage == "Baseline")

library(psych)
#Descriptives for all
describe(Baseline, na.rm=T, skew=FALSE, ranges=F)

##Descriptives by Sex
Baseline %>% describeBy(Baseline$Sex,
                        na.rm=T, skew=FALSE, ranges=F)


##Normality test
library(rstatix)
shapiro_test(Velocity)

