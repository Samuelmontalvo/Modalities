library(readxl)
Modalities_Data <- read_excel("Modalities_Data.xlsx")
View(Modalities_Data)
attach(Modalities_Data)

library(dplyr)
Modalities_Data <- Modalities_Data %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))

Baseline <- Modalities_Data %>% filter(Stage == "Baseline")

library(psych)
#Descriptives for all
Baseline %>% group_by(Sex)%>%
describe(Baseline, na.rm=T, skew=FALSE, ranges=F)

##Descriptives by Sex
Baseline %>% describeBy(Baseline$Sex,
                        na.rm=T, skew=FALSE, ranges=F)
