library(readxl)
Brachial <- read_excel("Modalities_Data.xlsx",
                         sheet = "Couri")
View(Brachial)
attach(Brachial)

library(tidyverse)
Brachial <- Brachial %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity,
                                 Low="Low",Moderate="Moderate", High="High"))


Retrograde <- Brachial %>% filter(Type == "Retrograde" &
                                      Intensity == "Baseline")

library(psych)
#Descriptives for all
describe(Retrograde, na.rm=T, skew=FALSE, ranges=F)
