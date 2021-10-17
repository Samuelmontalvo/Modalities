library(readxl)
Final_Data <- read_excel("Data.xlsx", sheet = "Data")
View(Data)
attach(Final_Data)
#priori power analysis
library(pwr)
pwr.anova.test(f=0.5,k=18,n=7,sig.level=0.05)

## convert sex and intensity to factors from characters
library(tidyverse)
Final_Data <- Final_Data %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity,
                            Low="Low",Moderate="Moderate", High="High")) %>%
  mutate(Modality=recode_factor(Modality, Rest="Rest",Bike= "Bike",
                ArmCrank="ArmCrank",Treadmill="Treadmill",
                                Squat="Squat",Bench="Bench",
                Biceps="Biceps"))


#subset for baseline TABLES
Baseline <- Final_Data %>% filter(Modality == "Baseline")

drop <- c("ID","Modality","Intensity","Load","LOAD_norm","Squat",
          "Bench","Biceps","RPE")

Baseline <- Baseline[,!(names(Baseline) %in% drop)]


Males <- Baseline %>% filter(Sex=="Male")
Females <- Baseline %>% filter(Sex=="Females")

t.test(lactate ~ Sex, alternative = "greater", var.equal = FALSE)

cor.test(Squat_kg,VO2_Bike,method="spearman")

model <- lm(Bench_kg~VO2_Treadmill, data = Baseline)
summary(model)

library(psych)
#Descriptives for all
tbl_baseline_all <- describe(Baseline, na.rm=T, skew=FALSE, ranges=F)%>%
  mutate(across(where(is.numeric), round, 2))

##Drop unused columns from describe package
describe_drop <- c("vars","n")

tbl_baseline_all <- tbl_baseline_all[,!(names(tbl_baseline_all) %in%
                                          describe_drop)]
## Table visualization
library(kableExtra)
tbl_baseline_all %>% kable()%>%
  kable_classic_2(full_width = F)

##Descriptives by Males
Male <- Baseline %>% filter(Sex == "Male")

tbl_baseline_Male <- describe(Male, na.rm=T, skew=FALSE, ranges=F)%>%
  mutate(across(where(is.numeric), round, 2))

##Drop unused columns from describe package

tbl_baseline_Male <- tbl_baseline_Male[,!(names(tbl_baseline_Male) %in%
                                          describe_drop)]
## Table visualization
tbl_baseline_Male %>% kable()%>%
  kable_classic_2(full_width = F)

## DEscriprives by Females
Female <- Baseline %>% filter(Sex == "Female")

tbl_baseline_Female <- describe(Female, na.rm=T, skew=FALSE, ranges=F)%>%
  mutate(across(where(is.numeric), round, 2))

##Drop unused columns from describe package

tbl_baseline_Female <- tbl_baseline_Female[,!(names(tbl_baseline_Female) %in%
                                            describe_drop)]
## Table visualization
tbl_baseline_Female %>% kable()%>%
  kable_classic_2(full_width = F)



####### Statisitcal Analysis

#create data frame and drop unused columns
SA <- Final_Data %>% select("ID","Sex","Intensity","Modality","VO2","Diameter",
                            "Velocity","Velocity_Ret", "ESS", "RE_B")

SA <-SA[!(SA$Modality=="Baseline"),]

##Normality Test
library(rstatix)
SA %>% shapiro_test(Diameter)
attach(SA)
#General Linear Mixed models

library(lme4)
library(lmerTest)
lmModel = lmer(RE_B ~ Modality*Intensity + (1|ID), data=SA, REML=TRUE)
# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

attach(SA)
pwc <- SA %>%
  group_by(Intensity) %>%
  pairwise_t_test(RE_B ~ Modality, paired = T,
                  p.adjust.method	= "holm")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
SA %>%
  group_by(Intensity)  %>% cohens_d(RE_B ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)



#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "RE_B")
# Boxplot of Vertical Jump Height
ggboxplot(SA, x = "Intensity", y = "RE_B",
          color = "Modality", palette = get_palette("Dark2", 7),
          ylab = "RE_B ") +
  stat_pvalue_manual(pwc,size = 5,hide.ns = TRUE)

ggplot(SA,aes(Intensity,Velocity, fill= factor(Modality))) +
  geom_boxplot() +
  stat_pvalue_manual(pwc,size = 7,hide.ns = TRUE)
