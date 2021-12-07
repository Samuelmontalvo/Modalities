library(readxl)
Df <- read_excel("Data.xlsx", sheet = "Data_update")
View(Data)
attach(Df)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(psych)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(lme4)
library(lmerTest)


#priori power analysis
library(pwr)
pwr.anova.test(f=0.5,k=18,n=7,sig.level=0.05)

## convert sex and intensity to factors from characters
Df <- Df %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity, Rest="Rest",
                            Low="Low",Moderate="Moderate", High="High")) %>%
  mutate(Modality=recode_factor(Modality, Rest="Rest",Bike= "Bike",
                ArmCrank="ArmCrank",Treadmill="Treadmill",
                                Squat="Squat",Bench="Bench",
                Biceps="Biceps"))

#Descriptives
##Drop unused columns from psych package
describe_drop <- c("vars","n")

#Baseline
Baseline_all_table <- Df %>% filter(Modality == "Baseline") %>%
  select(Age,Height,Weight,BMI,SBP,DBP,VO2_Treadmill,VO2_Bike,VO2_ArmCrank,
         Squat_kg,Bench_kg,Biceps_kg, ESS, ESS_retro,RE_B,) %>%
  describe(Baseline, na.rm=T, skew=FALSE, ranges=F) %>%
  mutate(across(where(is.numeric), round, 2))

Baseline_all_table <-Baseline_all_table[,!(names(Baseline_all_table) %in%
                                 describe_drop)] %>% kable()%>%
                  kable_classic_2(full_width = F)
Baseline_all_table

#MALE
Baseline_male_table <- Df %>% filter(Modality == "Baseline" & Sex =="Male") %>%
  select(Age,Height,Weight,BMI,SBP,DBP,VO2_Treadmill,VO2_Bike,VO2_ArmCrank,
         Squat_kg,Bench_kg,Biceps_kg, ESS, ESS_retro,RE_B,) %>%
  describe(Baseline, na.rm=T, skew=FALSE, ranges=F) %>%
  mutate(across(where(is.numeric), round, 2))

Baseline_male_table <-Baseline_male_table[,!(names(Baseline_male_table) %in%
                                             describe_drop)] %>% kable()%>%
  kable_classic_2(full_width = F)
Baseline_male_table

#Female
Baseline_female_table <- Df %>% filter(Modality == "Baseline" & Sex =="Female") %>%
  select(Age,Height,Weight,BMI,SBP,DBP,VO2_Treadmill,VO2_Bike,VO2_ArmCrank,
         Squat_kg,Bench_kg,Biceps_kg, ESS, ESS_retro,RE_B) %>%
  describe(Baseline, na.rm=T, skew=FALSE, ranges=F) %>%
  mutate(across(where(is.numeric), round, 2))

Baseline_female_table <-Baseline_female_table[,!(names(Baseline_female_table) %in%
                                               describe_drop)] %>% kable()%>%
  kable_classic_2(full_width = F)
Baseline_female_table


ESSandRE_table <- Df %>%  select(ESS, ESS_retro,RE_B) %>% describeBy(Intensity,
                      na.rm=T, skew=FALSE, ranges=F) %>% kable()%>%  kable_classic_2(full_width = F)

ESSandRE_table <- kable()%>%  kable_classic_2(full_width = F)
ESSandRE_table


####### Statistical Analysis
##Normality Test

Df %>% group_by(Modality) %>%  shapiro_test(ESS)

#General Linear Mixed models


lmModel = lmer(ESS ~ Modality + Intensity + Modality*Intensity + (1|ID),
               data=Df, REML=TRUE)
summary(lmModel)
# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

pwc <- Df %>%
  group_by(Intensity) %>%
  pairwise_t_test(ESS ~ Modality, paired = T,
                  p.adjust.method	= "holm")
pwc %>% kable()%>%
  kable_classic_2(full_width = F)

# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df %>%
  group_by(Intensity)  %>% cohens_d(ESS ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)
effect %>% kable()%>%
  kable_classic_2(full_width = F)


#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "ESS")
# Boxplot of Vertical Jump Height
ESS_plot <- ggboxplot(Df, x = "Intensity", y = "ESS",
          color = "Modality", palette = get_palette("Dark2", 8),
          ylab = "Endothelial Shear Stress (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 1,hide.ns = T)
ESS_plot
ggsave("ESS_plot.png")



## RE
lmModel2 = lmer(RE_B ~ Modality + Intensity + Modality*Intensity + (1|ID),
                data=Df, REML=TRUE)
summary(lmModel2)
# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

pwc2 <- Df %>%
  group_by(Intensity) %>%
  pairwise_t_test(RE_B ~ Modality, paired = T,
                  p.adjust.method	= "holm")
pwc2 %>% kable()%>%
  kable_classic_2(full_width = F)
# Effect size Cohen's D with Hedge's g correction for small sample size
effect2 <-Df %>%
  group_by(Intensity)  %>% cohens_d(RE_B ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)
effect2 %>% kable()%>%
  kable_classic_2(full_width = F)


pwc2 <- pwc2 %>% add_xy_position(x = "RE_B")
Reynols_plot <- ggboxplot(Df, x = "Intensity", y = "RE_B",
                                      color = "Modality", palette = get_palette("Dark2", 8),
                                      ylab = "Reynolds Number (RE)") +
  stat_pvalue_manual(pwc,size = 1,hide.ns = T)
Reynols_plot
ggsave("Reynols_plot.png")



## ESS retro
lmModel3 = lmer(ESS_retro ~ Modality + Intensity + Modality*Intensity + (1|ID),
                data=Df, REML=TRUE)
summary(lmModel3)
# mixed model
anova(lmModel3)
#test of the random effects in the model
rand(lmModel3)

pwc3 <- Df %>%
  group_by(Intensity) %>%
  pairwise_t_test(ESS_retro ~ Modality, paired = T,
                  p.adjust.method	= "holm")
pwc3 %>% kable()%>%
  kable_classic_2(full_width = F)
# Effect size Cohen's D with Hedge's g correction for small sample size
effect3 <-Df %>%
  group_by(Intensity)  %>% cohens_d(ESS_retro ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)
effect3 %>% kable()%>%
  kable_classic_2(full_width = F)


pwc3 <- pwc3 %>% add_xy_position(x = "ESS_retro")
ESS_retro_plot <- ggboxplot(Df, x = "Intensity", y = "ESS_retro",
                          color = "Modality", palette = get_palette("Dark2", 8),
                          ylab = "Endothelial Shear Stress Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc3,size = 1,hide.ns = T)
ESS_retro_plot
ggsave("ESS_retro_plot.png")


