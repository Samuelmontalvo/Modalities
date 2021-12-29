library(readxl)
Df <- read_excel("Data_Final.xlsx")
View(Df)
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
library(forcats)
library(cvcqv)


#priori power analysis
library(pwr)
pwr.anova.test(f=0.5,k=14,n=6,sig.level=0.05)

## convert sex and intensity to factors from characters
Df <- Df %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity, Rest="Rest",
                            Low="Low",Moderate="Moderate", High="High")) %>%
  mutate(Modality = fct_relevel(Modality, "Baseline", "Baseline two", "Arm-ergometer",
                                "Cycle-ergometer", "Treadmill", "Biceps", "Bench",
                                "Squat"))

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

library(doBy)
options(digits=3)
ESS_all <- summaryBy(ESS~Intensity +Modality, data=Df, FUN = c(mean,sd),
                     na.rm=TRUE)%>% kable()%>%kable_classic_2(full_width = F)
ESS_all
ESS_bySex <- summaryBy(ESS~Intensity + Sex + Modality, data=Df, FUN = c(mean,sd),
                       na.rm=TRUE) %>% kable()%>% kable_classic_2(full_width = F)
ESS_bySex

##BASELINE INDEPENDENT T TEST BY SEX
Male_B <- Df %>% filter(Sex == "Male" & Modality == "Baseline")
Female_B <- Df %>% filter(Sex == "Female" & Modality == "Baseline")

t.test(Male_B$Age,Female_B$Age,alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$Height,Female_B$Height,alternative = "two.sided",
       var.equal = FALSE)
t.test(Male_B$Weight,Female_B$Weight,alternative = "two.sided",
       var.equal = FALSE)
t.test(Male_B$BMI,Female_B$BMI,alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$SBP,Female_B$SBP,alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$DBP,Female_B$DBP,alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$VO2_Treadmill,Female_B$VO2_Treadmill,
       alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$VO2_Bike,Female_B$VO2_Bike,
       alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$VO2_ArmCrank,Female_B$VO2_ArmCrank,
       alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$Squat_kg,Female_B$Squat_kg,
       alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$Bench_kg,Female_B$Bench_kg,
       alternative = "two.sided", var.equal = FALSE)
t.test(Male_B$Biceps_kg,Female_B$Biceps_kg,
       alternative = "two.sided", var.equal = FALSE)

#############RELIABILITY ANALYSIS####################################

Reliability_ESS_baseline <- Df %>% filter(Modality=="Baseline") %>% select(ESS)
Reliability_ESS_baselinetwo <- Df %>% filter(Modality=="Baseline two") %>% select(ESS)
Reliability_ESS <- rbind(Reliability_ESS_baseline,
                         Reliability_ESS_baselinetwo)
##Miller methdo estimates from standard normal distribuition
cv_versatile(Reliability_ESS$ESS,method="miller", correction = TRUE)

Reliability_Re_baseline <- Df %>% filter(Modality=="Baseline") %>% select(RE_B)
Reliability_Re_baselinetwo <- Df %>% filter(Modality=="Baseline two") %>% select(RE_B)
Reliability_Re <- rbind(Reliability_Re_baseline,
                        Reliability_Re_baselinetwo)
##Miller methdo estimates from standard normal distribuition
cv_versatile(Reliability_Re$RE_B,method="miller", correction = TRUE)

####### Statistical Analysis
##Normality Test

Df %>% group_by(Modality) %>%  shapiro_test(ESS)

#General Linear Mixed models


lmModel = lmer(ESS ~ Modality + Intensity + Sex + Modality*Intensity + (1|ID),
               data=Df, REML=TRUE)
summary(lmModel)
# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

df2 <- Df %>% filter(Intensity == "Low" | Intensity == "Moderate" |
                       Intensity == "High")


pwc <- Df %>% group_by(Intensity) %>%
  pairwise_t_test(ESS ~ Modality, paired = T,
                  p.adjust.method	= "holm")
pwc %>% kable()%>%
  kable_classic_2(full_width = F)


ESS_group_pwc <- Df %>% filter(Intensity == "Low" | Intensity == "Moderate" |
                                      Intensity == "High") %>%
  group_by(Modality) %>%
  pairwise_t_test(ESS ~ Intensity, paired = T,
                  p.adjust.method	= "holm")
ESS_group_pwc %>% kable()%>%
  kable_classic_2(full_width = F)

Effect_group <- Df %>% filter(Intensity == "Low" | Intensity == "Moderate" |
                                Intensity == "High") %>%
  group_by(Modality)  %>% cohens_d(ESS ~ Intensity,
                                    paired = TRUE, hedges.correction = TRUE)
Effect_group

# Effect size Cohen's D with Hedge's g correction for small sample size
effect <- Df %>%
  group_by(Intensity)  %>% cohens_d(ESS ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)
effect %>% kable()%>%
  kable_classic_2(full_width = F)


#Plots
# Boxplot of ESS between

pwc <- pwc %>% add_xy_position(x = "Intensity")
ESS_between_plot <- ggboxplot(Df, x = "Intensity", y = "ESS",
                          color = "Modality", palette = get_palette("Dark2", 8),
                          ylab = "Endothelial Shear Stress (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 7,hide.ns = TRUE,"p.adj.signif")
ESS_between_plot
ggsave("ESS_between_plot.png")

# Boxplot of ESS within
ESS_within_plot <- ggboxplot(Df, x = "Intensity", y = "ESS",
          color = "Modality", palette = get_palette("Dark2", 8),
         ylab = "Endothelial Shear Stress (dynes/cm2)") +
  geom_segment(aes(x = 1.65, y = 120, xend = 3.66, yend = 120)) +
  geom_segment(aes(x = 1.65, y = 118, xend = 1.65, yend = 120)) +
  geom_segment(aes(x = 3.66, y = 118, xend = 3.66, yend = 120)) +
  annotate(geom="text", x=2.64, y=121, label="*",color="Black") +
  geom_segment(aes(x = 1.77, y = 115, xend = 3.79, yend = 115)) +
  geom_segment(aes(x = 1.77, y = 113, xend = 1.77, yend = 115)) +
  geom_segment(aes(x = 3.79, y = 113, xend = 3.79, yend = 115)) +
  annotate(geom="text", x=2.78, y=116, label="*",color="Black") +
  geom_segment(aes(x = 1.9, y = 110, xend = 3.93, yend = 110)) +
  geom_segment(aes(x = 1.9, y = 108, xend = 1.9, yend = 110)) +
  geom_segment(aes(x = 3.93, y = 108, xend = 3.93, yend = 110)) +
  annotate(geom="text", x=2.93, y=111, label="*",color="Black") +
  geom_segment(aes(x = 2.07, y = 15, xend = 4.08, yend = 15)) +
  geom_segment(aes(x = 2.07, y = 17, xend = 2.07, yend = 15)) +
  geom_segment(aes(x = 4.079, y = 17, xend = 4.079, yend = 15)) +
  annotate(geom="text", x=3.07, y=13.5, label="x",color="Black") +
  geom_segment(aes(x = 2.2, y = 10, xend = 3.21, yend = 10)) +
  geom_segment(aes(x = 2.2, y = 12, xend = 2.2, yend = 10)) +
  geom_segment(aes(x = 3.207, y = 12, xend = 3.207, yend = 10)) +
  annotate(geom="text", x=2.7, y=8.5, label="x",color="Black") +
  geom_segment(aes(x = 2.35, y = 5, xend = 4.35, yend = 5)) +
  geom_segment(aes(x = 2.35, y = 7, xend = 2.35, yend = 5)) +
  geom_segment(aes(x = 4.3457, y = 7, xend = 4.3457, yend = 5)) +
  annotate(geom="text", x=3.3, y=3, label="x",color="Black")
ESS_within_plot
ggsave("ESS_within_plot.png")


## RE
lmModel2 = lmer(RE_B ~ Modality + Intensity + Modality*Intensity + Sex + (1|ID),
                data=Df, REML=TRUE)
summary(lmModel2)
# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

pwc2 <- Df%>% filter(Intensity == "Low" | Intensity == "Moderate" |
                             Intensity == "High") %>%
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


pwc2 <- pwc2 %>% add_xy_position(x = "Intensity")
Reynols_plot <- ggboxplot(Df, x = "Intensity", y = "RE_B",
                                      color = "Modality", palette = get_palette("Dark2", 8),
                                      ylab = "Reynolds Number (RE)") +
  stat_pvalue_manual(pwc2,size = 7,hide.ns = TRUE)
Reynols_plot
ggsave("Reynols_plot.png")


Reynolds_error_plot <- ggerrorplot(Df, x = "Intensity", y = "RE_B",
            color = "Modality", palette = get_palette("Dark2", 8),
            ylab = "Reynolds Number (RE)") +
  geom_hline(yintercept=2000, linetype="dashed", color = "red")
Reynolds_error_plot
ggsave("Reynolds_error_plot.png")
