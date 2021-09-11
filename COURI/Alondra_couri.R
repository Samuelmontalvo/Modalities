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


Baseline <- Modalities %>% filter(Intensity == "Baseline")

library(psych)
#Descriptives for all
describe(Baseline, na.rm=T, skew=FALSE, ranges=F)

##CROP DATA

Alondra <- Modalities %>%
  filter(Modality == "Squat" | Modality == "Bench"| Modality =="Biceps")
attach(Alondra)

##Descriptives by Intensity and by modality

Squat <-Alondra %>% filter(Modality == "Squat")

Squat %>%
describeBy(Squat$Intensity,
                       na.rm=T, skew=FALSE, ranges=F)

Bench <- Alondra %>% filter(Modality == "Bench")

Bench %>%
  describeBy(Bench$Intensity,
             na.rm=T, skew=FALSE, ranges=F)

Biceps <- Alondra %>% filter(Modality == "Biceps")

Biceps %>%
  describeBy(Biceps$Intensity,
             na.rm=T, skew=FALSE, ranges=F)

##Normality test
library(rstatix)
shapiro_test(Velocity)

## Velocity Analysis

# Repeated Measures Anova (within 2 x 3 subjects)
res.aov <- anova_test(data = Alondra, dv = Velocity,
                      wid = ID, within = Intensity,
                      between = Modality, effect.size = "pes")

# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Alondra %>%
  group_by(Intensity) %>%
  pairwise_t_test(Velocity ~ Modality, paired = TRUE,
                  p.adjust.method	= "bonferroni")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
Alondra %>%
  group_by(Intensity)  %>% cohens_d(Velocity ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Intensity")
# Boxplot of Vertical Jump Height
ggboxplot(Alondra, x = "Intensity", y = "Velocity",
          color = "Modality", palette = get_palette("Set1", 3),
          ylab = "Anterograde Velocity (cm/s)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Velocity_Alondra_couri.png")


## Diameter analysis


# Repeated Measures Anova (within 2 x 3 subjects)
res.aov2 <- anova_test(data = Alondra, dv = Diameter,
                       wid = ID, within = Intensity,
                       between = Modality, effect.size = "pes")

# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc2 <- Alondra %>%
  group_by(Intensity) %>%
  pairwise_t_test(Diameter ~ Modality, paired = TRUE,
                  p.adjust.method	= "none")
pwc2

# Effect size Cohen's D with Hedge's g correction for small sample size
Alondra %>%
  group_by(Intensity)  %>%
  cohens_d(Diameter ~ Modality,
           paired = TRUE, hedges.correction = TRUE)


# Boxplot of Diameter
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Intensity")
ggboxplot(Alondra, x = "Intensity", y = "Diameter",
          color = "Modality", palette = get_palette("Set1", 4),
          ylab = "Diameter (cm)") +
  stat_pvalue_manual(pwc2,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Diameter_Alondra_couri.png")


#Womersley shear stress

# Repeated Measures Anova (within 2 x 3 subjects)
res.aov3 <- anova_test(data = Alondra, dv = Womersley,
                       wid = ID, within = Intensity,
                       between = Modality, effect.size = "pes")

# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov3)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc3 <- Alondra %>%
  group_by(Intensity) %>%
  pairwise_t_test(Womersley ~ Modality, paired = TRUE,
                  p.adjust.method	= "none")
pwc3

# Effect size Cohen's D with Hedge's g correction for small sample size
Alondra %>%
  group_by(Intensity)  %>%
    cohens_d(Womersley ~ Modality,
           paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc3 <- pwc3 %>% add_xy_position(x = "Intensity")
# Boxplot of Vertical Jump Height
ggboxplot(Alondra, x = "Intensity", y = "Womersley",
          color = "Modality", palette = get_palette("Set1", 3),
          ylab = " Womersley") +
  stat_pvalue_manual(pwc3,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Womersley_Alondra_couri.png")
