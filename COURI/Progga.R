library(readxl)
Modalities <- read_excel("Data.xlsx", sheet = "Data")
View(Modalities)
attach(Modalities)

library(tidyverse)
Modalities <- Modalities %>%
  mutate(Sex=recode_factor(Sex,Male="Male",Female="Female"))%>%
  mutate(Intensity=recode_factor(Intensity,
                                 Low="Low",Moderate="Moderate", High="High")) %>%
  mutate(Modality=recode_factor(Modality,
                                Squat="Squat",Treadmill="Treadmill", Bike="Bike",
                                ArmCrank="ArmCrank",Biceps="Biceps", Bench="Bench"))

Baseline <- Modalities %>% filter(Modality == "Baseline")


Progga <- Modalities %>%
  filter( Modality == "ArmCrank" | Modality == "Bike" | Modality == "Treadmill")
attach(Progga)

library(psych)
#Descriptives for all
describe(Baseline, na.rm=T, skew=FALSE, ranges=F)

##Descriptives by Intensity
Progga %>% describeBy(Progga$Intensity,
                       na.rm=T, skew=FALSE, ranges=F)

Squat <-Progga %>% filter(Modality == "Squat")

Squat %>%
  describeBy(Squat$Intensity,
             na.rm=T, skew=FALSE, ranges=F)


Treadmill <-Progga %>% filter(Modality == "Treadmill")

Treadmill %>%
  describeBy(Treadmill$Intensity,
             na.rm=T, skew=FALSE, ranges=F)


##Normality test
library(rstatix)
shapiro_test(ESS)

## ESS Analysis

# Repeated Measures Anova (within 2 x 3 subjects)
res.aov <- anova_test(data = Progga, dv = ESS,
                      wid = ID, within = Intensity,
                      between = Modality, effect.size = "pes")

# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Progga %>%
  group_by(Intensity) %>%
  pairwise_t_test(ESS ~ Modality, paired = TRUE,
                  p.adjust.method	= "holm")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
Progga %>%
  group_by(Intensity)  %>% cohens_d(ESS ~ Modality,
                                    paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Intensity")
# Boxplot of Vertical Jump Height
ESS_plot <- ggboxplot(Progga, x = "Intensity", y = "ESS",
                      color = "Modality", palette = get_palette("Set1", 3),
                      ylab = " ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("ESS_Progga_acsm.png")


## Reynols B analysis
# Repeated Measures Anova (within 2 x 3 subjects)

# Repeated Measures Anova (within 2 x 3 subjects)
res.aov2 <- anova_test(data = Progga, dv = RE_B,
                       wid = ID, within = Intensity,
                       between = Modality, effect.size = "pes")
# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc2 <- Progga %>%
  group_by(Intensity) %>%
  pairwise_t_test(RE_B ~ Modality, paired = TRUE,
                  p.adjust.method	= "none")
pwc2

# Effect size Cohen's D with Hedge's g correction for small sample size
Progga %>%
  group_by(Intensity)  %>%
  cohens_d(RE_B ~ Modality,
           paired = TRUE, hedges.correction = TRUE)


# Boxplot of Diameter
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Intensity")
RE_B_plot <- ggboxplot(Progga, x = "Intensity", y = "RE_B",
                       color = "Modality", palette = get_palette("Set1", 3),
                       ylab = "Reynolds number B") +
  stat_pvalue_manual(pwc2,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Reynolds_Proggaa_acsm.png")

## Arrange 2 figures into 1
library(ggpubr)
ggarrange(ESS_plot, RE_B_plot, ncol = 1, labels = c("A)", "B)"))
ggsave("ESS_RE_B_Progga_gridplot.png")
