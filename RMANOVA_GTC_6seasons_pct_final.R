#Repeated Measures ANOVA

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
#install.packages("colorspace")
library(colorspace)
library(dplyr)

str(Juv_1_3_RMANOVA)


bxp2 <- ggboxplot(Juv_1_3_RMANOVA, x = "time", y = "PLI_mm",
  color = "habitat", palette = "Chevliear1"
)
bxp2

Juv_GrowthRates_S4_6_RMANOVA_per$per_CL_increase <- asin(sqrt(Juv_GrowthRates_S4_6_RMANOVA_per$CL_pct/100))

#this oneeeee
bxp <- ggboxplot(Juv_GrowthRates_S5_6_RMANOVA_per, x = "time", y = "CL_pct", fill = "habitat", palette = "Warm")


#this one
bxp
bxp+ 
  scale_fill_discrete_qualitative(palette = "Warm", nmax = 5, order = c(1,2))



#s4 too
bxp2 <- ggboxplot(growth, x = "time", y = "PLI_mm", fill = "habitat", palette = "Warm")


#this one
bxp2+ 
  scale_fill_discrete_qualitative(palette = "Warm", nmax = 5, order = c(1,2))



#check normality
Juv_GrowthRates_S1_6_RMANOVA %>%
  group_by(habitat, time) %>%
  shapiro_test(PLI_mm)
#looks good

#QQ plot to check

ggqqplot(Juv_GrowthRates_S1_6_RMANOVA, "PLI_mm", ggtheme = theme_bw()) +
  facet_grid(time ~ habitat, labeller = "label_both")
#all looks good

growth <- Juv_GrowthRates_S1_4_RMANOVA

#RMAVOA

#THIS ONE 
anova_test(data = Juv_GrowthRates_S5_6_RMANOVA_per, formula = CL_pct ~ time*habitat + Error(GT_ID/time))
#all less than .0001

#s4 too
anova_test(data = growth, formula = PLI_mm ~ time*habitat + Error(GT_ID/time))
#all less than .0001

#outliers?
Juv_1_3_RMANOVA %>%
  group_by(habitat, time) %>%
  identify_outliers(PLI_mm)
#no outliers



get_anova_table(res.aov)

Juv_1_3_RMANOVA$GT_ID <-factor(Juv_1_3_RMANOVA$GT_ID)

one.way2 <- Juv_GrowthRates_S5_6_RMANOVA_per %>%
  group_by(time) %>%
  anova_test (formula = CL_pct ~ habitat) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")


one.way2
# S1-s2 <.0001 and S3 no , S4 no, S6 no, s5 yes p =.006



one.way2 <- growth %>%
  group_by(time) %>%
  anova_test (formula = PLI_mm ~ habitat) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")


one.way2
# S1-s2 <.0001 and S3 p =.021 (.084), p = 0.16 S4 (.064)


