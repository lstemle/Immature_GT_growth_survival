#Nitrogen analyses based on tortoise and habitat and species
#much of it is exploratory to decide what to include and understand the data


#load library
library(dplyr)

#load in data of plants collected in May 2022 or August 2022
List_of_plant_samples_collected_May3_5_metadata<- List_of_plant_samples_collected_May3_5_metadata 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ~~~~~~Group by species of plants

###Season August but group by species instead of tortoise
Plant_samples_Aug2022_N_and_C_meta$carbonration <-Plant_samples_Aug2022_N_and_C_meta$`C/N Ratio`

#group by same species
tort3<-Plant_samples_Aug2022_N_and_C_meta%>% 
  group_by(Species) %>% 
  summarize(tortN= mean(`Total %N`, na.rm=TRUE),
            tortCN = mean(carbonration, na.rm=TRUE),
            tortMaxN = max(`Total %N`, na.rm=TRUE),
            tortMaxCN = max(carbonration, na.rm=TRUE))#,
            #hab = (Habitat_type))

#output file
write.csv(tort, "Nitrogen CN avg max August 2022.csv")
getwd()

#group by same species for May data

tort4<-List_of_plant_samples_collected_May3_5_metadata %>% 
  group_by(Species) %>% 
  summarize(N= mean(per_N, na.rm=TRUE),
            CN = mean(c_n, na.rm=TRUE),
            MaxN = max(per_N, na.rm=TRUE),
            MaxCN = max(c_n, na.rm=TRUE))

#output file
write.csv(tort4, "Nitrogen CN avg max by species.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#does nitrogen vary significantly within species across seasons? 
sm1 <-aov(Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$per_N ~ Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$season)
summary(sm1)
#                                                                     Df Sum Sq Mean Sq F value Pr(>F)
#Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$season  1  1.398  1.3981    2.29  0.139
#Residuals                                                             35 21.370  0.6106    
#~~~~~~~~~~~~~~~~~~No it does not


#nitrogen vary across season?
boxplot(Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$per_N~ Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$season)
#when plotted by species instead of individually it looks like august has higher percent N


boxplot(Nitrogen_C_N_avg_max_by_tortoise_August_2022_updated$tortN ~ Nitrogen_C_N_avg_max_by_tortoise_August_2022_updated$habitat)
# sandhill actually has more N in August 

#growth vary across seasons?
boxplot(Nitrogen_C_N_avg_max_by_tortoise_August_2022_updated_ruderal$CL_Pct_Per_Day ~Nitrogen_C_N_avg_max_by_tortoise_August_2022_updated_ruderal$season)
#for ruderal growth is higher in May, avg. .00125 in may, august .0009

boxplot(Nitrogen_C_N_avg_max_by_tortoise_may_and_August_2022_updated_sandhill$CL_Pct_Per_Day~Nitrogen_C_N_avg_max_by_tortoise_may_and_August_2022_updated_sandhill$season)
#for sandhill growth is higher in august with .00095 and in may is .00075.



#does nitrogen vary by habitat type in Aug?
m1<- aov(Plant_samples_Aug2022$Total_perN~Plant_samples_Aug2022$Habitat_type)
summary(m1)
#                                    Df Sum Sq Mean Sq F value Pr(>F)
#Plant_samples_Aug2022$Habitat_type   1   0.05  0.0510   0.089  0.766
#Residuals                          127  72.95  0.5744
#~~~~~~~~~~~~~~~~~~~~No it doesn't

#lets log it to make sure
Plant_samples_Aug2022$logN <-log(Plant_samples_Aug2022$Total_perN)

shapiro.test(Plant_samples_Aug2022$Total_perN) #W = .94
shapiro.test(Plant_samples_Aug2022$logN) #better w and p



m2<- aov(Plant_samples_Aug2022$logN~Plant_samples_Aug2022$Habitat_type)
summary(m2) #still significantly different 

#FACTORIAL ANOVAs
m3 <- aov(per_N~ Habitat_type + season, data = List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_redone_N_samples)
summary(m3)
#Df Sum Sq Mean Sq F value Pr(>F)
#Habitat_type   1   0.50  0.4966   1.049  0.307
#season         1   0.47  0.4687   0.990  0.321
#Residuals    218 103.16  0.4732               


m4 <- aov(per_N~ Habitat_type * season, data = List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_redone_N_samples)
summary(m4)
#                     Df Sum Sq Mean Sq F value Pr(>F)  
#Habitat_type          1   0.50  0.4966   1.061 0.3042  
#season                1   0.47  0.4687   1.001 0.3182  
#Habitat_type:season   1   1.57  1.5664   3.346 0.0688 .
#Residuals           217 101.60  0.4682                 

m5 <- aov(per_N~ Habitat_type , data = List_of_plant_samples_collected_aug_Nitrogen_metadata_redone_N_samples)
summary(m5)

#is nitrogen different by season? is the trend lean towards more nitrogen in May?
boxplot(List_of_plant$per_N ~ List_of_plant$season) #doesn't really lean one way or the other

boxplot(List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_N_samples_ruderal$per_N~ List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_N_samples_ruderal$season)
#for ruderal, nitrogen is higher in may, est avg may 1.5, august 1.2

boxplot(List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_N_samples_sandhill$per_N~List_of_plant_samples_collected_May_and_aug_Nitrogen_metadata_N_samples_sandhill$season)
#for sandhill it is higher nitrogen in august avg 1.45 and lower in may, .9


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install.packages("tidyverse")
library(tidyverse)

#organize data by species to get the average nitrogen and carbon per specie 
average_species <- List_of_plant_samples_collected_aug_Nitrogen_metadata_redone_N_samples %>% 
  group_by(Species) %>% 
  summarise(per_N_avg= mean(per_N),
            per_c_avg = mean(per_C))


m6 <- aov(per_N~ Species, data = List_of_plant_samples_collected_aug_Nitrogen_metadata_redone_N_samples)            
summary(m6)

mean(average_species$per_N_avg)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#paired t test of samples N by season and species (also done in JMP bc more clearly shown they were paired)
write.csv(average_species, "august species with redone N species avg.csv")
getwd()


t.test(Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$per_N,Nitrogen_CN_avg_max_by_species_May_and_Aug_2022_outliers_fixed$aug_N, mu=0, alt="two.sided", paired = TRUE, conf.level = 0.95)
#t = 0.37987, df = 13, p-value = 0.7102
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -0.1135208  0.1619602
#sample estimates:
#  mean difference 0.02421971 
####no difference in (within) species by season



#~~~~~~~~~~~~~~~~~~~~~~ GROWTH

##annual June-June growth rate checking if different from mar-mar annual

m10 <- aov(june_june_annual$meanCLgrowth~june_june_annual$Habitat)
summary(m10)

#                         Df    Sum Sq   Mean Sq F value Pr(>F)  
#june_june_annual$Habitat  1 3.614e-07 3.614e-07   6.142 0.0266 *
#  Residuals                14 8.238e-07 5.880e-08   

#orrrr make sure type III is chosen appropriately 
m10<-lm(june_june_annual$meanCLgrowth~june_june_annual$Habitat)
Anova(m10, Type = "III")


#group by tortoise to get N and C per tortoise 
tort2<-List_of_plant_samples_collected_aug_Nitrogen_metadata_redone_N_samples %>% 
  group_by(Tort_ID) %>% 
  summarize(tortN= mean(per_N, na.rm=TRUE),
            tortC = mean(per_C, na.rm=TRUE))

#output file
write.csv(tort2, "Nitrogen by tort august.csv")
getwd()


#is nitrogen related to growth in august? 
mod1 <- lm(Nitrogen_by_tort_august_veg_indexes_remove_nos$per_CL_perday ~ Nitrogen_by_tort_august_veg_indexes_remove_nos$initial_CL+Nitrogen_by_tort_august_veg_indexes_remove_nos$tortN)
summary(mod1) 



#tortoise growth in comparison with nitrogen values and initial CL for August 2022 data
NGmod1 <- lm(Nitrogen_growth_August_2022$CL_Pct_Per_Day ~ Nitrogen_growth_August_2022$initial_CL+Nitrogen_growth_August_2022$tortN)
summary(NGmod1) 
#                                         Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                             3.646e-03  1.448e-03   2.518   0.0863 .
#Nitrogen_growth_August_2022$initial_CL -1.403e-05  4.783e-06  -2.933   0.0609 .
#Nitrogen_growth_August_2022$tortN      -1.939e-04  5.184e-04  -0.374   0.7333  

#Residual standard error: 0.0003374 on 3 degrees of freedom
#Multiple R-squared:  0.8141,	Adjusted R-squared:  0.6902 
#F-statistic:  6.57 on 2 and 3 DF,  p-value: 0.08013
#there are not a lot of nitrogen and fully used burrow match ups in august allowing low sample size so we do not compare
#growth and nitrogen



NGmod2 <- lm(Nitrogen_growth_August_2022$CL_Pct_Per_Day ~Nitrogen_growth_August_2022$tortN)
summary(NGmod2) 
#not sig without initial CL 



