##code by Leyna Stemle for Stepwise regression and Model Comparison GLMM 

#### Data from young tortoises in central Florida in two habitat types

#read in your libraries 
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(rstatix)
library(car)
library(lindia)
library(ggpubr)
library(agricolae)
library(effectsize)
library(corrplot)
library(MuMIn)
require(MuMIn)




#season 1 - late dry season 2021 (mar-june 2021)


#_______________________________ look at distributions
#test for normality too not just visually

#starting with drone data metrics then going to growth metrics
hist(mydata$X.GreenHerb)
shapiro.test(mydata$X.GreenHerb) #not normal
hist(mydata$X.BrownHerb)
shapiro.test(mydata$X.BrownHerb) #not normal
hist(mydata$X.TotalHerb)
shapiro.test(mydata$X.TotalHerb) #not normal but a little higher W
hist(mydata$X.Tree)
shapiro.test(mydata$X.Tree) #very not normal 
hist(mydata$X.Woody)
shapiro.test(mydata$X.Woody) #normal 
hist(mydata$X.Bare)
shapiro.test(mydata$X.Bare) #normal 
hist(mydata$CL_mm_per_day)
shapiro.test(data1$CL_mm_per_day) #normal
hist(data1$Initial_CL_mm)
shapiro.test(data1$Initial_CL_mm) #normal
hist(data1$PL_mm_per_day)
shapiro.test(data1$PL_Per_Day_mm) #normal, most normal
shapiro.test(data1$CL_pct_per_day)#normal
shapiro.test(data1$per_CL)#even more normal


#surveyed habitat variables
shapiro.test(data1$BareSoil) #normal
shapiro.test(data1$Wiregrass) #not normal
shapiro.test(data1$NativeGrass) #normal
shapiro.test(data1$Bahiagrass) #not normal
shapiro.test(data1$ExoticGrass) #not normal
shapiro.test(data1$NativeLegume) #normal
shapiro.test(data1$ExoticLegume) #not normal
shapiro.test(data1$Herb) #normal
shapiro.test(data1$MediumWoody) #normal
shapiro.test(data1$ShortWoody)# normal
shapiro.test(data1$TallWoody)# normal
shapiro.test(data1$Vine)#not normal
shapiro.test(data1$Palmetto)# not normal
shapiro.test(data1$Sedge)# not normal
shapiro.test(data1$MeanDiversity)# normal


#do tranformation for each season as well as exploratory histograms

#transform the data with arcsine square root divided by 100 since these are proportions and precentages 
data$Bahiagrass2 <- asin(sqrt(data$Bahiagrass/ 100))
data$Wiregrass2 <- asin(sqrt(data$Wiregrass/ 100))
data$BareSoil2 <- asin(sqrt(data$BareSoil/ 100))
data$NativeGrass2<- asin(sqrt(data$NativeGrass/ 100))
data$ExoticGrass2 <- asin(sqrt(data$ExoticGrass/ 100))
data$NativeLegume2 <- asin(sqrt(data$NativeLegume/ 100))
data$ExoticLegume2 <- asin(sqrt(data$ExoticLegume/ 100))
data$Herb2 <- asin(sqrt(data$Herb/ 100))
data$MediumWoody2 <- asin(sqrt(data$MediumWoody/ 100))
data$ShortWoody2 <- asin(sqrt(data$ShortWoody/ 100))
data$TallWoody2 <- asin(sqrt(data$TallWoody/ 100))
data$Vine2 <- asin(sqrt(data$Vine/ 100))
data$Palmetto2 <- asin(sqrt(data$Palmetto/ 100))
data$Sedge2 <- asin(sqrt(data$Sedge/ 100))
data$MeanDiversity2 <- asin(sqrt(data$MeanDiversity/ 100))

#start here with transformed datasheet
data1<-S1_data_transformed

#subset so we can use corrplot
mydata2 <- subset(data1, select=c(CL_pct_per_day, PL_Per_Day_mm, Wiregrass2, Bahiagrass2, ExoticGrass2, NativeLegume2, ExoticLegume2, Herb2,  Vine2, Sedge2, Palmetto2, TallWoody2, MediumWoody2
                                  , ShortWoody2, BareSoil2, Initial_CL_mm))


mydata3 <- subset(data1, select=c(CL_pct_per_day, Wiregrass2, Bahiagrass2, NativeLegume2, ExoticLegume2 , Herb2,  Vine2, Sedge2, TallWoody2, 
                                   Initial_CL_mm))


corrplot1 <- cor(mydata2)#wiregrass and bahia, woody with grass, palmetto, bare
corrplot1

corrplot2 <-cor(mydata3)#bahiagrass is largest relationship
corrplot2

data2 <-data1
#look at ontogenetic relationship
summary(lm(data2$CL_pct_per_day ~ data2$Initial_CL_mm)) #R2= .01 and p =.7
plot(data1$CL_pct_per_day ~ data1$Initial_CL_mm)

data2 %>%
  ggplot(aes(x= Initial_CL_mm, color = GT_ID)) + 
  geom_point(aes(y = CL_pct_per_day)) +
  geom_smooth(aes(y=CL_pct_per_day), method="lm", se=TRUE) +
  theme_few() + 
  facet_wrap(~Habitat)

data1 %>%
  ggplot(aes(x= Initial_CL_mm, color = GT_ID)) + 
  geom_point(aes(y = CL_pct_per_day)) +
  geom_smooth(aes(y=CL_pct_per_day), method="lm", se=TRUE) +
  theme_few() + 
  facet_wrap(~Habitat)

data3 %>%
  ggplot(aes(x= Initial_CL_mm, color = GT_ID)) + 
  geom_point(aes(y = CL_pct_per_day)) +
  geom_smooth(aes(y=CL_pct_per_day), method="lm", se=TRUE) +
  theme_few() + 
  facet_wrap(~Habitat)


library(MuMIn)
require(MuMIn)
options(na.action = "na.fail") # change the default "na.omit" to prevent models
# from being fitted to different datasets in case of missing values


globalmodelS1 <- lm(CL_pct_per_day ~Initial_CL_mm + Bahiagrass2 + NativeLegume2+ ExoticLegume2 + Herb2 + Vine2 + Sedge2 +Wiregrass2 +
                      Palmetto2+ TallWoody2 + MediumWoody2
                    + ShortWoody2 +BareSoil2,
                    data = data1, na.action = "na.fail")


combinations <- dredge(globalmodelS1)


print(combinations)#lowest AICc is initial and Bahia (when we make sure initial is always considered to be in the model)

mod1_veg <- lm(data1$CL_pct_per_day ~ data1$Bahiagrass2 + data1$Initial_CL_mm)
#let's check the VIF's
mod1_vifs2 <- car::vif(mod1_veg)
print(mod1_vifs2) #all good


#drone supervised classification S1 2021
data1$X.GreenHerb <- asin(sqrt(data1$XGreenHerb / 100))
data1$X.BrownHerb <- asin(sqrt(data1$XBrownHerb / 100))
data1$X.Bare <- asin(sqrt(data1$XBare / 100))
data1$X.Tree <- asin(sqrt(data1$XTree / 100))




globalmodel_S1drone <- lm(data1$CL_pct_per_day ~ data1$X.GreenHerb + data1$Initial_CL_mm + X.BrownHerb + X.Bare+
                     X.Tree, data = data1, na.action = "na.fail")

combinations2 <- dredge(globalmodel_S1drone)

print(combinations2)#lowest AICc is initial and Green herb 



################______________________________________________________________________________________________
#data1$per_CL <- asin(sqrt(data1$CL_pct_per_day/100))

globalmodel_per <- lm(data1$CL_pct_per_day ~ data1$X.GreenHerb + data1$Initial_CL_mm + X.BrownHerb + X.Bare +X.BrownHerb +
                     X.Tree, data = data1, na.action = "na.fail")

combinations_percent <- dredge(globalmodel_per)

print(combinations_percent)#lowest AICc is Green herb then GR and initial (negative relationship but not sig)


mod4_per <- lm(data1$CL_pct_per_day ~ data1$X.GreenHerb)
summary(mod4_per) #R2 = .60 p<.0001

mod1_per <-lm(data1$CL_pct_per_day ~ data1$X.GreenHerb + data1$Initial_CL_mm)
summary(mod1_per) #R2 = .64, initial cl p is .27 and Green Herb is significant
#let's check the VIF's
mod1_vifs <- car::vif(mod1_per)
print(mod1_vifs) #all good



####drone vegetation indices/spectral indices 

globalmodel_drone1<- lm(CL_pct_per_day ~ glimax_drone+Initial_CL_mm+ glimean_drone + gli90 +mgvri90 +mgvrimean_drone +mgvrimax_drone
                       , data = data1, na.action = "na.fail")

combinations_drone1 <- dredge(globalmodel_drone1)

print(combinations_drone1) #gli 90 and mgvri 90 and initial 

mod1_drone <-lm(CL_pct_per_day ~ gli90 +Initial_CL_mm+mgvri90, data = data1)


#let's check the VIF's
mod1_vifs2 <- car::vif(mod1_drone)
print(mod1_vifs2) #too high over 7 

mod1_drone2 <-lm(CL_pct_per_day ~ gli90 +Initial_CL_mm, data = data1) #so next choice is just gli90 in model 10 of the table
summary(mod1_drone2)
################______________________________________________________________________________________________


#season 2 june-sept 2021 early wet season

#data2
data2<-S2_data_transformed
shapiro.test(data2$CL_pct_per_day)#normal
data2$per_CL <- asin(sqrt(data2$CL_pct_per_day/ 100))
shapiro.test(data2$per_CL)#still normal

#data2$Initial_CL_mm < data2$Initial_CL_mm
#data1$PL_Per_Day_mm <- data2$PL_Per_Day_mm
data2$Bahiagrass2 <- asin(sqrt(data2$Bahiagrass/ 100))
data2$Wiregrass2 <- asin(sqrt(data2$Wiregrass/ 100))
data2$BareSoil2 <- asin(sqrt(data2$BareSoil/ 100))
data2$NativeGrass2<- asin(sqrt(data2$NativeGrass/ 100))
data2$ExoticGrass2 <- asin(sqrt(data2$ExoticGrass/ 100))
data2$NativeLegume2 <- asin(sqrt(data2$NativeLegume/ 100))
data2$ExoticLegume2 <- asin(sqrt(data2$ExoticLegume/ 100))
data2$Herb2 <- asin(sqrt(data2$Herb/ 100))
data2$MediumWoody2 <- asin(sqrt(data2$MediumWoody/ 100))
data2$ShortWoody2 <- asin(sqrt(data2$ShortWoody/ 100))
data2$TallWoody2 <- asin(sqrt(data2$TallWoody/ 100))
data2$Vine2 <- asin(sqrt(data2$Vine/ 100))
data2$Palmetto2 <- asin(sqrt(data2$Palmetto/ 100))
data2$Sedge2 <- asin(sqrt(data2$Sedge/ 100))
data2$MeanDiversity2 <- asin(sqrt(data2$MeanDiversity/ 100)) #this metric isn't really useful

data2$X.GreenHerb <- asin(sqrt(data2$XGreenHerb / 100))
shapiro.test(data2$X.GreenHerb) #not 100% normal but better

data2$X.BrownHerb <- asin(sqrt(data2$XBrownHerb / 100))
shapiro.test(data2$X.BrownHerb) #not normal but much closer

data2$X.Bare<- asin(sqrt(data2$XBare / 100)) #this metric isn't really useful 
shapiro.test(data2$X.Bare) #not normal but much closer

data2$X.Tree <- asin(sqrt(data2$XTree / 100))
shapiro.test(data2$X.Tree) #not normal but much closer

data2$X.Woody<- asin(sqrt(data2$XWoody/ 100))
shapiro.test(data2$X.Tree) #not normal but much closer


#jump to here if have file with transformed data file

mydata21 <- subset(data2, select=c(CL_pct_per_day, PL_Per_Day_mm, Wiregrass2, Bahiagrass2, ExoticGrass2, NativeLegume2, ExoticLegume2 , Herb2,  Vine2, Sedge2, Palmetto2, TallWoody2, MediumWoody2
                                  , ShortWoody2, BareSoil2, Initial_CL_mm))

corrplot2 <- cor(mydata21)#wiregrass and bahia highly correlated, woody with grass, legumes are highly correlated with grass, but if don't keep bahia then exotic legumes is highest
corrplot2

#select ones that are highly related to growth and that aren't too related to each other but exotic legume is highly correlated with a lot

#Also confirmed with JMP SAS program stepwise and all possible models holding initial CL in there
globalmodel3 <- lm(CL_pct_per_day ~Initial_CL_mm + ExoticLegume2+ ExoticGrass2 + Herb2 + Vine2 + Sedge2+ MediumWoody2 +ShortWoody2+
                   Palmetto2+ TallWoody2 +NativeLegume2+Wiregrass2 +Bahiagrass2, 
                  data = data2, na.action = "na.fail")


combinations3 <- dredge(globalmodel3)


print(combinations3)
#initial CL held in, plus exotic legume, sedge


mod5 <-lm(data2$CL_pct_per_day ~  data2$ExoticLegume2 +data2$Sedge2 +data2$Initial_CL_mm)
summary(mod5) #R2 = .54

#let's check the VIF's
mod5_vifs <- car::vif(mod5)
print(mod5_vifs) #all good

#_____________________________________________________________________________________-
###Season 2 drones - early wet season 2021


mydata4 <- subset(data2, select=c(CL_pct_per_day, X.GreenHerb, X.BrownHerb, X.Tree, X.TotalHerb,X.Woody,
                                                                    Initial_CL_mm))

corrplot3 <- cor(mydata4)
corrplot3

globalmodel4 <- lm(data2$CL_pct_per_day ~ data2$X.GreenHerb + data2$Initial_CL_mm + X.BrownHerb + X.Bare +
                     X.Woody, data = data2, na.action = "na.fail")

combinations4 <- dredge(globalmodel4)

print(combinations4)
#green herb is best with initial 


mod6 <-lm(data2$CL_pct_per_day ~  data2$X.GreenHerb  +data2$Initial_CL_mm)
summary(mod6) #R2=.28
#let's check the VIF's
mod6_vifs <- car::vif(mod6)
print(mod6_vifs) #all good



####drone vegetation indices/spectral indices season 2 june-sept 2021

globalmodel_drone2<- lm(CL_pct_per_day ~ gliMAX+Initial_CL_mm+ gliMEAN+ gliPCT +mgvriPCT +mgvriMEAN +mgvriMAX
                        , data = data2, na.action = "na.fail")

combinations_drone2 <- dredge(globalmodel_drone2)

print(combinations_drone2)# Initial_CL_mm+ mgvriMEAN +mgvriMAX


mod2_drone <- lm(CL_pct_per_day ~ Initial_CL_mm+ mgvriMEAN +mgvriMAX
                 , data = data2)
#let's check the VIF's
mod2_vifsd <- car::vif(mod2_drone)
print(mod2_vifsd) #high with both, need to remove one, high also with gliMAX so next option is just MGVRI MAX as model 25

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Season 3 late wet 2021 sept-dec 2021

#transforming
data3$Bahiagrass2 <- asin(sqrt(data3$Bahiagrass/ 100))
data3$Wiregrass2 <- asin(sqrt(data3$Wiregrass/ 100))
data3$BareSoil2 <- asin(sqrt(data3$BareSoil/ 100))
data3$NativeGrass2<- asin(sqrt(data3$NativeGrass/ 100))
data3$ExoticGrass2 <- asin(sqrt(data3$ExoticGrass/ 100))
data3$NativeLegume2 <- asin(sqrt(data3$NativeLegume/ 100))
data3$ExoticLegume2 <- asin(sqrt(data3$ExoticLegume/ 100))
data3$Herb2 <- asin(sqrt(data3$Herb/ 100))
data3$MediumWoody2 <- asin(sqrt(data3$MediumWoody/ 100))
data3$ShortWoody2 <- asin(sqrt(data3$ShortWoody/ 100))
data3$TallWoody2 <- asin(sqrt(data3$TallWoody/ 100))
data3$Vine2 <- asin(sqrt(data3$Vine/ 100))
data3$Palmetto2 <- asin(sqrt(data3$Palmetto/ 100))
data3$Sedge2 <- asin(sqrt(data3$Sedge/ 100))
data3$MeanDiversity2 <- asin(sqrt(data3$MeanDiversity/ 100))

data3$X.GreenHerb <- asin(sqrt(data3$XGreenHerb / 100))
shapiro.test(mydata$X.GreenHerb) #not 100% normal but better

data3$X.BrownHerb <- asin(sqrt(data3$XBrownHerb / 100))
shapiro.test(mydata$X.BrownHerb) #not normal but much closer

data3$X.Bare <- asin(sqrt(data3$XBare / 100))
shapiro.test(data3$X.Bare) #not normal but much closer

data3$X.Tree <- asin(sqrt(data3$XTree / 100))
shapiro.test(data3$X.Tree) #not normal but much closer

data3$X.Woody<- asin(sqrt(data3$XWoody/ 100))
shapiro.test(data3$X.Tree) #not normal but much closer


data3$per_CL <- asin(sqrt(data3$CL_pct_per_day/ 100))

#start here with transformed datasheet
data3<-S3_data_transformed

mydataS3 <- subset(data3, select=c(CL_pct_per_day, Wiregrass2, Bahiagrass2, ExoticGrass2, NativeLegume2, ExoticLegume2 , Herb2,  Vine2, Sedge2, Palmetto2, TallWoody2, MediumWoody2
                                    , ShortWoody2, BareSoil2, Initial_CL_mm))


mydataS3_h <- subset(data3, select=c(CL_pct_per_day, ExoticGrass2, NativeLegume2, Palmetto2, TallWoody2, Herb2,  Vine2, Sedge2, BareSoil2, Initial_CL_mm))


corrplot1 <- cor(mydataS3)#wiregrass and bahia, woody with grass, palmetto, bare
corrplot1
#short woody, tall woody palmetto, exotic legume are too correlated with bahia, so is wiregrass
#sedge and tall woody are highest with CL, so bahia is out, palmetto,  woody-short




#drones season 3 2021

globalmodelS3 <- lm(data3$CL_pct_per_day ~ data3$X.GreenHerb + data3$Initial_CL_mm + X.BrownHerb + X.Bare +
                     X.Woody, data = data3, na.action = "na.fail")

combinations5 <- dredge(globalmodelS3)

print(combinations5)
#green and initial only

mod9 <- lm(data3$CL_pct_per_day ~ data3$X.GreenHerb + data3$Initial_CL_mm)

summary(mod9)#r2 = .24
#let's check the VIF's
mod9_vifs <- car::vif(mod9)
print(mod9_vifs) #all good



globalmodel3 <- lm(CL_pct_per_day ~Initial_CL_mm + Wiregrass2 + Bahiagrass2 + ExoticGrass2 + Herb2 + Vine2 + NativeLegume2 +Sedge2+ TallWoody2 + ExoticLegume2+ ShortWoody2,
                   data = data3, na.action = "na.fail")


combinations3 <- dredge(globalmodel3)


print(combinations3)#sedge, tall woody, initial in top model

mod3_veg <- lm(CL_pct_per_day ~Initial_CL_mm + Sedge2+ TallWoody2,
               data = data3)
summary(mod3_veg)

#let's check the VIF's
mod3_vifs <- car::vif(mod3_veg)
print(mod3_vifs) #all good



####drone vegetation indices/spectral indices 

globalmodel_drone3<- lm(CL_pct_per_day ~ glimax+Initial_CL_mm+ glimean + gli90 +mgvri90 +mgvrimean +mgvrimax
                        , data = data3, na.action = "na.fail")

combinations_drone3 <- dredge(globalmodel_drone3) 

print(combinations_drone3)#initial only is best 

#let's check the VIF's
mod3_vifs <- car::vif(mod3_veg)
print(mod3_vifs) #all good



#load in data with habitat types
getwd()
mydataS3_h<- read.csv("Juv Growth Season 3 Master_drone_only.csv", fileEncoding = 'UTF-8-BOM')

mydataS3_h%>%
  ggplot(aes(sample = CL_pct_per_day)) +
  stat_qq() +
  stat_qq_line(col = "goldenrod") +
  facet_wrap(~mydata1$Habitat) +
  theme_minimal() +
  labs(title = "Normal Q-Q Plot")

anova_habitat <- aov(CL_pct_per_day~ Habitat, data = mydataS3_h)
summary(anova_habitat)
#so we know growth impacts habitat type, lets look at habitat variables

#subset data to not have habitat types or ID bc we aren't looking at those
mydata <- subset(mydataS3_h select=c(CL_pct_per_day, X.BrownHerb, X.GreenHerb, 
          X.Woody, X.TotalHerb, X.Bare, X.Tree, Initial_CL_mm))

#Take a look at the qqplot of the response variable
qqnorm(mydataS3_h$CL_pct_per_day)
qqline(mydata$CL_pct_per_day)


#season 4 late dry season 2022 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Mar-June 2022 (dec-mar is not counted because it has zero growth)
#drone only done, no on the ground plant surveys

data_4 <- S4_Mar_June22_PerCover
shapiro.test(data_4$CL_Pct_Per_Day)#normal

#going to make them a little more normal for good practice
data_4$X.GreenHerb <- asin(sqrt(data_4$XGreenHerb / 100))
shapiro.test(data_4$X.GreenHerb) #not 100% normal but better

data_4$X.BrownHerb <- asin(sqrt(data_4$XBrownHerb / 100))
shapiro.test(data_4$X.BrownHerb) #normal

data_4$X.Woody <- asin(sqrt(data_4$XWoody / 100))
shapiro.test(data_4$X.Woody) #normal

data_4$X.Tree <- asin(sqrt(data_4$XTree / 100))
shapiro.test(data_4$X.Tree) #normal

data_4$X.Bare <- asin(sqrt(data_4$XBare / 100))
shapiro.test(data_4$X.Bare) #normal


###S4 mar -june drones


mydata_4 <- subset(data_4, select=c(CL_Pct_Per_Day, X.GreenHerb, X.BrownHerb, X.Tree, X.Bare,X.Woody,
                                    Initial_CL_mm))

corrplot_4 <- cor(mydata_4)
corrplot_4 #bare and green too correlated


#all possible models~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
globalmodel_4 <- lm(CL_Pct_Per_Day ~ X.GreenHerb + Initial_CL_mm + X.BrownHerb + X.Tree + X.Bare +
                      X.Woody, data = data_4, na.action = "na.fail")

combinations_4 <- dredge(globalmodel_4)

print(combinations_4)
#doesn't work bc nas, now works! woody only (but include intial CL )




mod10 <-lm(data_4$CL_Pct_Per_Day ~  data_4$X.Woody+ data_4$Initial_CL_mm)
summary(mod10) #R2=.31 
#let's check the VIF's
mod10_vifs <- car::vif(mod10)
print(mod10_vifs) #all good


mod11 <-lm(data_4$CL_Pct_Per_Day ~  data_4$X.GreenHerb)
summary(mod11) #R2 = .08


#drone only taken for this season, no on the ground plant surveys 
#now we look at spectral data 
#data_4 <- S4_Mar_June22_PerCover
#shapiro.test(data_4$CL_Pct_Per_Day)#normal



#plot season 4 
data_4 %>%
  ggplot(aes(x= X.Woody)) + 
  geom_point(aes(y=CL_Pct_Per_Day)) +
  geom_smooth(aes(y=CL_Pct_Per_Day), method="lm", se=TRUE) +
  theme_few()



#all possible models - well this not very short in  R realistically, so I did this in JMP with forward and backbackward as well and it agreed

#multispectral indicies
globalmodel12 <- lm(CL_Pct_Per_Day~Initial_CL_mm + ndvimax+ rtvimean + rtvimax + rtvi90 +srreemean + srreemax +srree90 +
                    mtci90 +mtcimean +mtcimax  + ndvi90 + cirededge90 +cirededgemax + cirededgemean +cigreenmean +cigreenmax + cigreen90
                  , data = data_4, na.action = "na.fail")

combinations12 <- dredge(globalmodel12)

print(combinations12)
#same as JMP , the null model with just CL initial is best 


#try with drone spectral indices 
globalmodel_22 <- lm(CL_Pct_Per_Day ~ glimax+Initial_CL_mm+ glimean + gli90 + mgvri90 +mgvrimean +mgvrimax
                    , data = data_4, na.action = "na.fail")

combinations_22 <- dredge(globalmodel_22)

print(combinations_22)#mgvri max and initial chosen


mod4_sp <- lm(CL_Pct_Per_Day ~ Initial_CL_mm +mgvrimax
              , data = data_4)

#let's check the VIF's
mod4_vifs <- car::vif(mod4_sp)
print(mod4_vifs) #all good



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

###S5 june-sept 2022 drones
getwd()
#already transformed data sheet 
data_5 <- read.csv("S5_JuneSept22_data_transformed.csv")

#technically this is season 6 because because dec-mar 2022 had zero growth
data_5 <- S6_June_Sept22_submit
  
  
  
#going to make them a little more normal for good practice
data_5$X.GreenHerb <- asin(sqrt(data_5$XGreenHerb / 100))
shapiro.test(data_5$X.GreenHerb) #not 100% normal but better

data_5$X.BrownHerb <- asin(sqrt(data_5$XBrownHerb / 100))
shapiro.test(data_5$X.BrownHerb) #normal

data_5$X.Woody <- asin(sqrt(data_5$XWoody / 100))
shapiro.test(data_5$X.Woody) #normal

data_5$X.Tree <- asin(sqrt(data_5$XTree / 100))
shapiro.test(data_5$X.Tree) #normal enough

data_5$X.Bare <- asin(sqrt(data_5$XBare / 100))
shapiro.test(data_5$X.Bare) #normal


data_5$MeanCL1 <- data_5$MeanCL1...1
mydata_5 <- subset(data_5, select=c(CL_Pct_Per_Day, X.GreenHerb, X.BrownHerb, X.Tree, XBare, X.Woody,
                                    MeanCL1))

corrplot_5 <- cor(mydata_5)
corrplot_5 #tree and green too correlated, woody and brown too correlated, keep woody and brown 


#####swapping green herb for woody ~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata4 <- subset(mydata, select=c(CL_mm_per_day, X.BrownHerb, X.Woody, X.Bare, X.Tree, Initial_CL_mm))


#try with woody
globalmodel_6 <- lm(CL_Pct_Per_Day ~ X.BrownHerb + MeanCL1 + X.Woody + X.Bare +X.GreenHerb
                    , data = data_5, na.action = "na.fail")

combinations_6 <- dredge(globalmodel_6)

print(combinations_6)#woody and initial chosen



mod12 <-lm(data_5$CL_Pct_Per_Day ~  data_5$X.Woody +data_5$MeanCL1)
summary(mod12) #R2=.5 , p =.005 _woody is better than green herb
#       Min         1Q     Median         3Q        Max 
#-3.840e-04 -1.557e-04 -6.167e-05  1.471e-04  6.046e-04 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     2.133e-03  3.134e-04   6.807 5.93e-06 ***
#  data_5$X.Woody -4.447e-04  2.374e-04  -1.873   0.0806 .  
#data_5$MeanCL1 -6.100e-06  1.851e-06  -3.296   0.0049 ** 

#Residual standard error: 0.0002993 on 15 degrees of freedom
#Multiple R-squared:  0.5011,	Adjusted R-squared:  0.4346 
#F-statistic: 7.533 on 2 and 15 DF,  p-value: 0.005433

mod13 <-lm(data_5$CL_Pct_Per_Day ~  data_5$X.GreenHerb +data_5$MeanCL1)
summary(mod13) #R2 = .47, p =.008


#try with spectral indices


mydata5 <- subset(data_5, select=c(CL_Pct_Per_Day, ndvimax, MeanCL1, ndvi90, cirededge90, ciredegemax,cirededgemean,cigreenmean,cigreenmax, cigreen90
                ))


corrplot(mydata5)


#multispectral only 
globalmodel_7<- lm(CL_Pct_Per_Day ~
                    Initial_CL_mm + ndvimax+ rtvimean + rtvimax + rtvi90 +srreemean + srreemax +srree90 +
                     mtci90 +mtcimean +mtcimax  + ndvi90 + cirededge90 +ciredegemax + cirededgemean +cigreenmean +cigreenmax + cigreen90
                   , data = data_5, na.action = "na.fail")

combinations_7 <- dredge(globalmodel_7)

print(combinations_7)#ndvi max and ndvi 90 with initial chosen 

#try with spectral indices/vegetation indices 

globalmodel_drone<- lm(CL_Pct_Per_Day ~ glimax+Initial_CL_mm+ glimean + gli90 +mgvri90 +mgvrimean +mgvrimax
                   , data = data_5, na.action = "na.fail")

combinations_drone <- dredge(globalmodel_drone)

print(combinations_drone)#gli max and gli 90 and mgvri max with initial chosen but these are too correlated, 4
#lets try removing glimax because it has a really high vif

globalmodel_drone2<- lm(CL_Pct_Per_Day ~ Initial_CL_mm+ glimean + gli90 +mgvri90 +mgvrimean +mgvrimax
                       , data = data_5, na.action = "na.fail")

combinations_drone2 <- dredge(globalmodel_drone2)

print(combinations_drone2) ##gli 90 and mgvri max with initial chosen and have low enough VIFs



mod13 <-lm(data_5$CL_Pct_Per_Day ~  data_5$mgvrimax+data_5$CL_Pct_Per_Day)
summary(mod13)
#let's check the VIF's
mod13_vifs <- car::vif(mod13)
print(mod13_vifs) #all good




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#write to csvs
write.csv(data, "S1_data_transformed.csv")
write.csv(data2, "S2_data_transformed.csv")
write.csv(data3, "S3_data_transformed.csv")
write.csv(data4, "S4_data_transformed.csv")
write.csv(data_5, "S5_JuneSept22_data_transformed.csv")
getwd()




####__________________more ways to analyze and observe things

#let's check the VIF's
forward_vifs <- car::vif(forward)
print(forward_vifs)

backward_vifs <- car::vif(backward)
print(backward_vifs)
#all under 4 or 10 so that is good



#let's go back to checking the assumptions
#some of this we have already done and are just double checking/doing a different way
#R can by default display 6 different plots using lm data
#which = 2 will select the Q-Q plot
?plot.lm



# 1. linearity of the data 
plot(mod1, 1)
# we are looking for here is no fitted pattern (i.e. the red line should be relatively flat)
# checks out enough

# 2. independence of variables, checking for colinearity between predictor variables 
# there are two ways to get at this visually - correlation matrix, we already did 
# gplot to visualize
corrplot(cor(mydata2),method='circle')

# 3. normality of residuals 
plot(mod1, which = 2) #looks pretty good
#another way to do the same
plot(mod1, 2) #looking for a straight line with the points so it is pretty good
#Histogram of residuals
hist(mod1$residuals) #lookin good

# 4. homogeneity of variances 
plot(mod1, 3)
#what we are looking for here is a horizontal line with equally spread points
#we see that the variability  of the residual points stay around 
#the same value of the fitted outcome variable, suggesting that too much heteroscedasticity 
#does not exist
#this is an important assumption to make sure we do not violate 


#5. we already checked VIFs


#### Reminder you can use AIC to compare GLMMs you make too by using glance


#you can also do forward and backward stepwise but this has its issues, you can generally get the same results though 
#these models, can also get the same using JMP SAS all possible models with lowest Aicc



