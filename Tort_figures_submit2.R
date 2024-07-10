#Figures for growth and habitat GT paper, made by Leyna Stemle
##with code and edits from Jack Christie and Seokmin Kim
#figures on growth and habitat and size,
#map figures made in GIS, ArcPro


library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
library(colorspace)
library(dplyr)
#install.packages("renv")
library(renv)
library(cowplot)



#load in data as hab or rename it here
hab



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Figures 1 and 3 in GIS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Figure 2, led by coauthor Jack


weather.data <-finweath #import from xls
# remind R that date is a date, again
weather.data$Date.adjust <- as.Date(weather.data$Date.adjust, format = "%m/%d/%Y")
head(weather.data)

# homogenize days in the month
weather.data$Date.adjust <- as.Date(format(weather.data$Date.adjust, "%Y-%m-%1"))

# need to specify order legend is presented
legend_order <- c("Early Dry Season", "Late Dry Season", "Early Wet Season", "Late Wet Season")

# need to remove the first month since it isn't a complete month
weather.data <- weather.data[-c(1:16),]
weather.data <- weather.data[-c(546:549),]


save3 <-ggplot(weather.data, aes(x = factor(Date.adjust), y = Value, fill = season)) +
  geom_col() +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "", x = "Month", y = "Rainfall (cm)") +
  theme_bw() +
  scale_x_discrete(labels = c("Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021",
                              "Sep 2021", "Oct 2021", "Nov 2021", "Dec 2021",
                              "Jan 2022", "Feb 2022", "Mar 2022", "Apr 2022", "May 2022",
                              "Jun 2022", "Jul 2022", "Aug 2022", "Sep 2022")) +
  ylim(0, 40)+
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold", size = 20)) +
  scale_fill_manual(values = c("Early Dry Season" = "#eddca5", "Late Dry Season" = "#c99b38",
                               "Early Wet Season" = "#8fd7d7", "Late Wet Season" = "#00b0be"),
                    breaks = c("Early Dry Season", "Late Dry Season", 
                               "Early Wet Season", "Late Wet Season")) +
  guides(size = FALSE) +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

save3
#not saving right dpi unless png, or tiff
ggsave(save3, file="C:/Users/13143/Documents/GT/Animal Con paper/Tortoises/rain_final.png", 
       height=168, width=220, units="mm", dpi = 400)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Figure 4

save <-Graphs_CL_raw_data_june_june_annual3%>%
  ggplot(aes(x = age_num2, y = sept_2_avg, group = habitat)) + 
  labs(x = "Tortoise age (years)", y = "Mean Carapace Length (mm)")+
  scale_x_continuous(name="Tortoise age (years)", limits=c(0,10), breaks=c(0:10), labels=c(0:10))+ 
  # xlim(0,11) Having xlim and scale_x_continous breaks things somehow.
  ylim(0,450)+
  geom_point(aes(y=sept_2_avg,color=habitat, size = 1.5)) +
  #geom_smooth(aes(y=sept_2_avg), method="lm", se=TRUE) +
  #geom_line(aes(color= habitat, linetype = habitat, size =1)) + 
  scale_color_manual(values = c("orange", "steelblue"))+
  #facet_wrap(~habitat)+
  stat_smooth(method="lm", fullrange=TRUE) +
  theme_bw()+
  theme(text = element_text(size=20))

save
#helped with 
#https://stackoverflow.com/questions/38369884/how-can-i-change-the-x-axis-ticks-while-still-limiting-the-x-axis-range

getwd()

#save
ggsave(save, file="C:/Users/.../CL_age_extend10.png", 
       height=168, width=220, units="mm", dpi=400)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fig 5
#RMANOVA boxplot

#rename
Juv_1_6_RMANOVA2 <- Juv_GrowthRates_S1_6_RMANOVA_corrected_cleaner_gis

#make percentages instead of decimal
Juv_1_6_RMANOVA2$CL_pct <-  (Juv_1_6_RMANOVA2$CL_pct)*100
#plot in ggplot
bxp <- ggboxplot(Juv_1_6_RMANOVA2, x = "time", y = "CL_pct", fill = "habitat")


#print to see progress
bxp

#adding on
bxp <- ggboxplot(Juv_1_6_RMANOVA2, x = "time", y = "CL_pct", fill = "habitat", palette = c("orange", "steelblue"))+
  #scale_color_manual(values = c("orange", "steelblue"))+
  # scale_fill_discrete_qualitative(palette = "Warm", nmax = 5, order = c(1,2))+
  labs(x =" Season", y = "Carapace Length % Growth Per Day")+
  theme_bw() +
  scale_x_discrete(labels = c("Mar-June 2021", "June-Sept 2021", "Sept-Dec 2021", "Dec-Mar 2022", "Mar-June 2022", "June-Sept 2022")) +
  # ylim(0, 40)+
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        #axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold", size = 15)) +
  guides(size = FALSE) +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank())


#not saving right dpi unless png, or tiff
ggsave(bxp, file="C:/Users.../boxplotfigure5.png", 
       height=168, width=220, units="mm", dpi = 400)

#RMANOVA script
#correct for multiple comparisons
#Supplemental table S5

#Benjamini-hochberg method
one.way2 <- Juv_GrowthRates_S1_6_RMANOVA_corrected_cleaner_gis %>%
  group_by(time) %>%
  anova_test (formula = CL_pct ~ habitat) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")


#print results
one.way2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Panel A of Figure 6
plot1 <-hab %>%
  ggplot(aes(x= Bahiagrass,y=CL_pct_per_day)) + 
  geom_point(aes(colour= Habitat, size = 1.5)) +
  geom_smooth(aes(y=CL_pct_per_day), method="lm", se=TRUE) +
  labs(x ="Bahiagrass Presence", y = "Carapace Length % Growth Per Day")+
  scale_color_manual(values = c("orange", "steelblue"))+
  theme_bw()+
  theme(text = element_text(size=20))
  
plot1

#export plot
ggsave(plot1, file="C:/Users/13143/Documents/GT/Animal Con paper/Tortoises/Bahia_CL.tiff", 
       height=168, width=220, units="mm", dpi=350) #tiff saves really large, can do as jpeg or png then change to tiff


#Panel B of Figure 6
plot6 <-hab %>%
  ggplot(aes(x= XGreenHerb,y=CL_pct_per_day)) + 
  geom_point(aes(colour = Habitat, size = 1.5)) +
  geom_smooth(aes(y=CL_pct_per_day), method="lm", se=TRUE) +
  labs(x ="Percent Green Herb", y = "Carapace Length % Growth Per Day")+
  scale_color_manual(values = c("orange", "steelblue"))+
  theme_bw()+
  theme(text = element_text(size=20))
plot6

#export plot
ggsave(plot6, file="C:/Users/13143/Documents/GT/Animal Con paper/Tortoises/GReen_CL.tiff", 
       height=168, width=220, units="mm", dpi=350)

