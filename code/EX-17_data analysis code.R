##### EX-17: ECM COMPARISONS IN HAPIN GUATEMALA, DATA ANALYSIS CODE #####

#Packages to use
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(freqtables)
library(na.tools)
library(ggpubr)
library(RVAideMemoire)
library(ggthemes)
library(irr)
library(Metrics)
library(svglite)

##### DEMOGRAPHIC CHARACTERISTICS #####
#Load datasets
demographics <- read_csv(here("data", "EX17_Demograph_Guate.csv"))
visits <- read_csv(here("data", "Guate_Alldupvisits_nullremoved.csv"))

#Determine HHs with X number of visits. This line of code is used to compute Table S3, manually.
length(which(table(visits$hhid)== 2)) #Replace the number from 1 to 6

#Frequency tables by visit (timepoint). This line of code is used to compute Table S2, manually.
visits %>% freq_table(timepoint)

####Data transformations####
#Categorize ages from 18+, 24+ and 30+
demographics$age_cat <- cut(demographics$age_screening,
                            breaks = c(17, 24, 30, 35),
                            labels = c("18 to 23", "24 to 30", "30 to 35"))

#Categorize gestational weeks from 9+, 13+ and 17+
demographics$ga_cat <-  cut(demographics$ga_screen_weeks,
                            breaks= c(8, 13, 17, 21),
                            labels= c("9 to 12", "13 to 16", "17 to 20"))

#Categorize family size from 2-4, 5-7 and 8-11
demographics$family_cat <-  cut(demographics$family_size,
                                breaks= c(1, 4, 7, 12),
                                labels= c("2 to 4", "5 to 7", "8 to 12"))

#See changes in the 'demographics' data frame
glimpse(demographics)

####Descriptive statistics####
#The following codes are used to manually edit Table S1.

#Participant's Age
demographics %>% freq_table(age_cat)

#Participant's Age by Study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(age_cat)

#Mean and SD of Participant's Age
mean(demographics$age_screening)
sd(demographics$age_screening)

#Mean of Participant's Age by Study arm
demographics %>% 
  select(age_screening, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  summarise(mean= mean(age_screening))

#SD of Participant's Age by Study arm
demographics %>% 
  select(age_screening, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  summarise(SD= sd(age_screening))

#Participant's Gestational age
demographics %>% freq_table(ga_cat)

#Participant's Gestational age by Study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(ga_cat)

#Mean and SD of Participant's Gestational age
mean(demographics$ga_screen_weeks)
sd(demographics$ga_screen_weeks)

#Mean of Participant's Gestational age by Study arm
demographics %>% 
  select(ga_screen_weeks, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  summarise(mean= mean(ga_screen_weeks))

#SD of Participant's Gestational age by Study arm
demographics %>% 
  select(ga_screen_weeks, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm 
  summarise(SD= sd(ga_screen_weeks))

#Number of biomass stoves (Overall)
demographics %>% freq_table(stove_num)

#Number of biomass stoves by study arm
demographics %>%
  filter(treatment== "R") %>%  #Replace R by Q, to check in each study arm
  freq_table(stove_num)

#Main stove type (Overall)
demographics %>% freq_table(main_stove)

#Main stove type by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(main_stove)

#Trash disposal (Overall)
demographics %>% freq_table(trash)

#Trash disposal by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(trash)

#Lighting source (Overall)
demographics %>% freq_table(light)

#Lighting source by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(light)

#Tobacco smoking (Overall)
demographics %>% freq_table(tobacco)

#Tobacco smoking by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(tobacco)

#Mother education (Overall)
demographics %>% freq_table(mom_education)

#Mother education by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(mom_education)

#Mother occupation (Overall)
demographics %>% freq_table(mom_occupation)

#Mother occupation by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(mom_occupation)

#Family size (Overall)
demographics %>% freq_table(family_cat)

#Family size by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(family_cat)

#Mean and SD of Family size
mean(demographics$family_size)
sd(demographics$family_size)

#Mean of family size by study arm
demographics %>% 
  select(family_size, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  summarise(mean= mean(family_size))

#SD of family size by study arm
demographics %>% 
  select(family_size, treatment) %>% 
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  summarise(SD= sd(family_size))

#Family size by category (Overall)
demographics %>% freq_table(family_cat)

#Family size by category by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(family_cat)

#Primary cook at home (Overall)
demographics %>% freq_table(primary_cook)

#Primary cook at home by study arm
demographics %>%
  filter(treatment== "R") %>% #Replace R by Q, to check in each study arm
  freq_table(primary_cook)


##### PM2.5 CONCENTRATIONS #####
#Load dataset
concentrations_temp <- read_csv(here("data", "Allconcentrations_Guate.csv"))

####Data transformations####
#Create new column in dataset classifying by Arm
concentrations_temp <- mutate(concentrations_temp, Arm= case_when(
  startsWith(concatenated, "R") ~ "Control",
  startsWith(concatenated, "Q") ~ "Intervention"))

#Save as new file
write_csv(concentrations_temp, here("data", "processed-data", "EX17_allconc_Guate.csv"))

####Statistical summaries of concentrations####
#New data file, all concentrations
concentrations <- read.csv("data/processed-data/EX17_allconc_Guate.csv", na.rm(T))

#Summary of concentrations by type of ECM (Grav vs Neph), Study Arm and Visit. This output is used to manually create Tables S3 and S4
#Compute the min, max, mean, median and 1st and 3rd quartiles (IQR) of the concentrations
concentrations %>% 
  filter(ecm_type== "Nephelometric") %>% #Filter by type of ecm= 'Gravimetric' or 'Nephelometric'
  filter(concatenated== "QP2B1") %>% #Filter by visit and study arm, using the concatenated variable= 'QBL', 'RBL', 'QBLP1', 'RBLP1, etc.
  summary(all_conc)

#Compute the SDs of the concentrations
concentrations %>% 
  filter(ecm_type== "Nephelometric") %>% #Filter by type of ecm= 'Gravimetric' or 'Nephelometric'
  filter(concatenated== "QP2B1") %>% #Filter by visit and study arm, using the concatenated variable= 'QBL', 'RBL', 'QBLP1', 'RBLP1, etc.
  summarise(SD= sd(all_conc))

#Boxplots of concentrations by visit and type of ecm (Gravimetric or Nephelometric)
#Figure 3
figure3 <- concentrations %>% 
  ggplot(aes(x= visit, 
             y= all_conc, 
             fill= Arm))+ #select the x and y axis and differentiate fueltype by color
  geom_boxplot(varwidth = T)+ #type of plot (In this case boxplot)
  scale_fill_manual(values = c("sienna2", "steelblue2"))+ #Change filling colors
  facet_wrap(~ecm_type)+ #Differentiate Gravimetric and Nephelometric in the same graph
  theme_bw(base_size = 17)+ #Change plot theme, indicate the size of the bars
  geom_hline(yintercept =35, #The line should intercept at the 35 y-axis
             colour="black", #color of line
             linetype="dashed")+ #dashed line
  geom_signif(y_position = 2.88, #To show the stat. significance bar, this to place the y-position
              xmin = 0.6, #the minimum x position
              xmax = 1.4, #the maximum x position
              annotations = "NS", #to show only the non-significant parameters (in this case BL)
              tip_length = 0.01)+ #the tip length of the significance bar
  scale_y_log10()+ #Log scaling the y-axis for a better visualization
  xlab("Visit")+ #x-axis label
  ylab(expression(PM[2.5]~concentration~(ug/m^3))) #y-axis label, using underscripts and superscripts by the "expression" function

#Save this plot
ggsave(here("figures", "figure3.png"))

#Save plot as .svg file, for journal submission
ggsave("figures/figure3.svg", figure3, device = "svg")

##### ECM COMPARISONS #####
#Load dataset
ecmdups <- read_excel(here("data", "EX17_ECMComparisons.xlsx"))

####Data manipulation####
#Concatenate visit and treatment to classify comparisons according to fuel type
ecmdups$concatenated <- paste(ecmdups$treatment, ecmdups$visit)

#Create new column named "Group" categorizing visits by study arm, which reflects use of fuel.
ecmdups <- mutate(ecmdups, Group = case_when(
  endsWith(concatenated, "BL") ~ "Control",
  endsWith(concatenated, "R P1") ~ "Control",
  endsWith(concatenated, "R P2") ~ "Control",
  endsWith(concatenated, "Q P1") ~ "Intervention",
  endsWith(concatenated, "Q P2") ~ "Intervention",
  endsWith(concatenated, "R BLP1") ~ "Control",
  endsWith(concatenated, "R P1P2") ~ "Control",
  endsWith(concatenated, "R P2B1") ~ "Control",
  endsWith(concatenated, "Q BLP1") ~ "Intervention",
  endsWith(concatenated, "Q P1P2") ~ "Intervention",
  endsWith(concatenated, "Q P2B1") ~ "Intervention"))

#Create data set with only Baseline observations
#This is for the pre-randomization analysis (if necessary)
ecmbl <- ecmdups %>% filter(visit == "BL")

#Create data set with Control and Intervention post-randomization observations.
#This is for the post-randomization correlation analysis.
ecmpost <- ecmdups %>% filter(visit != "BL")

####Spearman Correlations####
#Create gravimetric and nephelometric dataframes
ecmgravs <- filter(ecmdups, Sample_type== "Gravimetric")
ecmnephs <- filter(ecmdups, Sample_type== "Nephelometric")

#Check N's for these dfs (For Figure 2)
length(rownames(ecmgravs)) #Grav
length(rownames(ecmnephs)) #Neph

#Create dataframes by study arm
ecmgravs_control <- filter(ecmgravs, Group== "Control") #Gravimetric-Control
ecmgravs_intervention <- filter(ecmgravs, Group== "Intervention") #Gravimetric-Intervention
ecmnephs_control <- filter(ecmnephs, Group== "Control") #Nephelometric-Control
ecmnephs_intervention <- filter(ecmnephs, Group== "Intervention") #Nephelometric-Intervention

#Check N's for these dfs (For Figure 2)
length(rownames(ecmgravs_control)) #Grav-Cont
length(rownames(ecmgravs_intervention)) #Grav-Int
length(rownames(ecmnephs_control)) #Neph-Cont
length(rownames(ecmnephs_intervention)) #Neph-Int

#Compute Spearman correlations (Overall correlations for Grav and Neph observations)
#To manually include in Table 1
spearman.ci(ecmgravs$ecm1_conc, ecmgravs$ecm2_conc, nrep = 1000, conf.level = 0.95) #Gravimetric
spearman.ci(ecmnephs$ecm1_conc, ecmnephs$ecm2_conc, nrep = 1000, conf.level = 0.95) #Nephelometric

#Compute Spearman correlations by study arm
#To manually include in Table 1
spearman.ci(ecmgravs_control$ecm1_conc, ecmgravs_control$ecm2_conc, nrep = 1000, conf.level = 0.95) #Gravimetric-Control
spearman.ci(ecmgravs_intervention$ecm1_conc, ecmgravs_intervention$ecm2_conc, nrep = 1000, conf.level = 0.95) #Gravimetric-Intervention
spearman.ci(ecmnephs_control$ecm1_conc, ecmnephs_control$ecm2_conc, nrep = 1000, conf.level = 0.95) #Nephelometric-Gravimetric
spearman.ci(ecmnephs_intervention$ecm1_conc, ecmnephs_intervention$ecm2_conc, nrep = 1000, conf.level = 0.95) #Nephelometric-Intervention

#Spearman correlation plots####
#Plot of correlations between duplicate gravimetric ECMs and duplicate nephelometric ECMs
#Figure 4
figure4 <- ecmdups %>%
  ggplot(aes(x= ecm1_conc,
             y= ecm2_conc))+ #Select x and y axis
  geom_point(shape=21, fill= "steelblue", color="black", size=2)+ #Select shape of points, filling color, color of border and size of shape
  geom_smooth(method = lm, colour= "navyblue", se= T)+ #Add the lm line
  facet_wrap(~Sample_type)+ #To differentiate Gravimetric and Nephelometric in the same graph
  xlab(expression(ECM1~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(ECM2~PM[2.5]~concentration~(ug/m^3)))+ #Label the y-axis
  theme_clean(base_size = 21)+ #Select theme of graph
  theme(aspect.ratio = 1)+ #Aspect ratio of graph "square"
  geom_abline(intercept = 0, slope = 1, linewidth= 0.5)+ #Insert a 45deg line
  stat_cor(method = "spearman", #Add the spearman correlation value on the graph
           alternative = "two.sided", #two-sided test
           cor.coef.name = "rho", #for Spearman symbol
           label.x.npc = "left", #x position
           label.y.npc = "top", #y position
           p.accuracy = 0.001, #accuracy of the p-value
           size= 5) #size of text

#Save this plot
ggsave(here("figures", "figure4.png"))

#Save plot as .svg file for journal submission
ggsave("figures/figure4.svg", figure4, device = "svg")

#Color palette for next plot
palette <- c("steelblue", "orangered2")

#Plot of correlations between duplicate gravimetric ECMs and duplicate nephelometric ECMs, by study arm
#Figure 5
figure5 <- ecmdups %>%
  ggplot(aes(x= ecm1_conc,
             y= ecm2_conc, color= Group))+ #Select x and y axis
  geom_point(shape=16, size=2)+ #Select shape of points, filling color, color of border and size of shape
  color_palette(palette)+
  geom_smooth(aes(group = Group),method = lm,se= T)+ #Add the lm line
  facet_wrap(~Sample_type)+ #To differentiate Gravimetric and Nephelometric in the same graph
  xlab(expression(ECM1~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(ECM2~PM[2.5]~concentration~(ug/m^3)))+ #Label the y-axis
  theme_clean(base_size = 20)+ #Select theme of graph
  theme(aspect.ratio = 1)+ #Aspect ratio of graph "square"
  geom_abline(intercept = 0, slope = 1, linewidth= 0.5)+ #Insert a 45deg line
  scale_x_log10()+ #Log transform x scale for better visualization
  scale_y_log10()+ #Log transform y scale for better visualization
  stat_cor(method = "spearman", #Add the spearman correlation value on the graph
           alternative = "two.sided", #two-sided test
           cor.coef.name = "rho", #for Spearman symbol
           label.x.npc = "left", #x position
           label.y.npc = "top", #y position
           p.accuracy = 0.001, #accuracy of the p-value
           size= 5) #size of text

#Save this plot
ggsave(here("figures", "figure5.png"))

#Save file as .svg for journal submission
ggsave("figures/figure5.svg", figure5, device = "svg")

#Plot of correlations between duplicate gravimetric ECMs and duplicate nephelometric ECMs, by study arm (Post Randomization comparisons only)
#Figure 6
figure6 <- ecmpost %>%
  ggplot(aes(x= ecm1_conc,
             y= ecm2_conc, color= Group))+ #Select x and y axis
  geom_point(shape=16, size=2)+ #Select shape of points, filling color, color of border and size of shape
  color_palette(palette)+
  geom_smooth(aes(group = Group),method = lm,se= T)+ #Add the lm line
  facet_wrap(~Sample_type)+ #To differentiate Gravimetric and Nephelometric in the same graph
  xlab(expression(ECM1~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(ECM2~PM[2.5]~concentration~(ug/m^3)))+ #Label the y-axis
  theme_clean(base_size = 20)+ #Select theme of graph
  theme(aspect.ratio = 1)+ #Aspect ratio of graph "square"
  geom_abline(intercept = 0, slope = 1, linewidth= 0.5)+ #Insert a 45deg line
  scale_y_log10()+ #Modify y axis for better visualization
  scale_x_log10()+ #Modify x axis for better visualization
  stat_cor(method = "spearman", #Add the spearman correlation value on the graph
           alternative = "two.sided", #two-sided test
           cor.coef.name = "rho", #for Spearman symbol
           label.x.npc = "left", #x position
           label.y.npc = "top", #y position
           p.accuracy = 0.001, #accuracy of the p-value
           size= 5) #size of text

#Save this plot
ggsave(here("figures", "figure6.png"))

#Save file as .svg for journal submission
ggsave("figures/figure6.svg", figure6, device = "svg")

####Bland-Altman Agreement####
#Create new column for concentration averages between both ECMs
ecmgravs$avg <- rowMeans(ecmgravs[,4:5]) #[,4:5] to select all rows and columns 4 and 5. Gravimetric dataset
ecmnephs$avg <- rowMeans(ecmnephs[,4:5]) #Nephelometric dataset
ecmdups$avg <- rowMeans(ecmdups[,4:5]) #Entire dataset

#Create new column for difference in measurements
ecmgravs$diff <- ecmgravs$ecm1_conc - ecmgravs$ecm2_conc #Gravimetric dataset
ecmnephs$diff <- ecmnephs$ecm1_conc - ecmnephs$ecm2_conc #Nephelometric dataset
ecmdups$diff <- ecmdups$ecm1_conc - ecmdups$ecm2_conc #Entire dataset

#find average difference
mean_diff_grav <- mean(ecmgravs$diff) #Gravimetric dataset
mean_diff_neph <- mean(ecmnephs$diff) #Nephelometric dataset

#find lower 95% confidence interval limits
lower_grav <- mean_diff_grav - 1.96 * sd(ecmgravs$diff) #Gravimetric dataset
lower_neph <- mean_diff_neph - 1.96 * sd(ecmnephs$diff) #Nephelometric dataset

#find upper 95% confidence interval limits
upper_grav <- mean_diff_grav + 1.96 * sd(ecmgravs$diff) #Gravimetric dataset
upper_neph <- mean_diff_neph + 1.96 * sd(ecmnephs$diff) #Nephelometric dataset

#Create Bland-Altman plots
#Bland-Altman plot for Gravimetric
baplot_grav <- ggplot(ecmgravs, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 17) +
  geom_text(x= 550, y=-10, size= 3.5, label= "Mean difference= 5.70 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(NA, 700)+ #Set the x-axis limits
  ylim(-280, 280) #Set the y-axis limits

#Bland-Altman plot for Nephelometric
baplot_neph <- ggplot(ecmnephs, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) +
  geom_hline(yintercept = mean_diff_neph) +
  geom_hline(yintercept = lower_neph, color = "navyblue", linetype="twodash") +
  geom_hline(yintercept = upper_neph, color = "navyblue", linetype="twodash") +
  theme_bw(base_size = 17) +
  geom_text(x= 550, y=-15, size= 3.5, label= "Mean difference= 4.33 µg/m3")+
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(NA, 700)+
  ylim(-280, 280)

#Combine plots in one Graph (This is for Figure 7)
baplot_sampletype <- ggarrange(baplot_grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                               baplot_neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                               nrow = 1, ncol = 2, #one row and two columns
                               labels = "AUTO", #automatically label plots as "A" and "B"
                               hjust = 0.1, #adjust horizontal position of labels
                               vjust = 1.1) #adjust vertical position of labels

#Add shared x and y-axis
annotate_figure(baplot_sampletype, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)")) #X-axis

#Save this plot
ggsave(here("figures", "figure7.png"))

#Save file as .svg for journal submission
ggsave("figures/figure7.svg", baplot_sampletype, device = "svg")

#Bland-Altman Agreement by study arm####
#Create new column for concentration Row averages
ecmgravs_control$avg <- rowMeans(ecmgravs_control[,4:5])
ecmgravs_intervention$avg <- rowMeans(ecmgravs_intervention[,4:5])
ecmnephs_control$avg <- rowMeans(ecmnephs_control[,4:5])
ecmnephs_intervention$avg <- rowMeans(ecmnephs_intervention[,4:5])

#Create new column for difference in measurements
ecmgravs_control$diff <- ecmgravs_control$ecm1_conc - ecmgravs_control$ecm2_conc
ecmgravs_intervention$diff <- ecmgravs_intervention$ecm1_conc - ecmgravs_intervention$ecm2_conc
ecmnephs_control$diff <- ecmnephs_control$ecm1_conc - ecmnephs_control$ecm2_conc
ecmnephs_intervention$diff <- ecmnephs_intervention$ecm1_conc - ecmnephs_intervention$ecm2_conc

#find average difference
mean_diff_grav_control <- mean(ecmgravs_control$diff)
mean_diff_grav_intervention <- mean(ecmgravs_intervention$diff)
mean_diff_neph_control <- mean(ecmnephs_control$diff)
mean_diff_neph_intervention <- mean(ecmnephs_intervention$diff)

#find lower 95% confidence interval limits
lower_grav_control <- mean_diff_grav_control - 1.96 * sd(ecmgravs_control$diff)
lower_grav_intervention <- mean_diff_grav_intervention - 1.96 * sd(ecmgravs_intervention$diff)
lower_neph_control <- mean_diff_neph_control - 1.96 * sd(ecmnephs_control$diff)
lower_neph_intervention <- mean_diff_neph_intervention - 1.96 * sd(ecmnephs_intervention$diff)

#find upper 95% confidence interval limits
upper_grav_control <- mean_diff_grav_control + 1.96 * sd(ecmgravs_control$diff)
upper_grav_intervention <- mean_diff_grav_intervention + 1.96 * sd(ecmgravs_intervention$diff)
upper_neph_control <- mean_diff_neph_control + 1.96 * sd(ecmnephs_control$diff)
upper_neph_intervention <- mean_diff_neph_intervention + 1.96 * sd(ecmnephs_intervention$diff)


#Create Bland-Altman plots
#Bland-Altman plot for Control-Gravimetric
baplot_control_grav <- ggplot(ecmgravs_control, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_control) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_control, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_control, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 18) +
  geom_text(x= 560, y= -20, size= 3.5 , label= "Mean difference= 8.82 µg/m3")+ #add Mean difference text manually (CHANGE `SIZE` ACCORDING FOR POSTER PRESENTATION)
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis 
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(NA, 700)+ #Set the x-axis limits
  ylim(-280, 280) #Set the y-axis limits

#Bland-Altman plot for Control-Nephelometric
baplot_control_neph <- ggplot(ecmnephs_control, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) +
  geom_hline(yintercept = mean_diff_neph_control) +
  geom_hline(yintercept = lower_neph_control, color = "navyblue", linetype="twodash") +
  geom_hline(yintercept = upper_neph_control, color = "navyblue", linetype="twodash") +
  theme_bw(base_size = 18) +
  geom_text(x= 560, y= 40, size= 3.5, label= "Mean difference= 6.48 µg/m3")+ #CHANGE `SIZE` ACCORDING FOR POSTER PRESENTATION
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(NA, 700)+
  ylim(-280, 280)

#Bland-Altman plot for Intervention-Gravimetric
baplot_intervention_grav <- ggplot(ecmgravs_intervention, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_intervention) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_intervention, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_intervention, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 17) +
  geom_text(x= 280, y= -9, size= 3.5, label= "Mean difference= 0.68 µg/m3")+ #add Mean difference text manually (Modify position) (CHANGE `SIZE` ACCORDINGLY FOR POSTER)
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  #xlim(NA, 110)+ #Set the x-axis limits
  #ylim(-60, 60) #Set the y-axis limits
  #xlim(NA, 700)+
  #ylim(-280, 280)
  xlim(NA, 350)+
  ylim(-140, 140)

#Bland-Altman plot for Intervention-Nephelometric
baplot_intervention_neph <- ggplot(ecmnephs_intervention, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=2.5) +
  geom_hline(yintercept = mean_diff_neph_intervention) +
  geom_hline(yintercept = lower_neph_intervention, color = "navyblue", linetype="twodash") +
  geom_hline(yintercept = upper_neph_intervention, color = "navyblue", linetype="twodash") +
  theme_bw(base_size = 17) +
  geom_text(x= 280, y= -9, size= 3.5, label= "Mean difference= 0.76 µg/m3")+ #Modify position as needed (CHANGE `SIZE` ACCORDINGLY FOR POSTER)
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  #xlim(NA, 110)+
  #ylim(-60, 60)
  #xlim(NA, 700)+
  #ylim(-280, 280)
  xlim(NA, 350)+
  ylim(-140, 140)

#Arrange all four plots together (Better visualization, but consider changing the axis and position of text)
baplot_all <- ggarrange(baplot_control_grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                        baplot_control_neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                        baplot_intervention_grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                        baplot_intervention_neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                        nrow = 2, ncol = 2, #one row and two columns
                        labels = "AUTO", #automatically label plots from "A" to "D"
                        hjust = -6, #adjust horizontal position of labels
                        vjust = 2) #adjust vertical position of labels

#Last step, this is for Figure 8.
annotate_figure(baplot_all, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)")) #X-axis

#Save this plot
ggsave(here("figures", "figure8.png"))

#Save plot as .svg
ggsave("figures/figure8.svg", baplot_all, device = "svg")

####Intra Class Correlations (ICC)####
#ICC analysis by grav and neph samples
#Log transform concentrations, creating new columns
ecmdups$log_ecm1 <- log(ecmdups$ecm1_conc)
ecmdups$log_ecm2 <- log(ecmdups$ecm2_conc)

#Create a new data frame for ECM grav. comparisons
grav_icc <- data.frame(A = c(ecmdups[1:220, 11]),
                       B = c(ecmdups[1:220, 12]))

#Create a new data frame for ECM neph. comparisons
neph_icc <- data.frame(A = c(ecmdups[221:441, 11]),
                       B = c(ecmdups[221:441, 12]))

#Perform the ICC oneway model (To manually input for Table 1)
icc(grav_icc, model = "oneway",
    type = "agreement", unit = "single") #Gravimetric measurements

icc(neph_icc, model = "oneway",
    type = "agreement", unit = "single") #Nephelometric measurements

#Intra Class Correlations by study arm####
#Log transform concentrations
#Gravimetric Control and Intervention
ecmgravs_control$log_ecm1 <- log(ecmgravs_control$ecm1_conc)
ecmgravs_control$log_ecm2 <- log(ecmgravs_control$ecm2_conc)
ecmgravs_intervention$log_ecm1 <- log(ecmgravs_intervention$ecm1_conc)
ecmgravs_intervention$log_ecm2 <- log(ecmgravs_intervention$ecm2_conc)

#Nephelometric Control and Intervention
ecmnephs_control$log_ecm1 <- log(ecmnephs_control$ecm1_conc)
ecmnephs_control$log_ecm2 <- log(ecmnephs_control$ecm2_conc)
ecmnephs_intervention$log_ecm1 <- log(ecmnephs_intervention$ecm1_conc)
ecmnephs_intervention$log_ecm2 <- log(ecmnephs_intervention$ecm2_conc)

#Create a new data frame for ECM gravimetric comparisons by study arm
grav_control_icc <- data.frame(A = c(ecmgravs_control$log_ecm1),
                            B = c(ecmgravs_control$log_ecm2))

grav_intervention_icc <- data.frame(A = c(ecmgravs_intervention$log_ecm1),
                           B = c(ecmgravs_intervention$log_ecm2))

#Create a new data frame for ECM nephelometric comparisons by study arm
neph_control_icc <- data.frame(A = c(ecmnephs_control$log_ecm1),
                            B = c(ecmnephs_control$log_ecm2))

neph_intervention_icc <- data.frame(A = c(ecmnephs_intervention$log_ecm1),
                           B = c(ecmnephs_intervention$log_ecm2))

#Perform the ICC oneway model, gravimetric comparisons by study arm
icc(grav_control_icc, model = "oneway",
    type = "agreement", unit = "single") #Control

icc(grav_intervention_icc, model = "oneway",
    type = "agreement", unit = "single") #Intervention

#Perform the ICC oneway model, nephelometric comparisons by study arm
icc(neph_control_icc, model = "oneway",
    type = "agreement", unit = "single") #Control

icc(neph_intervention_icc, model = "oneway",
    type = "agreement", unit = "single") #Intervention

####RMSE Analysis####
#To manually input in Table 1
rmse(ecmgravs$ecm1_conc, ecmgravs$ecm2_conc) #Gravimetric
rmse(ecmnephs$ecm1_conc, ecmnephs$ecm2_conc) #Nephelometric
rmse(ecmgravs_control$ecm1_conc, ecmgravs_control$ecm2_conc) #Gravimetric-Control
rmse(ecmgravs_intervention$ecm1_conc, ecmgravs_intervention$ecm2_conc) #Gravimetric-Intervention
rmse(ecmnephs_control$ecm1_conc, ecmnephs_control$ecm2_conc) #Nephelometric-Control
rmse(ecmnephs_intervention$ecm1_conc, ecmnephs_intervention$ecm2_conc) #Nephelometric-Intervention


####PERCENTILES (ADDITIONAL ANALYSES)####
####Data manipulation####
#Calculate quartiles
ecm_quartiles <- quantile(ecmdups$ecm1_conc, probs = c(0.25, 0.5, 0.75))
print(ecm_quartiles) #Print answer

#Add quantile variable to dataset, and assign to each percentile
ecmdups <- ecmdups %>% 
  mutate(Percentile = case_when(ecm1_conc < 24.34 ~ "A",
                                ecm1_conc >= 24.34 & ecm1_conc < 54.06 ~ "B",
                                ecm1_conc >= 54.06 & ecm1_conc < 126.8 ~ "C",
                                ecm1_conc >= 126.8 ~ "D"))

#Concatenate visit and treatment to classify comparisons according to fuel type
ecmdups$concatenated <- paste(ecmdups$treatment, ecmdups$visit)

#Create new column named "Group" categorizing visits by study arm, which reflects use of fuel.
ecmdups <- mutate(ecmdups, Group = case_when(
  endsWith(concatenated, "BL") ~ "Control",
  endsWith(concatenated, "R P1") ~ "Control",
  endsWith(concatenated, "R P2") ~ "Control",
  endsWith(concatenated, "Q P1") ~ "Intervention",
  endsWith(concatenated, "Q P2") ~ "Intervention",
  endsWith(concatenated, "R BLP1") ~ "Control",
  endsWith(concatenated, "R P1P2") ~ "Control",
  endsWith(concatenated, "R P2B1") ~ "Control",
  endsWith(concatenated, "Q BLP1") ~ "Intervention",
  endsWith(concatenated, "Q P1P2") ~ "Intervention",
  endsWith(concatenated, "Q P2B1") ~ "Intervention"))

#Create data frames for each percentile
ecmgravs_25 <- ecmdups %>% filter(Percentile == "A") %>% filter(Sample_type == "Gravimetric")
ecmnephs_25 <- ecmdups %>% filter(Percentile == "A") %>% filter(Sample_type == "Nephelometric")
ecmgravs_50 <- ecmdups %>% filter(Percentile == "B") %>% filter(Sample_type == "Gravimetric")
ecmnephs_50 <- ecmdups %>% filter(Percentile == "B") %>% filter(Sample_type == "Nephelometric")
ecmgravs_75 <- ecmdups %>% filter(Percentile == "C") %>% filter(Sample_type == "Gravimetric")
ecmnephs_75 <- ecmdups %>% filter(Percentile == "C") %>% filter(Sample_type == "Nephelometric")
ecmgravs_100 <- ecmdups %>% filter(Percentile == "D") %>% filter(Sample_type == "Gravimetric")
ecmnephs_100 <- ecmdups %>% filter(Percentile == "D") %>% filter(Sample_type == "Nephelometric")

####Spearman correlation plots####
#Color palette
cor_palette <- c("A"= "dodgerblue3", "B"= "darkolivegreen4", "C"= "chocolate2", "D"= "brown3")

#Plotting all correlations in one graph, color coding by percentile
#For Figure S5
figureS5 <- ecmdups %>%
  ggplot(aes(x= ecm1_conc,
             y= ecm2_conc))+ #Select the x and y variables
  geom_point(aes(colour= Percentile), size= 2)+ #Color code the points by percentile
  scale_color_manual(values = cor_palette,
                     labels = c("A" = "< 25th", #Relabel the legend manually
                                "B" = "25th-50th", 
                                "C" = "50th-75th", 
                                "D" = "> 75th"))+
  geom_smooth(aes(colour= Percentile), method = lm, se= T)+ #Add a cor line and color code it
  facet_wrap(~Sample_type)+ #To differentiate Gravimetric and Nephelometric in the same graph
  scale_x_log10()+ #Log transform x axis
  scale_y_log10()+ #Log transform y axis
  xlab(expression(ECM1~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(ECM2~PM[2.5]~concentration~(ug/m^3)))+ #Label the y-axis
  theme_clean(base_size = 21)+ #Select theme of graph
  theme(aspect.ratio = 1,  #Aspect ratio of graph "square"
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16))+
  geom_abline(intercept = 0, slope = 1, linewidth= 0.5)+ #Insert a 45deg line
  stat_cor(aes(colour= Percentile), method ="spearman", #Color code the percentiles, and then proceed
           alternative = "two.sided",
           cor.coef.name = "rho",
           label.x.npc = "left",
           label.y.npc =  "top",
           p.accuracy = 0.001,
           size= 5.5)

#Save figure S5
ggsave(here("figures", "figureS5.png"))

#Save Figure S5 as .svg
ggsave("figures/figureS5.svg", figureS5, device = "svg")

####Bland Altman agreement plots####
#Create new column for concentration averages between both ECMs
ecmgravs_25$avg <- rowMeans(ecmgravs_25[,4:5])
ecmnephs_25$avg <- rowMeans(ecmnephs_25[,4:5])
ecmgravs_50$avg <- rowMeans(ecmgravs_50[,4:5])
ecmnephs_50$avg <- rowMeans(ecmnephs_50[,4:5])
ecmgravs_75$avg <- rowMeans(ecmgravs_75[,4:5])
ecmnephs_75$avg <- rowMeans(ecmnephs_75[,4:5])
ecmgravs_100$avg <- rowMeans(ecmgravs_100[,4:5])
ecmnephs_100$avg <- rowMeans(ecmnephs_100[,4:5])

#Create new column for difference in measurements
ecmgravs_25$diff <- ecmgravs_25$ecm1_conc - ecmgravs_25$ecm2_conc
ecmnephs_25$diff <- ecmnephs_25$ecm1_conc - ecmnephs_25$ecm2_conc
ecmgravs_50$diff <- ecmgravs_50$ecm1_conc - ecmgravs_50$ecm2_conc
ecmnephs_50$diff <- ecmnephs_50$ecm1_conc - ecmnephs_50$ecm2_conc
ecmgravs_75$diff <- ecmgravs_75$ecm1_conc - ecmgravs_75$ecm2_conc
ecmnephs_75$diff <- ecmnephs_75$ecm1_conc - ecmnephs_75$ecm2_conc
ecmgravs_100$diff <- ecmgravs_100$ecm1_conc - ecmgravs_100$ecm2_conc
ecmnephs_100$diff <- ecmnephs_100$ecm1_conc - ecmnephs_100$ecm2_conc

#find average difference
mean_diff_grav_25 <- mean(ecmgravs_25$diff)
mean_diff_neph_25 <- mean(ecmnephs_25$diff)
mean_diff_grav_50 <- mean(ecmgravs_50$diff)
mean_diff_neph_50 <- mean(ecmnephs_50$diff)
mean_diff_grav_75 <- mean(ecmgravs_75$diff)
mean_diff_neph_75 <- mean(ecmnephs_75$diff)
mean_diff_grav_100 <- mean(ecmgravs_100$diff)
mean_diff_neph_100 <- mean(ecmnephs_100$diff)

#find lower 95% confidence interval limits
lower_grav_25 <- mean_diff_grav_25 - 1.96 * sd(ecmgravs_25$diff)
lower_neph_25 <- mean_diff_neph_25 - 1.96 * sd(ecmnephs_25$diff)
lower_grav_50 <- mean_diff_grav_50 - 1.96 * sd(ecmgravs_50$diff)
lower_neph_50 <- mean_diff_neph_50 - 1.96 * sd(ecmnephs_50$diff)
lower_grav_75 <- mean_diff_grav_75 - 1.96 * sd(ecmgravs_75$diff)
lower_neph_75 <- mean_diff_neph_75 - 1.96 * sd(ecmnephs_75$diff)
lower_grav_100 <- mean_diff_grav_100 - 1.96 * sd(ecmgravs_100$diff)
lower_neph_100 <- mean_diff_neph_100 - 1.96 * sd(ecmnephs_100$diff)

#find upper 95% confidence interval limits
upper_grav_25 <- mean_diff_grav_25 + 1.96 * sd(ecmgravs_25$diff)
upper_neph_25 <- mean_diff_neph_25 + 1.96 * sd(ecmnephs_25$diff)
upper_grav_50 <- mean_diff_grav_50 + 1.96 * sd(ecmgravs_50$diff)
upper_neph_50 <- mean_diff_neph_50 + 1.96 * sd(ecmnephs_50$diff)
upper_grav_75 <- mean_diff_grav_75 + 1.96 * sd(ecmgravs_75$diff)
upper_neph_75 <- mean_diff_neph_75 + 1.96 * sd(ecmnephs_75$diff)
upper_grav_100 <- mean_diff_grav_100 + 1.96 * sd(ecmgravs_100$diff)
upper_neph_100 <- mean_diff_neph_100 + 1.96 * sd(ecmnephs_100$diff)

#Create the plots
#25th percentile gravimetric
BA_25grav <- ggplot(ecmgravs_25, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_25) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_25, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_25, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 30, y= 12, size= 5, label= "Mean difference= -1.95 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(5, 40)+ #Set the x-axis limits
  ylim(-40, 40) #Set the y-axis limits

#25th percentile nephelometric
BA_25neph <- ggplot(ecmnephs_25, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_neph_25) + #Adding the mean difference line
  geom_hline(yintercept = lower_neph_25, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_neph_25, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 31, y= 12, size= 5, label= "Mean difference= -0.75 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(5, 40)+ #Set the x-axis limits
  ylim(-40, 40) #Set the y-axis limits

#Combine plots in one Graph (Figure S1)
baplot_25 <- ggarrange(BA_25grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                       BA_25neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                       nrow = 1, ncol = 2, #one row and two columns
                       labels = "AUTO", #automatically label plots as "A" and "B"
                       hjust = 0.1, #adjust horizontal position of labels
                       vjust = 1.1) #adjust vertical position of labels

#Add shared x and y-axis
annotate_figure(baplot_25, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1, size = 15), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)", size = 15)) #X-axis

#Save figure S1
ggsave(here("figures", "figureS1.png"))

#Save Figure S1 as .svg
ggsave("figures/figureS1.svg", baplot_25, device = "svg")

#50th percentile gravimetric
BA_50grav <- ggplot(ecmgravs_50, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_50) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_50, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_50, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 60, y= 30, size= 5, label= "Mean difference= 1.80 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(10, 80)+ #Set the x-axis limits
  ylim(-60, 60) #Set the y-axis limits

#50th percentile nephelometric
BA_50neph <- ggplot(ecmnephs_50, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_neph_50) + #Adding the mean difference line
  geom_hline(yintercept = lower_neph_50, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_neph_50, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 60, y= 30, size= 5, label= "Mean difference= -0.25 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(10, 80)+ #Set the x-axis limits
  ylim(-60, 60) #Set the y-axis limits

#Combine plots in one Graph (Figure S2)
baplot_50 <- ggarrange(BA_50grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                       BA_50neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                       nrow = 1, ncol = 2, #one row and two columns
                       labels = "AUTO", #automatically label plots as "A" and "B"
                       hjust = 0.1, #adjust horizontal position of labels
                       vjust = 1.1) #adjust vertical position of labels

#Add shared x and y-axis
annotate_figure(baplot_50, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1, size = 15), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)", size = 15)) #X-axis

#Save figure S2
ggsave(here("figures", "figureS2.png"))

#Save Figure S2 as .svg
ggsave("figures/figureS2.svg", baplot_50, device = "svg")

#75th percentile gravimetric
BA_75grav <- ggplot(ecmgravs_75, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=5) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_75) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_75, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_75, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 120, y= 32, size= 5, label= "Mean difference= 3.84 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(25, 160)+ #Set the x-axis limits
  ylim(-100, 100) #Set the y-axis limits

#75th percentile nephelometric
BA_75neph <- ggplot(ecmnephs_75, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=5) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_neph_75) + #Adding the mean difference line
  geom_hline(yintercept = lower_neph_75, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_neph_75, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  geom_text(x= 120, y= 52, size= 5, label= "Mean difference= 6.86 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(25, 160)+ #Set the x-axis limits
  ylim(-100, 100) #Set the y-axis limits

#Combine plots in one Graph (Figure S2)
baplot_75 <- ggarrange(BA_75grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                       BA_75neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                       nrow = 1, ncol = 2, #one row and two columns
                       labels = "AUTO", #automatically label plots as "A" and "B"
                       hjust = 0.1, #adjust horizontal position of labels
                       vjust = 1.1) #adjust vertical position of labels

#Add shared x and y-axis
annotate_figure(baplot_75, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1, size = 15), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)", size = 15)) #X-axis

#Save figure S3
ggsave(here("figures", "figureS3.png"))

#Save Figure S3 as .svg
ggsave("figures/figureS3.svg", baplot_75, device = "svg")

#100th percentile gravimetric
BA_100grav <- ggplot(ecmgravs_100, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_grav_100) + #Adding the mean difference line
  geom_hline(yintercept = lower_grav_100, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_grav_100, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(size = 22, hjust = 0.5))+
  geom_text(x= 540, y= -90, size= 5, label= "Mean difference= 17.48 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(90, 700)+ #Set the x-axis limits
  ylim(-280, 280) #Set the y-axis limits

#100th percentile nephelometric
BA_100neph <- ggplot(ecmnephs_100, aes(x = avg, y = diff)) + #Select the mean as the x-axis and the difference as the y-axis
  geom_point(shape=23, fill= "steelblue", color="black", size=4) + #Adding points selecting shape, color and size
  geom_hline(yintercept = mean_diff_neph_100) + #Adding the mean difference line
  geom_hline(yintercept = lower_neph_100, color = "navyblue", linetype="twodash") + #Adding the lower limit line
  geom_hline(yintercept = upper_neph_100, color = "navyblue", linetype="twodash") + #Adding the upper limit line
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(size = 22, hjust = 0.5))+
  geom_text(x= 540, y= -90, size= 5, label= "Mean difference= 12.57 µg/m3")+ #add Mean difference text manually
  xlab(expression(Average~PM[2.5]~concentration~(ug/m^3)))+ #Label the x-axis
  ylab(expression(PM[2.5]~concentration~difference~(ug/m^3)))+ #Label the y-axis
  xlim(90, 700)+ #Set the x-axis limits
  ylim(-280, 280) #Set the y-axis limits

#Combine plots in one Graph (This is for Figure 7)
baplot_100 <- ggarrange(BA_100grav + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Grav plot
                        BA_100neph + rremove("ylab") + rremove("xlab"), #remove x and y-axis label for Neph plot
                        nrow = 1, ncol = 2, #one row and two columns
                        labels = "AUTO", #automatically label plots as "A" and "B"
                        hjust = 0.1, #adjust horizontal position of labels
                        vjust = 1.1) #adjust vertical position of labels

#Add shared x and y-axis
annotate_figure(baplot_100, left = text_grob("PM2.5 Concentration Difference (µg/m3)", rot = 90, vjust = 1, size = 15), #Y-axis
                bottom = text_grob("Average PM2.5 Concentration (µg/m3)", size = 15)) #X-axis

#Save figure S4
ggsave(here("figures", "figureS4.png"))

#Save Figure S4 as .svg
ggsave("figures/figureS4.svg", baplot_100, device = "svg")
