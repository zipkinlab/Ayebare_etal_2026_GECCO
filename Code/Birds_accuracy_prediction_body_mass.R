
#-------------------------------------------------------------------------#
## Code to test the accuracy of assigning genus-level data for 
### a subset of bird species with known trait data  - body mass
#-------------------------------------------------------------------------#

#-----------------------# 
### Load libraries
#-----------------------# 
library(tidyverse)
library(data.table)

### Clear working environment
rm(list=ls())

#-----------------------#                                                    
#-Set Working Directory-#                                                         
#-----------------------#
setwd(" ")

#-------------------------------------------------#
### Import bird species trait and threat data
#-------------------------------------------------#
aves_traits <- read.csv("Aves_threats_traits_data.csv")
head(aves_traits)

#-------------------------------#
### Select a subset of columns
#-------------------------------#
aves_traits2 <- aves_traits  %>%
  select(IUCN_Species, Genus, category, Mass,Body_Mass,Trophic_Niche, Functional_Group)                                            

head(aves_traits2)
dim(aves_traits2)

#----------------------------------------------------------#
#### Estimate total number of species that lack trait data
#### - Body mass
#----------------------------------------------------------#
sum(is.na(aves_traits2$Mass))
### Functional groups
sum(is.na(aves_traits2$Trophic_Niche))

#-------------------------------------------------------
### Select bird species that have body mass data ###
#-------------------------------------------------------

aves_genera_with_mass_data <-  aves_traits2[!is.na(aves_traits2$Mass),]%>%  
  select(IUCN_Species, Genus, category, Mass,Body_Mass,Trophic_Niche, Functional_Group) 

aves_genera_with_mass_data
dim(aves_genera_with_mass_data)
#----------------------------------------------------------------------------------------# 
###### Estimate the average bodymass of species with trait data - at genus level###
###### left join genus average ###
#----------------------------------------------------------------------------------------# 
genus_mass_average <- aves_genera_with_mass_data %>% 
                       group_by(Genus) %>%    
                       summarize(count = n(),mean_genus = mean(Mass),sd_genus = sd(Mass)) %>%
                       filter(!is.na(sd_genus))
genus_mass_average


#----------------------------------------------------------------------------------------# 
#### select a subset genera with trait data - 2000
#----------------------------------------------------------------------------------------# 
set.seed(136)


#-------------------------------------------------------
###### Estimate the absolute difference - mass ###
#-------------------------------------------------------
Abs_mass_diff <- left_join(genus_mass_average, aves_genera_with_mass_data,
                              by = "Genus")%>% 
  select(IUCN_Species, Genus, Mass,Trophic_Niche, Functional_Group,
         count, mean_genus, sd_genus)%>% 
  slice_sample(n=2000) %>%
  mutate(abs_diff = abs (Mass - mean_genus)) %>%
  ### Estimation error in percentage - relation to true value
  mutate (Prop_pct = (abs_diff/Mass)*100)

head(Abs_mass_diff)



#---------------------------------------------------------
### Mean - absolute difference
#--------------------------------------------------------
Mean_Absolute_Difference  <- mean(Abs_mass_diff$Prop_pct)  
Mean_Absolute_Difference 

#---------------------------------------------------------
### Standard deviation - absolute difference
#-------------------------------------------------------
SD_Absolute_Difference  <- sd(Abs_mass_diff$Prop_pct)  
SD_Absolute_Difference


write.csv(Abs_mass_diff,"Birds_absolute_mass_difference.csv")  














