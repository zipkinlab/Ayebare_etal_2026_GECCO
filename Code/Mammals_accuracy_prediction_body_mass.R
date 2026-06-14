

#-------------------------------------------------------------------------#
## Code to test the accuracy of assigning genus-level data for 
### a subset of mammal species with known trait data  - body mass
#-------------------------------------------------------------------------#

#-----------------------# 
### Load libraries
#-----------------------# 
library(tidyverse)
library(data.table)

#---------------------------# 
### Clear working environment
#---------------------------# 
rm(list=ls())

#-----------------------#                                                    
#-Set Working Directory-#                                                         
#-----------------------#
##setwd(" ")

#-------------------------------------------------#
### Import mammal species trait and threat data
#-------------------------------------------------#
mammals_traits <- read.csv("mammals_traits_n_threats_data.csv")
head(mammals_traits)

#-------------------------------------------------#
### Select a subset of columns 
#-------------------------------------------------#

mammals_traits2 <- mammals_traits  %>%
                  select(scientific_name,accepted_spp_name,family_name, genus_name, category,
                  Mass.g,combined_body_mass,functional_group,phyl_eltonian_ftn_gp)                                            
head(mammals_traits2)
dim(mammals_traits2)

#----------------------------------------------------------#
#### Estimate total number of species that lack trait data
#### Body mass
#----------------------------------------------------------#
sum(is.na(mammals_traits2$Mass.g))
### Functional groups
sum(is.na(mammals_traits2$functional_group))

#----------------------------------------------------------# 
### Select mammal species that have body mass data ###
#----------------------------------------------------------# 

mammal_genera_with_mass_data <-  mammals_traits2[!is.na(mammals_traits2$Mass.g),]%>%  
                               select(accepted_spp_name,genus_name, category,Mass.g,functional_group) 

mammal_genera_with_mass_data

dim(mammal_genera_with_mass_data)


#----------------------------------------------------------------------------------------# 
###  Estimate the average bodymass of species with trait data - at genus level###
#----------------------------------------------------------------------------------------#
genus_mass_average <- mammal_genera_with_mass_data %>% 
                       #slice_sample(n=1500) %>%
                           group_by(genus_name) %>%    
                           summarize(count = n(),mean_genus = mean(Mass.g),sd_genus = sd(Mass.g))%>%
                          ### Select genera that have atleast 2 species
                           filter(!is.na(sd_genus))
genus_mass_average

#---------------------------------------------------------
#### select a subset genera with trait data - 1000
#---------------------------------------------------------
set.seed(134)

#--------------------------------------------------------- 
###### Estimate the absolute difference - mass ###
#---------------------------------------------------------
Abs_mass_diff  <- left_join(genus_mass_average, mammal_genera_with_mass_data,
                              by = "genus_name")%>% 
                              select(accepted_spp_name, genus_name, category,Mass.g,functional_group,
                              count, mean_genus, sd_genus)%>% 
                              slice_sample(n=1000) %>%
                              mutate(abs_diff = abs (Mass.g - mean_genus)) %>%
                              ### Estimation error in percentage - relation to true value
                              mutate (Prop_pct = (abs_diff/Mass.g)*100)

Abs_mass_diff

#---------------------------------------------------------
### Mean - absolute difference
#--------------------------------------------------------
Mean_Absolute_Difference  <- mean(Abs_mass_diff$Prop_pct)  
Mean_Absolute_Difference 

#---------------------------------------------------------
### Standard deviation - absolute difference
#--------------------------------------------------------
SD_Absolute_Difference  <- sd(Abs_mass_diff$Prop_pct)  
SD_Absolute_Difference


write.csv(Abs_mass_diff,"mammals_absolute_mass_difference.csv")














