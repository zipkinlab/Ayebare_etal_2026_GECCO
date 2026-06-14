
#--------------------------------------------------------------------#         
## Code to assess the prediction accuracy of diet functional groups – mammals                          
#--------------------------------------------------------------------#

#-----------------------#                                                    
#-Set Working Directory-#                                                         
#-----------------------#
setwd(" ")

### Clear working environment
rm(list=ls())


#----------------#                                                           
#-Load libraries-#                                                                   
#----------------# 
library(tidyverse)
library(data.table)

#------------------------------#                                            
#-Import mammal traits and threats data                                             
#------------------------------#  
mammals_traits <- read.csv("mammals_traits_n_threats_data.csv")
head(mammals_traits)

###------------------------ 
### Remove DD species (i.e., Data Deficient species)
###------------------------ 

mammals_traits <- mammals_traits %>% 
  filter(category != "DD")

#------------------------------#                                            
#-Select - species and trait data of interest                                           
#------------------------------# 
mammals_traits2 <- mammals_traits  %>%
  select(scientific_name,accepted_spp_name,family_name, genus_name, category,
         Mass.g,combined_body_mass,functional_group,phyl_eltonian_ftn_gp)                                            

head(mammals_traits2)
dim(mammals_traits2)


#------------------------------#                                            
#-Count how many species lack diet data - functional groups                                          
#------------------------------# 

sum(is.na(mammals_traits2$functional_group))
## 313


#----------------------------------------------------------------------------------#
### Select and summarize the genera of mammals that lack data on functional groups
#----------------------------------------------------------------------------------#

mammal_genera_without_diet_data <-  mammals_traits2[is.na(mammals_traits2$functional_group),]%>%  
  select(genus_name, category,Mass.g,functional_group) %>%                            
  group_by(genus_name) %>% summarize(count = n())

head(mammal_genera_without_diet_data)



#-----------------------------------------------------------------#
### Estimate the number of functional groups within each Genus
#-----------------------------------------------------------------#
genus_diet <- left_join(mammal_genera_without_diet_data, mammals_traits2,
                                by = "genus_name")%>% 
  select(accepted_spp_name, genus_name, category,Mass.g,functional_group) %>%                            
  group_by(genus_name, functional_group) %>% 
  summarize(n()) %>%
  filter(!is.na(functional_group))

genus_diet

#-----------------------------------------------------------------#
### Estimate the most common functional group per genus
#-----------------------------------------------------------------#

genus_diet2 <- as.data.table(genus_diet)
genus_diet3  <- genus_diet2[,.SD[which.max(`n()`)],by=genus_name]
genus_diet4 <- genus_diet3 %>%
              rename(Majority_fn_group =functional_group )
genus_diet4


#----------------------------------------------------------------------------------------------------#
### Assign the most common functional group per genus to the observed functional group of a species
#----------------------------------------------------------------------------------------------------#

genus_diet_major <- left_join(genus_diet4, mammals_traits2, by = "genus_name")%>% 
  select(accepted_spp_name, genus_name, category,Mass.g,functional_group,Majority_fn_group,`n()`) %>%                            
  filter(!is.na(Mass.g))

genus_diet_major

#-----------------------------------------------------------------------------#
### Assign a value of "ONE" if the most common functional group of the genus is similar
#### to the observed functional group of a species, and "zero" otherwise
#-----------------------------------------------------------------------------#
Prop_diet <- genus_diet_major %>% 
  mutate(diet_incid = case_when ((functional_group  == Majority_fn_group) ~ 1,TRUE ~ 0))

Prop_diet 

#----------------------------------------------------------------------------------------#
### Estimate the proportion of species - whose functional group was accurately predicted
#----------------------------------------------------------------------------------------#

prop_ftn_group_mammals <- sum(Prop_diet$diet_incid)/length(Prop_diet$diet_incid)
prop_ftn_group_mammals



write.csv(prop_ftn_group_mammals ,"prop_ftn_group_mammals.csv")  



