
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
#-Import aves traits and threats data                                             
#------------------------------#

setwd("C:/Threats_project/February_2026/Data_imputation/birds")
aves_traits <- read.csv("Aves_threats_traits_data.csv")
head(aves_traits)

###------------------------ 
### Remove DD species (i.e., Data Deficient species)
###------------------------ 

aves_traits <- aves_traits %>% 
  filter(category != "DD")

#------------------------------#                                            
#-Select - species and trait data of interest                                           
#------------------------------# 

aves_traits2 <- aves_traits  %>%
  select(IUCN_Species, Genus, category, Mass,Body_Mass,Trophic_Niche, Functional_Group)                                            


length(aves_traits2$IUCN_Species)
head(aves_traits2)
dim(aves_traits2)

#------------------------------#                                            
#-Count how many species lack trait data                                      
#------------------------------# 
sum(is.na(aves_traits2$Trophic_Niche))
sum(is.na(aves_traits2$Mass))



#----------------------------------------------------------------------------------#
### Select and summarize the genera of birds that lack data on functional groups
#----------------------------------------------------------------------------------#

aves_genera_without_diet_data <-  aves_traits2[is.na(aves_traits2$Trophic_Niche),]%>%  
  select(Genus, category,Mass,Functional_Group) %>%                            
  group_by(Genus) %>% summarize(count = n())

head(aves_genera_without_diet_data)


#-----------------------------------------------------------------#
### Estimate the number of functional groups within each Genus
#-----------------------------------------------------------------#
genus_diet <- left_join(aves_genera_without_diet_data, aves_traits2,
                        by = "Genus")%>% 
  select(IUCN_Species, Genus, category, Mass,Body_Mass,Trophic_Niche, Functional_Group)  %>%                            
  group_by(Genus, Trophic_Niche) %>% 
  summarize(n()) 

#-----------------------------------------------------------------#
### Estimate the most common functional group per genus
#-----------------------------------------------------------------#

genus_diet2 <- as.data.table(genus_diet)
genus_diet3  <- genus_diet2[,.SD[which.max(`n()`)],by=Genus]
genus_diet4 <- genus_diet3 %>%
  rename(Majority_fn_group =Trophic_Niche)
genus_diet4

#----------------------------------------------------------------------------------------------------#
### Assign the most common functional group per genus to the observed functional group of a species
#----------------------------------------------------------------------------------------------------#

genus_diet_major <- left_join(genus_diet4, aves_traits2, by = "Genus")%>% 
  select(IUCN_Species, Genus, category, Mass,Body_Mass,Trophic_Niche, Functional_Group,
         Functional_Group,Majority_fn_group,`n()`)%>% 
        filter(!is.na(Trophic_Niche))


#-----------------------------------------------------------------------------#
### Assign a value of "ONE" if the most common functional group of the genus is similar
#### to the observed functional group of a species, and "zero" otherwise
#-----------------------------------------------------------------------------#
Prop_diet <- genus_diet_major %>% 
  mutate(diet_incid = case_when ((Trophic_Niche == Majority_fn_group) ~ 1,TRUE ~ 0))

Prop_diet 

#----------------------------------------------------------------------------------------------------#
### Estimate the proportion of species - whose functional group was accurately predicted
#----------------------------------------------------------------------------------------------------#

prop_ftn_group_aves <- sum(Prop_diet$diet_incid)/length(Prop_diet$diet_incid)
prop_ftn_group_aves



write.csv(prop_ftn_group_aves ,"prop_ftn_group_aves.csv")  



