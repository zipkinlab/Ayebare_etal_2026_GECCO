
#--------------------------------------------------------------------#         
## Code to combine threat and trait data (diet & body mass) – mammals                          
#--------------------------------------------------------------------#

#-----------------------#                                                    
#-Set Working Directory-#                                                         
#-----------------------#
setwd(" ")

#----------------#                                                           
#-Load libraries-#                                                                   
#----------------#                                                   
library(tidyverse)                                                       
library(data.table)

#------------------------------#                                            
#-Import Phylacine trait data -                                               
#------------------------------#                                                    
# https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.2443
### diet, body size                                                           
phyl_traits <- read_csv("Phylacine_trait_data.csv")

#--------------------------#                                                        
## Remove extinct species -                                                         
#--------------------------#                                                
phyl_traits <- phyl_traits %>%                               
  filter(IUCN.Status.1.2 != "EX") %>%                                 
  filter(IUCN.Status.1.2 != "EP") %>%                       
  filter(IUCN.Status.1.2 != "EW") 
###--------------------------------------###                                 
### Import Phylacine - species synonyms -#                                  
###---------------------------------------###
## To enable combining phylacine trait data to Eltonian trait data           
## import phylacine synonyms
syno_names <- read_csv("Phylacine_synonyms.csv")                     
head(syno_names)                                                         
length( syno_names$Binomial.1.2)

###-------------------------------###                                       
####-Import  Eltonian trait data -#   https://doi.org/10.1890/13-1917.1                                              
###--------------------------------###                                
Elton_traits <- read_csv("Eltonian_trait_data.csv")                    
head(Elton_traits)                                          
length(Elton_traits$Scientific)
##### Replace the "space" between genus and species names with a "_" in the       
# Eltonian species names                                                     
# to match that of the phylacine trait data#
Elton_traits$Scientific                                             
Elton_traits <- Elton_traits %>%                                              
  mutate (sp_name = gsub(" ", "_", Elton_traits$Scientific))
length(Elton_traits$sp_name)

###---------------------------###                                         
###Import mammal threat data -#                                               
###---------------------------###
mamm_threats <- read_csv("iucn_mammal_threats.csv")                     
mamm_threats <- mamm_threats %>%                               
  mutate(IUCN.2024 = Species)

###-----------------------------------###                              
###Import mammal IUCN extinction risk -#                                         
###-----------------------------------###                                 
mamm_ex_risk <- read_csv("mammal_extinct_risk.csv")                   
head(mamm_ex_risk)

###---------------------------------------------------------------###             
## Remove species sub populations from IUCN extinction risk data - ##                  
# ------------------------------------------------------------------ 
mamm_ex_risk2 <- mamm_ex_risk[is.na(mamm_ex_risk$population),] %>% 
  select(scientific_name,order_name,family_name,genus_name, category, population)%>%
  ##### Remove extinct species & order sirenia (Aquatic herbivores) 
  filter(category != "EX") %>%                                            
  filter(category != "EW")  %>%                                     
  filter(order_name != "SIRENIA")

mamm_ex_risk2
dim(mamm_ex_risk2)


###----------------------------------------------------------------####            
#### left Join - mammal threat data to IUCN to extinction risk data  -#             
###---------------------------------------------------------------####
mamm_threats_n_status <-  left_join(mamm_ex_risk2, mamm_threats, by= c( "scientific_name" = "Species"))                            
dim(mamm_threats_n_status)
### iucn status                                     
unique(mamm_threats_n_status$category)

### ---------------------------------------------------------------------###         
#### Classify Phylacine diet trait data into five diet functional groups       
#### -------------------------------------------------------------------###
mamm_phyl_diet <- phyl_traits %>%                                              
  ## Remove order Sirenia (Aquatic herbivores)                      
  filter(Order.1.2 != "Sirenia") %>%                                              
  ### Create identifier for aquatic mammals                          
  mutate(Marine_fresh = Marine +Freshwater) %>%                               
  ###  Create identifier for terrestrial and aerial mammal               
  mutate(Terr_aerial = Terrestrial + Aerial) %>%                              
  ###  Create functional groups                                     
  mutate(functional_group = case_when ((Marine_fresh >=1 & Diet.Plant < 50) ~ "Aquatic_predator",
                                       (Terr_aerial >=1 & Diet.Invertebrate > 50 ) ~ "Invertivore",
                                       (Terr_aerial >= 1 & Diet.Vertebrate > 50 ) ~  "Vertivore",
                                       (Terr_aerial >= 1 & Diet.Plant > 50 ) ~ "Herbivore_terrestrial", TRUE ~ "Omnivore"))
### Summarize functional groups and orders table(mamm_phyl_diet$functional_group)                  
table(mamm_phyl_diet$Order.1.2)

### --------------------------------------------------------------------        
#### Classify Eltonian diet trait data into five diet functional groups 
# #### -------------------------------------------------------------------### 
mamm_elto <- Elton_traits %>%                                          
  ## Remove order serenia (Aquatic herbivores)                   
  filter(MSWFamilyLatin != "Dugongidae") %>%                   
  filter(MSWFamilyLatin != "Trichechidae") %>%                                    
  ### Create identifier for FruitNectSeed and herbivory (other plant parts)       
  mutate (FruitNectSeed = `Diet-Fruit` + `Diet-Nect` + `Diet-Seed`, Herb_any = FruitNectSeed + `Diet-PlantO`) %>%                                    
  filter(Herb_any > 50) %>%                                                   
  ###  Create two functional groups - based on plants     
  mutate(Eltonian_herbivory = case_when ((FruitNectSeed > `Diet-PlantO`) ~ "FruitNectSeed",TRUE ~ "Herbivore")) %>%
  ### Select variables of interest                        
  select(Scientific,MSWFamilyLatin,`BodyMass-Value`,sp_name,Eltonian_herbivory)  
## Summary herbivory functional groups               
table(mamm_elto$Eltonian_herbivory)



#### -----------------------------------###                                  
#### Synonyms based on Phylacine database-#                                         
#### -----------------------------------###
syno <- syno_names %>% select(Binomial.1.2,Family.1.2,EltonTraits.1.0.Genus,EltonTraits.1.0.Species,IUCN.2016.3.Genus,IUCN.2016.3.Species) %>%                                   
  ## Create scientific name based on Eltonian database            
  mutate(Eltonian_sp_name = paste(EltonTraits.1.0.Genus,EltonTraits.1.0.Species, sep = ' ')) %>%          
  ## Create scientific name based on IUCN database for 2016            
  mutate(IUCN_sp_name = paste(IUCN.2016.3.Genus,IUCN.2016.3.Species, sep = ' ')) %>%                                                                          
  ###Create scientific name with a " "                                           
  mutate (Binomial = gsub("_", " ", Binomial.1.2)) %>% select(Binomial.1.2,Binomial,Eltonian_sp_name,IUCN_sp_name)           
dim(syno)


###--------------------------------------------------------###              
##### left join - synonyms dataframe to Phylacine trait data -                         
###--------------------------------------------------------###
mamm_phyla_syno <- mamm_phyl_diet  %>% left_join(syno, by= "Binomial.1.2")  
length (mamm_phyla_syno$Binomial.1.2)

### Summary of functional groups                   
table(mamm_phyla_syno$functional_group)
write.csv(mamm_phyla_syno,file = "mammal_phylacine_synomnys.csv")

###----------------------------------------------------------###          
#### Left join phylacine functional group data to threat data -                        
###---------------------------------------------------------###
phyla_traits_threats <-  left_join(mamm_threats_n_status, mamm_phyla_syno,
                                   by= c( "scientific_name" =  "Binomial"))


### To check how many species with threats lack functional group data     
sum (is.na(phyla_traits_threats$functional_group))
## 638 species

###------------------------------------------------------------------------
### To check how many species have functional group data after the left join 
###--------------------------------------------------------------------------
sum (!is.na(phyla_traits_threats$functional_group))
## 5252 species

### Threats data has #5,890 species while the Phylacine trait data data has 5,474                                                                        

### 222 species in the phylacine database lack threats data
###--------------------------------------------------------------------------
#### Import synonyms obtained from the IUCN database using 
# 'rl_synonyms'   ## function                                                                         
###---------------------------------------------------------------------###
synonyms_iucn <- read.csv("synonyms_mammals_iucn.csv", h = T)
dim(synonyms_iucn)

###-----------------------------------------------###                            
## Left_join phylacine trait data to synonyms iucn                                     
###-----------------------------------------------###                 
mamm_phlya_syno_iucn <-  left_join(mamm_phyla_syno,synonyms_iucn,
                                   by= c("Binomial"  = "other_name")) %>% 
  mutate (accepted_spp_name = accpt_name)
mamm_phlya_syno_iucn
write.csv(mamm_phlya_syno_iucn, "mamm_phlya_syno_iucn.csv")

###--------------------------------------------------###                      
## Species in the phylacine database that have synonyms                              
###---------------------------------------------------### 
sum(!is.na(mamm_phlya_syno_iucn$accepted_spp_name))

###----------------------------------------------------------------###           
### Create a column that combines all names (considering synonyms)                 
###----------------------------------------------------------------###  
mamm_phyla_syno2 <-  within(mamm_phlya_syno_iucn, accepted_spp_name[is.na( accepted_spp_name)] <- Binomial[is.na( accepted_spp_name)]) %>%           
  mutate (spp_names_combined = accepted_spp_name) %>%                     
  ###Remove duplicate species names (i.e., with more than one synonym)   
  distinct(Binomial, .keep_all = TRUE)                            
head(mamm_phyla_syno2)

###----------------------------------------------------------------###       
## check whether there are NAs in the spp_names_combined                          
###----------------------------------------------------------------###   
sum(is.na(mamm_phyla_syno2$spp_names_combined))     
sum(!is.na(mamm_phyla_syno2$spp_names_combined))

### left_join
phyla_traits_threats2 <-  left_join(mamm_threats_n_status, mamm_phyla_syno2,
                                    by= c( "scientific_name" =  "spp_names_combined")) %>% 
  distinct(scientific_name, .keep_all = TRUE)    
dim(phyla_traits_threats2)                               
colnames(phyla_traits_threats2)

sum (is.na(phyla_traits_threats2$accpt_name))                                       
sum (is.na(phyla_traits_threats2$accepted_spp_name))
write.csv(phyla_traits_threats2 ,file = "mammamal_phyl_trait_n_threats_combined.csv")

### To check how many species with threats lack functional group data         
sum (is.na(phyla_traits_threats2$functional_group))                             

## Total species without trait data = 471
###--------------------------------------------------------###                 
### Check the genus of the mammal species that lack trait dat                       
###--------------------------------------------------------### 
genus_no_trait_data <-  phyla_traits_threats2[is.na(phyla_traits_threats2$functional_group),]%>% 
  group_by(genus_name) %>%                                             
  summarise(count= n())       
write.csv(genus_no_trait_data,"genus_no_trait_data.csv")

###--------------------------------------------------------###                      
### number of unique genera - that lack trait data                                   
###--------------------------------------------------------### 
length(genus_no_trait_data$genus_name)                                           
## 163
#### #### Estimate mean body mass                                                 
## using genera that have trait data                                    
genera_body_mass <-phyla_traits_threats2[!is.na(phyla_traits_threats2$functional_group),]%>% 
  left_join(genus_no_trait_data, by = "genus_name") %>%               
  filter(!is.na(count)) %>%                                     
  group_by(genus_name)  %>%                                           
  summarise(average_body_mass = mean(Mass.g))                        
genera_body_mass
length(unique(genera_body_mass$genus_name)) 
## Genera with body_mass ##
##152

#### #### Estimate functional group                                                 
## using genera that have trait data
genera_functional_group <- phyla_traits_threats2[!is.na(phyla_traits_threats2$functional_group),]%>% 
  left_join(genus_no_trait_data, by = "genus_name") %>%            
  filter(!is.na(count)) %>%                                            
  group_by(genus_name, functional_group)  %>%                       
  summarize(n())
genera_functional_group

###--------------------------------------------------------###                 
### Select rows that have the highest number of species per functional group 
###--------------------------------------------------------### 
genera_functional_group2 <- as.data.table(genera_functional_group) 
genera_functional_group3  <-genera_functional_group2[,.SD[which.max(`n()`)],by=genus_name]  
genera_functional_group2                                          
genera_functional_group3

###--------------------------------------------------------###                
#### Join average body mass and functional group data                               
###--------------------------------------------------------###
body_mass_n_functional_group <- genera_functional_group3 %>%  
  left_join(genera_body_mass, by = "genus_name") %>%                     
  mutate(fn_group = functional_group) %>%                     
  select(genus_name,fn_group, average_body_mass)
body_mass_n_functional_group

###--------------------------------------------------------###                 
#### Select species that lack trait data (body mass and diet categories)                
###--------------------------------------------------------###
species_with_no_trait_data <- phyla_traits_threats2[is.na(phyla_traits_threats2$functional_group),]%>%  
  left_join(genus_no_trait_data, by = "genus_name")
length(species_with_no_trait_data$Mass.g)

###-----------------------------------------------------------------------### 
### Join the estimated average body mass and diet categories to "species_with_no_trait_data"                                                   
###-------------------------------------------------------------------###   
estimated_bodymass_diet <- species_with_no_trait_data %>% left_join(body_mass_n_functional_group , by = "genus_name")  %>%  
  select(scientific_name, fn_group,average_body_mass)
### Check how many species lack body mass data at genus level    
sum(is.na(estimated_bodymass_diet$average_body_mass))

###------------------------------------------------------------------###          
#### Join the estimated bodymass and diet categories to                              
## "phyla_traits_threats2" that has all the species (i.e.,5890)                         
###------------------------------------------------------------###
phyla_traits_threats3 <- phyla_traits_threats2 %>% 
  left_join(estimated_bodymass_diet, by = "scientific_name") %>%            
  mutate (combined_fn_group  = fn_group) %>%                                  
  mutate (combined_body_mass  = average_body_mass)                 
phyla_traits_threats3

## Combine body mass                                                   
phyla_traits_threats3 <- within(phyla_traits_threats3, combined_fn_group[is.na(combined_fn_group)] <- functional_group[is.na( combined_fn_group)]) 
## Combine functional groups                                   
phyla_traits_threats3 <- within(phyla_traits_threats3, combined_body_mass[is.na(combined_body_mass)] <- Mass.g[is.na( combined_body_mass)]) 

### Check how many species lack trait data at genus level  sum(is.na(phyla_traits_threats3$combined_body_mass))
write.csv(phyla_traits_threats3, " phyla_traits_threats3.csv")

## Remove species that lack data on genus level body size & functional groups             
## Most are newly described species and data deficient
phyla_traits_threats4 <- phyla_traits_threats3 %>%  
  filter(!is.na(combined_body_mass))

### Check how many species lack trait data at genus level 
sum(is.na(phyla_traits_threats4$combined_body_mass))

###--------------------------------------------------------###              
### Join Eltonian trait data filtered for herbivores to phylacine and threat # data                                                                          
###--------------------------------------------------------### 
threats_phylacine_eltonian <-  left_join(phyla_traits_threats4, mamm_elto, by= c( "scientific_name" = "Scientific" )) 
threats_phylacine_eltonian
write.csv(threats_phylacine_eltonian,file = "threats_phylacine_eltonian.csv")
### Split  Terresterial herbivore functional into eltonian categories of # Fruit_nect_seed or herbivore
traits_n_threats_final <- threats_phylacine_eltonian %>%  
  mutate(phyl_eltonian_ftn_gp = case_when ((combined_fn_group == "Herbivore_terrestrial" & Eltonian_herbivory == "Herbivore") ~ "Herbivore",
                                           (combined_fn_group == "Herbivore_terrestrial" & Eltonian_herbivory == "FruitNectSeed") ~ "FruitNectSeed",
                                           (combined_fn_group == "Vertivore") ~ "Vertivore",
                                           (combined_fn_group == "Invertivore") ~ "Invertivore",
                                           (combined_fn_group == "Aquatic_predator") ~ "Aquatic_predator",TRUE ~ "Omnivore"))%>% 
select(-c(population, ...1,IUCN.2024,Binomial.1.2,Order.1.2,Family.1.2,Genus.1.2,Species.1.2,Terrestrial,Marine,Freshwater,Aerial,Life.Habit.Method, Life.Habit.Source,Mass.Method,Mass.Source,
          Mass.Comparison, Mass.Comparison.Source,Island.Endemicity,IUCN.Status.1.2,Added.IUCN.Status.1.2,Diet.Plant,Diet.Vertebrate,Diet.Invertebrate,Diet.Method,Diet.Source, Marine_fresh, Terr_aerial,
          Binomial, Eltonian_sp_name, IUCN_sp_name, X, accpt_name,fn_group,average_body_mass,`BodyMass-Value`,sp_name,Eltonian_herbivory,MSWFamilyLatin))

write.csv(traits_n_threats_final ,file = "mammals_traits_n_threats_data.csv")








