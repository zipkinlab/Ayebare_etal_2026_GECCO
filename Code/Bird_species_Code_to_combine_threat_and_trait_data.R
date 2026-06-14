#--------------------------------------------------------------------#         
## Code to combine threat and trait data (diet & body mass) – birds                           
#--------------------------------------------------------------------#
### Load libraries                                                    
library(tidyverse)                                                        
library(dplyr)                                                      
library(rredlist)                                                       
library(progress)

#-------------------------------------------------------------------------#
### Insert token (API key)                                                                                                             
### IUCN requires you to get your own API key, an alphanumeric string that 
### you need to send in for every request to access the database - IUCN Red List data
## rl_use_iucn() for help getting and storing 
#-------------------------------------------------------------------------#

token = ' '

# set working directory
setwd(" ")

# 1. Load Data                                                               
# Get IUCN species list: load list of threats                              
aves_function <- read.csv('aves_threat_data.csv')

# Get AVONET data: load BirdLife csv                                          
# https://opentraits.org/datasets/avonet.html                                
avonet <- read.csv('AVONET_BirdLife.csv')

# species with status                                                      
status <- read.csv('Birds_extinct_risk.csv') %>%            
select(scientific_name, category) %>%                                   
mutate(Species = scientific_name) %>%                                 
select(-scientific_name)

# 2. Select species from each data frame                                
IUCN_spp <- aves_function %>%  mutate(Species = Species) %>%
# remove extinct and extinct in wild because they do not have trait data    
left_join(status, by = 'Species' ) %>%                              
filter(!category %in% c('EX', 'EW')) %>% select(Species, category)   
avonet_spp <- avonet %>% mutate(Species = Species1) %>% select(Species) 

# 3. left join to see if spp. Match                                          
# if the species in the IUCN data matches the avonet data, that means there  
# is - trait data available for use 
join_spp <- IUCN_spp %>% 
inner_join(avonet_spp)

# 4. remove matches                                                             
# if entry is in the joined species, remove it from the IUCN dataset 
IUCN_unknown <- IUCN_spp %>%                                          
filter(!(Species %in% join_spp$Species))

# 5. RL Synonyms for unmatched IUCN species                                  
# add a progress bar 
pb <- progress_bar$new( format = "  downloading [:bar] :percent eta: :eta",         
                        total = length(IUCN_unknown$Species), clear = FALSE, width = 60)
# initialize list to store output                         
IUCN_possible_match <- list()                                                
# loop through unknown avonet species, and try to find a synonym
for (i in 1:length(IUCN_unknown$Species)) {            
  IUCN_possible_match[[i]] <- rl_synonyms(name = as.character(IUCN_unknown$Species[i]), key = token)                        
  pb$tick()
}

saveRDS(IUCN_possible_match, file = "IUCN_possible_match.rds")
# get accepted name & synonym from the possible matches list
IUCN_possibilities_list <- readRDS('IUCN_possible_match.rds')

# 5. match synonyms to unknown names                                         
# create a place to put the findings from rl_synonyms                   
IUCN_filled <- IUCN_unknown                                                
IUCN_filled$Synonym <- rep(NA, length(IUCN_filled$Species))
        
# load the rl_synonymns output file                               
IUCN_possibilities_list <- readRDS("IUCN_possible_match.rds")
# loop through the unknown species and fill in synonym and accepted name if found
for(i in 1:nrow(IUCN_filled)) {                                              
# Check if synonym exists 
if(length(IUCN_possibilities_list[[i]]$result$synonym) > 0) 
         {         
syn_list <- IUCN_possibilities_list[[i]]$result$synonym                
new_name <- syn_list[which(syn_list %in% avonet_spp$Species)] 
IUCN_filled$Synonym[i] <- ifelse(length(new_name)>0, new_name, NA)           
          } 
        else {                                                   
          IUCN_filled$Synonym[i] <- NA
        }
        }

# 6. Join IUCN names to the threats table                                     
# left join IUCN_filled to threat data by synonym                              
# threat data, two columns: IUCN name and [AVONET] Synonym                   
# all of the species that matched should be fine. But also add the ones that 
# we just figured out. 
IUCN_joined <- IUCN_spp %>%                                     
left_join(IUCN_filled, by = c("Species", "category")) %>%
# for NA synonyms, take value from spp name                      
mutate(Synonym = if_else(is.na(Synonym), Species, Synonym))                      
  
# load threat data                                                
aves_threats <- aves_function

# join synonyms & IUCN_name to the threats table 
IUCN_threats <- aves_threats %>% 
right_join(IUCN_joined, by = 'Species') %>% 
  
# reorder columns                                               
select(Species, Synonym, everything()) %>%                                 
select(-X)
  
  
# 7. Join avonet BirdLife data to the IUCN_threats table                      
avonet <- avonet %>%                                                          
# mutate species column so it matches the threats Synonym column  
mutate(Synonym = Species1) %>%    
  
# select Species1, Mass, and Trophic.Niche from the dataset          
select(Synonym, Mass, Trophic.Niche) 

# join avonet to IUCN_threats                                      
aves_threats_traits <- IUCN_threats %>%                                 
left_join(avonet, by = 'Synonym') %>%                           
rename(IUCN_Species = Species, Avonet_Species = Synonym, Trophic_Niche = Trophic.Niche) %>%                                                        
  mutate(Genus = str_split_i(IUCN_Species, " ", i=1)) %>%          
  group_by(Genus) %>%                                         
  mutate(Genus_Avg_Mass = mean(na.omit((Mass))),                     
         Genus_Trophic_data = length(na.omit(Trophic_Niche))) %>% 
  filter(Genus_Trophic_data > 0) %>%  # n = 3                          
  mutate(Genus_Trophic = names(sort(table(na.omit(Trophic_Niche)), decreasing=TRUE))[1]) %>%                                                
  ungroup() %>%                                                         
  mutate(Body_Mass = if_else(is.na(Mass), Genus_Avg_Mass, Mass),     
         Functional_Group = if_else(is.na(Trophic_Niche), Genus_Trophic, Trophic_Niche)) %>%                                                    
  select(-c(Mass, Trophic_Niche, Genus, Genus_Avg_Mass, Genus_Trophic_data, Genus_Trophic))
  
# save as CSV 
write.csv(aves_threats_traits, 'aves_threats_traits_data.csv')





















