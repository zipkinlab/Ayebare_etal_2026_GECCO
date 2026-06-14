
#---------------------------------------------------------------------#                                                         
## Code to download anthropogenic threat data - mammals                              
#---------------------------------------------------------------------#


### Load libraries                                                                                                        
library(tidyverse)                                                                                                            
library(dplyr)                                                                                                                      
library(rredlist)                                                                                                           
library(progress)

#-------------------------------------------------------------------------#
### Insert token (API key)                                                                                                             
### IUCN requires you to get your own API key, an alphanumeric string that 
### you need to send in for every request to access the database- IUCN Red List data
## rl_use_iucn() for help getting and storing 
#-------------------------------------------------------------------------#

token = ' '

#-------------------------------------------------------------------------#
### download mammals from list of all species                                                                              
# rl_sp --> class_name 'mammalia' --> list of all the mammals 
##-------------------------------------------------------------------------#
out = rl_sp(page = 1, token, all = TRUE)     
length(out)                                                                                                                                   
vapply(out, "[[", 1, "count")                                                                                                          
all_df = do.call(rbind, lapply(out, "[[", "result"))                                                                       
mammal_spp = all_df %>%                                                                                       
filter(class_name == 'MAMMALIA', is.na(infra_rank))


#-------------------------------------------------------------------------#
### derive threats table from each species
#-------------------------------------------------------------------------#
mammal_threats_table <- list()                                                                                                     
start <- Sys.time()
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                        total = length(mammal_spp$scientific_name), clear = FALSE, width = 60)
                        for (i in mammal_spp$scientific_name) 
                        {mammal_threats_table[[i]] <- rl_threats(name = as.character(i), key = token)  
                          pb$tick()
                        }


end <- Sys.time()                        				         
total_runtime = end - start                                          
cat("Total run time:", total_runtime, "\n")

### save threat data 
saveRDS(mammal_threats_table, file = "mammal_threats_table.rds")

                        

