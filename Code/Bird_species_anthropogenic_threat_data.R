
#-------------------------------------------------------------------------#
## Code to download anthropogenic threat data - birds  
#-------------------------------------------------------------------------#


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

#-------------------------------------------------------------------------#
### download aves from list of all species                                                                                      
### rl_sp --> class_name 'aves' --> list of all the aves 
#-------------------------------------------------------------------------#

out = rl_sp(page = 1, token, all = TRUE)                                                                               
length(out)                                                                                                                          
vapply(out, "[[", 1, "count")                                                                                                       
all_df = do.call(rbind, lapply(out, "[[", "result"))                                                                
aves_spp = all_df %>%                                                                                                     
filter(class_name == 'AVES', is.na(infra_rank))

#-------------------------------------------------------------------------#
### derive threats table from each species 
#-------------------------------------------------------------------------#                                                                 
aves_threats_table <- list()                                                                                                            
start <- Sys.time()                                                                                                                                
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",                                   
total = length(aves_spp$scientific_name), clear = FALSE, width = 60)        
for (i in aves_spp$scientific_name) {                                                                          
aves_threats_table[[i]] <- rl_threats(name = as.character(i), key = token)                              
pb$tick()
}

end <- Sys.time()                                                                                                           
total_runtime = end - start                                                                                                             
cat("Total run time:", total_runtime, "\n")

### save threat data                                                     
saveRDS (aves_threats_table, file = "aves_threats_table.rds")







