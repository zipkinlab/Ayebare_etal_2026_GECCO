## Set working directory                                                 
setwd(" ")

###------------------------                                                 
## Load libraries                                                           
###------------------------                                     
library(jagsUI)                                                    
library(tidyverse)

###------------------------                                                 
## Import data                                                                 
###------------------------                                                  
data <- read.csv("aves_threats_traits_data.csv")                                
data

###------------------------ 
### Remove DD species (i.e., Data Deficient species)
###------------------------                                                      
data <- data %>% 
        filter(category != "DD")

#------------------------#                                                    
### Threat – Hunting                                                        
#------------------------#
data_hunting <- data %>% select(Hunting,Functional_Group,Body_Mass) %>% 
  mutate(f_group = as.numeric(recode(Functional_Group, 'Aquatic predator' = '1','Frugivore' = '2', 
                                     'Granivore' = '3', 'Herbivore aquatic' = '4', 'Herbivore terrestrial' = '4',
                                     'Invertivore' = '5','Nectarivore' = '6','Omnivore' = '7','Vertivore' = '8', 
                                     'Scavenger' = '8')))%>% 
  mutate(hunting_f = as.numeric(recode(Hunting, '0' = '0', '1' = '0', '2' = '1', '3' = '1' )))%>% 
  mutate(bodymass.log10 = log10(Body_Mass)) %>% 
  mutate(bodymass_std = as.numeric(scale(bodymass.log10)))

###------------------------
### Functional group specific - body mass  
###------------------------                         
data_fn_body_mass <- data_hunting %>%                            
  mutate(f_group2 = recode(f_group, '1' ='Aquatic predator',  '2' = 'Frugivore', '3' = 'Granivore', 
                           '4' = 'Herbivore','5' = 'Invertivore', '6' = 'Nectarivore', '7' = 'Omnivore',
                           '8' = 'Vertivore' ))%>%
arrange(desc(f_group2))%>%                                    
  select(f_group2,bodymass_std) %>%                            
  group_by(f_group2) %>%                                            
  summarize(count = n(),                                                    
            
  mean = mean(bodymass_std),                                                   
  sd = sd(bodymass_std))
###------------------------------------
### mean body mass per functional group 
###------------------------------------

###------------------------                                   
## Aquatic predator
###------------------------                                                          
aq_mn <- data_fn_body_mass$mean[1] 

###------------------------                     
## Frugivore 
###------------------------                                                          
frug_mn <- data_fn_body_mass$mean[2]

###------------------------
## Granivore
###------------------------                                                                 
gran_mn <- data_fn_body_mass$mean[3]

###------------------------
## Herbivore
###------------------------                                                                 
herb_mn <- data_fn_body_mass$mean[4]

###------------------------
## Invertivore 
###------------------------                                                             
invert_mn <- data_fn_body_mass$mean[5]

###------------------------
## Nectarivore
###------------------------                                                              
nect_mn <- data_fn_body_mass$mean[6]

###------------------------
## Omnivore
###------------------------                                                            
omni_mn <- data_fn_body_mass$mean[7]

###------------------------
## Vertivore
###------------------------                                                                
vert_mn <- data_fn_body_mass$mean[8]

#-----------------------------------#                                      
#### Response categorical variable                                          
#-----------------------------------#                                   
hunting_f <- data_hunting$hunting_f

#-----------------------------------#                                     
#### Number of levels- response variable                                          
#-----------------------------------#                                     
threat_level <- length(unique(data_hunting$hunting_f))                        
threat_level 

#--------------------#                                                       
# Total of species                                                          
#--------------------#                                                       
N <- nrow(data_hunting)


#-----------------------------------#                                           
### Number of functional groups                                                  
#-----------------------------------#
fn <- length(unique(data_hunting$f_group ))


#---------------------#                                                       
### Functional groups                                                       
#---------------------#                                                      
fn_gp <- data_hunting$f_group

#-----------------------------------#                                      
### standardized log body mass # log10                                            
#-----------------------------------#                                          
bodymass <- data_hunting$bodymass_std

#-------------------------------#                                               
# Bundle and summarize data set                                                   
#-------------------------------#
data_threat <- list(hunting_f = hunting_f, N = N, threat_level = threat_level , fn_gp = fn_gp, 
                    fn = fn, bodymass=bodymass,aq_mn =aq_mn, frug_mn =frug_mn, gran_mn= gran_mn, 
                    nect_mn = nect_mn, herb_mn= herb_mn,invert_mn =invert_mn, vert_mn = vert_mn, omni_mn = omni_mn) 


#-----------------------------------#
###  Logistic regression model - Jags code
#-----------------------------------#
sink("birds_hunting_fn_bdy.txt")
cat("	
model{
	for(i in 1:N){
# non - threatened = 0; threatened = 1                                    
hunting_f[i] ~ dbern(prob[i])
# Linear predictor                                                     
logit(prob[i]) <- beta[fn_gp[i]]  + gamma*bodymass[i]
	}
# Priors - functional groups                                           
beta[1] ~ dnorm(0, 0.01)                                                     
beta[2] ~ dnorm(0, 0.01)                                                     
beta[3] ~ dnorm(0, 0.01)                                                
beta[4] ~ dnorm(0, 0.01)                                                 
beta[5] ~ dnorm(0, 0.01)                                                
beta[6] ~ dnorm(0, 0.01)                                                  
beta[7] ~ dnorm(0, 0.01)                                                    
beta[8] ~ dnorm(0, 0.01)

     
## Derived parmaters                                                        
q <- exp(beta[1])/(1 + exp(beta[1]))                                     
frug <-  exp(beta[2])/(1 + exp(beta[2]))                                     
gran <-  exp(beta[3])/(1 + exp(beta[3]))                                           
herb <-  exp(beta[4])/(1 + exp(beta[4]))                                    
invert <-  exp(beta[5])/(1 + exp(beta[5]))                                     
nect <-  exp(beta[6])/(1 + exp(beta[6]))                                   
omni <-  exp(beta[7])/(1 + exp(beta[7]))                                         
vert <-  exp(beta[8])/(1 + exp(beta[8]))

aq_mng <- exp(beta[1] + gamma * aq_mn )/(1 + exp(beta[1] * gamma *aq_mn)) 
frug_mng <-  exp(beta[2] + gamma * frug_mn)/(1 + exp(beta[2] + gamma * frug_mn))                                                              
gran_mng <-  exp(beta[3] + gamma * gran_mn)/(1 + exp(beta[3] + gamma * gran_mn))                                                                 
herb_mng <-  exp(beta[4] + gamma * herb_mn)/(1 + exp(beta[4] + gamma * herb_mn))                                                                 
invert_mng <-  exp(beta[5] + gamma * invert_mn)/(1 + exp(beta[5] + gamma * invert_mn))                                                            
nect_mng <-  exp(beta[6] + gamma * nect_mn)/(1 + exp(beta[6] + gamma * nect_mn))                                                              
omni_mng <-  exp(beta[7] + gamma * omni_mn )/(1 + exp(beta[7] * gamma * omni_mn))                                                                 
vert_mng <-  exp(beta[8] + gamma * vert_mn)/(1 + exp(beta[8] + gamma * vert_mn))
   

     
# Prior on the effect of body size                                       
gamma ~ dnorm(0, 0.01)

}# model loop
",fill=TRUE)
sink()

#### initial values                                                     
inits1 <- list( beta = c(4,1,2,-2,0,-1,3,-3),  gamma = -2)                            
inits2 <- list(beta = c(3,2,-2,3,-3,1,0,-1),  gamma = 2)                      
inits3 <- list( beta = c(2,3,-3,0,-1,2,-2,1),  gamma = 1)

inits <-  list (inits1, inits2, inits3)

## Paramaters to monitor
params <-c('beta','aq','frug','gran','herb','invert','nect','omni','vert','aq_mng', 'frug_mng','gran_mng','herb_mng','invert_mng','nect_mng','omni_mng','vert_mng','gamma')

#---------------#                                                           
#-MCMC settings-#                                                                  
#---------------#
nc <- 3                                                                        
ni <- 7000                                                                            
nb <- 1000                                                                    
nt <- 3

## Output                                                              
birds_hunting_fn_bdy <- jags( n.chains=nc,n.iter = ni, data=data_threat, n.burnin = nb,inits = inits, n.thin = nt,parallel = TRUE, parameters.to.save = params, model.file = "birds_hunting_fn_bdy.txt") # 
saveRDS(birds_hunting_fn_bdy, "birds_hunting_fn_bdy.rds")
birds_hunting_fn_bdy_summary  <- birds_hunting_fn_bdy$summary
write.csv(birds_hunting_fn_bdy_summary, "birds_hunting_fn_bdy.csv")
          
