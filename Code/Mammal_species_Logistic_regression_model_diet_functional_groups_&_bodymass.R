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

data <- read.csv("mammals_traits_n_threats_data.csv")


###------------------------ 
### Remove DD species (i.e., Data Deficient species)
###------------------------ 

data <- data %>% 
        filter(category != "DD")

#------------------------#
### Threat - hunting
#------------------------#
data_hunting <- data %>% 
  select(Hunting,phyl_eltonian_ftn_gp,combined_body_mass) %>%
  mutate(f_group = as.numeric(recode(phyl_eltonian_ftn_gp, 'Aquatic_predator' = '1','FruitNectSeed' = '2',
                                     'Herbivore' = '3','Invertivore' = '4','Omnivore' = '5', 'Vertivore' = '6')))%>%
  mutate(hunting_f = as.numeric(recode(Hunting, '0' = '0', '1' = '0', '2' = '1', '3' = '1' ))) %>%
  mutate(bodymass.log10 = log10(combined_body_mass)) %>%
  mutate(bodymass_std = as.numeric(scale( bodymass.log10)))



###------------------------
### Functional group specific - body mass  
###------------------------  

data_fn_body_mass <- data_hunting %>% 
  mutate(f_group2 = recode(f_group, '1' ='Aquatic_predator',  '2' = 'FruitNectSeed', 
                           '3 ' = 'Herbivore' , '4' = 'Invertivore', '5' = 'Omnivore', '6' =  'Vertivore'))%>%
  arrange(desc(f_group2)) %>% 
  select(f_group2,bodymass_std) %>% 
  group_by(f_group2) %>% 
  summarize(count = n(),
            mean = mean(bodymass_std), 
            sd = sd(bodymass_std))

data_fn_body_mass

###----------------------------------
### mean body mass per functional group
###----------------------------------

###------------------------
## Aquatic predator
###------------------------
aq_mn <- data_fn_body_mass$mean[1]
aq_mn

###------------------------
## FruitNectSeed
###------------------------
fns_mn <- data_fn_body_mass$mean[2]
fns_mn

###------------------------
## Herbivore
###------------------------
herb_mn <- data_fn_body_mass$mean[3]
herb_mn

###------------------------
## Invertivore
###------------------------
invert_mn <- data_fn_body_mass$mean[4]
invert_mn

###------------------------
## Omnivore
###------------------------
omni_mn <- data_fn_body_mass$mean[5]
omni_mn

###------------------------
## Vertivore
###------------------------
vert_mn <- data_fn_body_mass$mean[6]
vert_mn





#------------------------#
#-Create IDs and indices-#
#------------------------#

#-----------------------------------#
#### Response categorical variable
#-----------------------------------#

hunting_f <- data_hunting$hunting_f


#-----------------------------------#
#### Number of levels- response variable
#-----------------------------------#
threat_level <- length(unique(data_hunting$hunting))
threat_level 


#--------------------#
# Total of species
#--------------------#
N <- nrow(data_hunting)


#-----------------------------------#
### Number of functional groups
#-----------------------------------#
fn <- length(unique(data_hunting$f_group ))
fn

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
data_threat <- list(hunting_f = hunting_f, N = N, threat_level = threat_level , fn_gp = fn_gp, fn = fn, bodymass=bodymass,
                    aq_mn =aq_mn, fns_mn =fns_mn, herb_mn= herb_mn,invert_mn =invert_mn, 
                    vert_mn = vert_mn, omni_mn = omni_mn) 


#-----------------------------------#
###  Logistic regression model - Jags code
#-----------------------------------#

sink("mammals_hunting_fn_bdy.txt")
cat("

model{
	
	for(i in 1:N){
		
		# non - threatened = 0; threatened = 1
		hunting_f[i] ~ dbern(prob[i])

         # Linear predictor
		logit(prob[i]) <- beta[fn_gp[i]] + gamma*bodymass[i]
		
	}
          
         # Priors - functional groups
	beta[1] ~ dnorm(0, 0.01)
	beta[2] ~ dnorm(0, 0.01)
	beta[3] ~ dnorm(0, 0.01)
	beta[4] ~ dnorm(0, 0.01)
	beta[5] ~ dnorm(0, 0.01)
	beta[6] ~ dnorm(0, 0.01)
	
 # Prior on the effect of body size
	gamma ~ dnorm(0, 0.01)
        
## Derived parmaters

aq <- exp(beta[1])/(1 + exp(beta[1]))
fns <-  exp(beta[2])/(1 + exp(beta[2]))
herb <-  exp(beta[3])/(1 + exp(beta[3]))
invert <-  exp(beta[4])/(1 + exp(beta[4]))
omni <-  exp(beta[5])/(1 + exp(beta[5]))
vert <-  exp(beta[6])/(1 + exp(beta[6]))     

	
	aq_mng <- exp(beta[1] + gamma * aq_mn )/(1 + exp(beta[1] * gamma *aq_mn))
fns_mng <-  exp(beta[2] + gamma * fns_mn)/(1 + exp(beta[2] + gamma * fns_mn))
herb_mng <-  exp(beta[3] + gamma * herb_mn)/(1 + exp(beta[3] + gamma * herb_mn))
invert_mng <-  exp(beta[4] + gamma * invert_mn)/(1 + exp(beta[4] + gamma * invert_mn))
omni_mng <-  exp(beta[5] + gamma * omni_mn )/(1 + exp(beta[5] * gamma * omni_mn))
vert_mng <-  exp(beta[6] + gamma * vert_mn)/(1 + exp(beta[6] + gamma * vert_mn))        
	
	
}# model loop
",fill=TRUE)
sink()



#### initial values

inits1 <- list(beta = c(3,1,2,-2,0,-1),  gamma = -2)
inits2 <- list(beta = c(-1,2,-2,3,1,0),  gamma = 2)
inits3 <- list(beta = c(2,0,-1,2,-2,1), gamma = 1 )



inits <-  list (inits1, inits2, inits3)


## Paramaters to monitor
params <- c('beta','aq','fns','herb','invert','omni','vert','aq_mng','fns_mng','herb_mng',
            'invert_mng', 'omni_mng','vert_mng', 'gamma')



#---------------#
#-MCMC settings-#
#---------------#


nc <- 3
ni <- 7000
nb <- 1000
nt <- 3


## Output
mammals_hunting_fn_bdy <- jags( n.chains=nc,n.iter = ni, data=data_threat, n.burnin = nb,inits = inits, n.thin = nt,parallel = TRUE,
                                       parameters.to.save = params, model.file = "mammals_hunting_fn_bdy.txt") # 



saveRDS(mammals_hunting_fn_bdy, "mammals_hunting_fn_bdy.rds")

mammals_hunting_fn_bdy_summary  <- mammals_hunting_fn_bdy$summary

write.csv(mammals_hunting_fn_bdy_summary, "mammals_hunting_fn_bdy.csv")


















