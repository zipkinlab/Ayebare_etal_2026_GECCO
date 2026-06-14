###--------------------------------------------------------###               
##### Code to run logistic regression – functional group as only predictor                    
###--------------------------------------------------------### 
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
### Threat - hunting
#------------------------#
data_hunting <- data %>% 
  select(Hunting,Functional_Group) %>%
  mutate(f_group = as.numeric(recode(Functional_Group, 'Aquatic predator' = '1','Frugivore' = '2', 'Granivore' = '3', 'Nectarivore' = '4',
                                     'Herbivore aquatic' = '5', 'Herbivore terrestrial' = '5 ' , 'Invertivore' = '6',
                                     'Vertivore' = '7', 'Scavenger' = '7', 'Omnivore' = '8')))%>%
  mutate(hunting_f = as.numeric(recode(Hunting, '0' = '0', '1' = '0', '2' = '1', '3' = '1' )))



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
fn

#---------------------#
### Functional groups
#---------------------#
fn_gp <- data_hunting$f_group



#-------------------------------#
# Bundle and summarize data set
#-------------------------------#
data_threat <- list(hunting_f = hunting_f, N = N, threat_level = threat_level , fn_gp = fn_gp, fn = fn) 


#-----------------------------------#
###  Logistic regression model - Jags code
#-----------------------------------#

sink("birds_hunting_fn.txt")
cat("
	
model{
	
	for(i in 1:N){
		
		# non - threatened = 0; threatened = 1
		hunting_f[i] ~ dbern(prob[i])

         # Linear predictor
		logit(prob[i]) <- beta[fn_gp[i]] 
		
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

aq <- exp(beta[1])/(1 + exp(beta[1]))
frug <-  exp(beta[2])/(1 + exp(beta[2]))
gran <-  exp(beta[3])/(1 + exp(beta[3]))
nect <-  exp(beta[4])/(1 + exp(beta[4]))
herb <-  exp(beta[5])/(1 + exp(beta[5]))
invert <-  exp(beta[6])/(1 + exp(beta[6]))
vert <-  exp(beta[7])/(1 + exp(beta[7]))
omni <-  exp(beta[8])/(1 + exp(beta[8]))

	
}# model loop
",fill=TRUE)
sink()




#### initial values

inits1 <- list(beta = c(4,1,2,-2,0,-1,3,-3))
inits2 <- list(beta = c(3,2,-2,3,-3,1,0,-1))
inits3 <- list( beta = c(2,3,-3,0,-1,2,-2,1))



inits <-  list (inits1, inits2, inits3)


## Paramaters to monitor
params <- c('beta', 'aq','frug','gran','nect','herb','invert','vert','omni')

#---------------#
#-MCMC settings-#
#---------------#


nc <- 3
ni <- 7000
nb <- 1000
nt <- 3


## Output
birds_hunting_fn <- jags( n.chains=nc,n.iter = ni, data=data_threat, n.burnin = nb,inits = inits, n.thin = nt,parallel = TRUE,
                                       parameters.to.save = params, model.file = "birds_hunting_fn.txt") # 


saveRDS(birds_hunting_fn, "birds_hunting_fn.rds")


birds_hunting_summary  <- birds_hunting_fn$summary

write.csv(birds_hunting_summary, "birds_hunting_fn.csv")




















