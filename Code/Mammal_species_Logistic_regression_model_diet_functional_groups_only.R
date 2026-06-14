
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
data <- read.csv("mammals_traits_n_threats_data.csv")



###------------------------#
## Remove DD species                                                     
###------------------------ 

data <- data %>% 
        filter(category != "DD")




#------------------------#
### Threat - Hunting
#------------------------#

#### Select threatening driver ; hunting
data_hunting <- data %>% 
  select(Hunting,phyl_eltonian_ftn_gp) %>%
  mutate(f_group = as.numeric(recode(phyl_eltonian_ftn_gp, 'Aquatic_predator' = '1','FruitNectSeed' = '2',
                                     'Herbivore' = '3', 'Vertivore' = '4', 'Invertivore' = '5','Omnivore' = '6')))%>%
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
threat_level <- length(unique(data_hunting$Hunting))
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
###  Ordinal regression model:
### Modified from https://runjags.sourceforge.net/examples/squirrels.R
#-----------------------------------#

sink("hunting_fn.txt")
cat("
	
model{
	
	for(i in 1:N){
		
		# non - threatened = 0; threatened = 1
		hunting_f[i] ~ dbern(prob[i])

         # Linear predictor
		logit(prob[i]) <-  beta[fn_gp[i]] 
		
	}
          

     
		# Priors - functional groups
	beta[1] ~ dnorm(0, 0.01)
	beta[2] ~ dnorm(0, 0.01)
	beta[3] ~ dnorm(0, 0.01)
	beta[4] ~ dnorm(0, 0.01)
	beta[5] ~ dnorm(0, 0.01)
	beta[6] ~ dnorm(0, 0.01)

        
## Derived parmaters

aq <- exp(beta[1])/(1 + exp(beta[1]))
fns <-  exp(beta[2])/(1 + exp(beta[2]))
herb <-  exp(beta[3])/(1 + exp(beta[3]))
vert <-  exp(beta[4])/(1 + exp(beta[4]))
invert <-  exp(beta[5])/(1 + exp(beta[5]))
omni <-  exp(beta[6])/(1 + exp(beta[6]))
	
	
	
}# model loop
",fill=TRUE)
sink()



#### initial values

inits1 <- list(beta = c(3,1,2,-2,0,-1))
inits2 <- list(beta = c(-1,2,-2,3,1,0))
inits3 <- list(beta = c(2,0,-1,2,-2,1))



inits <-  list (inits1, inits2, inits3)



## Paramaters to monitor
params <- c('beta', 'aq','fns','herb','vert','invert','omni')


#---------------#
#-MCMC settings-#
#---------------#


nc <- 3
ni <- 7000
nb <- 1000
nt <- 3

## Output
mammals_hunting_fn <- jags( n.chains=nc,n.iter = ni, data=data_threat, n.burnin = nb,inits = inits, n.thin = nt,parallel = TRUE,
                                       parameters.to.save = params, model.file = "hunting_fn.txt") # 


saveRDS(mammals_hunting_fn, "mammals_hunting_fn.rds")

mammals_hunting_fn_summary  <- mammals_hunting_fn$summary

write.csv(mammals_hunting_fn_summary, "mammals_hunting_fn.csv")

