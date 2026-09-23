# Global impacts of anthropogenic threats on bird and mammal diet functional groups

Samuel Ayebare, Peter J. Williams, Sydney Ceyzyk, and  Elise F. Zipkin

Global Ecology and Conservation 2026, Volume 69, e04292

[10.1016/j.gecco.2026.e04292](https://doi.org/10.1016/j.gecco.2026.e04292)

## Abstract:
Anthropogenic threats vary in how they affect individual species, functional diversity, and consequently, ecosystems as a whole. However, which groups of species are most affected by specific threats remains poorly understood. To evaluate which bird and mammal functional groups are most affected by specific threats, we use data aggregated by the International Union for Conservation of Nature (IUCN) to calculate the extent to which diet functional groups and body size explain the global impacts of the most common anthropogenic threats. For both birds and mammals, we found that terrestrial vertivores, aquatic predators, and frugivores (including mammalian nectivores and granivores) are most negatively impacted by anthropogenic threats, with crops and livestock as the top threats to vertivores, climate change and pollution for aquatic predators, and logging for the frugivores. Threats associated with plantations and recreation/work affect less than 5% of species across functional groups. Body mass is positively correlated with the impact of anthropogenic threats especially for hunting, such that larger species are substantially more threatened by hunting than smaller species. Our results reveal that bird and mammal vulnerabilities vary by diet functional group, body mass, and the type of anthropogenic threat. Given our findings that functionally important groups (i.e., predators and frugivores) are disproportionately impacted by multiple anthropogenic threats, and that individual threats differ in their severity across functional groups, we recommend targeting conservation actions to mitigate threats in a manner that will preserve critical ecosystem functions.

*Keywords:* Anthropogenic threats; Birds; Mammals; Diet functional groups; Body mass; Crops; Hunting

## Code

This folder contains all the code required to:
                  i)   Download anthropogenic threat data from the International Union for Conservation of Nature (IUCN) online database for bird and mammal species.
                  ii)  Combine anthropogenic threat data and trait data (diet functional groups and body mass).
                  iii) Fit logistic regression models.

We carried out our data processing, cleaning and model fitting using R (R Core Team 2024)

1. **`Birds_anthropogenic_threat_data.R`**:  Code to download anthropogenic threat data for birds from the International Union for Conservation of Nature (IUCN) online database (https://www.iucnredlist.org/resources/threat-classification-scheme).
2. **`Bird_species_Code_to_combine_threat_and_trait_data.R`**: Code to combine anthropogenic threat and trait (diet functional groups and body mass) data for birds.
3. **`Bird_species_Logistic_regression_model_diet_functional_groups_only.R`**: Example code (hunting) to run a logistic regression model to examine the relationship between anthropogenic threats and bird functional groups. Similar code was used for all the anthropogenic threats.
4. **`Bird_species_Logistic_regression_model_diet_functional_groups_&_bodymass.R`**: Example code (hunting) to run a logistic regression model to examine the relationship between anthropogenic threats, and bird functional groups and body mass. Similar code was used for all the anthropogenic threats.
5. **`Birds_accuracy_prediction_functional_groups.R`**: Code to assess the prediction accuracy of bird functional groups.
6. **`Birds_accuracy_prediction_body_mass.R`**: Code to assess the prediction accuracy of body mass - birds.
7. **`Mammal_anthropogenic_threat_data.R`**:  Code to download anthropogenic threat data for mammals from the International Union for Conservation of Nature (IUCN) online database (https://www.iucnredlist.org/resources/threat-classification-scheme).
8. **`Mammal_species_Code_to_combine_threat_and_trait_data.R`**: Code to combine anthropogenic threat and trait (diet functional groups and body mass) data for mammals.
9. **`Mammal_species_Logistic_regression_model_diet_functional_groups_only.R`**: Example code (hunting) to run a logistic regression model to examine the relationship between anthropogenic threats and mammal functional groups. Similar code was used for all the anthropogenic threats.
10. **`Mammal_species_Logistic_regression_model_diet_functional_groups_&_bodymass.R`**: Example code (hunting) to run a logistic regression model to examine the relationship between anthropogenic threats, and mammal functional groups and body mass. Similar code was used for all the anthropogenic threats.
11. **`Mammals_accuracy_prediction_functional_groups.R`**: Code to assess the prediction accuracy of bird functional groups.
6. **`Mammals_accuracy_prediction_body_mass.R`**: Code to assess the prediction accuracy of body mass - mammals.

## Data

This folder contains:
            i) Anthropogenic threat data for bird and mammal species downloaded from the the International Union for Conservation of Nature (IUCN) online database. (https://www.iucnredlist.org/resources/threat-classification-scheme).
            ii) Trait data for bird and mammal species - obtained from the AVONET database for birds, and the PHYLACINE and EltonTraits databases for mammals.

1.  **`Bird_species_data`**: Folder containing IUCN Red List, and AVONET (i.e., diet functional groups, and body mass) 
                              data for birds.
2.   **`Mammal_species_data`**: Folder containing IUCN Red List, Eltonian and PHYLACINE (i.e., diet functional groups, and body 
                                mass) data for mammals.
