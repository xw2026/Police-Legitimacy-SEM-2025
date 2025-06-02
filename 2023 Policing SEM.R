library(lavaan)
library(semPlot)
library(semTools)
library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(sjPlot)

d <- read_sav("UMAS0078_W6_OUTPUT.sav")


# State Code
code_to_state <- c(
  "1" = "Alabama", "2" = "Alaska", "4" = "Arizona", "5" = "Arkansas", 
  "6" = "California", "8" = "Colorado", "9" = "Connecticut", "10" = "Delaware", 
  "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia", "15" = "Hawaii", 
  "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa", 
  "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine", 
  "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota", 
  "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", 
  "32" = "Nevada", "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico", 
  "36" = "New York", "37" = "North Carolina", "38" = "North Dakota", 
  "39" = "Ohio", "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", 
  "44" = "Rhode Island", "45" = "South Carolina", "46" = "South Dakota", 
  "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont", 
  "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", 
  "55" = "Wisconsin", "56" = "Wyoming", "60" = "American Samoa", 
  "64" = "Federated States of Micronesia", "66" = "Guam", 
  "68" = "Marshall Islands", "69" = "Northern Mariana Islands", 
  "70" = "Palau", "72" = "Puerto Rico", "74" = "U.S. Minor Outlying Islands", 
  "78" = "Virgin Islands", "81" = "Alberta", "82" = "British Columbia", 
  "83" = "Manitoba", "84" = "New Brunswick", "85" = "Newfoundland", 
  "86" = "Northwest Territories", "87" = "Nova Scotia", "88" = "Nunavut", 
  "89" = "Ontario", "90" = "Prince Edward Island", "91" = "Quebec", 
  "92" = "Saskatchewan", "93" = "Yukon Territory", "99" = "Not in the U.S. or Canada"
)

# Step 1: Map state codes to state names
d <- d %>%
  mutate(state = code_to_state[as.character(inputstate)])  # Ensure inputstate is character for matching

# Step 2: Define regional groupings
southern_states <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", 
                     "Kentucky", "Louisiana", "Maryland", "Mississippi", 
                     "North Carolina", "Oklahoma", "South Carolina", 
                     "Tennessee", "Texas", "Virginia", "West Virginia")

northeast_states <- c("Connecticut", "District of Columbia", "Maine", "Massachusetts", 
                      "New Hampshire", "New Jersey", "New York", "Pennsylvania", 
                      "Rhode Island", "Vermont")

midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", 
                    "Minnesota", "Missouri", "Nebraska", "North Dakota", 
                    "Ohio", "South Dakota", "Wisconsin")

western_states <- c("Arizona", "California", "Colorado", "Idaho", "Montana", 
                    "Nevada", "New Mexico", "Oregon", "Utah", "Washington", 
                    "Wyoming", "Alaska", "Hawaii")

# Step 3: Create region based on state
d <- d %>%
  mutate(region = case_when(
    state %in% southern_states ~ "South",
    state %in% northeast_states ~ "Northeast",
    state %in% midwest_states ~ "Midwest",
    state %in% western_states ~ "West",
    TRUE ~ "Other"
  ))

# Step 4: Convert region to a factor and check summary
d$region <- factor(d$region, levels = c("South", "Northeast", "Midwest", "West", "Other"))

# Check the distribution of the regions
summary(d$region)

# Convert birth year to age
d <- d %>%
  mutate(age = 2023 - as.numeric(as.character(birthyr)))

d$age

# Recode Gender
d$gender <- factor(d$gender4, 
                   levels = c("1", "2", "3", "4"), 
                   labels = c("man", "woman", "other", "other"))
summary(d$gender)

# Convert employment status to a dummy variable
d$unemployed <- ifelse(d$employ == 4, "unemployed", "employed") %>% as.factor()
summary(d$unemployed)

#Recode Education
d$education <- factor(d$educ, 
                      levels = c("1", "2", "3", "4", "5", "6"),
                      labels = c("high school and below", "high school and below", 
                                 "some college and above", "some college and above", 
                                 "some college and above", "some college and above"))

summary(d$education)
education_percentages <- prop.table(table(d$education)) * 100
education_percentages
#### ideology 
d$ideology <- factor(d$ideo5, 
                     levels = c("1", "2", "3", "4", "5", "6"), 
                     labels = c("Liberal", "Liberal", "Moderate", "Conservative", "Conservative", "NotSure"))

summary(d$ideology)
ideology_p <- prop.table(table(d$ideology)) * 100
### Race
d$race <- factor(d$race, 
                 levels = c("1", "2", "3", "4", "5", "6", "7", "8", "98", "99"), 
                 labels = c("White", "Black", "Hispanic", "Other", "Other", 
                            "Other", "Other", "Other", "Other", "Other"))
summary(d$race)


### Immigration
d$immigration <- factor(d$UML124a, 
                        levels = c("1", "2", "8", "9"), 
                        labels = c("yes", "no", "no", "no"))
summary(d$immigration)

### Family Income
income_labels <- c(
  "1" = "Less than $10,000",
  "2" = "$10,000 - $19,999",
  "3" = "$20,000 - $29,999",
  "4" = "$30,000 - $39,999",
  "5" = "$40,000 - $49,999",
  "6" = "$50,000 - $59,999",
  "7" = "$60,000 - $69,999",
  "8" = "$70,000 - $79,999",
  "9" = "$80,000 - $99,999",
  "10" = "$100,000 - $119,999",
  "11" = "$120,000 - $149,999",
  "12" = "$150,000 - $199,999",
  "13" = "$200,000 - $249,999",
  "14" = "$250,000 - $349,999",
  "15" = "$350,000 - $499,999",
  "16" = "$500,000 or more",
  "97" = "Prefer not to say",
  "998" = "skipped",
  "999" = "not asked"
)
d <- d %>%
  mutate(faminc_new = recode(as.character(faminc_new), !!!income_labels))
d <- d %>%
  mutate(FamilyIncome = case_when(
    faminc_new %in% c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999") ~ "$0–$39,999",
    faminc_new %in% c("$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999",
                      "$80,000 - $99,999", "$100,000 - $119,999") ~ "$40,000-$119,999",
    faminc_new %in% c("$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999",
                      "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more") ~ "$120,000 and above",
    faminc_new %in% c("Prefer not to say") ~ "prefer not to say",
    TRUE ~ NA_character_  # Handle any unexpected categories if necessary
  ))

d$FamilyIncome <- factor(d$FamilyIncome, levels = c("$0–$39,999", "$40,000-$119,999", "$120,000 and above", "prefer not to say"))
summary(d$FamilyIncome)

### Marital Status
d$marital <- factor(d$marstat, 
                    levels = c("1", "2", "3", "4", "5", "6", "8", "9"), 
                    labels = c("married", "separated", "divorced", "widowed", "never", 
                               "civil", "skip", "not asked"))
summary(d$marital)
### Wording
d$UML100_pipe <- as.factor(d$UML100_pipe)
d$UML100_pipe <- fct_recode(d$UML100_pipe, Minority = "minority")
summary(d$UML100_pipe)
d$UML100_pipe <- relevel(d$UML100_pipe, ref = "White")


## Obligation to Obey
d <- d %>% 
  mutate(obey = UML120 + UML121 + UML122 + UML123)

summary(d$obey)

# Procedural Justice
d$a <- d$UML118 %>% as.numeric()
summary(d$a)
d <- d %>% 
  mutate(UML118r = case_when(
    UML118 == 1 ~ 4, 
    UML118 == 2 ~ 3, 
    UML118 == 3 ~ 2,
    UML118 == 4 ~1
  ))

d <- d %>% 
  mutate(pj = UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119)
summary(d$pj)
# CFA
pjmod <- "PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119"
pjfit <- sem(pjmod, data = d)
summary(pjfit, standardized = TRUE, fit.measures = TRUE)
semPaths(pjfit)

semPaths(
  pjfit,
  what = "std",               # Show standardized estimates
  edge.label.cex = 1.2,       # Adjust the size of edge labels
  layout = "circle",          # Choose a layout for the graph
  sizeMan = 8,                # Adjust size of manifest variables
  sizeLat = 10,               # Adjust size of latent variables
  edge.color = "black",       # Set edge color
  style = "lisrel",           # Use LISREL-style diagram
  residuals = TRUE,           # Include residuals in the diagram
  nCharNodes = 0              # Show full names of variables
)



obmod <- "Obey =~ UML120 + UML121 + UML122 + UML123"
obfit <- sem(obmod, data = d)
summary(obfit, standardized = TRUE, fit.measures = TRUE)
semPaths(
  obfit,
  what = "std",               # Show standardized estimates
  edge.label.cex = 1.2,       # Adjust the size of edge labels
  layout = "tree",            # Choose a hierarchical tree layout
  sizeMan = 8,                # Adjust size of manifest variables
  sizeLat = 10,               # Adjust size of latent variables
  edge.color = "black",       # Set edge color
  style = "lisrel",           # Use LISREL-style diagram
  residuals = TRUE,           # Include residuals in the diagram
  nCharNodes = 0              # Show full names of variables
)


# combined model
cbmod <- '
  QT =~ UML112 + UML113 + UML114 + UML115 
  QD =~ UML116 + UML117 + UML118r + UML119
  PJ =~ QT + QD
  Obey =~ UML120 + UML121 + UML122 + UML123
'

cbfit <- sem(cbmod, data = d)

summary(cbfit, standardized = TRUE, fit.measures = TRUE)


cbmod1 <- '
Obey =~ UML120 + UML121 + UML122 + UML123
PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119
'
cbfit1 <- sem(cbmod1, data = d)

summary(cbfit1, standardized = TRUE, fit.measures = TRUE)

# Ensure categorical variables are factors
d <- d %>%
  mutate(
    gender = as.factor(gender),
    race = as.factor(race),
    region = as.factor(region),
    ideology = as.factor(ideology),
    marital = as.factor(marital),
    UML100_pipe = as.factor(UML100_pipe)
  )

# Convert categorical variables into dummy variables
dummies <- model.matrix(~ gender + race + region + ideology + marital + UML100_pipe, data = d)[, -1]  # Remove intercept

# Combine with original dataset (excluding the original categorical columns)
d <- cbind(d %>% select(-gender, -race, -region, -ideology, -marital, -UML100_pipe), dummies)

summary(as.factor(d$UML100_pipeBlack))

# Define the SEM model with dummy variables
sem_model <- '
  # Define latent variables
  Obey =~ UML120 + UML121 + UML122 + UML123
  PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119

  # Structural path: PJ predicting Obey with control variables
  Obey ~ PJ + age + unemployed + immigration + education + 
          genderwoman + genderother +    # Dummy variables for gender
          raceBlack + raceHispanic + raceOther +  # Dummy variables for race
          regionNortheast + regionMidwest + regionWest +  # Dummy variables for region
          ideologyModerate + ideologyConservative + ideologyNotSure +  # Dummy variables for ideology
          maritalseparated + maritaldivorced + maritalwidowed + maritalnever + maritalcivil +  # Dummy for marital status
          UML100_pipeMinority + UML100_pipeBlack + UML100_pipeLatino   # Dummy for wording
'

sem_model_1 <- '
  # Define latent variables
  PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119
  
  # Structural path: PJ predicting Obey with control variables
  PJ ~ age + unemployed + immigration + education + 
          genderwoman + genderother +    # Dummy variables for gender
          raceBlack + raceHispanic + raceOther +  # Dummy variables for race
          regionNortheast + regionMidwest + regionWest +  # Dummy variables for region
          ideologyModerate + ideologyConservative + ideologyNotSure +  # Dummy variables for ideology
          maritalseparated + maritaldivorced + maritalwidowed + maritalnever + maritalcivil +  # Dummy for marital status
          UML100_pipeMinority + UML100_pipeBlack + UML100_pipeLatino   # Dummy for wording
'
sem_model2 <- '
  # Define latent variables
  Obey =~ UML120 + UML121 + UML122 + UML123
  PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119

  # Structural path: PJ predicting Obey with control variables
  Obey ~ PJ + age + unemployed + immigration + education + 
          genderwoman + genderother +    # Dummy variables for gender
          raceBlack + raceHispanic + raceOther +  # Dummy variables for race
          regionNortheast + regionMidwest + regionWest +  # Dummy variables for region
          ideologyModerate + ideologyConservative + ideologyNotSure +  # Dummy variables for ideology
          maritalseparated + maritaldivorced + maritalwidowed + maritalnever + maritalcivil +  # Dummy for marital status
          UML100_pipeMinority + UML100_pipeBlack + UML100_pipeLatino   # Dummy for wording
  PJ ~ age + unemployed + immigration + education + 
          genderwoman + genderother +    # Dummy variables for gender
          raceBlack + raceHispanic + raceOther +  # Dummy variables for race
          regionNortheast + regionMidwest + regionWest +  # Dummy variables for region
          ideologyModerate + ideologyConservative + ideologyNotSure +  # Dummy variables for ideology
          maritalseparated + maritaldivorced + maritalwidowed + maritalnever + maritalcivil +  # Dummy for marital status
          UML100_pipeMinority + UML100_pipeBlack + UML100_pipeLatino   # Dummy for wording
'
# Fit the SEM model
sem_fit <- sem(sem_model, data = d, meanstructure = TRUE)
sem_fit1 <- sem(sem_model_1, data = d, meanstructure = TRUE)
sem_fit2 <- sem(sem_model2, data = d, meanstructure = TRUE)
# Display model summary
summary(sem_fit, fit.measures = TRUE, standardized = TRUE)
summary(sem_fit1, fit.measures = TRUE, standardized = TRUE)
summary(sem_fit2, fit.measures = TRUE, standardized = TRUE)
# Visualize SEM Paths
semPaths(
  sem_fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1.2,
  sizeMan = 8,
  sizeLat = 10,
  edge.color = "black",
  style = "lisrel",
  residuals = TRUE
)

semPaths(
  sem_fit,
)

semPaths(sem_fit1)

levels(d$education)
levels(d$immigration)
levels(d$unemployed)


#### Interaction ####
interaction_data <- indProd(
  data = d,
  var1 = c("UML120", "UML121", "UML122", "UML123"),  # PJ indicators
  var2 = c("UML100_pipeMinority", "UML100_pipeBlack", "UML100_pipeLatino"),  # Categorical moderator
  match = FALSE,  # No need to match variables one-to-one
  meanC = TRUE,  # Mean-center before interaction
  residualC = FALSE
)

colnames(interaction_data)
sem_model_interaction <- '
  # Define latent variables
  Obey =~ UML120 + UML121 + UML122 + UML123
  PJ =~ UML112 + UML113 + UML114 + UML115 + UML116 + UML117 + UML118r + UML119

  # Structural model with interaction terms
  Obey ~ PJ + UML100_pipeMinority + UML100_pipeBlack + UML100_pipeLatino +  
         UML120.UML100_pipeMinority + UML121.UML100_pipeMinority + UML122.UML100_pipeMinority + UML123.UML100_pipeMinority +
         UML120.UML100_pipeBlack + UML121.UML100_pipeBlack + UML122.UML100_pipeBlack + UML123.UML100_pipeBlack +
         UML120.UML100_pipeLatino + UML121.UML100_pipeLatino + UML122.UML100_pipeLatino + UML123.UML100_pipeLatino
'
sem_fit_interaction <- sem(sem_model_interaction, data = interaction_data, meanstructure = TRUE)
summary(sem_fit_interaction, fit.measures = TRUE, standardized = TRUE)


