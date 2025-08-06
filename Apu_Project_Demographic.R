# Load necessary libraries
library(dplyr)
library(readr)

# Load the datasets
library(haven)
NSDUH_2021 <- read_sav("Data/NSDUH_2021.SAV")
View(NSDUH_2021)

library(haven)
NSDUH_2022 <- read_sav("Data/NSDUH_2022.sav")
View(NSDUH_2022)

library(haven)
NSDUH_2023 <- read_sav("Data/NSDUH_2023.sav")
View(NSDUH_2023)



# Add a 'year' column to each dataset
NSDUH_2021 <- NSDUH_2021 %>% mutate(year = 2021)
NSDUH_2022 <- NSDUH_2022 %>% mutate(year = 2022)
NSDUH_2023 <- NSDUH_2023 %>% mutate(year = 2023)

# Combine all years
nsduh_all <- bind_rows(NSDUH_2021, NSDUH_2022, NSDUH_2023)

# Frequency table
table(nsduh_all$CATAGE)

nsduh_12_17 <- nsduh_all %>%
  filter(CATAGE == 1)
View(nsduh_12_17)


# Frequency table
table(nsduh_12_17$IRSEX)

# Frequency with percentages
prop.table(table(nsduh_12_17$IRSEX)) * 100

# Frequency table
table(nsduh_12_17$NEWRACE2)

# Frequency table
table(nsduh_12_17$YMDEYR)


# Frequency table
table(nsduh_12_17$YOPB2WK)


# Frequency table
table(nsduh_12_17$COUTYP4)

# Frequency table
table(nsduh_12_17$ALCYR)

# Frequency table
table(nsduh_12_17$MRJYR)

# Frequency table
table(nsduh_12_17$OPIANYYR)


@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Chi Square tests
  
# Frequency table
  table(nsduh_12_17$ALCYR, nsduh_12_17$IRSEX)

# Chi-square test
chisq.test(nsduh_12_17$ALCYR, nsduh_12_17$IRSEX)

chisq.test(nsduh_12_17$MRJYR, nsduh_12_17$IRSEX)

chisq.test(nsduh_12_17$OPIANYYR, nsduh_12_17$IRSEX)

chisq.test(nsduh_12_17$YMDEYR, nsduh_12_17$IRSEX) 






chisq.test(nsduh_12_17$ALCYR, nsduh_12_17$NEWRACE2)

chisq.test(nsduh_12_17$MRJYR, nsduh_12_17$NEWRACE2)

chisq.test(nsduh_12_17$OPIANYYR, nsduh_12_17$NEWRACE2)

chisq.test(nsduh_12_17$YMDEYR, nsduh_12_17$NEWRACE2)



chisq.test(nsduh_12_17$ALCYR, nsduh_12_17$COUTYP4)

chisq.test(nsduh_12_17$MRJYR, nsduh_12_17$COUTYP4)

chisq.test(nsduh_12_17$OPIANYYR, nsduh_12_17$COUTYP4)

chisq.test(nsduh_12_17$YMDEYR, nsduh_12_17$COUTYP4)


library(dplyr)
library(purrr)

# Group by county type and run chisq.test for each group
results_by_county <- nsduh_12_17 %>%
  group_by(COUTYP4) %>%
  summarise(
    chisq_test = list(chisq.test(table(ALCYR, IRSEX)))
  )

# To view the results
results_by_county$chisq_test



library(dplyr)
library(purrr)

# Group by county type and run chisq.test for each group
results_by_county <- nsduh_12_17 %>%
  group_by(COUTYP4) %>%
  summarise(
    chisq_test = list(chisq.test(table(MRJYR, IRSEX)))
  )

# To view the results
results_by_county$chisq_test




library(dplyr)
library(purrr)

# Group by county type and run chisq.test for each group
results_by_county <- nsduh_12_17 %>%
  group_by(COUTYP4) %>%
  summarise(
    chisq_test = list(chisq.test(table(OPIANYYR, IRSEX)))
  )

# To view the results
results_by_county$chisq_test


library(dplyr)
library(purrr)

# Group by county type and run chisq.test for each group
results_by_county <- nsduh_12_17 %>%
  group_by(COUTYP4) %>%
  summarise(
    chisq_test = list(chisq.test(table(YMDEYR, IRSEX)))
  )

# To view the results
results_by_county$chisq_test














@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
#Logistic Regression
  
  nsduh_12_17 <- nsduh_12_17 %>%
  mutate(
    SEX = factor(IRSEX, labels = c("Male", "Female")),
    RACE = factor(NEWRACE2),
    COUNTY = factor(COUTYP4),
    AGEGRP = factor(CATAG6)
  )

model_mj <- glm(MRJYR ~ SEX + RACE + COUNTY, 
                data = nsduh_12_17, family = binomial)

summary(model_mj)

# Odds Ratios
exp(coef(model_mj))

# 95% Confidence Intervals
exp(confint(model_mj))


model_alcohol <- glm(ALCYR ~ SEX + RACE + COUNTY, 
                     data = nsduh_12_17, family = binomial)

summary(model_alcohol)


# Odds Ratios
exp(coef(model_alcohol))

# 95% Confidence Intervals
exp(confint(model_alcohol))  

# Fit logistic regression model: opioid use predicted by sex, race, and county
model_opioid <- glm(OPIANYYR ~ SEX + RACE + COUNTY, 
                    data = nsduh_12_17, family = binomial)

# View summary of model
summary(model_opioid)

# Calculate odds ratios
exp(coef(model_opioid))

# Calculate 95% confidence intervals for odds ratios (may take a moment)
exp(confint(model_opioid))


# Recode YMDEYR to binary 0/1 (1 = Yes, 0 = No)
nsduh_12_17 <- nsduh_12_17 %>%
  mutate(YMDEYR_bin = case_when(
    YMDEYR == 1 ~ 1,
    YMDEYR == 2 ~ 0,
    TRUE ~ NA_real_  # Keep other values as missing
  ))


# Run logistic regression
model_mde <- glm(YMDEYR_bin ~ SEX + RACE + COUNTY,
                 data = nsduh_12_17, family = binomial)

# Summary of the model
summary(model_mde)

# Odds ratios
exp(coef(model_mde))

# 95% Confidence intervals (may take time)
exp(confint(model_mde))































  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  