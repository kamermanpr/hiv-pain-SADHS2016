############################################################
#                                                          #
#                      Load packages                       #
#                                                          #
############################################################
library(dplyr)
library(readr)
library(stringr)
library(foreign)

############################################################
#                                                          #
#                       Import data                        #
#                                                          #
############################################################
#--- Adult health questionnaire ---#
# Women
## File path will change depending on path to the downloaded DHS file 
adult_health_women <- read.spss('ZA_2016_DHS_05092022_740_160066/ZAAH71SV/ZAAHW71FL.SAV',
                                to.data.frame = TRUE,
                                add.undeclared.levels = 'no')

# Men
## File path will change depending on path to the downloaded DHS file 
adult_health_men <- read.spss('ZA_2016_DHS_05092022_740_160066/ZAAH71SV/ZAAHM71FL.SAV',
                              to.data.frame = TRUE,
                              add.undeclared.levels = 'no')

#--- HIV questionnaire ---#
## File path will change depending on path to the downloaded DHS file 
hiv <- read.spss('ZA_2016_DHS_05092022_740_160066/ZAAR71SV/ZAAR71FL.SAV',
                 to.data.frame = TRUE,
                 add.undeclared.levels = 'no')

############################################################
#                                                          #
#                        Clean data                        #
#                                                          #
############################################################
#--- Adult health questionnaire ---#
# Women
women_reduced <- adult_health_women %>% 
    # Select columns
    select(V001, V002, V003, V005, SWEIGHT, V021, V022, 
           V012, V190, S1443, S1444, starts_with('S1445')) %>% 
    # Rename columns
    rename(age_years = V012,
           wealth_index = V190,
           pain_now = S1443,
           pain_chronic = S1444,
           pain_back = S1445A,
           pain_neck.shoulder = S1445B,
           pain_head.face = S1445C,
           pain_stomach.abdomen = S1445D,
           pain_limbs = S1445E,
           pain_chest = S1445F,
           pain_other = S1445X) %>% 
    # Add sex column
    mutate(sex = 'Female') %>% 
    # Add ID column (cluster number + household number + line number)
    mutate(ID = paste0(V001, '_', V002, '_', V003)) %>% 
    # Create ID_household column (cluster number + household number)
    mutate(ID_household = paste0(V001, '_', V002)) %>% 
    # Create line_number column
    mutate(line_number = V003) %>% 
    # Remove columns that are no longer required
    select(-c(V001, V002, V003)) %>% 
    # Arrange columns
    select(ID, ID_household, line_number, V005, SWEIGHT, V021, V022, 
           sex, age_years, wealth_index, everything())

# Men
men_reduced <- adult_health_men %>% 
    # Select columns
    select(MV001, MV002, MV003, MV005, SMWEIGHT, MV021, MV022, MV190,
           MV012, SM1138, SM1139, SM1140A, SM1140B, SM1140C,
           SM1140D, SM1140E, SM1140F, SM1140X) %>% 
    # Rename columns
    rename(V001 = MV001, 
           V002 = MV002, 
           V003 = MV003,
           V005 = MV005,
           SWEIGHT = SMWEIGHT,
           V021 = MV021,
           V022 = MV022,
           age_years = MV012,
           wealth_index = MV190,
           pain_now = SM1138,
           pain_chronic = SM1139,
           pain_back = SM1140A,
           pain_neck.shoulder = SM1140B,
           pain_head.face = SM1140C,
           pain_stomach.abdomen = SM1140D,
           pain_limbs = SM1140E,
           pain_chest = SM1140F,
           pain_other = SM1140X) %>% 
    # Add sex column
    mutate(sex = 'Male') %>% 
    # Add ID column (cluster number + household number + line number)
    mutate(ID = paste0(V001, '_', V002, '_', V003)) %>% 
    # Create household ID (cluster number + household number)
    mutate(ID_household = paste0(V001, '_', V002)) %>% 
    # Create line_number column
    mutate(line_number = V003) %>% 
    # Remove columns that are no longer required
    select(-c(V001, V002, V003)) %>% 
    # Arrange columns
    select(ID, ID_household, line_number, V005, SWEIGHT, V021, V022, 
           sex, age_years, wealth_index, everything())

# Join women's and men's questionnaires
sex_combined <- women_reduced %>% 
    bind_rows(men_reduced)

#--- HIV questionnaire ---#
hiv_clean <- hiv %>% 
    # Select columns
    select(HIVCLUST, HIVNUMB, HIVLINE,
           HIV05, HIV03) %>% 
    # Add ID column (cluster number + household number + line number)
    mutate(ID = paste0(HIVCLUST, '_', HIVNUMB, '_', HIVLINE)) %>% 
    # Create ID_household column (cluster number + household number)
    mutate(ID_household = paste0(HIVCLUST, '_', HIVNUMB)) %>% 
    # Create line_number column
    mutate(line_number = HIVLINE) %>% 
    # Remove columns that are no longer required
    select(-c(HIVCLUST, HIVNUMB, HIVLINE)) %>% 
    # Rename columns
    rename(HIV_sample_weight = HIV05,
           HIV_status = HIV03) %>% 
    # Order columns
    select(ID, ID_household, line_number,
           HIV_sample_weight, HIV_status)

############################################################
#                                                          #
#          Merge hiv_clean and sex_combined data           #
#                                                          #
############################################################
data_joined <- sex_combined %>% 
    left_join(hiv_clean)

############################################################
#                                                          #
#                       Process data                       #
#                                                          #
############################################################
data <- data_joined %>%
    # Correct sample weights
    mutate(SWEIGHT = SWEIGHT / 1000000,
           HIV_sample_weight = HIV_sample_weight / 1000000) %>% 
    # Remove extra spaces in levels
    mutate(HIV_status = str_squish(HIV_status)) %>% 
    # Generate age categories (by 10-year)
    mutate(age_category = case_when(
        age_years >= 15 & age_years <= 24 ~ '15-24 years',
        age_years >= 25 & age_years <= 34 ~ '25-34 years',
        age_years >= 35 & age_years <= 44 ~ '35-44 years',
        age_years >= 45 & age_years <= 54 ~ '45-54 years',
        age_years >= 55 & age_years <= 64 ~ '55-64 years',
        age_years >= 65 ~ '65+ years'
    )) %>% 
    # Clean pain columns
    mutate(across(.cols = starts_with('pain_'), ~ ifelse(is.na(.x),
                                                               yes = 'No',
                                                               no = .x))) %>% 
    # Fix issue with last ifelse
    mutate(across(.cols = starts_with('pain_'), ~ case_when(
        .x == '1' ~ 'No',
        .x == '2' ~ 'Yes',
        TRUE ~ 'No')))

############################################################
#                                                          #
#                        Save data                         #
#                                                          #
############################################################
if(!dir.exists('data_clean')) {
    dir.create('data_clean')
}

write_csv(x = data,
          file = 'data_clean/data.csv')
