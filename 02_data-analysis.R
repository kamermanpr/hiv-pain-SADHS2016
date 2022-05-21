############################################################
#                                                          #
#                      Data analysis                       #
#                                                          #
############################################################

#--- Load packages ---#
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(survey)
library(parameters)
library(ggplot2)
library(patchwork)

#--- Import and process data ---#
data <- read_csv('data_clean/data.csv') %>% 
    mutate(HIV_status = factor(HIV_status),
           age_category = factor(age_category),
           sex = factor(sex),
           pain_now = factor(pain_now),
           pain_chronic = factor(pain_chronic),
           pain_acute = factor(case_when(
               pain_now == 'Yes' & pain_chronic == 'No' ~ 'Yes',
               TRUE ~ 'No'
           )))

#--- Basic information: HIV status and demographics ---#
# Generate design object
design_HIV <- svydesign(ids = ~V021,
                        strata = ~V022,
                        weights = ~HIV_sample_weight,
                        data = filter(data, !is.na(HIV_sample_weight)))

# Crude numbers
nrow(filter(data, !is.na(HIV_sample_weight)))

# Crude numbers by HIV status
xtabs(~HIV_status, data = filter(data, !is.na(HIV_sample_weight)))

# Get HIV prevalence
hiv_status <- svymean(~HIV_status, design = design_HIV)

hiv_status 

confint(hiv_status)

# Get sex distribution
sex <- svymean(~sex, design = design_HIV)

sex

confint(sex)

# Get mean age 
age <- svyquantile(~age_years, design = design_HIV, 
                   quantiles = 0.5)

age

confint(age)

# Get pain (current)
pain_now <- svymean(~pain_now, design = design_HIV)

pain_now

confint(pain_now)

# Get pain (chronic)
pain_chronic <- svymean(~pain_chronic, design = design_HIV)

pain_chronic

confint(pain_chronic)

# Get pain (acute)
pain_acute <- svymean(~pain_acute, design = design_HIV)

pain_acute

confint(pain_acute)

# Get sex ~ HIV status
hiv_sex <- svyby(~sex,
                 by = ~HIV_status,
                 FUN = svymean,
                 design = design_HIV)

hiv_sex

confint(hiv_sex)

plot(t(svytable(~sex + HIV_status, design = design_HIV)))

model_parameters(svychisq(~sex + HIV_status, design = design_HIV))

# Get age ~ HIV status
hiv_age <- svyby(~age_years,
                 by = ~HIV_status,
                 FUN = svymean,
                 design = design_HIV)

hiv_age

confint(hiv_age)

svyboxplot(age_years ~ HIV_status, design = design_HIV)

model_parameters(svyglm(age_years ~ HIV_status, design = design_HIV))

############################################################
#                                                          #
#                     Primary analysis                     #
#                                                          #
############################################################
#--- Prevalence of pain (now) in HIV ---#
pain_now <- svyby(~pain_now,
                  by = ~HIV_status,
                  FUN = svymean,
                  design = design_HIV) 

pain_now

pain_now_ci <- as.data.frame(confint(pain_now)) %>% 
    rownames_to_column() %>% 
    separate(col = rowname, 
             into = c('hiv_status', 'pain_status'),
             sep = ':') %>% 
    mutate(row = row_number())

pain_now_ci

model_parameters(svyglm(pain_now ~ HIV_status, 
                        family = quasibinomial(),
                        design = design_HIV), 
                 exponentiate = TRUE)

#--- Prevalence of pain now by sex and HIV status ---#
pain_now_sex <- svyby(~pain_now,
                      by = ~HIV_status + sex,
                      FUN = svymean,
                      design = design_HIV) 

pain_now_sex

pain_now_sex_ci <- as.data.frame(confint(pain_now_sex)) %>% 
    rownames_to_column() %>% 
    separate(col = rowname, 
             into = c('hiv_status', 'sex', 'pain_status'),
             sep = c('[.:]')) %>% 
    mutate(row = row_number())

pain_now_sex_ci

model_parameters(svyglm(pain_now ~ HIV_status + sex, 
                        family = quasibinomial(),
                        design = design_HIV), 
                 exponentiate = TRUE)

#--- Prevalence of pain (chronic) in HIV ---#
pain_chronic <- svyby(~pain_chronic,
                      by = ~HIV_status,
                      FUN = svymean,
                      design = design_HIV)

pain_chronic

pain_chronic_ci <- as.data.frame(confint(pain_chronic)) %>% 
    mutate(row = row_number())

pain_chronic_ci

model_parameters(svyglm(pain_chronic ~ HIV_status, 
                        family = quasibinomial(),
                        design = design_HIV), 
                 exponentiate = TRUE)

#--- Prevalence of chronic pain by sex and HIV status ---#
pain_hiv_sex <- svyby(~pain_chronic,
                      by = ~HIV_status + sex,
                      FUN = svymean,
                      design = design_HIV)

pain_hiv_sex

confint(pain_hiv_sex)

model_parameters(svyglm(pain_chronic ~ HIV_status + sex, 
                        family = quasibinomial(),
                        design = design_HIV), 
                 exponentiate = TRUE)

############################################################
#                                                          #
#                         Figures                          #
#                                                          #
############################################################
theme_set(new = theme_bw(base_size = 20) +
              theme(panel.grid = element_blank(),
                    plot.title = element_text(size = 20),
                    plot.subtitle = element_text(size = 16),
                    axis.text = element_text(colour = '#000000')))


#--- Pain now ---#
pain_now_df <- data.frame(group = c('Pain (acute or chronic)', 
                                    'Pain (acute or chronic)'),
                          status = c('HIV+', 'HIV-'),
                          estimate = c(100 * pain_now$pain_nowYes[[2]],
                                       100 * pain_now$pain_nowYes[[1]]),
                          CI_lower = c(100 * pain_now_ci$`2.5 %`[[5]],
                                       100 * pain_now_ci$`2.5 %`[[4]]),
                          CI_upper = c(100 * pain_now_ci$`97.5 %`[[5]],
                                       100 * pain_now_ci$`97.5 %`[[4]]))

pain_now_df

ggplot(data = pain_now_df) +
    aes(x = status,
        y = estimate,
        ymin = CI_lower,
        ymax = CI_upper) +
    geom_errorbar(width = 0.3) +
    geom_point(size = 7) +
    labs(title = 'Currently has pain (acute or chronic)',
         x = NULL,
         y = 'Prevalence (%)') +
    scale_y_continuous(limits = c(15, 35)) +
    scale_x_discrete(labels = c('HIV-negative', 'HIV-positive'))

#--- Chronic pain ---#
pain_chronic_df <- data.frame(group = c('Pain (chronic only)', 
                                        'Pain (chronic only)'),
                              status = c('HIV+', 'HIV-'),
                              estimate = c(100 * pain_chronic$pain_chronicYes[[2]],
                                           100 * pain_chronic$pain_chronicYes[[1]]),
                              CI_lower = c(100 * pain_chronic_ci$`2.5 %`[[5]],
                                           100 * pain_chronic_ci$`2.5 %`[[4]]),
                              CI_upper = c(100 * pain_chronic_ci$`97.5 %`[[5]],
                                           100 * pain_chronic_ci$`97.5 %`[[4]]))

pain_chronic_df

ggplot(data = pain_chronic_df) +
    aes(x = status,
        y = estimate,
        ymin = CI_lower,
        ymax = CI_upper) +
    geom_errorbar(width = 0.3) +
    geom_point(size = 7) +
    labs(title = 'Currently has pain (chronic only)',
         x = NULL,
         y = 'Prevalence (%)') +
    scale_y_continuous(limits = c(15, 25)) +
    scale_x_discrete(labels = c('HIV-negative', 'HIV-positive'))

#--- Combined plot ---#
pain_combined_df <- bind_rows(pain_now_df, pain_chronic_df) %>% 
    mutate(group = relevel(factor(group), ref = 'Pain (acute or chronic)'))

pain_combined_df

p_combined <- ggplot(data = pain_combined_df) +
    aes(x = status,
        y = estimate,
        ymin = CI_lower,
        ymax = CI_upper) +
    geom_errorbar(width = 0.3) +
    geom_point(size = 7) +
    labs(title = NULL,
         x = NULL,
         y = 'Prevalence (%)') +
    scale_y_continuous(limits = c(15, 35)) +
    scale_x_discrete(labels = c('HIV-negative', 'HIV-positive')) +
    facet_wrap(~group, ncol = 2)

ggsave(filename = 'figure-1.png',
       plot = p_combined,
       height = 5,
       width = 10)
