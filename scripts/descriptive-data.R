library(tidyverse)
library(psych)
library(stargazer)


# Prepare data ------------------------------------------------------------

full_sample <- data

earth_day_sample <- full_sample %>% filter(started < '2021-04-22')
low_ocsi_sample <- full_sample %>% filter(quiz_score < 5)
samples <- list(full_sample, earth_day_sample, low_ocsi_sample)


# Descriptive Data  -------------------------------------------------------

socio_full <- full_sample %>% select(c('age', 'quiz_score', 'treatment_group', 'gender', 'education', 'income'))
socio_earth_day <- earth_day_sample %>% select(c('age', 'quiz_score', 'treatment_group', 'gender', 'education', 'income'))
socio_low_ocsi <- low_ocsi_sample %>% select(c('age', 'quiz_score', 'treatment_group', 'gender', 'education', 'income'))

#Gender 
gender_full <- table(socio_full$gender)
gender_day <- table(socio_earth_day$gender)
gender_ocsi <- table(socio_low_ocsi$gender)
gender <- list(gender_full, gender_day, gender_ocsi)
gender_table <- bind_cols(gender)

#Education 
edu_full <- table(socio_full$education)
edu_day <- table(socio_earth_day$education)
edu_ocsi <- table(socio_low_ocsi$education)

edu <- list(edu_full, edu_day, edu_ocsi)
edu_table <- bind_cols(edu)


#Income 
inc_full <- table(socio_full$income)
inc_day <- table(socio_earth_day$income)
inc_ocsi <- table(socio_low_ocsi$income)

income <- list(inc_full, inc_day, inc_ocsi)
income_table <- bind_cols(income)

sample_income_eduation <- bind_rows(list(edu_table, income_table))


quantile(as.double(full_sample$income), na.rm=TRUE)

residence_countries <- as.data.frame(table(full_sample$residence_country))

stargazer(residence_countries, 
          title = "Respondents' Countries of Residence",
          label = "tab:respondent-countries",
          header = FALSE,
          table.placement = c('!htpb'),
          summary = FALSE,
          out = './tables/respondent-countries.tex')


groups_full <- describeBy(socio_full, group = full_sample$treatment_group, mat=TRUE)

groups_full_out <- groups_full %>%
                    select(c(n, mean, sd)) %>%
                    round(digits=2) 

groups_earth <- describeBy(socio_earth_day, group = socio_earth_day$treatment_group, mat=TRUE)

groups_earth_out <- groups_earth %>% select(c(n, mean, sd)) %>%
                                    round(digits=2) 
groups_ocsi <- describeBy(socio_low_ocsi, group = socio_low_ocsi$treatment_group, mat=TRUE)

groups_ocsi_out <- groups_ocsi %>% select(c(n, mean, sd)) %>%
    round(digits=2) 

