library(tidyverse)
library(broom)
library(psych)
library(car)
library(stargazer)
library(sandwich)
library(tikzDevice)

# Prepare data ------------------------------------------------------------
full_sample <- data 
earth_day_sample <- full_sample %>% filter(started < '2021-04-22')


quantile(full_sample$quiz_score)
low_ocsi_sample <- full_sample %>% filter(quiz_score < 5)

samples <- list(full_sample, earth_day_sample, low_ocsi_sample)
names(samples) <-  c("full_sample",  "earth_day", "ocsi")


# Group Means Risk -------------------------------------------------------------

#Total perceived risk
risk_total_means <- lapply(samples,
                           function(sample) group_by(sample, treatment_group)%>%
                                   dplyr::summarise(
                                           count = n(),
                                           mean = round(mean(risk_total, na.rm = TRUE), 2),
                                           sd = round(sd(risk_total, na.rm = TRUE),2)
                                   )
)

risk_total_means_out <- as.data.frame(risk_total_means)
#Risk for self
risk_self_means <- lapply(samples,
                          function(sample) group_by(sample, treatment_group)%>%
                                  dplyr::summarise(
                                          count = n(),
                                          mean = round(mean(risk_self, na.rm = TRUE), 2),
                                          sd = round(sd(risk_self, na.rm = TRUE),2)
                                  )
)

risk_self_means_out <- as.data.frame(risk_self_means)

#Risk other
risk_other_means <- lapply(samples,
                           function(sample) group_by(sample, treatment_group)%>%
                                   dplyr::summarise(
                                           count = n(),
                                           mean = round(mean(risk_other, na.rm = TRUE), 2),
                                           sd = round(sd(risk_other, na.rm = TRUE),2)
                                   )
)

risk_other_means_out <- as.data.frame(risk_other_means)

risk_means_out <- rbind(risk_total_means_out, risk_other_means_out, risk_self_means_out)
risk_means_out <-  subset(risk_means_out, select = -c(5,9) )

stargazer(risk_means_out,
          label = "tab:risk_means_comparison",
          summary = FALSE,
          rownames = FALSE,
          header = FALSE,
          out = './tables/group_means_risk.tex')

# Differences between risk perception dimensions for each sample ----------

# Paired t-Test Risk Self vs Risk Others
lapply(samples,
       function(sample) t.test(sample$risk_other, sample$risk_self,
                               paired = TRUE))

# Print SD for text
lapply(samples,
       function(sample) round(sd(sample$risk_other), 3))


lapply(samples,
       function(sample) round(sd(sample$risk_self), 3))

# Risk Total ANOVA

aovs_risk_total <- lapply(samples, 
                        function(sample) aov(risk_total ~ factor(treatment_doom) * factor(treatment_inevitable), 
                                             data = sample)
                        )


aovs_risk_total_corrected <-  lapply(aovs_risk_total,
                           function(aov) Anova(aov, type='II'))

# Prepares correct row labels to ANOVA output
lookup  <- c("factor(treatment_doom)" = "Doom",
             "(Intercept)" = "(Intercept)",
             "factor(treatment_inevitable)" = "Inevitable",
             "factor(treatment_doom):factor(treatment_inevitable)" = "Doom $\\times$ Inevitable",
             "Residuals" = "Residuals")


aovs_risk_out <- map_df(aovs_risk_total_corrected, tidy) %>%
        mutate_if(is.numeric, ~round(., 3)) %>%
        rename("Sum Sq" = "sumsq", "$F$ Statistic" = "statistic", "p-value"="p.value") 

aovs_risk_out$`p-value` <- as.character(aovs_risk_out$`p-value`)
aovs_risk_out$`p-value` <- sub("^0+", "", aovs_risk_out$`p-value`) 
aovs_risk_out$term <- as.character(lookup[aovs_risk_out$term])


stargazer(aovs_risk_out, 
          title = "ANOVA of Total Perceived Risk by Treatment Condition",
          label = "tab:total_risk_anova_treatment",
          object.names = TRUE,
          rownames = FALSE,
          notes = "Unequal group sizes adjusted using type 2 correction.",
          header = FALSE,
          summary = FALSE,
          out = "./tables/anova-risk-type-II.tex")

#ANOVA Assumptions 

# Plots 

for (name in names(aovs_risk_total)) {
        file_name = paste("./figures/residual_plot_", name, ".tex", sep="")
        tikz(file = file_name,
             width = 5,
             height = 3)
        par(mfrow=c(1,2)) # Change the panel layout to 2 x 2
        plot(aovs_risk_total[[name]],1)
        plot(aovs_risk_total[[name]], 2)
        dev.off()
}


# MANOVA Risk for Self and Others

#fit the MANOVA model
manova_model <- "cbind(risk_self, risk_other, risk_total) ~ treatment_doom * treatment_inevitable"

models <- lapply(samples,
                 function(sample) manova(as.formula(manova_model), data = sample))

#view the results
lapply(models, 
       function(model) summary(model, ))

lapply(models, 
       function(model) summary.aov(model, ))

manova_type_II <- lapply(models,
                   function(model) Anova(model, type='II'))

lapply(manova_type_II, function(m) summary(m))
# Directed t-Tests  Risks by Treatment Condition

t_test_directed_total_risk <- lapply(samples,
                                     function(sample) t.test(risk_total ~ treatment_doom, data = sample,
                                                             alternative='less'))

t_test_directed_self_risk <- lapply(samples,
                                    function(sample) t.test(risk_self ~ treatment_doom, data = sample,
                                                            alternative='less'))

t_test_directed_other_risk <- lapply(samples,
                                     function(sample) t.test(risk_other ~ treatment_doom, data = sample,
                                                             alternative='less'))


t_test_directed_total_risk
t_test_directed_self_risk
t_test_directed_other_risk

# Risk Perception OLS -----------------------------------------------------

risk_types <-  list("risk_self",
                    "risk_other",
                    "risk_total")

rhs <- "~ factor(treatment_doom)*factor(treatment_inevitable) + quiz_score + as.double(education) + as.double(income) + gender + age"

risk_fits_full_sample <- lapply(risk_types,
                                function(type) lm(paste0(type, rhs), 
                                                  data = full_sample))


risk_fits_earth_day_sample <- lapply(risk_types,
                                     function(type) lm(paste0(type, rhs),
                                                       data = earth_day_sample))



risk_fits_low_ocsi_sample <- lapply(risk_types,
                                    function(type) lm(paste0(type, rhs),
                                                      data = low_ocsi_sample))


risk_perception_models <- list(risk_fits_full_sample, risk_fits_earth_day_sample, risk_fits_low_ocsi_sample) 


stargazer(risk_perception_models,
          title = "Analysis of Risk Perception Factors",
          label = "tab:risk_perception_ols",
          dep.var.caption = c("Perception of Risk for"),
          dep.var.labels.include = FALSE,
          add.lines = "Treatment Condition:",
          column.labels = c("Self",
                            "Other",
                            "Total",
                            "Self",
                            "Other",
                            "Total",
                            "Self",
                            "Other",
                            "Total"),
          covariate.labels = c("\\hspace{1em}Doom",
                               "\\hspace{1em}Inevitable",
                               "\\hspace{1em}Inevitable $\\times$ Doom",
                               "OCSI Score",
                               "Education",
                               "Income",
                               "Gender : Male",
                               "Gender : Other",
                               "Age"),
          
          align=TRUE, 
          no.space=TRUE,
          out.header = FALSE,
          order = c(1,2,9,3,4,5,6,7,8),
          out = './tables/risk-perceptions-ols.tex')




# Lottery Donation --------------------------------------------------------

# Tickets donated
lottery_donation_means <- lapply(samples,
                           function(sample) group_by(sample, treatment_group)%>%
                               dplyr::summarise(
                                   count = n(),
                                   mean = round(mean(lottery_donation, na.rm = TRUE), 2),
                                   sd = round(sd(lottery_donation, na.rm = TRUE),2)
                               )
)

lottery_donation_means <- as.data.frame(lottery_donation_means)
lottery_donation_means <-  subset(lottery_donation_means, select = -c(5,9) )

stargazer(lottery_donation_means,
          label="tab:lottery_means_comparison",
          summary=FALSE,
          header=FALSE,
          out = './tables/group_means_lottery.tex')

# ANOVA of Treatment Condidition

aovs_lottery <- lapply(samples, 
                       function(sample) aov(lottery_donation ~ factor(treatment_doom) * factor(treatment_inevitable), 
                                            data = sample)
)


aovs_lottery <-  lapply(aovs_lottery,
                        function(aov) Anova(aov, type='II'))

aovs_lottery_out <- map_df(aovs_lottery, tidy) %>%
        mutate_if(is.numeric, ~round(., 3)) %>%
        rename("Sum Sq" = "sumsq", "$F$ Statistic" = "statistic", "p-value"="p.value")

aovs_lottery_out$term <- as.character(lookup[aovs_lottery_out$term])
aovs_lottery_out$`p-value` <- as.character(aovs_lottery_out$`p-value`)
aovs_lottery_out$`p-value` <- sub("^0+", "", aovs_lottery_out$`p-value`) 
aovs_lottery_out$term <- as.character(lookup[aovs_lottery_out$term])





stargazer(aovs_lottery_out, 
          title = "ANOVA of Donation Decision by Treatment Condition",
          label = "tab:lottery_donation_anova_treatment",
          notes = "Unequal group sizes adjusted using type 2 correction.",
          object.names = TRUE,
          rownames = FALSE,
          header = FALSE,
          summary = FALSE,
          out='./tables/lottery-donation-anova-type-II.tex')

# Plots 

for (name in names(aovs_lottery)) {
        file_name = paste("./figures/residual_plot_anova_lottery_", name, ".tex", sep="")
        tikz(file = file_name,
             width = 5,
             height = 3)
        par(mfrow=c(1,2)) # Change the panel layout to 2 x 2
        plot(aovs_risk_total[[name]],1)
        plot(aovs_risk_total[[name]], 2)
        dev.off()
}


# Directed t Test for Treatment Condition of Lottery Donation
t_test_directed_lottery_inevitable <- lapply(samples,
                                             function(sample) t.test(lottery_donation ~ treatment_inevitable, data=sample,
                                                                     alternative='greater'))
t_test_directed_lottery_doom <- lapply(samples,
                                       function(sample) t.test(lottery_donation ~ treatment_doom, data=sample,
                                                               alternative='greater'))
t_test_directed_lottery_inevitable
t_test_directed_lottery_doom

# OLS Analysis

model_lottery_risk_total <- 'lottery_donation ~ risk_total + 
                                     trust + quiz_score + as.double(education) + 
                                     as.double(income) + gender + age'

model_lottery_risk_types <- 'lottery_donation ~  risk_self + risk_other +
                                     trust + quiz_score + as.double(education) + 
                                     as.double(income) + gender + age'

lottery_donation_risk_total <- lapply(samples,
                                function(sample) lm(model_lottery_risk_total, data = sample))


lottery_donation_risk_types <- lapply(samples,
                                function(sample) lm(model_lottery_risk_types, data = sample))


lottery_donations <- list(lottery_donation_risk_total, lottery_donation_risk_types)

lottery_full_sample <- lapply(lottery_donations, `[[`, 'full_sample')  
lottery_earth_day <- lapply(lottery_donations, `[[`, 'earth_day')  
lottery_low_ocsi <- lapply(lottery_donations, `[[`, 'ocsi')  
lottery_out <- list(lottery_full_sample, lottery_earth_day, lottery_low_ocsi)

stargazer(lottery_out,
          title = "Moderation Analysis of Donation Decision",
          label = "tab:lottery_donation_ols",
          column.labels = c("Full Sample", "Before Earth Day", "Low OCSI Score"),
          column.separate = c(2, 2, 2),  
          dep.var.labels = "Amount of Tickets Donated to Charity",
          covariate.labels = c("Total Perceived Risk",
                               "Perceived Risk for Self",
                               "Perceived Risk for Others",
                               "Social Trust",
                               "OCSI Score",
                               "Education",
                               "Income",
                               "Gender : Male",
                               "Gender : Other",
                               "Age"),
          align=TRUE, 
          no.space=TRUE,
          omit.stat = "ser",
          out.header = FALSE,
          out = './tables/lottery-donation-ols.tex')
