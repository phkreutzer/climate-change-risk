library(tidyverse)
library(tikzDevice)

rm(list=ls()[! ls() %in% c("data", "summarySE")])

graphing_data <- data %>% pivot_longer(cols = c('risk_self', 'risk_other', 'risk_total'),
                                       names_to = "risk_level",
                                       values_to = "risk_level_value")

graphing_data$risk_level <-factor(graphing_data$risk_level, labels = c("For Others", "For Self", "Combined"))

full_sample <- graphing_data
earth_day_sample <- full_sample %>% filter(started < '2021-04-22')

low_ocsi_sample <- full_sample %>% filter(ocsi_score < 5)

samples = list(full_sample, earth_day_sample, low_ocsi_sample)
names(samples) = c("full_sample", "earth_day", "ocsi")

# Graphing ----------------------------------------------------------------
lund_colors = c("#b8d8dc", "#fec5cb", "#b6e3b3", "#d5c4a7")

theme_thesis <- function(base_size = 10)
{
    theme_classic(base_size = base_size) +
        theme(
            legend.key = element_blank(), 
            strip.background = element_blank(), 
            
            text = element_text(),
            strip.text.x = element_text(size=rel(1.1)),
            strip.text.y = element_text(size=rel(1.1)),
            
            legend.title = element_text(size=rel(0.8))
        ) 
}

theme_set(theme_thesis())


# Main Plots --------------------------------------------------------------


# Risk Perception Boxplots

risk_perception_boxplot = list()

for (name in names(samples)){
    p = ggplot(samples[[name]],
               aes(x = factor(treatment_group),
                   y = risk_level_value,
                   fill = treatment_group)) +
        geom_boxplot() +
        facet_wrap(~risk_level) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              strip.background = element_blank()) +
        ylab("Perceived Risk Level") +
        scale_fill_manual(values = lund_colors, name = "Treatment Group")
    
    risk_perception_boxplot[[name]] = p
    
}

# Save plots to tex makes a separate file for each plot.
for (name in names(risk_perception_boxplot)) {
    file_name = paste("./figures/risk_perception_boxplot_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(risk_perception_boxplot[[name]])
    dev.off()
}

# Risk Percetion x OCSI Score

risk_perception_ocsi = list()

for (name in names(samples)){
    p = ggplot(samples[[name]], aes(x = ocsi_score, y = risk_level_value)) +
        geom_jitter(alpha=0.3, color='#b8d8dc', width=0.3) +
        geom_smooth(method = 'lm', se=FALSE, color='#2c2962') +
        stat_smooth(method = "lm", colour = '#38357e', geom = "ribbon", linetype=2, 
                    show.legend=TRUE, fullrange = TRUE, fill = NA) +
        facet_wrap(~risk_level) +
        ylab("Perceived Risk Level") +
        xlab("OCSI Score") 
    risk_perception_ocsi[[name]] = p
    
}

# Save plots to tex makes a separate file for each plot.
for (name in names(risk_perception_ocsi)) {
    file_name = paste("./figures/risk_ocsi_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(risk_perception_ocsi[[name]])
    dev.off()
}


# Lottery Donation Hist
lottery_donation_hist = list()


for (name in names(samples)){
    p = ggplot(samples[[name]], aes(x = factor(lottery_donation))) +
        geom_bar(stat = 'count', fill="#b8d8dc")+
        lemon::facet_rep_wrap(~treatment_group, ncol=2, repeat.tick.labels = TRUE)+
        xlab("Lottery Donation")+
        ylab("Count")
    lottery_donation_hist[[name]] = p
    
}

# Save plots to tex makes a separate file for each plot.
for (name in names(lottery_donation_hist)) {
    file_name = paste("./figures/lottery_donation_hist_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(lottery_donation_hist[[name]])
    dev.off()
}

# Mean Plots --------------------------------------------------------------
rm(list=ls()[! ls() %in% c("data", "summarySE", "lund_colors", "theme_thesis")])

pd <- position_dodge(0.1) # move them .05 to the left and right

# Prepare data to match the analysis  data 

full_sample <- data 
earth_day_sample <- full_sample %>% filter(started < '2021-04-22')
earth_day_sample
quantile(full_sample$ocsi_score)

low_ocsi_sample <- full_sample %>% filter(ocsi_score < 5)

samples <- list(full_sample, earth_day_sample, low_ocsi_sample)
names(samples) <-  c("full_sample", "earth_day", "ocsi")


# Create new plotting data frames with SE for each risk perception dimension

risk_other_se <- lapply(samples, 
                        function(sample) summarySE(sample, measurevar="risk_other", 
                                                   groupvars=c("treatment_doom", "treatment_inevitable"))
)

risk_self_se <- lapply(samples, 
                       function(sample) summarySE(sample, measurevar="risk_self", 
                                                  groupvars=c("treatment_doom", "treatment_inevitable"))
)


risk_total_se <- lapply(samples, 
                        function(sample) summarySE(sample, measurevar="risk_total", 
                                                   groupvars=c("treatment_doom", "treatment_inevitable"))
)

# Mean Plots Risk Others

mean_plot_others_list = list()
names(risk_other_se) <- c("full_sample", "earth_day", "ocsi")

for (name in names(risk_other_se)){
    p = ggplot(risk_other_se[[name]], aes(x=treatment_doom, y=risk_other, 
                           color=factor(treatment_inevitable))) + 
        geom_errorbar(aes(ymin=risk_other-se, ymax=risk_other+se), width=.1, position=pd) +
        geom_line(position=pd, size=1) +
        geom_point(position=pd, size=4) +
        ylab("Perceived Risk for Others") +
        xlab("Treatment Condition") +
        scale_x_continuous(n.breaks = 2,labels=c("Gloom", "Doom") ) +
        scale_color_manual(values = lund_colors, 
                           name="Treatment Condition", 
                           labels=c("Evitable", "Inevitable")
        ) 
    mean_plot_others_list[[name]] = p
    
}
    
# Save plots to tex makes a separate file for each plot.
for (name in names(mean_plot_others_list)) {
    file_name = paste("./figures/risk_other_mean_plot_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(mean_plot_others_list[[name]])
    dev.off()
    }


# Mean Plots Risk Self

mean_plot_self_list = list()
names(risk_self_se) <-  c("full_sample", "earth_day", "ocsi")

for (name in names(risk_self_se)){
    p = ggplot(risk_self_se[[name]], aes(x=treatment_doom, y=risk_self, 
                                          color=factor(treatment_inevitable))) + 
        geom_errorbar(aes(ymin=risk_self-se, ymax=risk_self+se), width=.1, position=pd) +
        geom_line(position=pd, size=1) +
        geom_point(position=pd, size=4) +
        ylab("Perceived Risk for Self") +
        xlab("Treatment Condition") +
        scale_x_continuous(n.breaks = 2,labels=c("Gloom", "Doom") ) +
        scale_color_manual(values = lund_colors, 
                           name="Treatment Condition", 
                           labels=c("Evitable", "Inevitable")
        ) 
    mean_plot_self_list[[name]] = p
    
}

# Save plots to tex makes a separate file for each plot.
for (name in names(mean_plot_self_list)) {
    file_name = paste("./figures/risk_self_mean_plot_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(mean_plot_self_list[[name]])
    dev.off()
}

# Mean Plots Risk Total

mean_plot_total_list = list()
names(risk_total_se) <- c("full_sample", "earth_day", "ocsi")


for (name in names(risk_total_se)){
    p = ggplot(risk_total_se[[name]], aes(x=treatment_doom, y=risk_total, 
                                         color=factor(treatment_inevitable))) + 
        geom_errorbar(aes(ymin=risk_total-se, ymax=risk_total+se), width=.1, position=pd) +
        geom_line(position=pd, size=1) +
        geom_point(position=pd, size=4) +
        ylab("Perceived Risk for Total") +
        xlab("Treatment Condition") +
        scale_x_continuous(n.breaks = 2,labels=c("Gloom", "Doom") ) +
        scale_color_manual(values = lund_colors, 
                           name="Treatment \\\ Condition", 
                           labels=c("Evitable", "Inevitable")
        ) 
    mean_plot_total_list[[name]] = p
    
}

# Save plots to tex makes a separate file for each plot.
for (name in names(mean_plot_total_list)) {
    file_name = paste("./figures/risk_total_mean_plot_", name, ".tex", sep="")
    tikz(file = file_name,
         width = 5,
         height = 3)
    print(mean_plot_total_list[[name]])
    dev.off()
}
