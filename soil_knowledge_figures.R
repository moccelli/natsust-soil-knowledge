## Smallholder households' farming knowledge affects soil ability in marginal areas 
# Occelli, Mantino, Ragaglini, Dell'Acqua, Pé and Nuvolari

# Load libraries
library(ggplot2)
library(tidyverse)

# Get your working directory

# Load data
data <- read_excel("Ability_knowledge_master.xlsx", 
                   col_types = c("numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
data <- na.omit(data)
View(data)

# Figure 2 of the paper

# Graphic on the distribution of soil residuals

data %>% mutate(
  region = factor(region,
                  levels=c("1","2"),
                  labels=c("Amhara", "Tigray") 
  )
) %>% ggplot(aes(factor(q_tot_npk), fill=region)) +
  geom_bar(aes(y = (..count..)/273),position="dodge", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("Amhara"="#b2182b" , "Tigray"="#2166ac")) +
  theme_classic() +
  xlab("Soil Ability") + 
  ylab("Farmers (%)") +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(fill="Region of the study") -> s1

# Soil ability for all villages in Amhara
data_amhara %>% mutate(village = factor(village,
                                        levels=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                        labels=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12") 
))  %>% ggplot(aes(x=village, y= q_tot_npk))+
  geom_boxplot(fill=NA, alpha=0.5) + 
  geom_jitter(aes(colour=village), width=0.25, alpha=1, show.legend = F) + 
  #position = position_jitter(width = 0.3)) +
  geom_hline(yintercept=7.5, linetype="dashed") + 
  scale_colour_manual(values = c("1" = "#1b9e77","2" = "#ef8a62","3" = "#7570b3", 
                                 "4"="#e34a33", "5"="#67a9cf", "6"="#2166ac", 
                                 "7"="#762a83", "8"="#af8dc3", "9"="#8c510a", "10"="#d8b365", 
                                 "11"="#01665e", "12"="#5ab4ac")) +
  labs(y = "Soil Ability", x="Villages in Amhara") +
  theme_classic() -> a1

# Soil ability for all villages in Tigray
data_tigray %>% mutate(village = factor(village,
                                        levels=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                        labels=c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12") 
))  %>% ggplot(aes(x=village, y= q_tot_npk))+
  geom_boxplot(fill=NA, alpha=0.5) + 
  geom_jitter(aes(colour=village), width=0.25, alpha=1, show.legend = F) + 
  #position = position_jitter(width = 0.3)) +
  geom_hline(yintercept=7.5, linetype="dashed") + 
  scale_colour_manual(values = c("1" = "#1b9e77","2" = "#ef8a62","3" = "#7570b3", 
                                 "4"="#e34a33", "5"="#67a9cf", "6"="#2166ac", 
                                 "7"="#762a83", "8"="#af8dc3", "9"="#8c510a", "10"="#d8b365", 
                                 "11"="#01665e", "12"="#5ab4ac")) +
  labs(y = "Soil Ability", x="Villages in Tigray") +
  theme_classic() -> t1

# Merging of the three precedent plots

cowplot::plot_grid(s1, a1,t1, nrow = 1, labels = c('A','B','C')) -> final
ggsave(file = "final.pdf",final, width = 11.69, height = 4, dpi = 300, units = "in" )

# Figure 3 of the paper

# Graphic on relation soil ability and on-farm resources
data %>% mutate(
  region = factor(region,
                  levels=c("1","2"),
                  labels=c("Amhara", "Tigray") 
  )
) %>% ggplot(aes(x=factor(q_tot_npk), y=kcal_prod, fill=region)) +
  geom_boxplot() +
  #coord_cartesian(xlim=quantile(data$kcal_prod), c(10,40))+
  scale_fill_manual(values = c("Amhara"="#b2182b" , "Tigray"="#2166ac")) +
  theme_classic() +
  scale_y_continuous(limits = c(-1, 60000))+
  xlab("Soil Ability") + 
  ylab("kcal/ha produced") +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  labs(fill="Region of the study") +
  ggsave(file = "soil_income.pdf", width = 9.35, height = 6.61, dpi = 300, units = "in" )

# Figures in the supplementary material
# Fig. SF1a 
data %>% mutate(
  region = factor(region,
                  levels=c("1","2"),
                  labels=c("Amhara", "Tigray") 
  )
) %>% ggplot(aes(x=factor(q_tot_npk), y=yield_revenue, fill=region)) +
  geom_boxplot() +
  #coord_cartesian(xlim=quantile(data$kcal_prod), c(10,40))+
  scale_fill_manual(values = c("Amhara"="#b2182b" , "Tigray"="#2166ac")) +
  theme_classic() +
  scale_y_continuous(limits = c(-1, 40000))+
  xlab("Soil Ability") + 
  ylab("ETB/ha produced") +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  labs(fill="Region of the study") +
  ggsave(file = "SF1a.pdf", width = 9.35, height = 6.61, dpi = 300, units = "in" )

# Fig. SF1b
data %>% mutate(
  region = factor(region,
                  levels=c("1","2"),
                  labels=c("Amhara", "Tigray") 
  )
) %>% ggplot(aes(x=factor(q_tot_npk), y=prot_produced, fill=region)) +
  geom_boxplot() +
  #coord_cartesian(xlim=quantile(data$kcal_prod), c(10,40))+
  scale_fill_manual(values = c("Amhara"="#b2182b" , "Tigray"="#2166ac")) +
  theme_classic() +
  scale_y_continuous(limits = c(-1, 500))+
  xlab("Soil Ability") + 
  ylab("prot (% DM) /ha produced") +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  labs(fill="Region of the study") +
  ggsave(file = "SF1b.pdf", width = 9.35, height = 6.61, dpi = 300, units = "in" )
