###############################################################
### Figure S3 and statistical comparisons of leaf traits 
###############################################################

#Library 
library(car)
library(lme4)
library(nlme)
library(sjPlot)
library(sjmisc)
library(stargazer)
library(data.table)
library(ggplot2)

#Datasets
data <- fread("/home/antguz/Documents/PLSR-models/Data/01-Master data/01-traits.csv")
data[Life_form == "Tree", Life_form := "Trees"]
data[Life_form == "Liana", Life_form := "Lianas"]
data$Life_form <- as.factor(data$Life_form)
data$Life_form <- factor(data$Life_form, levels = c("Lianas", "Trees"))

###Comparisons between life forms
LMA_lmer <- lmer(log10(LMA) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(LMA_lmer))
qqline(resid(LMA_lmer))
summary(LMA_lmer)
tab_model(LMA_lmer, p.val = "kr", show.df = TRUE)

WC_lmer <- lmer(log10(WC) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(WC_lmer))
qqline(resid(WC_lmer))
summary(WC_lmer)
tab_model(WC_lmer, p.val = "kr", show.df = TRUE)

EWT_lmer <- lmer(log10(EWT) ~ Life_form + (1|Acronym), data = data)
qqnorm(resid(EWT_lmer))
qqline(resid(EWT_lmer))
summary(EWT_lmer)
tab_model(EWT_lmer, p.val = "kr", show.df = TRUE)

LMA <- as.data.frame(ranef(LMA_lmer))
LMA$trait <- "LMA"
WC <- as.data.frame(ranef(WC_lmer))
WC$trait <- "WC"
EWT <- as.data.frame(ranef(EWT_lmer))
EWT$trait <- "EWT"

data_model <- rbind(LMA, WC, EWT)
colnames(data_model)[3] <- "Species"

data_model <- merge(data_model, data[,c(1,4)], by.x = "Species", by.y = "Acronym", all.x = TRUE, all.y = FALSE)

order <- c("S. obovata", "C. alata", "B. simarouba", "C. americana", "J. curcas", "S. glandulosum", "B. ungulata", "H. courbaril", "G. sepium", "Q. oleoides", "S. mexicanum", "O. veraguensis", "B. crassifolia", "G. ulmifolia", "C. odorata", "T. americana", "P. aculeata", "C. vitifolium", "S. glauca", "L. speciosa", "R. trinervis", "Forsteronia sp.", "F. spicata", "A. chica", "C. aequinoctialis", "C. diversifolia", "Paulinia sp.", "C. racemosa", "T. volubilis", "H. panamensis", "Heteropterys sp.", "H. reclinata", "G. polygama", "S. atrolineata", "S. schiedeana")

data_model$trait <- as.factor(data_model$trait)
data_model$trait <- factor(data_model$trait, levels = c("LMA", "WC", "EWT"))
data_model$Species <- as.factor(data_model$Species)
data_model$Species <- factor(data_model$Species, levels = order)

pa <- c("#e66101", "#5e3c99")

plot <- ggplot(data_model, aes(x = Species, y= condval, fill = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = 'grey', size = 0.3) +
  geom_errorbar(aes(ymin= condval-condsd, ymax= condval+condsd), colour = '#08306B', width= 0.1) +
  geom_point(size = 2, shape = 21, colour = "white") +
  scale_fill_manual("Life form" , values = pa) +
  scale_y_continuous(limits = c(-0.45, 0.45), expand=c(0,0)) +
  coord_flip() +
  theme_bw(base_size = 14) +
  ylab("Conditional variance-covariance") +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(face = "italic")) +
  facet_grid(. ~ trait, labeller = label_parsed)

  
tiff("Figure_S3.tif", width = 19, height = 15, units = "cm", res = 600)

plot

dev.off()

