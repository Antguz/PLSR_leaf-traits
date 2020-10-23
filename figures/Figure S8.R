###############################################################
###Figure S8
###############################################################

#Note: inputs come from code_base.R

#Library
library(data.table)
library(ggpubr)
library(ggplot2)

#Load and prepare data
#Traits
traits <- fread("traits_testing.csv")
colnames(traits)[3:5] <- c("LMA", "WC", "EWT")
traits$LMA <- 10^traits$LMA
traits$WC <- 10^traits$WC
traits$EWT <- 10^traits$EWT

#Residuals reflectance
ref_LMA <- fread("ref_LMA_residuals_test.csv")
ref_WC <- fread("ref_WC_residuals_test.csv")
ref_EWT <- fread("ref_EWT_residuals_test.csv")

cwt_LMA <- fread("cwt_LMA_residuals_test.csv")
cwt_WC <- fread("cwt_WC_residuals_test.csv")
cwt_EWT <- fread("cwt_EWT_residuals_test.csv")


summary_data <- function(x) {
  y <- x
  mean <- mean(y)
  sd_lower <- mean - sd(y)
  sd_upper <- mean + sd(y)
  return(c(sd_upper = sd_upper, mean = mean, sd_lower = sd_lower))
}

ref_LMA_sum <- as.data.table(t(apply(ref_LMA, 1, summary_data)))
ref_WC_sum <- as.data.table(t(apply(ref_WC, 1, summary_data)))
ref_EWT_sum <- as.data.table(t(apply(ref_EWT, 1, summary_data)))
cwt_LMA_sum <- as.data.table(t(apply(cwt_LMA, 1, summary_data)))
cwt_WC_sum <- as.data.table(t(apply(cwt_WC, 1, summary_data)))
cwt_EWT_sum <- as.data.table(t(apply(cwt_EWT, 1, summary_data)))

ref_LMA <- cbind(traits[,c(1,3)], ref_LMA_sum)
ref_WC <- cbind(traits[,c(1,4)], ref_WC_sum)
ref_EWT <- cbind(traits[,c(1,5)], ref_EWT_sum) 
cwt_LMA <- cbind(traits[,c(1,3)], cwt_LMA_sum)
cwt_WC <- cbind(traits[,c(1,4)], cwt_WC_sum)
cwt_EWT <- cbind(traits[,c(1,5)], cwt_EWT_sum)

ref_LMA$Trait <- "LMA"
ref_WC$Trait <- "WC"
ref_EWT$Trait <- "EWT"
cwt_LMA$Trait <- "LMA"
cwt_WC$Trait <- "WC"
cwt_EWT$Trait <- "EWT"


pa <- c("#33B09F", "#B66A34")
tamano <- 12
tamano2 <- 10

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", fill="grey90", linetype="solid"))

th2 <- theme_bw(base_size = 11) + theme(plot.background = element_blank(),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        plot.margin = margin(4, 10, 1, 1, "pt"),
                                        axis.text.x = element_text(color = "black", size = tamano2),
                                        axis.text.y = element_text(color = "black", size = tamano2),
                                        strip.text.x = element_text(size = 12, color = "black"),
                                        strip.text.y = element_text(size = 12, color = "black"),
                                        strip.background = element_rect(color= "black", linetype="solid"))

LMA_range <- c(10, 200)
WC_range <- c(40, 90)
EWT_range <- c(35, 290)
size_point <- 1.5

###Plots

hist_a <- ggplot(ref_LMA, aes(x= LMA, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

hist_b <- ggplot(ref_WC, aes(x= WC, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

hist_c <- ggplot(ref_EWT, aes(x= EWT, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))



ref_a <- ggplot(ref_LMA, aes(x = LMA, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55, 55), expand = c(0, 0)) +
  xlab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("LMA Residuals", sep = "")))  +
  th

ref_a_l <- ggplot(ref_LMA, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-55, 55), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

ref_b <- ggplot(ref_WC, aes(x = WC, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  xlab(expression(paste("Observed WC (%)", sep = "")))  +   
  ylab(expression(paste("WC Residuals", sep = "")))  +
  th

ref_b_l <- ggplot(ref_WC, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

ref_c <- ggplot(ref_EWT, aes(x = EWT, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-75, 75), expand = c(0, 0)) +
  xlab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("EWT Residuals", sep = "")))  +
  th

ref_c_l <- ggplot(ref_EWT, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-75, 75), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_a <- ggplot(cwt_LMA, aes(x = LMA, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55, 55), expand = c(0, 0)) +
  xlab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("LMA Residuals", sep = "")))  +
  th

cwt_a_l <- ggplot(cwt_LMA, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-55, 55), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_b <- ggplot(cwt_WC, aes(x = WC, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  xlab(expression(paste("Observed WC (%)", sep = "")))  +   
  ylab(expression(paste("WC Residuals", sep = "")))  +
  th

cwt_b_l <- ggplot(cwt_WC, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-15, 15), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

cwt_c <- ggplot(cwt_EWT, aes(x = EWT, y = mean, fill = Life_form, colour = Life_form)) +
  geom_hline(yintercept= 0, linetype= "solid", color = "black", size = 0.05) +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) +  
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-75, 75), expand = c(0, 0)) +
  xlab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("EWT Residuals", sep = "")))  +
  th

cwt_c_l <- ggplot(cwt_EWT, aes(x= mean, fill= Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = c(-75, 75), expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

tiff("Figure_S8.tif", width = 21, height = 11.67, units = "cm", res = 600)

ggarrange(hist_a, NULL, hist_b, NULL, hist_c, NULL,
          ref_a, ref_a_l, ref_b, ref_b_l, ref_c, ref_c_l,
          cwt_a, cwt_a_l, cwt_b, cwt_b_l, cwt_c, cwt_c_l,
          ncol = 6, nrow = 3,  align = "hv", 
          labels = c("", "", "", "", "", "", "a", "", "b", "", "c", "", "d", "", "e", "", "f", ""), 
          font.label = list(size = 14, color = "black", face = "plain", family = NULL),
          label.x = 0.26,
          label.y = 0.99,
          widths = c(2, 1, 2, 1, 2, 1), 
          heights = c(1, 2, 2),
          common.legend = TRUE)

dev.off()
