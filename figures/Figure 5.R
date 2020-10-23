###############################################################
###Figure 5
###############################################################

#Note: inputs come from code_base.R

###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(ggExtra)
library(plyr)

###Data and preparations
ref_LMA_predict <- fread("ref_LMA_predict_test.csv")
ref_WC_predict <- fread("ref_WC_predict_test.csv")
ref_EWT_predict <- fread("ref_EWT_predict_test.csv")
cwt_LMA_predict <- fread("cwt_LMA_predict_test.csv")
cwt_WC_predict <- fread("cwt_WC_predict_test.csv")
cwt_EWT_predict <- fread("cwt_EWT_predict_test.csv")

summary_data <- function(x) {
  y <- x
  mean <- mean(y)
  sd_lower <- mean - sd(y)
  sd_upper <- mean + sd(y)
  return(c(sd_upper = sd_upper, mean = mean, sd_lower = sd_lower))
}

ref_LMA_sum <- as.data.table(t(apply(ref_LMA_predict, 1, summary_data)))
ref_WC_sum <- as.data.table(t(apply(ref_WC_predict, 1, summary_data)))
ref_EWT_sum <- as.data.table(t(apply(ref_EWT_predict, 1, summary_data)))
cwt_LMA_sum <- as.data.table(t(apply(cwt_LMA_predict, 1, summary_data)))
cwt_WC_sum <- as.data.table(t(apply(cwt_WC_predict, 1, summary_data)))
cwt_EWT_sum <- as.data.table(t(apply(cwt_EWT_predict, 1, summary_data)))

observed <- fread("traits_testing.csv")
colnames(observed)[3:5] <- c("LMA", "WC", "EWT")
observed$LMA <- 10^observed$LMA
observed$WC <- 10^observed$WC
observed$EWT <- 10^observed$EWT

ref_LMA <- cbind(observed[,c(1,3)], ref_LMA_sum)
ref_WC <- cbind(observed[,c(1,4)], ref_WC_sum)
ref_EWT <- cbind(observed[,c(1,5)], ref_EWT_sum) 
cwt_LMA <- cbind(observed[,c(1,3)], cwt_LMA_sum)
cwt_WC <- cbind(observed[,c(1,4)], cwt_WC_sum)
cwt_EWT <- cbind(observed[,c(1,5)], cwt_EWT_sum)

ref_LMA[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
ref_WC[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
ref_EWT[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_LMA[, c("fit", "lwr", "upr") := as.data.table((predict(lm(LMA ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_WC[, c("fit", "lwr", "upr") := as.data.table((predict(lm(WC ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]
cwt_EWT[, c("fit", "lwr", "upr") := as.data.table((predict(lm(EWT ~ mean, .SD), newdata = .SD, interval = "prediction"))), by = "Life_form"]


#Parameters for figure
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

LMA_range <- c(0, 300)
WC_range <- c(40, 95)
EWT_range <- c(20, 450)
size_point <- 1.5

A <- ggplot(ref_LMA, aes(x = mean, fill = Life_form)) +
     geom_density(alpha = 0.2) +
     scale_fill_manual(values = pa) +
     scale_color_manual(values = pa) +
     scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
     theme_void() +
     theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))


B <- ggplot(cwt_LMA, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

C <- ggplot(ref_LMA, aes(x = mean, y = LMA, fill = Life_form, colour = Life_form)) +
     geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
     geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
     geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
     geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
     scale_fill_manual(values= pa) +
     scale_color_manual(values= pa) +
     scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
     scale_y_continuous(limits = LMA_range, expand = c(0, 0)) +
     xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
     ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
     geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
     geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
     th


D <- ggplot(cwt_LMA, aes(x = mean, y = LMA, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  scale_y_continuous(limits = LMA_range, expand = c(0, 0)) +
  xlab(expression(paste("Predicted LMA (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed LMA (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

E <- ggplot(cwt_LMA, aes(x = LMA, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = LMA_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


Fa <- ggplot(ref_WC, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

G <- ggplot(cwt_WC, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

H <- ggplot(ref_WC, aes(x = mean, y = WC, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

I <- ggplot(cwt_WC, aes(x = mean, y = WC, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  scale_y_continuous(limits = WC_range, expand = c(0, 0)) +
  xlab(expression(paste("Predicted WC (%)", sep = "")))  +   
  ylab(expression(paste("Observed WC (%)", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

J <- ggplot(cwt_WC, aes(x = WC, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = WC_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()

K <- ggplot(ref_EWT, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

L <- ggplot(cwt_EWT, aes(x = mean, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt"))

M <- ggplot(ref_EWT, aes(x = mean, y = EWT, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0)) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

N <- ggplot(cwt_EWT, aes(x = mean, y = EWT, fill = Life_form, colour = Life_form)) +
  geom_abline(intercept = 0, slope = 1, color= "grey", linetype= "solid", size= 0.3)+
  geom_errorbarh(aes(xmin = sd_lower, xmax = sd_upper, height = 0, colour = Life_form), alpha = 0.2) +
  geom_point(shape = 21, color = "white", size = size_point, alpha = 0.8) + 
  geom_smooth(method='lm', formula = y ~ x, se = FALSE, size = 0.5) + 
  scale_fill_manual(values= pa) +
  scale_color_manual(values= pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  scale_y_continuous(limits = EWT_range, expand = c(0, 0)) +
  xlab(expression(paste("Predicted EWT (g m"^-2, ")", sep = "")))  +   
  ylab(expression(paste("Observed EWT (g m"^-2, ")", sep = "")))  +
  geom_line(aes(x = mean, y = lwr, colour = Life_form), linetype = "dashed", size= 0.3) +
  geom_line(aes(x = mean, y = upr, colour = Life_form), linetype = "dashed", size= 0.3) +
  th

O <- ggplot(cwt_EWT, aes(x = EWT, fill = Life_form)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = pa) +
  scale_color_manual(values = pa) +
  scale_x_continuous(limits = EWT_range, expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(4, 6, 0, 0, "pt")) + rotate()


tiff("Figure_5.tif", width = 15, height = 21, units = "cm", res = 600)

ggarrange(A, B, NULL, 
          C, D, E,
          Fa, G, NULL,
          H, I, J,
          K, L, NULL,
          M, N, O,
          labels = c("", "", "", "a", "b", "", "", "", "", "c", "d", "", "", "", "", "e", "f", ""), 
          font.label = list(size = 15, color = "black", face = "plain", family = NULL),
          label.x = 0.26,
          label.y = 0.99,
          ncol = 3, nrow = 6,  align = "hv", 
          widths = c(2, 2, 1), 
          heights = c(1, 2, 1, 2, 1, 2),
          common.legend = TRUE)

dev.off()
     
