###############################################################
###Figure S1
###############################################################

#Libraries
###Libraries
library(ggplot2)
library(ggpubr)
library(scales)
library(data.table)
library(Rmisc)
library(wmtsa)

###Create the scales
data_vis <- read.csv("02-vis_ressav.csv") #vis-nir
data_lwir <- read.csv("02-lwir_ressav.csv") #lwir

CWT <- function(data, range) {
  complete <- data
  row_length <- length(names(data))-1
  
  for(i in 1:row_length) {
    value <- data[,(1+i)]
    cwt_value <- wavCWT(value, n.scale= 15, scale.range= c(1, 9), variance= 1, wavelet= "gaussian2")
    cwt_value <- as.matrix(cwt_value)[,9]
    complete[,(1+i)] <- cwt_value
  }
  complete <- subset(complete, Wavelength >= range[1] & Wavelength <= range[2])
  complete
}

data_vis <- CWT(data_vis, c(0.450, 1.0))
data_lwir <- CWT(data_lwir, c(2.6, 13))

write.csv(data_vis, "03-vis_scale9.csv", row.names = FALSE)
write.csv(data_lwir, "03-lwir_scale9.csv", row.names = FALSE)

####
ID <- fread("01-traits.csv")
ID <- ID[,1]

###Data load and prepare for figures
vis_scale1 <- fread("03-vis_scale1.csv")
vis_scale2 <- fread("03-vis_scale2.csv")
vis_scale3 <- fread("03-vis_scale3.csv")
vis_scale4 <- fread("03-vis_scale4.csv")
vis_scale5 <- fread("03-vis_scale5.csv")
vis_scale6 <- fread("03-vis_scale6.csv")
vis_scale7 <- fread("03-vis_scale7.csv")
vis_scale8 <- fread("03-vis_scale8.csv")
vis_scale9 <- fread("03-vis_scale9.csv")

lwir_scale1 <- fread("03-lwir_scale1.csv")
lwir_scale2 <- fread("03-lwir_scale2.csv")
lwir_scale3 <- fread("03-lwir_scale3.csv")
lwir_scale4 <- fread("03-lwir_scale4.csv")
lwir_scale5 <- fread("03-lwir_scale5.csv")
lwir_scale6 <- fread("03-lwir_scale6.csv")
lwir_scale7 <- fread("03-lwir_scale7.csv")
lwir_scale8 <- fread("03-lwir_scale8.csv")
lwir_scale9 <- fread("03-lwir_scale9.csv")

###
data_order <- function(data, order){
  data_t <- as.data.frame(t(data[,-1]))
  colnames(data_t) <- as.character(data$Wavelength)
  
  complete <- cbind(order, data_t)
  
  complete <- as.data.frame((complete))
  
  cframe <- NA
  
  for(i in 1:nrow(complete)) {
    Value <- as.numeric(complete[i, c(2:ncol(complete))])
    Wavelength <- data$Wavelength
    Life_form <- rep(as.character(complete$Life_form[i]), length(Value))
    frame <- data.frame(Spectra = i, Life_form = Life_form, Wavelength = Wavelength, Value = Value)
    
    cframe <- rbind(cframe, frame)
  }
  
  cframe <- as.data.table(cframe[2:nrow(cframe),])
}

###Data preparation
vis_scale1 <- data_order(vis_scale1, ID)
vis_scale2 <- data_order(vis_scale2, ID)
vis_scale3 <- data_order(vis_scale3, ID)
vis_scale4 <- data_order(vis_scale4, ID)
vis_scale5 <- data_order(vis_scale5, ID)
vis_scale6 <- data_order(vis_scale6, ID)
vis_scale7 <- data_order(vis_scale7, ID)
vis_scale8 <- data_order(vis_scale8, ID)
vis_scale9 <- data_order(vis_scale9, ID)

lwir_scale1 <- data_order(lwir_scale1, ID)
lwir_scale2 <- data_order(lwir_scale2, ID)
lwir_scale3 <- data_order(lwir_scale3, ID)
lwir_scale4 <- data_order(lwir_scale4, ID)
lwir_scale5 <- data_order(lwir_scale5, ID)
lwir_scale6 <- data_order(lwir_scale6, ID)
lwir_scale7 <- data_order(lwir_scale7, ID)
lwir_scale8 <- data_order(lwir_scale8, ID)
lwir_scale9 <- data_order(lwir_scale9, ID)


#Mean
vis_1_mean <- vis_scale1[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_2_mean <- vis_scale2[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_3_mean <- vis_scale3[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_4_mean <- vis_scale4[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_5_mean <- vis_scale5[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_6_mean <- vis_scale6[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_7_mean <- vis_scale7[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_8_mean <- vis_scale8[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
vis_9_mean <- vis_scale9[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]

lwir_1_mean <- lwir_scale1[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_2_mean <- lwir_scale2[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_3_mean <- lwir_scale3[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_4_mean <- lwir_scale4[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_5_mean <- lwir_scale5[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_6_mean <- lwir_scale6[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_7_mean <- lwir_scale7[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_8_mean <- lwir_scale8[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]
lwir_9_mean <- lwir_scale9[, .(Reflectance = mean(Value)), by = c("Life_form", "Wavelength")]


###Data plot----------------------------------
pa <- c("#33B09F", "#B66A34")
tamano <- 10
tamano2 <- 10

th <- theme_bw(base_size = tamano) + theme(plot.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black", size = tamano2),
                                           axis.text.y = element_text(color = "black", size = tamano2),
                                           strip.text.x = element_text(size = tamano, color = "black"),
                                           strip.text.y = element_text(size = tamano, color = "black"),
                                           strip.background = element_rect(color= "black", linetype="solid"))

scalea <- scale_x_continuous(limits = c(0.45, 1), expand = c(0, 0), breaks = c(0.45, 0.6, 0.8, 1), labels = c(0.45, 0.6, 0.8, 1)) 
scaleb <- scale_x_continuous(limits = c(2.6, 13), expand = c(0, 0), breaks = c(3, 5, 7, 9, 11, 13), labels = c(3, 5, 7, 9, 11, 13))

####
#S1
s1_a <- ggplot() +
  geom_line(data= vis_scale1, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_1_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.004, 0.004), expand = c(0, 0), breaks = c(-0.003, 0, 0.003), labels = c(-0.003, 0, 0.003)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 1")

s1_b <- ggplot() +
  geom_line(data= lwir_scale1, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_1_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.004, 0.004), expand = c(0, 0), breaks = c(-0.003, 0, 0.003), labels = c(-0.003, 0, 0.003)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S2
s2_a <- ggplot() +
  geom_line(data= vis_scale2, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_2_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.014, 0.014), expand = c(0, 0), breaks = c(-0.01, 0, 0.01), labels = c(-0.01, 0, 0.01)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 2")

s2_b <- ggplot() +
  geom_line(data= lwir_scale2, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_2_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.014, 0.014), expand = c(0, 0), breaks = c(-0.01, 0, 0.01), labels = c(-0.01, 0, 0.01)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S3
s3_a <- ggplot() +
  geom_line(data= vis_scale3, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_3_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.035, 0.035), expand = c(0, 0), breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 3")

s3_b <- ggplot() +
  geom_line(data= lwir_scale3, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_3_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.035, 0.035), expand = c(0, 0), breaks = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S4
s4_a <- ggplot() +
  geom_line(data= vis_scale4, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_4_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.061, 0.061), expand = c(0, 0), breaks = c(-0.05, 0, 0.05), labels = c(-0.05, 0, 0.05)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 4")

s4_b <- ggplot() +
  geom_line(data= lwir_scale4, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_4_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.061, 0.061), expand = c(0, 0), breaks = c(-0.05, 0, 0.05), labels = c(-0.05, 0, 0.05)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S5
s5_a <- ggplot() +
  geom_line(data= vis_scale5, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_5_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.091, 0.091), expand = c(0, 0), breaks = c(-0.05, 0, 0.05), labels = c(-0.05, 0, 0.05)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 5")

s5_b <- ggplot() +
  geom_line(data= lwir_scale5, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_5_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.091, 0.091), expand = c(0, 0), breaks = c(-0.05, 0, 0.05), labels = c(-0.05, 0, 0.05)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S6
s6_a <- ggplot() +
  geom_line(data= vis_scale6, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_6_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.151, 0.151), expand = c(0, 0), breaks = c(-0.1, 0, 0.1), labels = c(-0.10, 0, 0.10)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 6")

s6_b <- ggplot() +
  geom_line(data= lwir_scale6, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_6_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.151, 0.151), expand = c(0, 0), breaks = c(-0.1, 0, 0.1), labels = c(-0.10, 0, 0.10)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S7
s7_a <- ggplot() +
  geom_line(data= vis_scale7, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_7_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.215, 0.215), expand = c(0, 0), breaks = c(-0.2, 0, 0.2), labels = c(-0.20, 0, 0.20)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 7")

s7_b <- ggplot() +
  geom_line(data= lwir_scale7, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_7_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.215, 0.215), expand = c(0, 0), breaks = c(-0.2, 0, 0.2), labels = c(-0.20, 0, 0.20)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S8
s8_a <- ggplot() +
  geom_line(data= vis_scale8, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_8_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.265, 0.265), expand = c(0, 0), breaks = c(-0.25, 0, 0.25), labels = c(-0.25, 0, 0.25)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 8")

s8_b <- ggplot() +
  geom_line(data= lwir_scale8, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_8_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.265, 0.265), expand = c(0, 0), breaks = c(-0.25, 0, 0.25), labels = c(-0.25, 0, 0.25)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

#S9
s9_a <- ggplot() +
  geom_line(data= vis_scale9, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= vis_9_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.30, 0.31), expand = c(0, 0), breaks = c(-0.3, 0, 0.3), labels = c(-0.3, 0, 0.3)) +
  scalea + th + ylab("Wavelet") + xlab("") + theme(legend.position="none") +
  labs(subtitle = "Scale 9")

s9_b <- ggplot() +
  geom_line(data= lwir_scale9, aes(x= Wavelength, y= Value, group = Spectra), size= 0.5, linetype = "solid", colour = "gray80", alpha = 0.2) +
  geom_line(data= lwir_9_mean, aes(x= Wavelength, y= Reflectance, colour = Life_form), size= 0.5, linetype = "solid") + 
  scale_colour_manual(values = pa) +
  scale_y_continuous(limits = c(-0.30, 0.31), expand = c(0, 0), breaks = c(-0.3, 0, 0.3), labels = c(-0.3, 0, 0.3)) +
  scaleb + th + xlab("") + ylab("") + theme(legend.position="none")

###Export

tiff("Figure_S1.tif", width = 25, height = 30, units = "cm", res = 600)

ggarrange(s1_a, s1_b,
          s2_a, s2_b,
          s3_a, s3_b,
          s4_a, s4_b,
          s5_a, s5_b,
          s6_a, s6_b,
          s7_a, s7_b,
          s8_a, s8_b,
          s9_a, s9_b,
          ncol = 2, nrow = 9,  align = "hv", 
          widths = c(2, 4), 
          heights = c(2, 2),
          common.legend = TRUE)

dev.off()














