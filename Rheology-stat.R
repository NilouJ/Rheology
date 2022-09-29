library(tidyverse)
library(ggtext)
library(patchwork)
library(tidyverse)
library(ggplot2)
library(dplyr)
### FREQ SWEEP
Path <- ("C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix")
### TIME SWEEP
Path <- ("C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix")
setwd("C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming")

# LOAD DATASETS ###############################################################
### FREQ SWEEP
data <- read.csv(file = 'C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix/BIGDATA_Freq.csv')
### TIME SWEEP
data <- read.csv(file = 'C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix/BIGDATA_Time.csv')
# data$Channel <- as.factor(data$Channel)
data$file <- as.factor(data$file)
head(data)

# levels(data$file)
sd(data$time)

###############################################################################
plot_lst <- vector("list", length = length(levels(data$file)))
N0 <- c()
mean0 <- c()
sd0 <- c()
se0 <- c()
N1 <- c()
mean1 <- c()
sd1 <- c()
se1 <- c()

###Frequency Sweep
for (fname in levels(data$file)) {

  data_fname <- data %>% filter(file == fname) %>%
    mutate(Storage.Modulus = Storage.Modulus / 1000,
           Loss.Modulus = Loss.Modulus/ 1000)

  N0[[fname]] <- length(data_fname$Storage.Modulus)
  mean0[[fname]] <- mean(data_fname$Storage.Modulus)
  sd0[[fname]] <- sd(data_fname$Storage.Modulus)
  se0[[fname]] <- sd0[[fname]]/ sqrt(N0[[fname]])
  sd0 <- as.numeric(sd0)
  se0 <- as.numeric(se0)
  mean0 <- as.numeric(mean0)

  N1[[fname]]<- length(data_fname$Loss.Modulus)
  mean1[[fname]]<- mean(data_fname$Loss.Modulus)
  sd1[[fname]]<- sd(data_fname$Loss.Modulus)
  se1[[fname]]<- sd1[[fname]] / sqrt(N1[[fname]])
  sd1 <- as.numeric(sd1)
  se1 <- as.numeric(se1)
  mean1 <- as.numeric(mean1)

  # Time[fname] <- mean(data_fname$time)

  GG <- ggplot(data_fname, aes(x= Frequency) , xlim = c(0.1,10), fill= fname) +
    geom_errorbar(aes(ymin= Storage.Modulus - sd0/2.5, ymax= Storage.Modulus + sd0/2.5), width=0.02,
                  position= position_dodge(0.05), color="light gray") +
    geom_point(aes(y= Storage.Modulus)) +
    geom_errorbar(aes(ymin=Loss.Modulus - sd1/2.5, ymax=Loss.Modulus + sd1/2.5), width=0.02,
                  position=position_dodge(0.05), color="light gray") +
    geom_point(aes(y= Loss.Modulus), shape = 1) +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      limits = c(0.1, 10)) +

    scale_y_log10() +

    xlab("Frequency (Hz)") +
    ylab("Storage/Loss Modulus [kPa]") +
    ggtitle(fname)+
    theme_classic()

  # GG + scale_x_log10()
  GG + annotation_logticks()
  GG + ggtitle(fname)+ theme(plot.title = element_text(size = 0.5)) +
    GG + geom_text()

  plot_lst[[fname]] <- GG

  ggsave(plot = GG, path = Path, filename = fname, width = 4, height = 3, device = "png")

}
#################################################################################################################
###Time Sweep

for (fname in levels(data$file)) {

  data_fname <- data %>% filter(file == fname) %>%
    mutate(Storage.Modulus = Storage.Modulus / 1000,
           Loss.Modulus = Loss.Modulus/ 1000)
  N0[[fname]] <- length(data_fname$Storage.Modulus)
  mean0[[fname]] <- mean(data_fname$Storage.Modulus)
  sd0[[fname]] <- sd(data_fname$Storage.Modulus)
  se0[[fname]] <- sd0[[fname]]/ sqrt(N0[[fname]])

  sd0 <- as.numeric(sd0)

  N1[[fname]]<- length(data_fname$Loss.Modulus)
  mean1[[fname]]<- mean(data_fname$Loss.Modulus)
  sd1[[fname]]<- sd(data_fname$Loss.Modulus)
  se1[[fname]]<- sd1[[fname]] / sqrt(N1[[fname]])

  sd1 <- as.numeric(sd1)
  # Time[fname] <- mean(data_fname$time)



  GG <- ggplot(data_fname, aes(x= Time) , xlim = c(0.1,10), fill= fname) +
    geom_errorbar(aes(ymin= Storage.Modulus - sd0/2.5, ymax= Storage.Modulus + sd0/2.5), width=0.02,
                  position= position_dodge(0.05), color="light gray") +
    geom_point(aes(y= Storage.Modulus)) +
    geom_errorbar(aes(ymin=Loss.Modulus - sd1/2.5, ymax=Loss.Modulus + sd1/2.5), width=0.02,
                  position=position_dodge(0.05), color="light gray") +
    geom_point(aes(y= Loss.Modulus), shape = 1) +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      limits = c(1, 500)) +

    scale_y_log10() +

    xlab("Time (s)") +
    ylab("Storage/Loss Modulus [kPa]") +
    ggtitle(fname)+
    theme_classic()

  # GG + scale_x_log10()
  GG + annotation_logticks()
  GG + ggtitle(fname)+ theme(plot.title = element_text(size = 0.5)) +
    GG + geom_text()

  plot_lst[[fname]] <- GG

  ggsave(plot = GG, path = Path, filename = fname, width = 4, height = 3, device = "png")

}

# Combine all plots
plot_grid(plotlist = plot_lst,width = 3, height = 3,
          ncol =3, align = "hv")

###################################################################################
Time = c()
for (fname in levels(data$file)) {
  data_fname <- data %>% filter(file == fname)
  Time[fname] <- mean(data_fname$time)
  print(mean(data_fname$time))
}

SUMMARY <- data.frame(N0, mean0, sd0, se0, N1, mean1, sd1, se1)
# SUMMARY <- data.frame(SUMMARY)
head(SUMMARY)
SUMMARY <- SUMMARY %>% mutate(sample =row.names(SUMMARY) , Time = Time)
SUMMARY$Time <- as.factor(SUMMARY$Time)
head(SUMMARY)

# levels(SUMMARY$Time) <- c("#C0C0C0", "#696969")
# levels(SUMMARY$Time)

storage <- ggplot(SUMMARY, aes(x = levels(data$file), y= mean0 )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean0-0.5*sd0, ymax=mean0+0.5*sd0), width=.2,
                position=position_dodge(.9)) +
  xlab(NULL) +
  ylab("Storage Modulus [kPa]") +
  theme_classic()+
  ggtitle("Effect of ion mixture on Storage modulus of gel after 24h incubation in DMEM")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 8))

storage
ggsave(plot = storage, filename = "storage.png",  path = Path, width = 4, height = 3, device = "png")

loss <- ggplot(SUMMARY, aes(x = levels(data$file), y= mean1 )) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=mean1-sd1*0.5, ymax=mean1+sd1*0.5), width=.2,
                position=position_dodge(.9)) +
  xlab(NULL) +
  ylab("Loss Modulus [kPa]") +
  theme_classic()+
  ggtitle("Effect of ion mixture on Loss modulus of gel after 24h incubation in DMEM")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 8))

loss
ggsave(plot = loss, filename = "Loss.png",  path = Path, width = 4, height = 3, device = "png")

tand <- ggplot(SUMMARY, aes(x = levels(data$file), y= mean1/mean0 )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean1/mean0-0.1*sd1/sd0, ymax=mean1/mean0+0.1*sd1/sd0), width=.2,
                position=position_dodge(.9)) +
  xlab(NULL) +
  ylab((expression("tan("*delta*") [-]"))) +
  theme_classic()+
  ggtitle(expression("Effect of ion mixture on ("*delta*") [-]  of gel after 24h incubation in DMEM")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 8))

tand
ggsave(plot = tand, filename = "tand.png",  path = Path, width = 4, height = 3, device = "png")

#########################################################################################
### FREQ SWEEP
write.csv(SUMMARY, file = 'C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix/SUMMARY_Freq.csv')
### TIME SWEEP
write.csv(SUMMARY, file = 'C:/Users/nilou/OneDrive - UNSW/Reports/R-Programming/CaCl2 stability/gel stability data/xlsx/comparison 24h mix/SUMMARY_Time.csv')