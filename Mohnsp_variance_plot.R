# Run from the project root; replace directory values if needed.
input_dir <- file.path("data", "derived")
output_dir <- file.path("output", "mohn")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(ggplot2)
library(tidyverse)
mohn<-read.csv(file.path(input_dir,"BET_morhsp.csv"))
mohn$m <-substr(mohn$Model,2,2)
mohn$s <-substr(mohn$Model,5,6)
mohn$a <-substr(mohn$Model,9,11)
mohn$h <-substr(mohn$Model,14,15)

summary(aov(Value ~ m,data=mohn))
summary(aov(Value ~ s,data=mohn))
summary(aov(Value ~ a,data=mohn))
summary(aov(Value ~ h,data=mohn))

png(file.path(output_dir,"BET_m.png"))

ggplot(mohn, aes(x = factor(m), y = Value)) +
  geom_boxplot(
    width = 0.6,
    fill = "grey85",
    colour = "black",
    linewidth = 0.6,
    fatten = 2,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  labs(
    x = "Tag-mixing period (quarters)",
    y = expression("Mohn's " * rho)
  ) +
  coord_cartesian(ylim = c(-0.11, -0.04)) +  # adjust if needed
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(linewidth = 0.6)
  )
dev.off()
png(file.path(output_dir,"BET_s.png"))
ggplot(mohn, aes(x = factor(s), y = Value)) +
  geom_boxplot(
    width = 0.6,
    fill = "grey85",
    colour = "black",
    linewidth = 0.6,
    fatten = 2,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  labs(
    x = "Size data weighting divisor",
    y = expression("Mohn's " * rho)
  ) +
  coord_cartesian(ylim = c(-0.11, -0.04)) +  # adjust if needed
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(linewidth = 0.6)
  )
dev.off()
mohn %>% ggplot(aes(x=a,y=Value)) + geom_boxplot() + theme_bw()
mohn %>% ggplot(aes(x=h,y=Value)) + geom_boxplot() + theme_bw()

png(file.path(output_dir,"histogram_BET.png"))
mohn %>% ggplot(aes(x=Value))+geom_histogram(color="black", fill="white") + theme_bw()+
  geom_vline(xintercept = 0.2,linetype="dashed") + geom_vline(xintercept = -0.15,linetype="dashed")+
   xlab(bquote(paste("Mohn's ", rho))) + ylab("Count") +  scale_x_continuous(breaks=seq(-0.2, 0.2, 0.05)) + 
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(bquote(paste("Histogram of BET Mohn's ", rho))) + guides(lty="none")
dev.off()
                                                                              
##try to plot it better
# thresholds for "pass"
lower_thr <- -0.15
upper_thr <-  0.20   # if you want an upper threshold

  mohn<-mohn |> mutate(Fill=ifelse(between(Value,-0.15,0.2), "pass","no"))
  vlines <- mohn %>% group_by(m) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("tag mixing",m)) %>% select(-m)
  vlines <- rbind(vlines,
    (mohn %>% group_by(s) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("size data weighting",s)) %>% select(-s)))
  vlines <- rbind(vlines, 
    (mohn %>% group_by(a) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("age data weighting",a)) %>% select(-a)))
  vlines <- rbind(vlines,
    (mohn %>% group_by(h) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("steepness",h)) %>% select(-h)))
  
    
  # 1. Histogram data + pass/fail
p_base <- ggplot(mohn, aes(x = Value, fill = Fill)) +
  geom_histogram(binwidth = 0.01, colour = "black") +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "grey30"),
                    name = "Pass") +
  labs(x = "Mohn's rho", y = "Count") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        legend.position = "right")+ scale_x_continuous(limits =c(-0.15,0.01))



# 2. Add vertical lines: black threshold + coloured scenario lines
p <- p_base +
  # thick black line for threshold (lower)
  geom_vline(data=vlines, aes(xintercept = rho_mean,colour = Scenario),
             size = 1.4) + 
  scale_colour_discrete(name = as.factor("Scenario"))
png(file.path(output_dir,"histogram_BET.png"),width=1200,height=600)
p +geom_vline(xintercept = -0.15,linetype="dashed")+
  xlab(bquote(paste("Mohn's ", rho))) + ylab("Count")  + 
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(bquote(paste("Histogram of BET Mohn's ", rho))) + guides(lty="none")
dev.off()


##yellowfin 
mohn<-read.csv(file.path(input_dir,"YFT_morhsp.csv"))
mohn$m <-substr(mohn$Model,2,2)
mohn$s <-substr(mohn$Model,5,6)
mohn$a <-substr(mohn$Model,9,11)
mohn$h <-substr(mohn$Model,14,15)

summary(aov(Value ~ m,data=mohn))
summary(aov(Value ~ s,data=mohn))
summary(aov(Value ~ a,data=mohn))
summary(aov(Value ~ h,data=mohn))

png(file.path(output_dir,"YFT_m.png"))

ggplot(mohn, aes(x = factor(m), y = Value)) +
  geom_boxplot(
    width = 0.6,
    fill = "grey85",
    colour = "black",
    linewidth = 0.6,
    fatten = 2,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  labs(
    x = "Tag-mixing period (quarters)",
    y = expression("Mohn's " * rho)
  ) +
  coord_cartesian(ylim = c(-0.11, -0.04)) +  # adjust if needed
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(linewidth = 0.6)
  )
dev.off()
png(file.path(output_dir,"YFT_s.png"))
ggplot(mohn, aes(x = factor(s), y = Value)) +
  geom_boxplot(
    width = 0.6,
    fill = "grey85",
    colour = "black",
    linewidth = 0.6,
    fatten = 2,
    outlier.shape = 16,
    outlier.size = 1.5
  ) +
  labs(
    x = "Size data weighting divisor",
    y = expression("Mohn's " * rho)
  ) +
  coord_cartesian(ylim = c(-0.11, -0.04)) +  # adjust if needed
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(linewidth = 0.6)
  )
dev.off()

mohn %>% ggplot(aes(x=a,y=Value)) + geom_boxplot() + theme_bw()
mohn %>% ggplot(aes(x=h,y=Value)) + geom_boxplot() + theme_bw()
 
png(file.path(output_dir,"histogram_YFT.png"))
mohn %>% ggplot(aes(x=Value))+geom_histogram(color="black", fill="white") + theme_bw()+
  geom_vline(xintercept = 0.2,linetype="dashed") + geom_vline(xintercept = -0.15,linetype="dashed")+
  xlab(bquote(paste("Mohn's ", rho))) + ylab("Count") +  scale_x_continuous(breaks=seq(-0.2, 0.2, 0.05)) + 
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(bquote(paste("Histogram of YFT Mohn's ", rho))) + guides(lty="none")
dev.off()

##try to plot it better
# thresholds for "pass"
lower_thr <- -0.15
upper_thr <-  0.20   # if you want an upper threshold

mohn<-mohn |> mutate(Fill=ifelse(between(Value,-0.15,0.2), "pass","no"))
vlines <- mohn %>% group_by(m) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("tag mixing",m)) %>% select(-m)
vlines <- rbind(vlines,
                (mohn %>% group_by(s) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("size data weighting",s)) %>% select(-s)))
vlines <- rbind(vlines, 
                (mohn %>% group_by(a) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("age data weighting",a)) %>% select(-a)))
vlines <- rbind(vlines,
                (mohn %>% group_by(h) %>%summarise(rho_mean = mean(Value), .groups = "drop") %>% mutate(Scenario=paste0("steepness",h)) %>% select(-h)))


# 1. Histogram data + pass/fail
p_base <- ggplot(mohn, aes(x = Value, fill = Fill)) +
  geom_histogram(binwidth = 0.01, colour = "black") +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "grey30"),
                    name = "Pass") +
  labs(x = "Mohn's rho", y = "Count") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        legend.position = "right")+ scale_x_continuous(limits =c(-0.15,0.01))



# 2. Add vertical lines: black threshold + coloured scenario lines
p <- p_base +
  # thick black line for threshold (lower)
  geom_vline(data=vlines, aes(xintercept = rho_mean,colour = Scenario),
             size = 1.4) + 
  scale_colour_discrete(name = as.factor("Scenario"))
png(file.path(output_dir,"histogram_YFT.png"),width=1200,height=600)
p +geom_vline(xintercept = -0.15,linetype="dashed")+
  xlab(bquote(paste("Mohn's ", rho))) + ylab("Count")  + 
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) + ggtitle(bquote(paste("Histogram of YFT Mohn's ", rho))) + guides(lty="none")
dev.off()
