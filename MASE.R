# Run from the project root. The original calculations and summaries are unchanged.
bet_frq <- file.path("data", "bet", "bet.frq")
bet_full_grid <- file.path("data", "bet", "grid", "full")
output_dir <- file.path("output", "mase")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## calculate the MASE value for each model on the sbsbf0 and cpue indices
library(FLR4MFCL)
library(ggplot2)
library(tidyverse)
library(yardstick)
mase_fun <- function(actual, forecast) {
  # Ensure actual and forecast are of the same length
  if (length(actual) != length(forecast)) {
    stop("Actual and forecast must be of the same length")
  }
 
    mae_forecast <- mean(abs(actual-forecast),na.rm=TRUE)
   # calculate the 
    naive_forecast <- actual[-length(actual)]
    mae_naive <- mean(abs(actual[-1]-naive_forecast))

  # Calculate MASE
  mase_value <- mae_forecast / mae_naive
  
  return(mase_value)
}

## MAE function 
mae_fun <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

## RMSE function
rmse_fun <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAPE Function 
mape_fun <- function(actual, forecasted) {
  return(mean(abs((actual - forecasted) / actual) * 100))
}


# read in the based file (frq file)
## director for retrospective 
sadir <- file.path("data", "bet", "result", "stoc_rec")
model_name <-gsub(".cpue_sim.all.csv","",list.files(sadir,pattern=".cpue_sim.all.csv"))
mase.cpue.all<-NA
mae.cpue.all<-NA
rmse.cpue.all<-NA
mape.cpue.all<-NA
mase.horizon.all <-NA

##cpue
for (model in model_name){
  print(model)
  # Read IN full frq file
 # fullfrq <- read.MFCLFrq(file.path(bet_full_grid, model, "bet.frq"))
  fullfrq <- read.MFCLFrq(bet_frq)
  
  data_retro <- realisations(fullfrq) %>% 
  filter(fishery %in% c(33:41)) %>%
    mutate(base=catch/effort,yrqtr=year+(month+1)/12-0.125) %>% filter(year>2013) %>% select(yrqtr, base,fishery) 
  
  mase <- matrix(NA,nrow=9,ncol=7)
  mae <- matrix(NA,nrow=9,ncol=7)
  rmse <- matrix(NA,nrow=9,ncol=7)
  mape <- matrix(NA,nrow=9,ncol=7)
  
  cpue.all <- read.csv(file.path(sadir, paste0(model,".cpue_sim.all.csv")),row.names = 1)
  cpue.all <- cpue.all[c(2:nrow(cpue.all)),]
  data_retro_ind <- cpue.all %>%
  select(retro_year,yrqtr, cpue,fishery,itn) %>%
  filter(yrqtr>retro_year+1)  
    data <- data_retro_ind %>% 
      inner_join(data_retro, by=c("yrqtr","fishery")) %>%
      arrange(retro_year, itn, fishery, yrqtr)
    
    mase <- data %>% group_by(retro_year,itn,fishery) %>% summarise(MASE=mase_fun(base,cpue))  %>% 
      group_by(fishery) %>% summarise(median=quantile(MASE,0.5,na.rm=TRUE), higher_95=quantile(MASE,0.975,na.rm=TRUE), lower_95=quantile(MASE,0.025,na.rm=TRUE),prop_less_than_1=mean(MASE<1,na.rm=TRUE))
 
    mase.horizon <- data %>%
      mutate(
        forecast_horizon = 2021 - retro_year
      ) %>%
      filter(forecast_horizon %in% 1:7) %>%
      group_by(retro_year, forecast_horizon, itn, fishery) %>%
      summarise(
        MASE = mase_fun(base, cpue),
        .groups = "drop"
      ) %>%
      group_by(fishery, forecast_horizon) %>%
      summarise(
        n = sum(!is.na(MASE)),
        median = median(MASE, na.rm = TRUE),
        q2.5 = quantile(MASE, 0.025, na.rm = TRUE),
        q97.5 = quantile(MASE, 0.975, na.rm = TRUE),
        minimum = min(MASE, na.rm = TRUE),
        maximum = max(MASE, na.rm = TRUE),
        prop_less_than_1 = mean(MASE < 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(fishery, forecast_horizon)
      
    # mae <- data %>% filter(yrqtr>2015) %>% group_by(retro_year,itn,fishery) %>% summarise(MAE=mae_fun(base,cpue))  %>% 
    #   group_by(fishery) %>% summarise(median=quantile(MAE,0.5,na.rm=TRUE),nine=quantile(MAE,0.9,na.rm=TRUE),ten=quantile(MAE,0.1,na.rm=TRUE))
    # 
    # rmse <- data %>% filter(yrqtr>2015) %>% group_by(retro_year,itn,fishery) %>% summarise(RMSE=rmse_fun(base,cpue))  %>% 
    #   group_by(fishery) %>% summarise(median=quantile(RMSE,0.5,na.rm=TRUE),nine=quantile(RMSE,0.9,na.rm=TRUE),ten=quantile(RMSE,0.1,na.rm=TRUE))
    # 
    # mape <- data %>% filter(yrqtr>2015) %>% group_by(retro_year,itn,fishery) %>% summarise(MAPE=mape_fun(base,cpue))  %>% 
    #   group_by(fishery) %>% summarise(median=quantile(MAPE,0.5,na.rm=TRUE),nine=quantile(MAPE,0.9,na.rm=TRUE),ten=quantile(MAPE,0.1,na.rm=TRUE))
    
  mase$model <- model
  mase.cpue.all <- rbind(mase.cpue.all,mase) 
  
  mase.horizon$model <- model
  mase.horizon.all<-rbind(mase.horizon.all, mase.horizon)
  # 
  # mae$model <- model
  # mae.cpue.all <- rbind(mae.cpue.all,mae)
  # 
  # rmse$model <- model
  # rmse.cpue.all <- rbind(rmse.cpue.all,rmse) 
  # 
  # mape$model <- model
  # mape.cpue.all <- rbind(mape.cpue.all,mape) 
}  

mase.cpue.all <- mase.cpue.all[-1,]
write.csv(mase.cpue.all,file.path(output_dir,"MASE_CPUE_stoc.csv"))
mase.horizon.all <- mase.horizon.all[-1,]
write.csv(mase.horizon.all,file.path(output_dir,"MASE_CPUE_horizon_all.csv"))

mae.cpue.all <-mae.cpue.all[-1,]
write.csv(mae.cpue.all,file.path(output_dir,"MAE_CPUE_stoc.csv"))

rmse.cpue.all <-rmse.cpue.all[-1,]
write.csv(rmse.cpue.all,file.path(output_dir,"RMSE_CPUE_stoc.csv"))

mape.cpue.all <-mape.cpue.all[-1,]
write.csv(mape.cpue.all,file.path(output_dir,"MAPE_CPUE_stoc.csv"))


## look at the distribution of the MASE 
##look at those 
mase.cpue.all <- read.csv(file.path(output_dir,"MASE_CPUE_stoc.csv")) %>%
  mutate(region=paste0("Region ",fishery-32))
mae.cpue.all <- read.csv(file.path(output_dir,"MAE_CPUE_stoc.csv")) %>%
  mutate(region=paste0("Region ",fishery-32))
rmse.cpue.all <- read.csv(file.path(output_dir,"RMSE_CPUE_stoc.csv"))%>%
  mutate(region=paste0("Region ",fishery-32))
mape.cpue.all <- read.csv(file.path(output_dir,"MAPE_CPUE_stoc.csv"))%>%
  mutate(region=paste0("Region ",fishery-32))
mase.horizon.all <- read.csv(file.path(output_dir,"MASE_CPUE_horizon_all.csv"))%>%
  mutate(region=paste0("Region ",fishery-32))

## report species and region, including the median, interquartile range, range, and the proportion of model configurations with MASE less than 1. 
 mase_by_region <- mase.cpue.all %>%
   mutate(region = paste0("Region ", fishery - 32)) %>%
   group_by(fishery, region) %>%
   summarise(
     n_models = sum(!is.na(median)),
     median_MASE = median(median, na.rm = TRUE),
     q2.5 = quantile(median, 0.025, na.rm = TRUE),
     q97.5 = quantile(median, 0.975, na.rm = TRUE),
     minimum = min(median, na.rm = TRUE),
     maximum = max(median, na.rm = TRUE),
     prop_less_than_1 = mean(median < 1, na.rm = TRUE),
     .groups = "drop"
   )

 mase_by_region_table <- mase_by_region %>%
   transmute(
     Fishery = fishery,
     Region = region,
     `Number of models` = n_models,
     `Median MASE` = round(median_MASE, 2),
     `95% interval` = sprintf("%.2f–%.2f", q2.5, q97.5),
     Range = sprintf("%.2f–%.2f", minimum, maximum),
     `Proportion < 1` = round(prop_less_than_1, 2)
   )

 
## ANOVA

 
 mase.cpue.all$m <-substr(mase.cpue.all$model,2,2)
 mase.cpue.all$s <-substr(mase.cpue.all$model,5,6)
 mase.cpue.all$a <-substr(mase.cpue.all$model,9,11)
 mase.cpue.all$h <-substr(mase.cpue.all$model,14,15)
 mase.cpue.all$r <-substr(mase.cpue.all$region,8,9)
 
 summary(aov(median ~ m,data=mase.cpue.all))
 summary(aov(median ~ s,data=mase.cpue.all))
 summary(aov(median ~ a,data=mase.cpue.all))
 summary(aov(median ~ h,data=mase.cpue.all))
 summary(aov(median ~ r,data=mase.cpue.all))
 
 
 png(file.path(output_dir,"Histogram_BET_cpue_MASE.png"),width=1200,height=800)
mase.cpue.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white",binwidth = 0.18) + theme_bw()+
 ## geom_vline(xintercept = 0.2,linetype="dashed") + 
  geom_vline(xintercept = 1,linetype="dashed")+
  facet_wrap(~region,ncol=3)+
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 20),strip.text = element_text(size = 20)) + labs(
          title = "Distribution of MASE Values for BET Hindcast CPUE Forecast Accuracy",
          x = "MASE Values",
          y = "Frequency") + guides(lty="none")
dev.off()

png(file.path(output_dir,"Histogram_BET_cpue_MAE.png"),width=1200,height=800)
mae.cpue.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  ## geom_vline(xintercept = 0.2,linetype="dashed") + 
  ## geom_vline(xintercept = 1,linetype="dashed")+
  facet_wrap(~region,ncol=3)+
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=15),
        axis.title=element_text(size=20),plot.title = element_text(size = 20),strip.text = element_text(size = 20)) + labs(
          title = "Distribution of MAE Values for BET Hindcast CPUE Forecast Accuracy",
          x = "MAE Values",
          y = "Frequency") + guides(lty="none")
dev.off()

png(file.path(output_dir,"Histogram_BET_cpue_RMSE.png"),width=1200,height=800)
rmse.cpue.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  facet_wrap(~region,ncol=3)+
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=15),
        axis.title=element_text(size=20),plot.title = element_text(size = 20),strip.text = element_text(size = 20)) + labs(
          title = "Distribution of RMSE Values for BET Hindcast CPUE Forecast Accuracy",
          x = "RMSE Values",
          y = "Frequency") + guides(lty="none")
dev.off()


png(file.path(output_dir,"Histogram_BET_cpue_MAPE.png"),width=1200,height=800)
mape.cpue.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  facet_wrap(~region,ncol=3)+
    theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=15),
        axis.title=element_text(size=20),plot.title = element_text(size = 20),strip.text = element_text(size = 20)) + labs(
          title = "Distribution of MAPE Values for BET Hindcast CPUE Forecast Accuracy",
          x = "MAPE Values",
          y = "Frequency") + guides(lty="none")
dev.off()



##table by horizon 
mase_by_horizon <- mase.horizon.all %>%
  mutate(region = paste0("Region ", fishery - 32)) %>%
  group_by(fishery, region,forecast_horizon) %>%
  summarise(
    n_models = n_distinct(model[!is.na(median)]),
    median_MASE = median(median, na.rm = TRUE),
    q2.5 = quantile(median, 0.025, na.rm = TRUE),
    q97.5 = quantile(median, 0.975, na.rm = TRUE),
    minimum = min(median, na.rm = TRUE),
    maximum = max(median, na.rm = TRUE),
    prop_less_than_1 = mean(median < 1, na.rm = TRUE),
    .groups = "drop"
  )


write.csv(mase_by_horizon,file.path(output_dir,"MASE_by_horizon.csv"))

normalize <- function(data) {
  (data - mean(data)) / sd(data)
}



##sbsbf0
mase.sbsbf0.all <- NA
mae.sbsbf0.all  <- NA
rmse.sbsbf0.all <- NA
mape.sbsbf0.all <- NA

for (model in model_name){
  print(model)
  # Read IN full frq file
  Rep <- read.MFCLRep(file.path(bet_full_grid, model, "plot-final.par.rep"))
  
  data_retro <- SBSBF0recent(Rep) %>% 
    as.data.frame() %>%
    filter(year>2013) %>% select(year,data) %>%
    rename(base=data)
  
  
  sbsbf0.all <- read.csv(file.path(sadir, paste0(model,".sbsbf0.all.csv")),row.names = 1)
  sbsbf0.all <- sbsbf0.all[-1,]
  data_retro_ind <- sbsbf0.all %>%
    select(retro_year,year, data,itn) %>%
    filter(year>=retro_year+1)  
  
  data <- data_retro_ind %>% 
    merge(data_retro, by=c("year"),all=TRUE)

  mase <- data %>% group_by(retro_year,itn) %>% summarise(MASE=mase_fun(base,data))  %>% ungroup() %>%
   summarise(median=quantile(MASE,0.5,na.rm=TRUE),nine=quantile(MASE,0.9,na.rm=TRUE),ten=quantile(MASE,0.1,na.rm=TRUE))
  
  mae <- data %>% filter(year>2014) %>% group_by(retro_year,itn) %>% summarise(MAE=mae_fun(base,data))  %>% ungroup() %>%
   summarise(median=quantile(MAE,0.5,na.rm=TRUE),nine=quantile(MAE,0.9,na.rm=TRUE),ten=quantile(MAE,0.1,na.rm=TRUE))
  
  rmse <- data %>% filter(year>2014) %>% group_by(retro_year,itn) %>% summarise(RMSE=rmse_fun(base,data))  %>% ungroup() %>%
   summarise(median=quantile(RMSE,0.5,na.rm=TRUE),nine=quantile(RMSE,0.9,na.rm=TRUE),ten=quantile(RMSE,0.1,na.rm=TRUE))
  
  mape <- data %>% filter(year>2014) %>% group_by(retro_year,itn) %>% summarise(MAPE=mape_fun(base,data))  %>% ungroup() %>%
    summarise(median=quantile(MAPE,0.5,na.rm=TRUE),nine=quantile(MAPE,0.9,na.rm=TRUE),ten=quantile(MAPE,0.1,na.rm=TRUE))
  
    
  # Rep <- read.MFCLRep(file.path(bet_full_grid, model, "plot-final.par.rep"))
  # 
  # data_retro <- SBSBF0recent(Rep) %>% 
  #   as.data.frame() %>%
  #   select(year,data) %>%
  #   rename(base=data)
  # 
  # 
  # sbsbf0.all <- read.csv(file.path(sadir, paste0(model,".sbsbf0.all.csv")),row.names = 1)
  # sbsbf0.all <- sbsbf0.all[-1,c("year","data","retro_year","itn")]
  # 
  # trend <- merge(data_retro,sbsbf0.all, by="year") %>% 
  # filter(!is.na(base)) %>%
  # group_by(itn,retro_year) %>% mutate(norm_base=normalize(base),norm_data=normalize(data)) %>% 
  # ungroup() %>%
  #   group_by(year,retro_year) %>%
  # summarise(median_base=quantile(norm_base,0.5,na.rm=TRUE),nine_base=quantile(norm_base,0.9,na.rm=TRUE),ten_base=quantile(norm_base,0.1,na.rm=TRUE),
  #           median_data=quantile(norm_data,0.5,na.rm=TRUE),nine_data=quantile(norm_data,0.9,na.rm=TRUE),ten_data=quantile(norm_data,0.1,na.rm=TRUE))
  # 
  # ggplot(trend,aes(x=year))+geom_line(aes(y=median_data,color=as.factor(retro_year))) + 
  #   geom_line(aes(y=median_base),color="black") + theme_bw()
  # 
  mase$model <- model
  mase.sbsbf0.all <- rbind(mase.sbsbf0.all,mase) 
  
  mae$model <- model
  mae.sbsbf0.all <- rbind(mae.sbsbf0.all,mae)
  
  rmse$model <- model
  rmse.sbsbf0.all <- rbind(rmse.sbsbf0.all,rmse) 
  
  mape$model <- model
  mape.sbsbf0.all <- rbind(mape.sbsbf0.all,mape) 
  
}  

mase.sbsbf0.all <- mase.sbsbf0.all[-1,]
write.csv(mase.sbsbf0.all,file.path(output_dir,"MASE_sbsbf0_stoc.csv"))
mae.sbsbf0.all <- mae.sbsbf0.all[-1,]
write.csv(mae.sbsbf0.all,file.path(output_dir,"MAE_sbsbf0_stoc.csv"))
rmse.sbsbf0.all <- rmse.sbsbf0.all[-1,]
write.csv(rmse.sbsbf0.all,file.path(output_dir,"RMSE_sbsbf0_stoc.csv"))
mape.sbsbf0.all <- mape.sbsbf0.all[-1,]
write.csv(mape.sbsbf0.all,file.path(output_dir,"MAPE_sbsbf0_stoc.csv"))



png(file.path(output_dir,"Histogram_BET_sbsbf0_MASE.png"),width=1200,height=800)
mase.sbsbf0.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  ## geom_vline(xintercept = 0.2,linetype="dashed") + 
  geom_vline(xintercept = 1,linetype="dashed")+
  xlab("MASE") + ylab("Count") +  
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) +
    labs(title = "Distribution of MASE Values for BET Hindcast SBSBF0recent Forecast Accuracy",
    x = "MASE Values",
    y = "Frequency")+ 
   guides(lty="none")
dev.off()

mae.sbsbf0.all <- read.csv(file.path(output_dir,"MAE_sbsbf0_stoc.csv"))
png(file.path(output_dir,"Histogram_BET_sbsbf0_MAE.png"),width=1200,height=800)
mae.sbsbf0.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  xlab("MAE") + ylab("Count") +  
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) +
  labs(title = "Distribution of MAE Values for BET Hindcast SBSBF0recent Forecast Accuracy",
       x = "MAE Values",
       y = "Frequency")+ 
  guides(lty="none")
dev.off()

rmse.sbsbf0.all <- read.csv(file.path(output_dir,"RMSE_sbsbf0_stoc.csv"))
png(file.path(output_dir,"Histogram_BET_sbsbf0_RMSE.png"),width=1200,height=800)
rmse.sbsbf0.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  xlab("RMSE") + ylab("Count") +  
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) +
  labs(title = "Distribution of RMSE Values for BET Hindcast SBSBF0recent Forecast Accuracy",
       x = "RMSE Values",
       y = "Frequency")+ 
  guides(lty="none")
dev.off()

mape.sbsbf0.all <- read.csv(file.path(output_dir,"MAPE_sbsbf0_stoc.csv"))
png(file.path(output_dir,"Histogram_BET_sbsbf0_MAPE.png"),width=1200,height=800)
mape.sbsbf0.all %>% ggplot(aes(x=median))+geom_histogram(color="black", fill="white") + theme_bw()+
  theme(legend.title = element_blank(),legend.text=element_text(size = 20), legend.position = "bottom",axis.text=element_text(size=20),
        axis.title=element_text(size=20),plot.title = element_text(size = 30)) +
  labs(title = "Distribution of RMSE Values for BET Hindcast SBSBF0recent Forecast Accuracy",
       x = "MAPE Values",
       y = "Frequency")+ 
  guides(lty="none")
dev.off()


