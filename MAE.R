# Run with working directory set to the project root. No setwd() is required.
# Edit these relative paths to match where your assessment inputs are stored.
yft_results <- file.path("data", "yft", "result")
bet_results <- file.path("data", "bet", "result")
yft_full_grid <- file.path("data", "yft", "grid", "full")
bet_full_grid <- file.path("data", "bet", "grid", "full")
output_dir <- file.path("output", "depletion")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(FLR4MFCL)
library(tidyverse)

## MAE function 
mae_fun <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
sadir <- file.path(yft_results, "stoc_rec")
model_name <-gsub(".cpue_sim.all.csv","",list.files(sadir,pattern=".cpue_sim.all.csv"))
depletion.by.horizon.all<-NULL
depletion.by.model.all<-NULL

for (model in model_name){
  print(model)
  # Read IN full frq file
  Rep <- read.MFCLRep(file.path(yft_full_grid, model, "plot-final.par.rep"))
  
  data_retro <- SBSBF0recent(Rep) %>% 
    as.data.frame() %>%
    filter(year>2013) %>% select(year,data) %>%
    rename(base=data)
  
  
  sbsbf0.all <- read.csv(file.path(sadir,paste0(model,".sbsbf0.all.csv")),row.names = 1)
  sbsbf0.all <- sbsbf0.all[-1,]
  data_retro_ind <- sbsbf0.all %>%
    select(retro_year,year, data,itn) %>%
    filter(year>=retro_year+1)  
  
  data <- data_retro_ind %>% 
    merge(data_retro, by=c("year"),all=TRUE)
  
  depletion.by.horizon.itn <- data %>%
    filter(!is.na(retro_year), year > retro_year) %>%
    mutate(
      forecast_horizon = year - retro_year,
      signed_diff = data - base,
      abs_diff = abs(signed_diff)
    ) %>%
    group_by(itn, forecast_horizon) %>%
    summarise(
      abs_diff = median(abs_diff, na.rm = TRUE),       # across peels
      signed_diff = median(signed_diff, na.rm = TRUE),
      n_peels = n_distinct(retro_year),
      .groups = "drop"
    )
  
  depletion.by.horizon <- depletion.by.horizon.itn %>%
    group_by(forecast_horizon) %>%
    summarise(
      median_abs_diff = median(abs_diff, na.rm = TRUE),
      median_signed_diff = median(signed_diff, na.rm = TRUE),
      n_peels = first(n_peels),
      .groups = "drop"
    ) %>%
    mutate(model = model)
  
  depletion.by.horizon.all <- bind_rows(
    depletion.by.horizon.all,
    depletion.by.horizon
  )
  
  
  depletion.by.model <- depletion.by.horizon.itn %>%
    group_by(itn) %>%
    summarise(
      overall_abs_diff = median(abs_diff, na.rm = TRUE), # across horizons
      overall_signed_diff = median(signed_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    summarise(
      n_iterations = n_distinct(itn),
      median_abs_diff = median(overall_abs_diff, na.rm = TRUE),
      abs_q2.5 = quantile(overall_abs_diff, 0.025, na.rm = TRUE),
      abs_q97.5 = quantile(overall_abs_diff, 0.975, na.rm = TRUE),
      median_signed_diff = median(overall_signed_diff, na.rm = TRUE)
    ) %>%
    mutate(model = model)

  
  depletion.by.model$model <- model
  depletion.by.model.all <- rbind(depletion.by.model.all,depletion.by.model) 
  
}  

write.csv(depletion.by.model.all,file.path(output_dir,"yft_stoc_by_model.csv"))

depletion.summary <- depletion.by.horizon.all %>%
  group_by(forecast_horizon) %>%
  summarise(
    n_models = n_distinct(model),
    grid_median_abs_diff = median(median_abs_diff),
    q2.5 = quantile(median_abs_diff, 0.025),
    q97.5 = quantile(median_abs_diff, 0.975),
    grid_median_signed_diff = median(median_signed_diff),
    n_peels = first(n_peels),
    .groups = "drop"
  )
write.csv(depletion.summary,file.path(output_dir,"yft_stoc_by_horizon.csv"))


#deterministic
sadir <- file.path(bet_results, "fix_rec")
model_name <- basename(
  list.dirs(sadir, recursive = FALSE, full.names = TRUE)
)
depletion.by.horizon.all<-NULL



for (model in model_name) {
  print(model)
  
  full_rep <- read.MFCLRep(
    file.path(bet_full_grid, model, "plot-final.par.rep")
  )
  
  full_depletion <- SBSBF0recent(full_rep) %>%
    as.data.frame() %>%
    filter(year %in% 2015:2021) %>%
    select(year, base = data)
  
  deterministic_data <- purrr::map_dfr(2014:2020, function(peel) {
    rep_file <- file.path(sadir, model, paste0(peel, ".rep"))
    peel_rep <- read.MFCLRep(rep_file)
    
    SBSBF0recent(peel_rep) %>%
      as.data.frame() %>%
      filter(year > peel, year <= 2021) %>%
      select(year, data) %>%
      mutate(retro_year = peel)
  })
  
  depletion.by.horizon <- deterministic_data %>%
    inner_join(full_depletion, by = "year") %>%
    mutate(
      forecast_horizon = year - retro_year,
      signed_diff = data - base,
      abs_diff = abs(signed_diff)
    ) %>%
    group_by(forecast_horizon) %>%
    summarise(
      median_abs_diff = median(abs_diff),
      median_signed_diff = median(signed_diff),
      n_peels = n_distinct(retro_year),
      .groups = "drop"
    ) %>%
    mutate(model = model)
  
  depletion.by.horizon.all <- bind_rows(
    depletion.by.horizon.all,
    depletion.by.horizon
  )
}

depletion.by.model <- depletion.by.horizon.all %>%
  group_by(model) %>%
  summarise(
    overall_abs_diff = median(median_abs_diff),
    overall_signed_diff = median(median_signed_diff),
    .groups = "drop"
  )

write.csv(depletion.by.model,file.path(output_dir,"bet_fix_by_model.csv"))

depletion.summary <- depletion.by.horizon.all %>%
  group_by(forecast_horizon) %>%
  summarise(
    n_models = n_distinct(model),
    grid_median_abs_diff = median(median_abs_diff),
    q2.5 = quantile(median_abs_diff, 0.025),
    q97.5 = quantile(median_abs_diff, 0.975),
    grid_median_signed_diff = median(median_signed_diff),
    n_peels = first(n_peels),
    .groups = "drop"
  )
write.csv(depletion.summary,file.path(output_dir,"bet_fix_by_horizon.csv"))

