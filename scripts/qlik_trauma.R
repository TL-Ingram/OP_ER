##### --------------------------------------------------------------------------
qlik_trauma <- function(q){
# Load packages
  library(glue)
  library(tidyverse)
  library(lubridate)
  library(padr)
  library(fpp3)
  library(here)
  library(forecast)
  library(tsibble)
# glue, tidyverse, lubridate, here, 
#       hrbrthemes, readr, forecast, fpp3, padr)
  

##### --------------------------------------------------------------------------
# Define forecast length (each unit is a month)
# q <- read_csv(here("op_monthly.csv"))

#### --------------------------------------------------------------------------
# Initialise lists for historic and forecasts per speciality storage
{
  list_paths <- list()
  list_wl <- list()
}
  # Filter to speciality
  # for (j in speciality) {
    wl_prep <- q # %>%
      # mutate(date = as.Date(date, origin = "UTC", format = "%d/%m/%Y"))
     # mutate(date = as.Date(date, format = ))

    # Write historic paths to list
    list_wl[[paste0("Trauma & Orthopaedics")]] <- wl_prep
    wl_keys <- bind_rows(list_wl)
    
    # Filter to init date, filling date gaps and imputing missing wl size
    wl_ready <- wl_prep %>%
      as_tsibble(., index = "date", regular = TRUE)


      # Filter to halt date
      train_set <- wl_ready %>%
        select(., date, patients)
      
      # Build forecasting models to test
      STLF <- decomposition_model(
        STL((patients) ~ season(period = 7, window = "periodic")),
        ETS(season_adjust ~ season("N"))
      )
      model_frame <- train_set %>%
        fill_gaps(patients = mean(patients)) %>%
        fabletools::model(
          ets = ETS(patients, trace = F),
          stl = STLF,
          arima = ARIMA(patients, stepwise = F, approximation = F, trace = F),
          nnar = NNETAR(patients, stepwise = F, trace = F),
        ) %>%
        mutate(combination = (ets + arima + nnar + stl)/4)

      # Generate future sample paths
      sim_paths <- model_frame %>%
        generate(h = 7, times = 10)
      

      all_path <- sim_paths %>%
        filter(.model == c("combination")) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
        as_tibble(.)

      if(dim(all_path)[1] >= 5) {

      # Write forecast paths to list
      list_paths[[paste0("Trauma & Orthopaedics")]] <- all_path
      path_keys <- bind_rows(list_paths)

      # Write speciality forecast path to a fable object
      sim_results <- all_path %>%
        as_tibble(., index = date) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
        group_by(.model, date) %>%
        summarise(dist = distributional::dist_sample(list(.sim)),
                  .groups = "drop_last") %>%
        ungroup(.) %>%
        as_fable(index = date, key = .model, distribution = dist,
                 response="patients")

#       spec_forecast <- sim_results %>%
#         filter(.model == "combination") %>%
#         autoplot(data = wl_prep, level = 80, size = 0.6, alpha = 0.7) +
#         geom_line(data = wl_prep, aes(x = date, y = patients), linewidth = 0.6,
#                   alpha = 0.7, colour = "grey50") +
#         # scale_x_date(breaks = "6 months", date_labels = "%b-%Y") +
#         scale_colour_discrete(guide = "none") +
#         theme_bw() +
#         theme(axis.text.x = element_text(angle = 30, vjust = 0.5,
#                                            hjust=0.5)) +
#         labs(fill = "",
#              x = "",
#              y = "Patients",
#              title = "Outpatient List",
#              level = "")
# 
#       spec_forecast

      # Save plot
      # file_name <- glue("{j}_Outpatient_fc")
      # ggsave(here("plots",
      #             filename=paste0(file_name, ".png")), device = "png")


yearly_patients <- all_path %>%
  mutate(., .sim = if_else(.sim < 1, 0, .sim)) %>%
  group_by(., date) %>%
  summarise(., "patients" = round(mean(.sim), 0)) %>%
  ungroup(.) %>%
  mutate(., speciality = "Trauma & Orthopaedics") %>%
  bind_rows(., wl_keys) %>%
  # filter(., month(date) == month(1)) %>%
  arrange(., date) %>%
  ungroup(.)

write_csv(yearly_patients, "C:/R/Scripts/MASTER/admission_prediction/yearly_patients.csv")
      }
      return(yearly_patients)
}
##### --------------------------------------------------------------------------


# create dataframe and rowbind by rowno.
# new one should include forward looking dates

# horizon <-
#   data.frame(
#     arrival_dtm = max(as.POSIXct(q$date, origin = "UTC", format = "%d/%m/%Y %H:%M:%S")))
