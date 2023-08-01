##### --------------------------------------------------------------------------
# Load packages
library(librarian)
shelf(odbc, DBI, glue, tidyverse, lubridate, here, 
      hrbrthemes, readr, forecast, fpp3, padr)


##### --------------------------------------------------------------------------
# Read in .csv generated from Qlik app "TI-testing"
{
op_monthly <- read_csv(here("OP_monthly.csv")) %>%
  select(1:3)
  # filter(., outpatient_backlog_flag == 1) %>%
  # group_by(date, speciality) %>%
  # summarise(patients = n()) %>%
  # ungroup(.)

# Pull out character vector of all speciality names from op_monthly
speciality <- op_monthly %>%
  select(Specialty) %>%
  distinct(Specialty) %>%
  pull(Specialty)
}


##### --------------------------------------------------------------------------
# Define forecast length (each unit is a month)
h = 36


##### --------------------------------------------------------------------------
# Initialise lists for historic and forecasts per speciality storage
{
  list_paths <- list()
  list_wl <- list()
}
  # Filter to speciality
  for (j in speciality) {
    wl_prep <- op_monthly %>%
      mutate(date = floor_date(ymd(date)),
             date = yearmonth(date)) %>%
      filter(., Specialty == j)
    
    if(dim(wl_prep)[1] >= 12) {
      if(sum(str_detect(wl_prep$Specialty, "Rehabilitation")) < 1) {
        if(sum(str_detect(wl_prep$Specialty, "Plastic Surgery")) < 1) {
        
    # Write historic paths to list
    list_wl[[paste0(j)]] <- wl_prep
    wl_keys <- bind_rows(list_wl)
    
    # Filter to init date, filling date gaps and imputing missing wl size
    print(glue("Speciality: {j}"))
    
    wl_ready <- wl_prep %>%
      as_tsibble(., index = "date", regular = TRUE)
      
      # Filter to halt date
      train_set <- wl_ready %>%
        select(., date, patients)
   
      # Build forecasting models to test
      STLF <- decomposition_model(
        STL((patients) ~ season(window = "periodic")),
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
      try(sim_paths <- model_frame %>%
        generate(h = h, times = 50))

      
      all_path <- sim_paths %>%
        filter(.model == c("combination")) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim),
               Specialty = j) %>%
        as_tibble(.)
      
      if(dim(all_path)[1] >= 5) {
      
      # Write forecast paths to list
      list_paths[[paste0(j)]] <- all_path
      path_keys <- bind_rows(list_paths)
      
      # Write speciality forecast path to a fable object
      # sim_results <- all_path %>%
      #   filter(.model == "combination") %>%
      #   as_tibble(., index = date) %>%
      #   mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
      #   group_by(.model, date) %>%
      #   summarise(dist = distributional::dist_sample(list(.sim)), 
      #             .groups = "drop_last") %>%
      #   ungroup(.) %>%
      #   as_fable(index = date, key = .model, distribution = dist, 
      #            response="patients")
   
      # spec_forecast <- sim_results %>%
      #   autoplot(data = wl_prep, level = 80, size = 0.6, alpha = 0.7) +
      #   geom_line(data = wl_prep, aes(x = date, y = patients), linewidth = 0.6,
      #             alpha = 0.7, colour = "grey50") +
      #   # scale_x_date(breaks = "6 months", date_labels = "%b-%Y") +
      #   scale_colour_discrete(guide = "none") +
      #   theme_bw() +
      #   theme(axis.text.x = element_text(angle = 30, vjust = 0.5, 
      #                                      hjust=0.5)) +
      #   labs(fill = "",
      #        x = "",
      #        y = "Patients",
      #        title = glue("{j} Outpatient List"),
      #        level = "")
      # 
      # spec_forecast
      # 
      # # Save plot
      # file_name <- glue("{j}_Outpatient_fc")
      # ggsave(here("plots", 
      #             filename=paste0(file_name, ".png")), device = "png")
      }

yearly_patients <- path_keys %>%
  mutate(., .sim = if_else(.sim < 1, 0, .sim)) %>%
  mutate(., .sim = replace_na(.sim, 0)) %>%
  rename("patients" = .sim) %>%
  # group_by(., date, Specialty) %>%
  # summarise(., "patients" = round(mean(.sim), 0),
  #           "max" = quantile(.sim, 0.8),
  #           "min" = quantile(.sim, 0.2)) %>%
  ungroup(.) %>%
  bind_rows(., wl_keys) %>%
  mutate(date = as.Date(date, origin="1899-12-30", format = "%Y/%m/%d")) %>%
  write_csv(., here(glue("all_forecast.csv")))
      }
    }
    }
  }
##### --------------------------------------------------------------------------