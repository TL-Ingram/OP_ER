# Load packages
library(librarian)
shelf(odbc, DBI, glue, tidyverse, lubridate, here, hrbrthemes, readr, forecast, fpp3, padr)


# connect to sql server --------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldwsql1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)


# read "reporting_theatres" dbo ------------------------------------------------
system.time(data_op <- dbReadTable(con, "reporting_outpatient_WaitingList"))


# initial theatre cleanup ------------------------------------------------------
#process_one <- data_op %>%
  
op_test <- read_csv(here("OP_test.csv")) %>%
  rename(., "date" = SnapshotDate,
         "speciality" = Specialty) %>%
  group_by(date, speciality) %>%
  summarise(patients = n()) %>%
  ungroup(.)

speciality <- op_test %>%
  select(speciality) %>%
  distinct(speciality) %>%
  pull(speciality)
# op_ACTIVITY <- read_csv(here("OP_ACTIVITY.csv")) 
h = 50
{
  list_paths <- list()
  list_wl <- list()
}
  # Filter to speciality
  for (j in speciality) {
    wl_prep <- op_test %>%
      mutate(date = floor_date(ymd(date)),
             date = yearmonth(date)) %>%
      filter(., speciality == "Trauma & Orthopaedics")
    
    # Write historic paths to list
    list_wl[[paste0("Trauma & Orthopaedics")]] <- wl_prep
    wl_keys <- bind_rows(list_wl)
    
    
    # Filter to init date, filling date gaps and imputing missing wl size
    print(glue("Waiting list: {i} \nSpeciality: {j}"))
    
    wl_ready <- wl_prep %>%
      # filter(date > train_init) %>%
      as_tsibble(., index = "date", regular = TRUE)
    
    
    # # Script continuance test
    # if(dim(wl_ready)[1] >= 1) {
    #   print(glue("Building models and running simulations..."))
    #   
      
      
      # Filter to halt date
      train_set <- wl_ready %>%
        select(., date, patients)
      ?window
      # Build forecasting models to test
      # STLF <- decomposition_model(
      #   STL((patients) ~ season(window = Inf)),
      #   ETS(season_adjust ~ season("N"))
      # )
      model_frame <- train_set %>%
        fill_gaps(patients = mean(patients)) %>%
        fabletools::model(
          ets = ETS(patients, trace = F),
          # stlf = STLF)
          arima = ARIMA(patients, stepwise = F, approximation = F, trace = F),
          nnar = NNETAR(patients, stepwise = F, trace = F),
        ) %>%
        mutate(combination = (ets + arima + nnar)/3)
      
      # Generate future sample paths
      sim_paths <- model_frame %>%
        generate(h = h, times = 20)

      
      all_path <- sim_paths %>%
        filter(.model == c("combination")) %>%
        mutate(.sim = if_else(.sim < 1, 0, .sim)) %>%
        as_tibble(.)
      
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
      
      spec_forecast <- sim_results %>%
        filter(.model == "combination") %>%
        autoplot(data = wl_prep, level = 80, size = 0.6, alpha = 0.7) +
        geom_line(data = wl_prep, aes(x = date, y = patients), size = 0.6,
                  alpha = 0.7, colour = "grey50") +
        scale_x_date(breaks = "6 months", date_labels = "%b-%Y") +
        scale_colour_discrete(guide = "none") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.5, 
                                           hjust=0.5)) +
        labs(fill = "",
             x = "",
             y = "Patients",
             title = glue("T&O list"),
             level = "")
      
      spec_forecast
      

      
      

op_test %>%
  # filter(Specialty == "Trauma & Orthopaedics") %>%
  ggplot(aes(x = SnapshotDate, y = count)) +
  geom_line() +
  facet_wrap(. ~ Specialty, scales = "free_y")

#summarise snapshot_date and group by spec