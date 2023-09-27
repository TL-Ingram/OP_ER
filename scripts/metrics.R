##### --------------------------------------------------------------------------
op_metrics <- function(q){
# Pull out vector of initial wl size
# init_size <- read_csv(here("data/OP_monthly.csv")) 
    # Load packages
    library(tidyverse)
    library(here)

test <- q

#  Outpatient activity qvd
# test <- read_csv(here("data/OP_ACTIVITY_cardio.csv"))
op_act <- test |>
    mutate(date_letter_received_dt = as.Date(date_letter_received_dt, origin = "1900-01-01")) |>
    mutate(appointment_dt = as.Date(appointment_dt, origin = "1900-01-01")) |>
    mutate(month = as.numeric(strftime(date_letter_received_dt, format = "%m", origin = "1900-01-01"))) |>
    filter(appointment_type %in% "Follow Up") |>
    filter(month <= as.numeric(strftime(today(), format = "%m", origin = "1900-01-01")) - 1) #|>
    
op_act_qvd <- op_act |>
    select(specialty_spec_code_description,
           date_letter_received_dt,
           appointment_dt) |>
    rename("demand" = date_letter_received_dt,
           "capacity" = appointment_dt) |>
    pivot_longer(-specialty_spec_code_description, names_to = "metric", values_to = "date") |>
    group_by(specialty_spec_code_description, metric, date) |>
    count() |>
    filter(date < today()) |>
    mutate(month = as.numeric(strftime(date, format = "%m", origin = "1900-01-01"))) |>
    group_by(specialty_spec_code_description, metric, month) |>
    summarise(average = sum(n)/30) |>
    filter(month <= (as.numeric(strftime(today(), format = "%m", origin = "1900-01-01")) - 1) |
               (as.numeric(strftime(today(), format = "%m", origin = "1900-01-01")) - 7)) |>
# filter(month <= month(today())-1 | month >= month(today() -7)) |>
    group_by(specialty_spec_code_description, metric) |>
    summarise(average = mean(average))



# simulation
waiting_list_sim <- function(wl_size, lambda_demand, capacity, horizon) {
    # null vector vec
    vec <- c()
    # initialize values as in the code
    current_patients <- 0
    remaining_patients <- wl_size
    date <- today()
    # while loop with condition hours != 0
    while (horizon != 0) {
        # update values
        current_patients <- remaining_patients + rpois(1, lambda_demand)
        remaining_patients <- max(current_patients - capacity, 0)
        print(paste(current_patients, remaining_patients))
        vec <- c(vec, remaining_patients)
        #update hours
        horizon <- horizon-1;
    }
    # return the vector
    return(vec)
}

# replicate simulation x times
list_sim <- list()
answer <- lapply(1:2, function(i) {waiting_list((1000),
                                                (filter(q, specialty == "Cardiology" & metric == "demand")$average),
                                                (filter(q, specialty == "Cardiology" & metric == "capacity")$average),
                                                180)}) |>
    as_tibble(.name_repair = "unique") |>
    rowid_to_column("index") |>
    pivot_longer(c(2:3), names_to = "rep", values_to = "value") |>
    mutate(specialty = "Cardiology")

list_sim[[paste0(j)]] <- answer
wl_sim <- bind_rows(list_sim)
}
