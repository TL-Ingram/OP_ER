##### --------------------------------------------------------------------------
sim_wl <- function(q, additions_factor, removals_factor, 
                   selected_specialty, user){
    # Load packages
    library(tidyverse)
    library(here)
    library(pracma)
    source("db_connect_functions.R")

#######
#######
#######
    
# run this for testing only
# q <- read_csv(here("data/OP_activity_testing.csv"))

#######
#######
#######
    
# qlik script starts here
qlik_data <- q
op_act <- qlik_data |>
    mutate(date_letter_received_dt = as.Date(date_letter_received_dt, 
                                             origin = "1900-01-01")) |>
    mutate(appointment_dt = as.Date(appointment_dt, origin = "1900-01-01")) |>
    mutate(month = as.numeric(strftime(date_letter_received_dt, format = "%m", 
                                       origin = "1900-01-01"))) |>
    filter(month <= as.numeric(strftime(today(), format = "%m", 
                                        origin = "1900-01-01")) - 1) |>
    filter(appointment_type == "Follow Up")

op_act_qvd <- op_act |>
    select(specialty_spec_code_description,
           date_letter_received_dt,
           appointment_dt) |>
    rename("demand" = date_letter_received_dt,
           "capacity" = appointment_dt,
           "specialty" = specialty_spec_code_description) |>
    pivot_longer(-specialty, names_to = "metric", values_to = "date") |>
    group_by(specialty, metric, date) |>
    count() |>
    filter(date < today()) |>
    mutate(month = as.numeric(strftime(date, format = "%m", 
                                       origin = "1900-01-01"))) |>
    group_by(specialty, metric, month) |>
    summarise(average = sum(n)/30) |>
    filter(month <= (as.numeric(strftime(today(), format = "%m", 
                                         origin = "1900-01-01")) - 1) |
               (as.numeric(strftime(today(), format = "%m", 
                                    origin = "1900-01-01")) - 7)) |>
    group_by(specialty, metric) |>
    summarise(average = mean(average))

# create current list size df ---
current_size <- qlik_data |>
    select(c("specialty_spec_code_description", "current_size")) |>
    distinct(specialty_spec_code_description, current_size) |>
    rename("specialty" = specialty_spec_code_description,
           "init_size" = current_size)

additions_factor = 1
removals_factor = 1
user = "test"
selected_specialty = "Cardiology"

# simulation
waiting_list_sim <- function(wl_size, lambda_demand, a_factor, capacity,
                             r_factor, horizon) {
    # null vector vec
    vec <- c()
    # initialize values as in the code
    current_patients <- 0
    remaining_patients <- wl_size
    date <- today()
    end <- today() + 180
    # while loop with condition hours != 0
    while (horizon != 0) {
        # print(horizon)
        # update values
        current_patients <- remaining_patients +
            rpois(1, lambda_demand*(a_factor))
        print(current_patients)
        # if (horizon < 50) {
        #     current_patients = current_patients - (current_patients*(exp(0.0001)))
        # }
        if (horizon < 160) {
        remaining_patients <- max(current_patients - capacity*(r_factor*exp(1/(horizon))), 0)
        } else {
            remaining_patients <- max(current_patients - capacity*(r_factor), 0)
            
        }
        print(remaining_patients)
        vec <- c(vec, remaining_patients)
        # update hours
        horizon <- horizon-1;
        }
    # return the vector
    return(vec)
}

# 2300+exp(1/10)
# replicate simulation x times
list_sim <- list()
answer <- lapply(1:50, function(i) {
    waiting_list_sim(
        (filter(current_size, specialty == selected_specialty)$init_size),
        (filter(op_act_qvd, specialty == selected_specialty &
                    metric == "demand")$average),
        (as.numeric(additions_factor)),
        (filter(op_act_qvd, specialty == selected_specialty &
                    metric == "capacity")$average),
        (as.numeric(removals_factor)),
        180)}) |>
    as_tibble(.name_repair = "unique") |>
    rowid_to_column("index") |>
    pivot_longer(c(2:51), names_to = "rep", values_to = "value") |>
    mutate(specialty = selected_specialty)
    

# mutate(date = seq(today(), length = n(), by = '1 day')) |>
    # mutate(date = as.Date(date, origin = "1900-01-01"))

list_sim[[paste0(selected_specialty)]] <- answer
wl_sim <- bind_rows(list_sim)
ggplot(data = wl_sim, aes(x = index, y = value, colour = rep)) +
    geom_step()
    
