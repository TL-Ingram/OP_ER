##### --------------------------------------------------------------------------
qlik_cardio <- function(q){
# Pull out vector of initial wl size
init_size <- read_csv(here("data/OP_monthly.csv")) |>

    

specialty <- init_size |>
    select(Specialty) |>
    distinct(Specialty) |>
    pull()

# Outpatient activity qvd
op_act_qvd <- read_csv(here("data/OP_ACTIVITY_cardio.csv")) |>
    mutate(date_letter_received_dt = date(dmy_hms(date_letter_received_dt)),
           appointment_dt = date(dmy_hms(appointment_dt)),
           month = month(date_letter_received_dt)) |>
    filter(month <= month(today()) -1,
           appointment_type %in% "Follow Up")


act_filter <- op_act_qvd |>
    select(specialty_spec_code_description,
        date_letter_received_dt,
           appointment_dt) |>
    rename("demand" = date_letter_received_dt,
           "capacity" = appointment_dt) |>
    pivot_longer(-specialty_spec_code_description, names_to = "metric", values_to = "date") |>
    group_by(specialty_spec_code_description, metric, date) |>
    count() |>
    drop_na() |> 
    filter(date < today()) |>
    mutate(month = month(date)) |>
    group_by(specialty_spec_code_description, metric, month) |>
    summarise(average = sum(n)/30) |>
    filter(month <= month(today())-1 | month >= month(today() -7)) |>
    group_by(specialty_spec_code_description, metric) |>
    summarise(average = mean(average))


#####
# daily metric line plot
# ggplot(data = act_filter, aes(x = date, y = n, fill = metric)) +
#     geom_col() +
#     facet_grid(metric ~ ., scales = "fixed") +
#     theme_bw()
#####

waiting_list <- function(wl_size, lambda_demand, capacity, horizon) {
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
for (j in specialty) {
answer <- lapply(1:25, function(i) {waiting_list((filter(init_size, Specialty == j)$patients),
                                                 (filter(act_filter, specialty_spec_code_description == j & metric == "demand")$average),
                                                 (filter(act_filter, specialty_spec_code_description == j & metric == "capacity")$average),
                                                 180)}) |>
    as_tibble(.name_repair = "unique") |>
    rowid_to_column("index") |>
    pivot_longer(-"index", names_to = "rep", values_to = "value") |>
    mutate(spec = j)

list_sim[[paste0(j)]] <- answer
wl_sim <- bind_rows(list_sim)
}

# pull the last waiting list size and plop that in the remaining_patients
# plot simulation paths
ggplot(answer, aes(x = index, y = value, colour = rep)) +
    geom_line(alpha = 0.6)
}
