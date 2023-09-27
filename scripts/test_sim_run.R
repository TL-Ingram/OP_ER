##### --------------------------------------------------------------------------
sim_wl <- function(q, additions_factor, removals_factor, selected_specialty){
    
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
                                                     (filter(q, specialty == selected_specialty & metric == "demand")$average),
                                                     (filter(q, specialty == selected_specialty & metric == "capacity")$average),
                                                     180)}) |>
        as_tibble(.name_repair = "unique") |>
        rowid_to_column("index") |>
        pivot_longer(c(2:3), names_to = "rep", values_to = "value") |>
        mutate(specialty = selected_specialty)

    list_sim[[paste0(j)]] <- answer
    wl_sim <- bind_rows(list_sim)
}
