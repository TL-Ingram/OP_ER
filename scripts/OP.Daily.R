# load packages ----------------------------------------------------------------
library(librarian)
shelf(tidyverse, odbc, here)


# connect to sql server --------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldevsqlfrm1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)


#####
# pull .csv
# data <- read_csv(here("data/OP_daily.csv")) %>%
#     filter(., speciality == "Trauma & Orthopaedics",
#            outpatient_backlog_flag == 1)
# read "op_wl" dbo -------------------------------------------------------------
# system.time(data_theatre <- dbReadTable(con, "reporting_outpatient_waitingList"))

# from OP_waitinglist ----------------------------------------------------------
# test_data <- dbGetQuery(con, '
#            SELECT *
#             FROM [nhs_reporting].[dbo].[reporting_outpatient_WaitingList]
#            WHERE Snapshot_Date > getdate() - 7;
#            ')
# 
# filter_data <- test_data %>%
#     filter(., AppointmentType %in% "Waiting List - Follow Up")
# 
# wl_test_data %>%
#     group_by(AppointmentType) %>%
#     summarise(count = n())

# from op_activity -------------------------------------------------------------
# act_test_data <- dbGetQuery(con,
#            "SELECT
#            calendar_month_year,
#            appointment_serial,
#            nhs_number,
#            appointment_type,
#            referral_priority,
#            specialty_local_desc,
#            date_letter_received_dt,
#            date_app_booked_date,
#            attendance_date,
#            appointment_dt,
#            DischargeDate,
#            cancellation_date,
#            dna_flag,
#            attend_or_dna_flag
#            FROM [nhs_reporting].[dbo].[reporting_outpatient_activity]
#            WHERE date_letter_received_dt > getdate() - 365
#            AND appointment_type = 'Follow Up'
#            AND DischargeDate IS NOT NULL")



##### 
# dw sql query -----------------------------------------------------------------
# uncomment when deploying
# activity_data <- dbGetQuery(con,
                            "SELECT *
                            FROM [nhs_reporting].[dbo].[reporting_outpatient_activity]
                            WHERE date_letter_received_dt > getdate() - 365;")
# write query to csv for ease of loading
# write_csv(activity_data, here("data/OP_activity.csv"))
act_test_data <- read_csv(here("data/OP_activity.csv"))

speciality <- act_test_data %>%
    distinct(specialty_local_desc) %>%
    pull(specialty_local_desc)

act_list <- list()


#####
# filter pulled data -----------------------------------------------------------
for (i in speciality) {
act_filter <- act_test_data |>
    select(calendar_month_year,
           appointment_serial,
           nhs_number,
           appointment_type,
           referral_priority,
           specialty_local_desc,
           date_letter_received_dt,
           attendance_date,
           DischargeDate,
           cancellation_date,
           attend_or_dna_flag,
           appointment_type) |>
    filter(appointment_type %in% "Follow Up",
           specialty_local_desc == "CARDIOLOGY") |>
    mutate(attendance_date = if_else(!is.na(cancellation_date), NA, attendance_date)) |>
    select(date_letter_received_dt,
           DischargeDate) |>
    rename("demand" = date_letter_received_dt,
           "capacity" = DischargeDate) |>
    pivot_longer(everything(), names_to = "metric", values_to = "date") |>
    group_by(metric, date) |>
    count() |>
    drop_na() |> 
    filter(date < today()) |>
    mutate(month = month(date),
           spec = "CARDIOLOGY")


# daily metric line plot
# ggplot(data = act_filter, aes(x = date, y = n, fill = metric)) + 
#     geom_col() +
#     facet_grid(metric ~ ., scales = "fixed") + 
#     theme_bw()


# daily metrics
act_metric <- act_filter |>
    group_by(spec, metric, month) |>
    summarise(average = sum(n) / 30)


# Calculate the average monthly metrics for Orthopaedic Surgery first

act_list[[paste0(i, "_")]] <- act_metric
act_keys <- bind_rows(act_list)
}


# # test poisson plot
# plot <- rpois(1000,3.1) |>
#     as_tibble() |>
#     ggplot(aes(x = value)) +
#     geom_density()
# plot
    

#####
# simulation T&O ---------------------------------------------------------------
waiting_list <- function(lambda_demand, capacity, days) {
    # null vector vec
    vec <- c()
    # initialize values as in the code
    current_patients <- 0
    remaining_patients <- 1000
    date <- today()
    # while loop with condition hours != 0
    while (days != 0) {
        # update values
        date <- date + 1
        current_patients <- remaining_patients + rpois(1, lambda_demand)
        remaining_patients <- max(current_patients - capacity, 0)
        print(paste(current_patients, remaining_patients))
        vec <- c(vec, remaining_patients)
        #update hours
        days <- days-1;
    }
    # return the vector
    return(vec)
}


# replicate simulation x times
answer <- lapply(1:25, function(i) {waiting_list((filter(act_metric, metric == "demand")$average),
                                                 (filter(act_metric, metric == "capacity")$average),
                                                 365)}) |>
    as_tibble(.name_repair = "unique") |>
    rowid_to_column("index") |>
    pivot_longer(-"index", names_to = "rep", values_to = "value") |>
    mutate(spec = "Cardiology")

# pull the last waiting list size and plop that in the remaining_patients
# plot simulation paths
ggplot(answer, aes(x = index, y = value, colour = rep)) +
    geom_line(alpha = 0.6)

# work out each months previous rates and use this for each upcoming month
# do for all specialty's
