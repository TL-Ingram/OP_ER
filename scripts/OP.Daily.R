# load packages
library(librarian)
shelf(tidyverse, odbc)

# connect to sql server --------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "wwldevsqlfrm1",
                      Database = "nhs_reporting",
                      UID      = "",
                      PWD      = "",
                      Port     = 1433)


# read "op_wl" dbo ------------------------------------------------
# system.time(data_theatre <- dbReadTable(con, "reporting_outpatient_waitingList"))

# from OP_waitinglist ----------------------------------------------------------
test_data <- dbGetQuery(con, '
           SELECT *
            FROM [nhs_reporting].[dbo].[reporting_outpatient_WaitingList]
           WHERE Snapshot_Date > getdate() - 7;
           ')

filter_data <- test_data %>%
    filter(., AppointmentType %in% "Waiting List - Follow Up")

wl_test_data %>%
    group_by(AppointmentType) %>%
    summarise(count = n())

# from op_activity -------------------------------------------------------------
act_test_data <- dbGetQuery(con,
           "SELECT
           date_letter_received_dt,
           appointment_dt,
           DischargeDate,
           FROM [nhs_reporting].[dbo].[reporting_outpatient_activity]
           WHERE date_letter_received_dt > getdate() - 365
           AND appointment_type = 'Follow Up'
           AND DischargeDate IS NOT NULL")

act_filter <- act_test_data |>
    filter(appointment_type %in% "Follow Up") %>%
    group_by()
# Build scenario test
# Work out distribution of arrivals onto list, removals off list

# pull .csv
data <- read_csv(here("data/OP_daily.csv")) %>%
    filter(., speciality == "Trauma & Orthopaedics",
           outpatient_backlog_flag == 1)

# for capacity use appointment_dt. I think DischargeDate is different.
# for demand use date_letter_received_dt
