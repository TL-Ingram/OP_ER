error_log <- file("//wwlqliksense1/QlikShare/Data/EXCEL/DataScience/Output/Log.log", open="wt")
sink(error_log, type="message")

print(R.Version()$version.string)


tryCatch({
#RUN SCRIPT FOR OP_monthly_forecast
source("//wwlqliksense1/QlikShare/Data/EXCEL/DataScience/Scripts.Live/OP.Monthly.R")},

error = function(t){
source("//wwlqliksense1/QlikShare/Data/EXCEL/DataScience/Scripts.Live/Error.emails/OP.Monthly.Email.R")
})


#RUN AUTOMATED EMAIL FOR COMPLETION OF RUNS
source("//wwlqliksense1/QlikShare/Data/EXCEL/DataScience/Scripts.Live/Completion.Email.OP.Backlog.R")
