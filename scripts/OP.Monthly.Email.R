library(sendmailR)

from <- sprintf("sense.alert@wwl.nhs.uk" ,"Qlik Sense 1")

to_Brian <- sprintf("brian.wood@wwl.nhs.uk")

to_Tom <- sprintf("thomas.ingram@wwl.nhs.uk")

subject <- paste("Error OP.Monthly.Forecast", Sys.Date(), sep = " ")



opening<-paste0("Hello Data Science Team,\n")
middle<-paste("\nThe forecast for the monthly outpatient backlog has errored on",Sys.Date(),"please see logs.\n", sep=" ")
end<-paste0("\nBest wishes from the eternal one")

body<-paste(opening, middle, end, sep = "\n")

sendmail(from, to_Brian, subject, body,
         control=list(smtpServer= "ExternalMail.xwwl.nhs.uk" , smtpPort = 25))

sendmail(from, to_Tom, subject, body,
         control=list(smtpServer= "ExternalMail.xwwl.nhs.uk" , smtpPort = 25))
