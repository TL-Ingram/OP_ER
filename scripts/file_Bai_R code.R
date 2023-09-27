##inputs####
library(readxl)
inputs <- read_excel("input.xlsx")
future_length <- as.numeric(inputs[1,1])

capacity <- as.data.frame(inputs[1:future_length,2:7])
colnames(capacity) <- c("new","1FU","FU","inp1","inp2","inp3")

referrals <- as.data.frame(inputs[1:future_length,9:11])
colnames(referrals) <- c("twoWeek","urgent","routine")

startSize <- as.matrix(inputs[1:6,14:16])
colnames(startSize) <- c("twoWeek","urgent","routine")
rownames(startSize) <- c("new","1FU","FU","inp1","inp2","inp3")

rates <- vector("list",3)
for (i in 1:3){
  rates[[i]] <- matrix(0,nrow = 6,ncol = 7)
  colnames(rates[[i]]) <- c("new","inp1","1FU","inp2","FU","inp3","discharge")
  rownames(rates[[i]]) <- c("new","inp1","1FU","inp2","FU","inp3")
}

##routing rates options####
{ratesource <- readline(prompt = "routing probabilites source (inputs / records):")
  if (ratesource == "inputs") {
    rates[[1]] <- as.matrix(inputs[1:6,19:25])
    rates[[2]] <- as.matrix(inputs[8:13,19:25])
    rates[[3]] <- as.matrix(inputs[15:20,19:25])
  } else {
    ##rates####
    #a new column needed to be created as "index" - to record which time of appointment it was for each patient
    #add a new column guide:
    #1. sort the dataset by "oldest to newest" on column "appointment_dt"
    #2. add a new column "index" with the formula "countif($a$2:a2,a2)", and drag it down to apply to all
    #3. save as a new csv. file
    urology <- read.csv("urology_data.csv")
    urology <- urology[urology$outcome_description!="NULL",]
    allPatient <- vector('list',3)
    allPatient[[1]] <- urology[urology$referral_priority=="2 Week",]
    allPatient[[2]]  <- urology[urology$referral_priority=="Urgent",]
    allPatient[[3]]  <- urology[urology$referral_priority=="Routine",]
    newAppointment <- vector("list",3)
    for (i in 1:3){
      newAppointment[[i]] <- allPatient[[i]][allPatient[[i]]$index==1,]
    }
    firstFU <- vector("list",3)
    for (i in 1:3){
      firstFU[[i]] <- allPatient[[i]][allPatient[[i]]$index==2,]
    }
    FU <- vector("list",3)
    for (i in 1:3){
      FU[[i]] <- allPatient[[i]][allPatient[[i]]$index>2,]
    }

    
    #new to inp1
    for (i in 1:3){
      rates[[i]]["new","inp1"] <- nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty!="NULL",])/nrow(newAppointment[[i]])
    }
    #new to discharge
    for (i in 1:3){
      rates[[i]]["new","discharge"] <- nrow(newAppointment[[i]][newAppointment[[i]]$outcome_description == "Discharged"&newAppointment[[i]]$inp_specialty=="NULL",])/nrow(newAppointment[[i]])
    }
    #new to 1FU
    for (i in 1:3){
      rates[[i]]["new","1FU"] <- nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty=="NULL"&newAppointment[[i]]$outcome_description!="Discharged",])/nrow(newAppointment[[i]])
    }
    #inp1 to 1FU
    for (i in 1:3){
      rates[[i]]["inp1","1FU"] <- nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty!="NULL"&newAppointment[[i]]$outcome_description!="Discharged",])/nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty!="NULL",])
    }
    #inp1 to discharge
    for (i in 1:3){
      rates[[i]]["inp1","discharge"] <- nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty!="NULL"&newAppointment[[i]]$outcome_description =="Discharged",])/nrow(newAppointment[[i]][newAppointment[[i]]$inp_specialty!="NULL",])
    }
    #1FU to inp2
    for (i in 1:3){
      rates[[i]]["1FU","inp2"] <- nrow(firstFU[[i]][firstFU[[i]]$inp_specialty!="NULL",])/nrow(firstFU[[i]])
    }
    #1FU to discharge
    for (i in 1:3){
      rates[[i]]["1FU","discharge"] <- nrow(firstFU[[i]][firstFU[[i]]$inp_specialty=="NULL"&firstFU[[i]]$outcome_description=="Discharged",])/nrow(firstFU[[i]])
    }
    #1FU to FU
    for (i in 1:3){
      rates[[i]]["1FU","FU"] <- nrow(firstFU[[i]][firstFU[[i]]$inp_specialty=="NULL"&firstFU[[i]]$outcome_description!="Discharged",])/nrow(firstFU[[i]])
      
    }
    #FU to discharge
    for (i in 1:3){
      rates[[i]]["FU","discharge"] <- nrow(FU[[i]][FU[[i]]$outcome_description=="Discharged"&FU[[i]]$inp_specialty=="NULL",])/nrow(FU[[i]])
    }
    #FU to FU
    for (i in 1:3){
      rates[[i]]["FU","FU"] <- nrow(FU[[i]][FU[[i]]$outcome_description!="Discharged"&FU[[i]]$inp_specialty=="NULL",])/nrow(FU[[i]])
    }
    #FU to inp3
    for (i in 1:3){
      rates[[i]]["FU","inp3"] <- nrow(FU[[i]][FU[[i]]$inp_specialty!="NULL",])/nrow(FU[[i]])
    }
    #inp2 to FU
    for (i in 1:3){
      rates[[i]]["inp2","FU"] <- nrow(firstFU[[i]][firstFU[[i]]$inp_specialty!="NULL"&firstFU[[i]]$outcome_description!="Discharged",])/nrow(firstFU[[i]][firstFU[[i]]$inp_specialty!="NULL",])
    }
    #inp2 to discharge
    for (i in 1:3){
      rates[[i]]["inp2","discharge"] <- nrow(firstFU[[i]][firstFU[[i]]$inp_specialty!="NULL"&firstFU[[i]]$outcome_description =="Discharged",])/nrow(firstFU[[i]][firstFU[[i]]$inp_specialty!="NULL",])
    }
    #inp3 to FU
    for (i in 1:3){
      rates[[i]]["inp3","FU"] <- nrow(FU[[i]][FU[[i]]$inp_specialty!="NULL"&FU[[i]]$outcome_description!="Discharged",])/nrow(FU[[i]][FU[[i]]$inp_specialty!="NULL",])
    }
    #inp3 to discharge
    for (i in 1:3){
      rates[[i]]["inp3","discharge"] <- nrow(FU[[i]][FU[[i]]$inp_specialty!="NULL"&FU[[i]]$outcome_description =="Discharged",])/nrow(FU[[i]][FU[[i]]$inp_specialty!="NULL",])
    }
  }
}






##set storage lists####
waitingList <- vector("list",3)
for(i in 1:3){
  waitingList[[i]] <- as.data.frame(matrix(0,nrow = future_length + 1,ncol = 6))
  colnames(waitingList[[i]]) <- c("new","1FU","FU","inp1","inp2","inp3")
  waitingList[[i]][1,] <- startSize[,i]
}

inflow <- vector("list",3)
for (i in 1:3){
  inflow[[i]] <- as.data.frame(matrix(0,nrow = future_length,ncol = 6))
  colnames(inflow[[i]]) <- c("new","1FU","FU","inp1","inp2","inp3")
  inflow[[i]]$new <- referrals[,i]
}

capacity <- vector("list",3)
for (i in 1:3) {
  capacity[[i]] <- as.data.frame(matrix(0,nrow = future_length,ncol = 6))
  colnames(capacity[[i]]) <- c("new","1FU","FU","inp1","inp2","inp3")
}
capacity[[1]] <- total_capacity
capacity[[1]][is.na(capacity[[1]])] <- 0

##


##waiting list####
#waiting list function####
waiting_list <- function(capacity,twoWeek_waitingList,twoWeek_inflow,
                         urgent_remain_capacity,urgent_waitingList,urgent_inflow,
                         routine_remain_capacity,routine_waitingList,routine_inflow){
  twoWeek_waitingList_trans <- twoWeek_waitingList[i] + twoWeek_inflow[i] - min(capacity[i],twoWeek_waitingList[i])
  if(twoWeek_waitingList_trans>=0){
    twoWeek_waitingList[i+1] <- twoWeek_waitingList_trans
    urgent_waitingList[i+1] <- urgent_waitingList[i] + urgent_inflow[i]
    routine_waitingList[i+1] <- routine_waitingList[i] + routine_inflow[i]
  } else {
    twoWeek_waitingList[i+1] <- 0
    urgent_remain_capacity[i] <- capacity[i] - twoWeek_waitingList[i] - twoWeek_inflow[i]
    urgent_waitingList_trans <- urgent_waitingList[i] + urgent_inflow[i] - min(urgent_remain_capacity[i],urgent_waitingList[i])
    if(urgent_waitingList_trans>=0){
      urgent_waitingList[i+1] <- urgent_waitingList_trans
      routine_waitingList[i+1] <- routine_waitingList[i] + routine_inflow[i]
    } else {
      urgent_waitingList[i+1] <- 0
      routine_remain_capacity[i] <- urgent_remain_capacity[i] - urgent_waitingList[i] - urgent_inflow[i]
      routine_waitingList_trans <- routine_waitingList[i] + routine_inflow[i] - min(routine_remain_capacity[i],routine_waitingList[i])
      if(routine_waitingList_trans >= 0){#the number of routine people being served = ramained capacity
        routine_waitingList[i+1] <- routine_waitingList_trans
      } else {#all of the routine patients are being served
        routine_waitingList[i+1] <- 0
      } 
    }
  }
  return(c(twoWeek_waitingList[i+1],urgent_waitingList[i+1],routine_waitingList[i+1]))
}
#new appointment stage####
for (i in 1:future_length){
  for (p in 1:3){
    waitingList[[p]]$new[i+1] <- waiting_list(capacity[[1]]$new,waitingList[[1]]$new,inflow[[1]]$new,
                                              capacity[[2]]$new,waitingList[[2]]$new,inflow[[2]]$new,
                                              capacity[[3]]$new,waitingList[[3]]$new,inflow[[3]]$new)[p]
    #inflow to inp1
    inflow[[p]]$inp1[i] <- min(capacity[[1]]$new[i], waitingList[[1]]$new[i] + inflow[[1]]$new[i]) * rates[[p]]["new","inp1"]
    #inflow to 1FU
    inflow[[p]]$`1FU`[i] <- min(capacity[[1]]$new[i], waitingList[[1]]$new[i] + inflow[[1]]$new[i]) * rates[[p]]["new","1FU"]
  }
}
#inp1 stage####
for (i in 1:future_length){
  for (p in 1:3){
    waitingList[[p]]$inp1[i+1] <- waiting_list(capacity[[1]]$inp1,waitingList[[1]]$inp1,inflow[[1]]$inp1,
                                               capacity[[2]]$inp1,waitingList[[2]]$inp1,inflow[[2]]$inp1,
                                               capacity[[3]]$inp1,waitingList[[3]]$inp1,inflow[[3]]$inp1)[p]
    #inflow to 1FU
    inflow[[p]]$`1FU`[i] <- inflow[[p]]$`1FU`[i] + min(capacity[[p]]$inp1[i], waitingList[[p]]$inp1[i] + inflow[[p]]$inp1[i]) * rates[[p]]["inp1","1FU"]
    
  }
}
##

#1FU stage####
for (i in 1:future_length){
  for (p in 1:3) {
    waitingList[[p]]$`1FU`[i+1] <- waiting_list(capacity[[1]]$`1FU`,waitingList[[1]]$`1FU`,inflow[[1]]$`1FU`,
                                              capacity[[2]]$`1FU`,waitingList[[2]]$`1FU`,inflow[[2]]$`1FU`,
                                              capacity[[3]]$`1FU`,waitingList[[3]]$`1FU`,inflow[[3]]$`1FU`)[p]
    #inflow to inp2
    inflow[[p]]$inp2[i] <- min(capacity[[p]]$`1FU`[i], waitingList[[p]]$`1FU`[i] + inflow[[p]]$`1FU`[i]) * rates[[p]]["1FU","inp2"]
    #inflow to FU
    inflow[[p]]$FU[i] <- min(capacity[[p]]$`1FU`[i], waitingList[[p]]$`1FU`[i] + inflow[[p]]$`1FU`[i]) * rates[[p]]["1FU","FU"]
  }
}
##

#inp2 stage####
for (i in 1:future_length){
  for (p in 1:3){
    waitingList[[p]]$inp2[i+1] <- waiting_list(capacity[[1]]$inp2,waitingList[[1]]$inp2,inflow[[1]]$inp2,
                                               capacity[[2]]$inp2,waitingList[[2]]$inp2,inflow[[2]]$inp2,
                                               capacity[[3]]$inp2,waitingList[[3]]$inp2,inflow[[3]]$inp2)[p]
    #inflow to FU
    inflow[[p]]$FU[i] <- inflow[[p]]$FU[i] + min(capacity[[p]]$inp2[i], waitingList[[p]]$inp2[i] + inflow[[1]]$inp2[i]) * rates[[p]]["inp2","FU"]
  }
}
##

#FU stage####
for (i in 1:future_length){
  for (p in 1:3){
    waitingList[[p]]$FU[i+1] <- waiting_list(capacity[[1]]$FU,waitingList[[1]]$FU,inflow[[1]]$FU,
                                             capacity[[2]]$FU,waitingList[[2]]$FU,inflow[[2]]$FU,
                                             capacity[[3]]$FU,waitingList[[3]]$FU,inflow[[3]]$FU)[p]
    #inflow to inp3
    inflow[[p]]$inp3[i] <- min(capacity[[p]]$FU[i], waitingList[[p]]$FU[i] + inflow[[1]]$FU[i]) * rates[[p]]["FU","inp3"]
    
    #inflow to FU
    #inflow[[1]]$FU[i] <- min(capacity[[p]]$FU[i], waitingList[[p]]$FU[i-1] + inflow[[1]]$FU[i]) * rates[[p]]["FU","FU"]
    
  }
}
##
#inp3 stage####
for (i in 1:future_length){
  for (p in 1:3){
    waitingList[[p]]$inp3[i+1] <- waiting_list(capacity[[1]]$inp3,waitingList[[1]]$inp3,inflow[[1]]$inp3,
                                               capacity[[2]]$inp3,waitingList[[2]]$inp3,inflow[[2]]$inp3,
                                               capacity[[3]]$inp3,waitingList[[3]]$inp3,inflow[[3]]$inp3)[p]
    ##inflow to FU
    #inflow[[1]]$FU[i] <- inflow[[1]]$FU[i] + min(capacity[[p]]$inp3[i], waitingList[[p]]$inp3[i-1] + inflow[[1]]$inp3[i]) * rates[[p]]["inp3","FU"]
  }
}
##

##waiting time####
#waiting time function####
waiting_time <- function(capacity,waitingList_type){
  waitingTime <- vector("numeric",future_length)
  for (i in 1:future_length){
    overflow <- 0
    m <- i
    total_out <- capacity[m]
    if (waitingList_type[i] == 0){
      waitingTime[i] <- 0
    } else {
      repeat{
        if(waitingList_type[i] <= total_out){
          break
        }
        m <- m+1
        if (m == future_length+1){
          overflow <- 1
          break
        }
        total_out <- total_out + capacity[m]
      }
      if (overflow == 1){
        waitingTime[i] <- NA
      } else {
        waitingTime[i] <- m - i + 1
      }
    }
  }
  return(waitingTime)
}

waitingTime_stage <- vector("list",3)
for (i in 1:3) {
  waitingTime_stage[[i]] <- as.data.frame(matrix(0,nrow = future_length,ncol = 6))
  colnames(waitingTime_stage[[i]]) <- c("new","1FU","FU","inp1","inp2","inp3")
  for (p in 1:6){
    waitingTime_stage[[i]][,p] <- waiting_time(capacity[[i]][,p],waitingList[[i]][,p])
  }
}

waitingTime <- vector("list",3)
for (i in 1:3){
  waitingTime[[i]] <- as.data.frame(matrix(0,ncol = 14,nrow = future_length))
  colnames(waitingTime[[i]]) <- c("pw1","pw2","pw3","pw4","pw5","pw6","pw7",
                                  "pw8","pw9","pw10","pw11","pw12","pw13","pw14")
  
  #1.new
  waitingTime[[i]][,1] <- waitingTime_stage[[i]]$new
  #2.new -> 1FU
  for (k in 1:future_length){
    waitingTime[[i]][,2][k] <- waitingTime[[i]][,1][k] + waitingTime_stage[[i]][,2][waitingTime[[i]][,1][k]+k]
  }
  #3.new -> 1FU -> FU
  for (k in 1:future_length){
    waitingTime[[i]][,3][k] <- waitingTime[[i]][,2][k] + waitingTime_stage[[i]][,3][waitingTime[[i]][,2][k]+k]
  }
  #4.new -> 1FU -> FU -> inp3
  for (k in 1:future_length){
    waitingTime[[i]][,4][k] <- waitingTime[[i]][,3][k] + waitingTime_stage[[i]][,6][waitingTime[[i]][,3][k]+k]
  }
  #5.new -> 1FU -> inp2
  for (k in 1:future_length){
    waitingTime[[i]][,5][k] <- waitingTime[[i]][,2][k] + waitingTime_stage[[i]][,5][waitingTime[[i]][,2][k]+k]
  }
  #6.new -> 1FU -> inp2 -> FU
  for (k in 1:future_length){
    waitingTime[[i]][,6][k] <- waitingTime[[i]][,5][k] + waitingTime_stage[[i]][,3][waitingTime[[i]][,5][k]+k]
  }
  #7.new -> 1FU -> inp2 -> FU -> inp3
  for (k in 1:future_length){
    waitingTime[[i]][,7][k] <- waitingTime[[i]][,6][k] + waitingTime_stage[[i]][,6][waitingTime[[i]][,6][k]+k]
  }
  #8.new -> inp1
  for (k in 1:future_length){
    waitingTime[[i]][,8][k] <- waitingTime[[i]][,1][k] + waitingTime_stage[[i]][,4][waitingTime[[i]][,1][k]+k]
  }
  #9.new -> inp1 -> 1FU
  for (k in 1:future_length){
    waitingTime[[i]][,9][k] <- waitingTime[[i]][,8][k] + waitingTime_stage[[i]][,2][waitingTime[[i]][,8][k]+k]
  }
  #10.new -> inp1 -> 1FU -> FU
  for (k in 1:future_length){
    waitingTime[[i]][,10][k] <- waitingTime[[i]][,9][k] + waitingTime_stage[[i]][,3][waitingTime[[i]][,9][k]+k]
  }
  #11.new -> inp1 -> 1FU -> inp2
  for (k in 1:future_length){
    waitingTime[[i]][,11][k] <- waitingTime[[i]][,9][k] + waitingTime_stage[[i]][,5][waitingTime[[i]][,9][k]+k]
  }
  #12.new -> inp1 -> 1FU -> FU -> inp3
  for (k in 1:future_length){
    waitingTime[[i]][,12][k] <- waitingTime[[i]][,10][k] + waitingTime_stage[[i]][,6][waitingTime[[i]][,10][k]+k]
  }
  #13.new -> inp1 -> 1FU -> inp2 -> FU
  for (k in 1:future_length){
    waitingTime[[i]][,13][k] <- waitingTime[[i]][,11][k] + waitingTime_stage[[i]][,3][waitingTime[[i]][,11][k]+k]
  }
  #14.new -> inp1 -> 1FU -> inp2 -> FU -> inp3
  for (k in 1:future_length){
    waitingTime[[i]][,14][k] <- waitingTime[[i]][,13][k] + waitingTime_stage[[i]][,6][waitingTime[[i]][,13][k]+k]
  }
}
##outputs####
{patient_type <- as.numeric(readline(prompt = "patients type (1/2/3)(1 for two week, 2 for urgent, 3 for routine): "))
pathway <- as.numeric(readline(prompt = "No. pathway: "))
print(waitingTime[[patient_type]][,pathway])
plot(waitingTime[[patient_type]][,pathway],type = "l")}








#change capacity####
{change_index <- readline(prompt = "Do you want to change capacity? (yes/no): ")
  if (change_index == "yes") {
    change_type <- readline(prompt = "Which type of capacity? (new/1FU/FU/inp1/inp2/inp3): ")
    change_week <- as.numeric(readline(prompt = "Which week? "))
    change_capacity <- as.numeric(readline(prompt = "new capacity:"))
    if (change_type == "new"){
      capacity$new[change_week] <- change_capacity
    } else {
      if (change_type == "1FU") {
        capacity$`1FU`[change_week] <- change_capacity
      } else {
        if (change_type == "FU"){
          capacity$FU[change_week] <- change_capacity
        } else {
          if (change_type == "inp1"){
            capacity$inp1[change_week] <- change_capacity
          } else {
            if (change_type == "inp2") {
              capacity$inp2[change_week] <- change_capacity
            } else {
              if (change_type == "inp3") {
                capacity$inp3[change_week] <- change_capacity
              }
            }
          }
        }
      }
    }
  } }