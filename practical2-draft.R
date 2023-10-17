qsim <- function(mf,mb,a.rate,trb,trf,tmb,tmf,maxb,time=7200,stop_queue=1800){
  ## Time and stop_queue is set as a default value to limited the interest within 2 hour and defined that check-in closes 30 minutes before departure. 
  
  ## Initialization
  French_queue <- array(0, dim = mf) ## number of cars in each queue in French Passport control stations 
  British_queue <- array(0, dim = mb) ## number of cars in each queue in British Passport control stations
  French_time <- array(0, dim = mf) ## time is still needed for processing of each queue in French Passport control stations 
  British_time <- array(0, dim = mb) ## time is still needed for processing of each queue in British Passport control stations 
  
  ## Initialize the return list
  nf <- numeric(0)
  nb <- numeric(0)
  eq <- numeric(0)

  ## Start iterations
  for (i in 1:time){
    
    ## testing! 
    # if(100<i && i<150){
    #   print(French_queue)
    #   print(French_time)
    #   print(British_queue)
    #   print(British_time)
    # }
    
    ## Decide if there is new car will come, if it will come, distribute the car to the shortest queue
    if (i<time-stop_queue && runif(1)< a.rate){
      French_join <- which.min(French_queue)
      French_queue[French_join] <- French_queue[French_join]+1
      if(French_time[French_join] <= 0){
        French_time[French_join] <- runif(1, min = tmf, max = tmf+trf) ## model the time as uniform
      }
    }
    
    ## Initialize a variable to store an index
    smallest_available_ind <- c(0)
    
    ## Reduce one second for the processing time and check if there is a car need to move
    if (i > 1){
      ## Reduce the processing time
      processing_french_ind <- which(French_time>0)
      French_time[processing_french_ind] <- French_time[processing_french_ind] - 1
      processing_british_ind <- which(British_time>0)
      British_time[processing_british_ind] <- British_time[processing_british_ind] - 1
      
      ## Find which queue in French has finished
      French_finished_ind <- intersect(which(French_queue>0),which(French_time<=0))
      
      ## Check the number of queues in British is available
      british_avaliable_ind <- which(British_queue < maxb)
      if (length(French_finished_ind)>0 && length(british_avaliable_ind)>0){
        if (length(French_finished_ind) <= length(british_avaliable_ind)){
          smallest_available_ind <- order(British_queue, decreasing = FALSE)[1:length(French_finished_ind)]
        }else{
          ## In case where the number of the queue in British is smaller than that in French
          smallest_available_ind <- order(British_queue, decreasing = FALSE)[1:length(british_avaliable_ind)]
          French_finished_ind <- sample(French_finished_ind, length(british_avaliable_ind))
        }
      }
      
      
      ## Find how cars move from French Control Center to British Control Center
      French_queue[French_finished_ind] <- French_queue[French_finished_ind] - 1
      
      ## See if the one leaving is the last car in the queue. The processing time won't be updated if the car is the last one.  
      French_update_ind <- intersect(French_finished_ind, which(French_queue>0))
      French_time[French_update_ind] <- runif(length(French_update_ind), min = tmf, max = tmf+trf)
      
      ## Drive the car to British queue
      British_queue[smallest_available_ind] <- British_queue[smallest_available_ind] + 1
    
      
      ## Check the British Control Center and set the British processing time for next car
      british_ini_ind <- intersect(intersect(which(British_time <= 0), which(British_queue > 0)),smallest_available_ind)
      British_time[british_ini_ind] <- runif(length(british_ini_ind), min = tmb, max = tmb+trb)
      
      ## Delete the finished car of the whole checking process
      queueed_index <- which(British_queue > 0)
      finished_index <- which(British_time <= 0)
      british_ini_index <- intersect(queueed_index, finished_index)
      British_queue[british_ini_index] <- British_queue[british_ini_index] - 1
      
    }
    
    
    ## Record average queue time and expected waiting time in each new model
    nf <- append(nf, mean(French_queue))
    nb <- append(nb, mean(British_queue))
    eq <- append(eq, mean(French_queue)*mean(French_time)+mean(British_queue)*mean(British_time))
    
  }
  
  
  ## Return a list with 3 named elements
  return_list <- list(nf = nf, nb = nb, eq = eq)
  return(return_list)
  
}

## Start simulation
return_list <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

## Read the named element form return list
nf <- return_list$nf
nb <- return_list$nb
eq <- return_list$eq


## Start simulation with tmb is changed to 40
return_list2 <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)

## Read the named element form return list
nf2 <- return_list2$nf
nb2 <- return_list2$nb
eq2 <- return_list2$eq


## Produce a 4 panel 2 row, 2 column plot
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

## For better visualization, these graphs are plotted in minutes instead of seconds
## Figure 1 : how the French and British queue lengths are changing over time in minutes 
plot(NULL, xlim = c(1,7200/60), ylim = c(1,max(nf)), type = "n", xlab = "Minutes", ylab = "Average car in queue")
lines(nf[seq(1,length(nf),by=60)], col = "blue", lwd = 2)
lines(nb[seq(1,length(nb),by=60)], col = "red", lwd = 2)
legend("topleft", legend = c("French", "British"), col = c("blue", "red"), lwd = 2)
title(main = "Queue Changes of French & British")

## Figure 2: how the expected queuing time changes over time in minutes
plot(NULL, xlim = c(1,7200/60), ylim = c(1,max(eq)), type = "n", xlab = "Minutes", ylab = "Waiting Time (second)")
lines(eq[seq(1,length(eq),by=60)], col = "green", lwd = 2)
title(main = "Expected Queuing Time")

## Figure 3: how the French and British queue lengths are changing over time in minutes when tmb = 40
plot(NULL, xlim = c(1,7200/60), ylim = c(1,max(nf2)), type = "n", xlab = "Minutes", ylab = "Average car in queue")
lines(nf2[seq(1,length(nf2),by=60)], col = "blue", lwd = 2)
lines(nb2[seq(1,length(nb2),by=60)], col = "red", lwd = 2)
legend("topleft", legend = c("French", "British"), col = c("blue", "red"), lwd = 2)
title(main = "Queue Changes of French & British, tmb = 40")

## Figure 4: how the expected queuing time changes over time in minutes when tmb = 40
plot(NULL, xlim = c(1,7200/60), ylim = c(1,max(eq2)), type = "n", xlab = "Minutes", ylab = "Waiting Time (second)")
lines(eq2[seq(1,length(eq2),by=60)], col = "green", lwd = 2)
title(main = "Expected Queuing Time, tmb = 40")


## Estimate the probability of at least one car missing the ferry departure 
car <- 0
for (i in 1:100){
  return_list_missing <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)
  if (return_list_missing$eq[7200] != 0){
    car <- car + 1
  }
}

cat("The probability of at least one car missing the ferry departure is ", car, "%")
