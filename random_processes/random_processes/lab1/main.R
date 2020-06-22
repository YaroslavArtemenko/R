while(TRUE){
  for(i in 1:5){
    cat("Enter", i, "to solve", i, "question : \n") 
  }
  cat("Enter 0 to end this session : \n")
  choice <- scan(n=1)
  if(choice == 1) source('task1.R')
  else if(choice == 2) source('task2.R')
  else if(choice == 3) source('task3.R')
  else if(choice == 4) source('task4.R')
  else if(choice == 5) source('task5.R')
  else if(choice == 0) break
}