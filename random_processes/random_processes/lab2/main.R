while(TRUE){
  for(i in 1:3){
    cat("Enter", i, "to check", i, "task\n") 
  }
  cat("Enter 0 to exit\n")
  choice <- scan(n=1)
  if(choice == 1) source('task1.R')
  else if(choice == 2) source('task2.R')
  else if(choice == 3) source('task3.R')
  else if(choice == 0) break
}