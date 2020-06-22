while(TRUE){
  for(i in 1:7){
    cat("Нажмите", i, "чтобы выбрать", i, "вопрос : \n") 
  }
  cat("Нажмите 0 чтобы закончить выпонение : \n")
  choice <- scan(n=1)
  if(choice == 1) source('лаба1-1.R')
  else if(choice == 2) source('лаба1-2.R')
  else if(choice == 3) source('лаба1-3.R')
  else if(choice == 4 & choice == 5) source('лаба1-4,5.R')
  else if(choice == 6) source('лаба1-6.R')
  else if(choice == 7) source('лаба1-7.R')
  else if(choice == 0) break
}

