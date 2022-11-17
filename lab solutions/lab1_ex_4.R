my.numbers <- c(1, 2, 3, 4, 5, 6)

#For loop to generate lottery numbers and prize winnings
prize <- numeric(5200)
for(i in 1:5200){
  
  #Generate lottery numbers
  draw <- sample(1:50, 6)
  
  #Check how many match my numbers
  numbers.matched <- sum(my.numbers %in% draw)
  
  #Compute prize winings
  if(numbers.matched < 3)
    prize[i] <- 0
  else if(numbers.matched == 3)
    prize[i] <-  30
  else if(numbers.matched == 4)
    prize[i] <-  140
  else if(numbers.matched == 5)
    prize[i] <-  1750
  else
    prize[i] <- 1000000
}

#Summarise prize winnings
table(prize)
hist(prize)
sum(prize) - 2*5200
