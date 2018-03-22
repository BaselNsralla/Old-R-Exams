library('stringr')
library('lubridate')


q1_a <- function (a, b){
  return((factorial(a)-b^a)^-0.1)
}

q1_b <- function() {
  data(OrchardSprays)
  my_df <- data.frame()
  a <- as.numeric(row.names.data.frame(OrchardSprays))
  my_df <- OrchardSprays[a%%2 != 0, 1:2]
  return(my_df)
}

q1_c <- function() {
  l  <- c(rep(letters, 1000))
  return(l)
}

q1_d <- function() {
  m <- matrix(c(1,2,3,2,1), nrow=5, ncol = 3)
}


q2_a <- function() {
  rows <- 7
  for(i in 1:rows){
    print(rep('xyz', rows-i + 1))
    print(rep('abc', i))
  }
}

q2_b <- function() {
  print('this is wierd')
  suc <- 0
  itr <- 1
  while(suc <= 50) {
    suc <- itr + suc
    itr <- itr + 1
    if (suc%%2 == 0) {
      print('GO!')
    } else if(suc%% 5 == 0){
      print(sin(suc)) 
    } else {
      print(suc)
    }
  }
}


q3_a <- function () {
  
}
