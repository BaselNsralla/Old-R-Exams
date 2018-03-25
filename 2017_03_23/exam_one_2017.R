library('dplyr')
library('stringr')
library('lubridate')
q1_a_b_c <- function () {
  name <- c('Kalle', 'Jane', 'Andrej', 'Lisa', 'Kim')
  analysis <- c(9, 19, 6, 8, 11)
  sociology <- c(5, 13, 22, 27, 13)
  sex <- c('Male', 'Female', 'Male', 'Female', 'Female')
  studenter <- data.frame(name, analysis, sociology, sex)

  
  # Find people who passed
  index_calc <- studenter$analysis >= 10
  index_calc
  studenter
  passes <- studenter[index_calc, 'name']
  passes
 
  data(ChickWeight) 
  time_21 <- filter(ChickWeight, ChickWeight$Time == 21)
  
  res <- as.data.frame(group_by(time_21, Diet) %>% summarise(Weight = mean(weight)))
  res
}

q2_a <- function() {
  for (i in -150:300) {
    if (i%%127 == 0) {
      print(paste0(i, ' can be devided by 127'))
    }
    if (i%%61) {
      print(paste0(i, ' can be devided by 61'))
    }
  }
  old_x <- 1
  x <- 2
  while (x <= 1000) {
    tmp_x <- x
    x <- 1 - x + old_x^2
    old_x <- tmp_x
  }
  print(x)
}



q3_a_b_c <- function () {
  born <- ymd('1412/01/06')
  prisoned <- ymd('1430/05/23')
  burned <- ymd('1431/05/30')
  wday(prisoned, label= TRUE)
  days_in_prison <- interval(prisoned, burned) / days(1)
  days_in_alive <- interval(born, burned) / months(1)
  days_in_alive
  
  
  ?readLines
  text <- readLines("https://raw.githubusercontent.com/STIMALiU/KursRprgm/master/OldExams/20170323/brb3kap.txt")
  text <- str_to_lower(paste(text, collapse=''))
  text
  str_match_all(text, '[\\W|^]lag[\\W|$]')  
}

gcd <- function (A,b) {
  if (B == 0) {
    return(A)
  } else if(A > B){
    A <- A - B
    return(gcd(A, B))
  } else {
    B <- B - A
    return(gcd(A, B))
  }
} 


?runif
my_rexp <- function (n ,rate){
  draws <- runif(n)
  TT <- -log(draws) / rate
  print(TT)
  return(TT)
}
hist(my_rexp(1000, 0.1))
