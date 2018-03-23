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


q3_b <- function () {
  file_path <- paste0(getwd(),'/Dev/R/old_r_exams/2017_08_14/robot.txt')
  robot_txt <- readLines(con=file_path)
  print(robot_txt)
  str_detect(robot_txt, '\\Wthe\\W')
}

q3_c <- function () {
  vote_right_date <- ymd("1919-5-24")
  second_date <- ymd("1921-12-09")
  poland_date <- ymd("1918-11-28")
  todays_date <- ymd("2018-03-22")
  i  <-  wday(vote_right_date,label = TRUE)
  ii <-  days(second_date - vote_right_date)
  iii <- floor(days(second_date - poland_date)@day / 7)
  iv <- as.period(interval(vote_right_date, todays_date), unit = 'month')
}


my_curve(x= c(-1,0,1,2,4.5), a= 5)
my_curve <- function (x, a=3) {
  m <- sapply(x, FUN=function(num){
    if (num <= -2) {
      return(4)
    } else if(x > -2 && x < 1) {
      return(num^2)
    } else {
      return(6-(a*num))
    }
  }, simplify=TRUE)
  return(m)
}

curve(expr= my_curve, from = -3, to = 3)

my_var(x=1:100)
my_var <- function (x) {
  sum <- 0
  m <- mean(x)
  for (i in x) {
    sum <- sum + (i-m)^2
  }
  return((1/(length(x)-1))*sum)
}
