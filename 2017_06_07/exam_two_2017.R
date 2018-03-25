library(stringr)
library(lubridate)

q1_a <- function () {
  r <- round((1 - cos(pi*2.3))^(1-sqrt(7)), 4)
}

q1_b <- function() {
  data(iris)
  data(AirPassengers)
  data(trees)
  l <- list(iris = iris, AirPassengers=AirPassengers, trees = trees)
  l
}

q1_c <- function () {
  my_data <- rnorm(5*3*2, mean=14,sd=2)
  my_data
  my_array <- array(my_data, c(5,3,2))
  my_array
  dim(my_array)
}

q1_d <- function () {
  indecies <- iris$Species=="versicolor"
  data.frame(Sepal.Length = iris$Sepal.Length[indecies], Petal.Length = iris$Petal.Length[indecies], Species = iris$Species[indecies])
}

q2_a <- function () {
  X <- matrix(0, nrow= 3, ncol= 4)
  for (i in 1:nrow(X)) {
    for (j in 1:ncol(X)) {
      X[i, j] <- (i-2.5)^j
    }
  }
}

q2_b <- function() {
  current_area <- 0
  r <- 1
  while (current_area < 1000) {
    current_area <- (pi * r^2) + current_area
    if (current_area < 1000) {
      print(current_area)
    }
    r <- r+1
  }
}

q3_b <- function() {
  file_path <- paste0(getwd(),'/Dev/R/old_r_exams/2017_08_14/robot.txt')
  robot <- readLines(con=file_path)
  cat(robot)
  str_extract_all(robot, '[A-Za-z\\d]{14}')
}

q3_c <- function () {
  kaffe <- read.csv("https://raw.githubusercontent.com/STIMALiU/KursRprgm/master/OldExams/20170607/kaffe_data.csv")
  kaffe$mon <- sapply(kaffe$datum, function(datum) {
   
    return( month(as.Date(datum)))
  })
  ?sort
  i <-aggregate(kaffe$kaffe, by=list(kaffe$mon), mean)
  i[agg$x == max(i$x),][,1]
  
  max_kaffe <- as.Date(kaffe[kaffe$kaffe == max(kaffe$kaffe), 'datum'])
  min_kaffe <- as.Date(kaffe[kaffe$kaffe == min(kaffe$kaffe), 'datum'])
  
  ii <- wday(as.Date(kaffe[kaffe$kaffe == min(kaffe$kaffe), 'datum']), label=TRUE)
  iii <- interval(min_kaffe, max_kaffe) / weeks(1)
  perfect <- interval(ymd('1994-01-01'), ymd('1995-12-31'))
  
  # FUnkar i o med att sakerna är sorterade
  index<-which(kaffe$datum=="1994-01-01"):which(kaffe$datum=="1995-12-31")
  iv <- sd(kaffe$kaffe[index])
}

x1 <- c("(a+b)^2", "(2+6)^2", "(d-t)^2")
qq4 <- q4(x1)
print(qq4 )

str_contains <- function (x, pattern) {
 aa <- as.vector(str_match(x, pattern))
 if (is.na(aa)) {
   return(FALSE)
 }
 return(TRUE)
}

q4 <- function(x) {
  x <- sapply(x, function(exp) {
    mtc <- as.vector(str_match(exp,'\\((.+)\\)'))[2]
    operator_type <- as.vector(str_match(mtc, '\\+|-'))
    real_numbers <- unlist(str_split(mtc, '\\+|-'))
    if(str_contains(real_numbers[1], '\\d') ){
       part1 <- as.character(as.numeric(real_numbers[1])^2)
    } else {
      part1 <- paste0(real_numbers[1],'^2')
    } 
    
    if(str_contains(real_numbers[2], '\\d')){
      part3 <- as.character(as.numeric(real_numbers[2])^2)
    } else {
      part3 <- paste0(real_numbers[2],'^2')
    }
    
    if (str_contains(real_numbers[1], '\\d') && str_contains(real_numbers[2], '\\d')) {
      part2 <- as.character(as.numeric(real_numbers[1])*as.numeric(real_numbers[2])*2)
    } else {
      part2 <- paste0('2*',real_numbers[1],'*',real_numbers[2])
    }
    parts <- paste0(part1,operator_type, part2, '+', part3)
    print(paste0(exp, ' = ', parts))
    return(paste0(exp, ' = ', parts))
  }, USE.NAMES = FALSE)
  
  return(x)
}

q5 <- function () {
  aa <- read.csv("/Users/fdsfasddsfsdff/Dev/R/old_r_exams/fmr.csv")
  ## THE CSV IS BROKEN'
  X <- as.matrix(aa[,2:length(aa[,1])])
  y <- as.matrix(aa[,1])
  XT <- t(X)
  II <- diag(length(X[1,]))
  b_hat_1 <- solve(XT%*%X + 5*II)*XT%*%y
  b_hat_2 <- solve(XT%*%X + 100*II)*XT%*%y
  y_hat_1 <- X%*%b_hat_1
  y_hat_2 <- X%*%b_hat_2
  time <- 1:72
  aa$time <- time
  ggplot(aes(x = aa$time, y = y)) + geom_line(aes(y = y_hat_1, color = 'blue')) +
    geom_line(aes(y = y_hat_2, color = 'red'))
}
 



