library(tidyr)
library(ggplot2)

q1  <- function() {
  names <- c('Mike', 'Luke', 'Adrian', 'Sonja')
  wage <- c(24000, 17000, 31000, 36000)
  manager <- c(TRUE, FALSE, FALSE, TRUE)
  staff <- list(names=names, wage = wage, manager = manager)
  higher_wage <- staff[[1]][wage > 30000]
  indicies <- manager == FALSE & wage > 20000
  wage_cp <- staff[[2]][indicies]
  wage_cp <- wage_cp * 0.9
  wage_cp
  staff[[2]][indicies] <- wage_cp
}

q2 <- function() {
  vector <- c(3,2,12,14)
  for (i in vector){
    print(i)
  }
  sum <- 0 
  for (i in vector) {
    sum <- i + sum
  }
  limit <- 0
  while(limit <= 0.9) {
    limit <- runif(1,min = 0, max = 1)
    statement <- paste0(limit, ' is samller than 0.9')
    print(statement)
  }
}

q3_a <- function (stick) {
  return(sd(stick)/mean(stick))
}

q3_b <- function () {
  return(rexp(10, 1/2))
}

q3_c <- function () {
  obs <- c()
  for (i in c(1:100)) {
    m <- q3_b()
    obs[i] <- q3_a(m)
    print(obs)
  }
  df <- data.frame(x = obs)
  ggplot(df, aes(x = df)) + geom_histogram(aes(fill = 'blue'))
}

factor_to_int <- function(fac) {
  return(as.character(as.integer(fac)))
}

q4 <- function () {
  frame <- as.data.frame(read.table('https://raw.githubusercontent.com/STIMALiU/KursRprgm/master/OldExams/20120330/fishes.txt'))
  colnames(frame) <- c('length', 'age', 'watertemp')
  frame <- frame[-1,]
  frame$length <- factor_to_int(frame$length)
  frame$age <- factor_to_int(frame$age)
  frame$watertemp <- factor_to_int(frame$watertemp)
  model <- lm(frame$length ~ frame$age + frame$watertemp)
  res <- residuals(model)
  df <- data.frame(x=res, y= frame$age)
  ggplot(df, aes(x=x, y=y)) + geom_point(aes(color='red'),shape=3) + ylab('nice resudlas')
}

q5 <- function() {
  seq <- seq(0, 3.14, by=0.03)
  seq_x <- seq[seq < 3.06]
  sin_y <- sin(seq_x)
  df <- data.frame(x= seq_x, y=sin_y)
  ggplot(df, aes(x= x, y = y)) + geom_line()
}

plot_function <- function(applied, interval, nPoints) {
  xValues <- seq(interval[1], interval[2], nPoints)
  yValurs <- applied(xValues)
  plot(xValues, yValues, type='l')
}





