library('lubridate')
library('stringr')
library('ggplot2')

q1_a <- function (x = 0.5) {
  return(sin(x^3/log(x, 10)))
}

q1_b <- function() {
  data(trees)
  varians_variable <- trees$Height[trees$Girth >= 11]
  return(var(varians_variable))
}

q1_c <- function (lambda = 2) {
  set.seed(1:5)
  a <- c()
  a <- rpois(10*3, lambda = lambda)  
  mat <- matrix(a, nrow = 3, ncol = 10)
  return(mat)
}

q1_d <- function() {
  ls <- list(y = q1_a(), my_var = q1_b(), my_mat = q1_c())    
  my_list <- ls
  return(my_list)
}

q2_a <- function() {
  nrow <- 3
  ncol <- 4
  mat <- matrix(NA, nrow= nrow, ncol= ncol)
  for(i in 1:3) {
    for (j in 1:4) {
      mat[i,j] <- (j * cos(pi*i) / i)
    }
  }
}

q2_b <- function() {
  seeker <- 0
  count <- 0
  while(count < 13){
    if (seeker %% 13 == 0) {
      print(seeker)
      count <- count + 1
    }
    seeker <- seeker + 1
  }
}

q3_a <- function() {
  albert_born   <- ydm('1879-14-03')
  albert_died   <- ydm('1955-18-04')
  florence_born <- ydm('1820-12-05')
  florence_died <- ydm('1910-13-08')
  i   <- interval(florence_born, (albert_born-days(1))) %/% weeks(1)
  ii  <- interval(albert_born, florence_died) %/% months(1)
  iii <- interval(albert_born, albert_died) %/% days(1) + interval(florence_born, florence_died) %/% days(1)
  iv <- wday(florence_died, label=TRUE)
}

q3_b <- function(f) {
  splitted <- str_split(f,'x\\^')
  a <- splitted[1]
  n <- splitted[2]
  der_n <- as.numeric(n) - 1
  der_a <- as.numeric(a) * der_n
  return(paste0('dy/dx=',der_a,'x^',der_n))
}

q4_a <- function(x, alpha) {
  res <- c()
  if (all(x == c(0) | x == c(1))) {
    res[1] <- mean(x) + qnorm(1-alpha/2, mean=mean(x),sd=sd(x)) * sqrt(1-mean(x)/length(x))
    res[2] <- mean(x) - qnorm(1-alpha/2, mean=mean(x),sd=sd(x)) * sqrt(1-mean(x)/length(x))
  } else {
    res[1] <- mean(x) + qnorm(1-alpha/2, mean=mean(x),sd=sd(x)) * (sd(x)/sqrt(length(x)))         
    res[2] <- mean(x) - qnorm(1-alpha/2, mean=mean(x),sd=sd(x)) * (sd(x)/sqrt(length(x)))           
  }
  return(res)
}

q5_a <- function(x) {
  res <- list()
  for (i in 1:length(x)) {
    res[[i]] <- summary(x[[i]])
  }
  return(res)
}

q6 <- function() {
  data(PlantGrowth)
  PlantGrowth$group
  testing <- t.test(PlantGrowth$weight[PlantGrowth$group == 'trt2'], PlantGrowth$weight[PlantGrowth$group == 'trt1'] ,alternative=c('two.sided'), paired = FALSE)
  testing[[4]][1]
  testing[[4]][2]
  ggplot(mpg, aes(x=mpg$cty, y=displ)) + geom_point(color='red') + facet_grid(drv ~.) + geom_smooth(method="lm")
  barplot(table(mpg$drv), col="red")
}



