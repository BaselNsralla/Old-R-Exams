library('lubridate')
library('stringr')
library('ggplot2')

q1 <- function(x=10) {
  a <- exp(x-3) * x^(3-log(x))
  b <- list(NULL, NULL, NULL, NULL, NULL)
  logi <- rep(c(TRUE, FALSE), 5) 
  char <- letters[1:10]
  num <- c(10:1)
  c <- data.frame(num, logi, char)
}

q2 <- function() {
  for (i in 1:2) {
    print(paste0('yttre index ', i))
    for (j in 1:4) {
      print(paste0('inre index ', j))
    }
  }
  
  findings <- 0
  seeked <- 17
  while (findings < 5) {
    if (seeked %% 17 == 0) {
      findings <- findings + 1
      print(seeked)
    }
    seeked <- seeked + 1
  }
}

q3_a_b <- function () {
  text <- readLines('https://raw.githubusercontent.com/STIMALiU/KursRprgm/master/OldExams/20160615/R_IDE.txt')
  list <- str_match_all(text, '(^[A-Z][A-Za-z\\s]+)\\-\\s')
  IDE <- lapply(list, function(element){
    return(element[1,2])
  })
  IDE <- unlist(IDE)
}

q3_c <- function() {
  first_date  <- ymd('1992-12-02')
  second_date <- ymd('2016-06-15')
  first_to_second <- interval(first_date, second_date)
  all_days <- seq(first_date, second_date, 1)
  sum(wday(all_days) == 3)
}

data(trees)
a1 <- lm(Volume~Girth, data = trees)

q3_d(a1)
q3_d <- function(lm_model) {
   return(paste0("y = ", round(lm_model[1]$coefficients[1],2), ' + ', round(lm_model[1]$coefficients[2],2), '*', attr(lm_model$terms, "term.labels")))
}

q4_a <- function(n, lambda) {
  return((exp(-lambda)* lambda^n)/factorial(n))
}

q4_b <- function(x, a=1, b=1) {
  all <- sapply(x, function(el) {
    if (el > 0) {
      return(a*exp(-el))
    } else if (el < 0) {
      return(sin(b*el)/el)
    }
    return(1)
  })
  return(all)
}

q4_c <- function(path, func=mean, col) {
  df <- read.csv(path)
  apply(df, 2, func)
}

q5 <- function() {
  data(CO2)
  randoms <- rt(250, 7, 12)
  ggplot(CO2, aes(x = conc, y =uptake)) + geom_point(aes(color=Treatment))
  df <- as.data.frame(read.table('https://raw.githubusercontent.com/STIMALiU/KursRprgm/master/OldExams/20160615/HUS_eng.txt', header=TRUE,sep=',', dec="."))
  df
  ggplot(df, aes(x=no.bedroom, y=air.condition)) + geom_bar(stat='identity') 
}





