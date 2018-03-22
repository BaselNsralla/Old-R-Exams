
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
  ?matrix
  m <- matrix(c(1,2,3,2,1), nrow=5, ncol = 3)
}
