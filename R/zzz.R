#' internal

rotate_vec <- function(v, thete){
  return(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow = T, ncol = 2) %*% v)
}

rotate_X <- function(X, theta){
  X %*% t(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), byrow = F, ncol = 2))
}

decision_half_circle <- generate_decision_function(boundary_half_circle)

center <- function(X, center){
  return(X - center)
}

uncenter <- function(X, center){
  return(X + center)
}

scale_unit <- function(X, min, max){
  if(any(is.na(max - min))){
    warning(sprintf("denominator has zero element whab scaling"))
  }
  X <-  2 * (X - min)/(max - min) - 1
  return(X)
}

unscale_unit <- function(X, min, max){
  return((X + 1)/2 * (min - max) + min)
}
