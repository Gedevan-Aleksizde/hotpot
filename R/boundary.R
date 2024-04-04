#' @export
boundary_half_circle <- function(x, .r = sqrt(2)/2, a = 0, b = 0){
  return(ifelse(x < 0, sqrt(.r^2 - (x + .r - a)^2) + b, -sqrt(.r^2 - (x - .r - a)^2 + b)))
}

#' @export
boundary_sigmoid <- function(x, a = 0, b = 1, intercept = -.5){
  return(1/(1 + exp(-(x -a)/b)) + intercept)
}
