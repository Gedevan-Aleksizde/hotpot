#' Hotpot learner
#' @export
hotpot <- function(
    X, y, scale = T, boundary_function = boundary_half_circle, lower = c(theta = -pi), upper = c(theta = pi), verbose = F,
    method =  c("Brent", "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")){
  method <- method[1]
  if("data.frame" %in% class(X)){
    message("Argument X should be a matrix, but a data.frame is given. only the first 2 columns used.")
    X <- as.matrix(X[, 1:2])
  }
  if(NCOL(X) != 2){
    stop(sprintf("Matrix X should have just 2 columns, but X has %d columns", NCOL(X)))
  }
  if(NROW(X) != NROW(y)){
    stop("Matrix X and y have wrong length, but X has %d rows and y has %d rows", NROW(X), NROW(Y))
  }
  if(scale){
    col_cent <- colMeans(X)
    col_min <- apply(X, 2, min)
    col_max <- apply(X, 2, max)

    X_ <- center(X, col_cent)
    X_ <- scale_unit(X_, col_min, col_max)
  }
  else {
    col_cent <- NA
    col_min <- NA
    col_max <- NA
    X_ <- X
  }
  objective_function <- generate_decision_function(boundary_function)
  obj_function <- function(params, X, y, decision_function = objective_function){
    X <- rotate_X(X, params[1])
    v <- mean(apply(X, 1, decision_function) == y, na.rm = T)
    if(verbose) message(sprintf("theta = %g, value = %g", -params[1], v))
    return(-v)
  }
  r <- optim(c(theta = 0), fn = obj_function, X = X_, y = y, lower = lower, upper = upper, method = method)
  params_model <- as.list(r$par) # as.list(r$maximum)
  names(params_model)[1] <- "theta"
  params_model[["theta"]] <- -params_model[["theta"]]
  params <- list(
    parameters = params_model,
    params_normalize = list(
      center = col_cent,
      minimum = col_min,
      maximum = col_max,
      scale = scale
    ),
    boundary = boundary_function,
    value = -r$value,
    convergence = r$convergence,
    count = r$count,
    X = X,
    y = y)
  class(params) <- "hotpot"
  return(params)
}
