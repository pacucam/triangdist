#This .R document is for the creation of the 4 demanded functions of the package.

#' @title Density function for the Triangular Distribution
#' @param x Numeric vector of quantiles
#' @param min Lower limit (a)
#' @param max Upper limit (b)
#' @param mode Mode (c)
#' @export
#'
dtriang <- function(x, min, max, mode) {
  if (min >= max) {
    stop("Error: Min has to be smaller than maximum.")
  }
  else if (mode < min) {
    stop("Error: The mode has to be bigger than the minimum")
  }
  else if (max < mode) {
    stop("Error: The mode has to be smaller than the maximum")
  }
  #Vectorized inputs
  n <- max(length(x), length(min), length(max), length(mode))
  x <- rep(x, length.out = n)
  min <- rep(min, length.out = n)
  max <- rep(max, length.out = n)
  mode <- rep(mode, length.out = n)
  res <- numeric(length(x))
  h <- 2 / (max - min)
  #First situation: a<=x<c
  left_part <- min <= x & mode >= x
  res[left_part] <- (2 * (x[left_part]-min[left_part])) /
                       ((max[left_part] - min[left_part]) * (mode[left_part] - min[left_part]))
  #Second situation: c<x<=b
  right_part <- mode <= x & max >= x
  res[right_part] <- ((-2 * (x[right_part] - max[right_part])) /
                        ((max[right_part] - min[right_part]) * (max[right_part] - mode[right_part])))
  #Last situation: if c = x
  res[x == mode] <- h

  return(res)
}


#' @title Distribution function for the Triangular Distribution
#' @param q Numeric vector of quantiles
#' @param min Lower limit (a)
#' @param max Upper limit (b)
#' @param mode Mode (c)
#' @export
ptriang <- function(q, min, max, mode) {
  # 1. Validaciones
  if (any(min >= max))
    stop("Error: Min has to be smaller than maximum.")
  else if (any(mode < min))
    stop("Error: The mode has to be bigger than the minimum")
  else if (any(max < mode))
    stop("Error: The mode has to be smaller than the maximum")

  #Recycling (for vectors)
  n <- max(length(q), length(min), length(max), length(mode))
  q <- rep(q, length.out = n)
  min <- rep(min, length.out = n)
  max <- rep(max, length.out = n)
  mode <- rep(mode, length.out = n)

  res <- numeric(n)
  #First situation: a <= q < c
  left_part <- q >= min & q < mode
  res[left_part] <- ((q[left_part] - min[left_part])**2) /
    ((max[left_part] - min[left_part]) * (mode[left_part] - min[left_part]))

  #Second situation: c <= q <= b
  right_part <- q >= mode & q <= max
  res[right_part] <- 1 - ((max[right_part] - q[right_part])^2) /
    ((max[right_part] - min[right_part]) * (max[right_part] - mode[right_part]))

  #Last situation: q > max
  res[q > max] = 1

  return(res)
}
