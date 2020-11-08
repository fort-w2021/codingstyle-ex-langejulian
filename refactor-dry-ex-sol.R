### Solution refactor-dry-ex ###

set.seed(123) # setting seed for reproducibility
x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <- z^2

# Compute the two sided 95%-confidence interval using the standard normal
#   distribution.
# input:
#   x: numeric vector
# output: numeric vector with the lower and upper bound of the confidence interval
compute_confidence_interval <- function(x) {
  checkmate::assert_numeric(x, finite = TRUE, min.len = 2, any.missing = FALSE)
  number_of_observations <- length(x)
  mean <- mean(x)
  standard_deviation <- sd(x)
  half_width <- 1.96 * standard_deviation / sqrt(number_of_observations)
  c(mean - half_width, mean + half_width)
}

compute_confidence_interval(x)
compute_confidence_interval(y)
compute_confidence_interval(z)
compute_confidence_interval(v)
