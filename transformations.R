arcsine_transform <- function(x, is_percent = TRUE) {
  if (is_percent) {
    return(asin(sqrt(x / 100)))
  } else {
    return(asin(sqrt(x)))
  }
}

logit_transform <- function(x, is_percent = TRUE) {
  if (is_percent) {
    x = x / 100
    return(log(x / (1 - x)))
  } else {
    return(log(x / (1 - x)))
  }
}

inverse_logit <- function(x, to_percent = TRUE) {
  val <- exp(x) / (1 + exp(x))
  if (to_percent) {
    return(val * 100)
  } else {
    return(val)
  }
}

inverse_asin <- function(x, to_percent = TRUE) {
  if (to_percent) {
    return(sin(x) ** 2 * 100)
  } else {
    return(sin(x) ** 2)
  }
}