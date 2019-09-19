#' Construct hilo intervals
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param level Default `NULL` does not include 'level'. Otherwise values of
#' length 1 or as length of `lower`, expected between 0 and 100.
#'
#' @return A "hilo" object
#' 
#' @author Earo Wang & Mitchell O'Hara-Wild
#' 
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, level = 95)
#'
#' @export
new_hilo <- function(lower, upper, level) {
  vec_assert(lower, double())
  vec_assert(upper, double())
  vec_assert(level, double())
  
  if (any(upper < lower, na.rm = TRUE)) {
    abort("`upper` can't be lower than `lower`.")
  }
  len <- length(lower)
  
  if (any(level < 0 | level > 100, na.rm = TRUE))
    abort("'level' can't be negative or greater than 100.")
  
  if (length(level) == 1)
    level <- rep_len(level, len)
  else if (length(level) != len)
    abort(gettextf("'level' should be of length 1 or %d.", len))
  
  vctrs::new_rcrd(
    list(lower = lower, upper = upper, level = level),
    class = "hilo"
  )
}

#' Compute hilo intervals
#' 
#' Used to extract a specified prediction interval at a particular confidence 
#' level from a distribution or fable.
#' 
#' @param x Object to create hilo from
#' @inheritParams hilo.fcdist
#' 
#' @export
hilo <- function(x, ...){
  UseMethod("hilo")
}

#' @export
hilo.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hilo()`, you can create a custom `hilo` with `new_hilo()`",
    class(x)
  ))
}

#' Is the object a hilo
#' 
#' @param x An object.
#' 
#' @export
is_hilo <- function(x) {
  inherits(x, "hilo")
}

#' @export
format.hilo <- function(x, ...) {
  x <- vec_data(x)
  limit <- paste(
    format(x$lower, justify = "right", ...),
    format(x$upper, justify = "right", ...),
    sep = ", "
  )
  paste0("[", limit, "]", x$level)
}

#' @export
is.na.hilo <- function(x) {
  # both lower and upper are NA's
  x <- vec_data(x)
  is.na(x$lower) & is.na(x$upper)
}

#' @export
vec_math.hilo <- function(.fn, .x, ...){
  out <- vec_data(.x)
  out[["lower"]] <- get(.fn)(out[["lower"]], ...)
  out[["upper"]] <- get(.fn)(out[["upper"]], ...)
  vec_restore(out, .x)
}

#' @export
vec_arith.hilo <- function(op, x, y, ...){
  out <- dt_x <- vec_data(x)
  if(is_hilo(y)){
    dt_y <- vec_data(y)
    out[["lower"]] <- get(op)(dt_x[["lower"]], dt_y[["lower"]])
    out[["upper"]] <- get(op)(dt_x[["upper"]], dt_y[["upper"]])
  }
  else{
    out[["lower"]] <- get(op)(dt_x[["lower"]], y)
    out[["upper"]] <- get(op)(dt_x[["upper"]], y)
  }
  vec_restore(out, x)
}