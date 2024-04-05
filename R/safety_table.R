# This function creates a summary for a numeric vector

#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export numeric_summary
#'
#' @examples

numeric_summary <- function(x, na.rm=FALSE){

  min = min(x, na.rm=na.rm)
  max = max(x, na.rm=na.rm)
  mean = mean(x, na.rm=na.rm)
  sd = sd(x, na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))

  c(min=min, max=max, mean=mean, sd=sd, length=length, Nmiss=Nmiss)

}

# This function creates a summary for a character vector
# This function creates a summary for a numeric vector

#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export char_summary
#'
#' @examples

char_summary <- function(x, na.rm=FALSE){

  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}
