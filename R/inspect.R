#' inspect gives S3/S4-like access to fields:
#'
#' Returns value of the field passed in object parameter
#'
#' Returns value of the field passed in object parameter
#'
#' @param object Name of object to inspect
#' @param what ???
#' @return Value of field passed in the object parameter
#' @export inspect
inspect <- function(object, what)
  object$field(what)
