#' inspect provides S3/S4-like access to fields of a PcAuxData Reference Class object.
#' @param object An initialized RC object of class PcAuxData.
#' @param what A character string naming the field to access in object.
#' @return The current value stored in the what field of object.
#' @export inspect

inspect <- function(object, what) {
  object$field(what)
}
