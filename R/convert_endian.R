#' @title Convert Endian from GIFTI
#' @description Converts Endian format from GIFTI
#'
#' @param endian character passed from GIFTI XML
#'
#' @return Character string
#' @export
convert_endian = function(endian) {
  endian = gsub("Endian", "", endian)
  endian = tolower(endian)
  return(endian)
}