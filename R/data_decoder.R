#' @title Array Data Decoder
#' @description Decodes values from a GIFTI image
#'
#' @param values text from XML of GIFTI image
#' @param encoding encoding of GIFTI values
#' @param datatype Passed to \code{\link{convert_binary_datatype}}
#' @param endian Endian to pass in \code{\link{readBin}}
#'
#' @return Vector of values
#' @export
#'
#' @importFrom base64enc base64decode
data_decoder = function(
  values,
  encoding = c("ASCII",
               "Base64Binary",
               "GZipBase64Binary"),
  datatype = NULL,
  endian = c("little", "big")) {
  encoding = match.arg(encoding)
  if (encoding == "ASCII") {
    return(values)
  }
  if (encoding == "Base64Binary") {
    values = base64enc::base64decode(values)
  }
  if (encoding == "GZipBase64Binary") {
    values = memDecompress(values, type = "gzip")
  }

  L = convert_binary_datatype(datatype = datatype)
  size = L$size
  what = L$what
  endian = match.arg(endian)
  values = readBin(con = values,
                what = what,
                size = size,
                endian = endian,
                n = length(values) * 2
  )

  return(values)
}