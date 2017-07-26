#' @title Array Data Encoder
#' @description Encodes values for a GIFTI image
#'
#' @param values values to be encoded
#' @param encoding encoding of GIFTI values
#' @param datatype Passed to \code{\link{convert_binary_datatype}}
#' @param endian Endian to pass in \code{\link{readBin}}
#'
#' @return Single character vector
#' @export
#'
#' @importFrom base64enc base64encode
#' @examples
#' if (have_gifti_test_data(outdir = NULL)) {
#'    gii_files = download_gifti_data(outdir = NULL)
#'    L = gifti_list(gii_files[1])
#'    orig = L$DataArray$Data[[1]]
#'    encoding = attributes(L$DataArray)$Encoding
#'    datatype = attributes(L$DataArray)$DataType
#'    endian = attributes(L$DataArray)$Endian
#'    vals =  data_decoder(orig, encoding = encoding,
#'    datatype = datatype, endian = endian)
#'    enc = data_encoder(vals, encoding = encoding,
#'    datatype = datatype, endian = endian)
#'    enc == orig
#' }
data_encoder = function(
  values,
  encoding = c("ASCII",
               "Base64Binary",
               "GZipBase64Binary"),
  datatype = NULL,
  endian = c("little", "big",
             "LittleEndian", "BigEndian")) {

  encoding = match.arg(encoding)
  if (encoding == "ASCII") {
    dig_opt = options()$digits
    on.exit({
      options(digits = dig_opt)
    })
    options(digits = 7)
    values = as.character(values)
    values = paste(values, collapse = "\n")
    return(values)
  }
  L = convert_binary_datatype(datatype = datatype)
  size = L$size
  # what = L$what
  endian = match.arg(endian)
  endian = tolower(endian)
  endian = gsub("endian$", "", endian)
  values = writeBin(object = values,
                   con = raw(),
                   size = size,
                   endian = endian
  )
  if (grepl("GZip", encoding)) {
    values = memCompress(values, type = "gzip")
  }
  if (grepl("Base64Binary", encoding)) {
    values = base64enc::base64encode(values)
  }
  return(values)
}