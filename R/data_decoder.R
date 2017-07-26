#' @title Array Data Decoder
#' @description Decodes values from a GIFTI image
#'
#' @param values text from XML of GIFTI image
#' @param encoding encoding of GIFTI values
#' @param datatype Passed to \code{\link{convert_binary_datatype}}
#' @param endian Endian to pass in \code{\link{readBin}}
#' @param ext_filename if \code{encoding = "ExternalFileBinary"}, then
#' this is the external filename
#' @param n number of values to read.  Relevant if
#' \code{encoding = "ExternalFileBinary"}
#'
#' @return Vector of values
#' @export
#'
#' @importFrom base64enc base64decode
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
data_decoder = function(
  values,
  encoding = c("ASCII",
               "Base64Binary",
               "GZipBase64Binary",
               "ExternalFileBinary"),
  datatype = NULL,
  endian = c("little", "big",
             "LittleEndian", "BigEndian"),
  ext_filename = NULL,
  n = NULL) {

  encoding = match.arg(encoding)
  if (encoding == "ExternalFileBinary") {
    msg = paste0(
      "Encoding says ExternalFileBinary, but file ",
      "doesn't exist in",
      " the same directory as the gii.  File was ",
      ext_filename)
    if (is.null(ext_filename)) {
      stop( msg)
    }
    if (!file.exists(ext_filename)) {
      stop( msg)
    }
    values = ext_filename
  }
  if (encoding == "ASCII") {
    values = strsplit(values, "\n")
    values = values[[1]]
    values = values[ !values %in% "" ]
    values = trimws(values)
    values = strsplit(values, " ")
    values = lapply(values, as.numeric)
    values = unlist(values)
    return(values)
  }
  if (grepl("Base64Binary", encoding)) {
    values = base64enc::base64decode(values)
    n = length(values) * 2
  }
  if (grepl("GZip", encoding)) {
    values = memDecompress(values, type = "gzip")
    n = length(values) * 2
  }

  L = convert_binary_datatype(datatype = datatype)
  size = L$size
  what = L$what
  endian = match.arg(endian)
  endian = tolower(endian)
  endian = gsub("endian$", "", endian)
  values = readBin(
    con = values,
    what = what,
    size = size,
    endian = endian,
    n = n
  )

  return(values)
}