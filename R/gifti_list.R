#' @title Convert GIFTI to List
#' @description Reads in a GIFTI file and coerces it to a list
#'
#' @param file file name of GIFTI file
#'
#' @return List of elements
#' @export
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
gifti_list = function(file) {
  ######################
  # Allow to read gii.gz
  ######################
  file = decompress_gii(file)
  doc = read_xml(file)
  doc = as_list(doc)
  class(doc) = "gifti_list"
  return(doc)
}