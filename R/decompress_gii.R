#' @title Decompress Gzipped GIFTI (with extension .gz)
#' @description If a GIFTI file is compressed, as in .gii.gz, this will decompress
#' the file.  This has nothing to do with the encoding WITHIN the file
#'
#' @param file file name of GIFTI file
#'
#' @return Filename of decompressed GIFTI
#' @export
#' @examples
#' if (have_gifti_test_data()) {
#'    gii_files = download_gifti_data()
#'    outfile = decompress_gii(gii_files[1])
#'    print(outfile)
#' }
decompress_gii = function(file) {
  ext = tolower(tools::file_ext(file))
  if (ext == "gz") {
    # destfile = tempfile(fileext = ".gii")
    file = R.utils::gunzip(file,
                           # destname = destfile,
                           temporary = TRUE,
                           remove = FALSE,
                           overwrite = TRUE)
  }
  return(file)
}