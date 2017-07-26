#' @title Download GIFTI Test Data
#' @description Downloads GIFTI test data from
#' \url{https://www.nitrc.org/frs/download.php/411/BV_GIFTI_1.3.tar.gz}
#' @param outdir Output directory for test file directory
#' @param overwrite Should files be overwritten if already exist?
#' @param ... additional arguments to \code{\link{download.file}}
#'
#' @return Vector of file names
#' @export
#'
#' @importFrom utils download.file untar
#' @importFrom R.utils gzip
download_gifti_data = function(
  outdir = system.file(package = "gifti"),
  overwrite = FALSE,
  ...
) {
  # simple workaround for vignettes
  if (is.null(outdir)) {
    outdir = ""
  }
  if (outdir == "") {
    outdir = tempdir()
  }
  expected_files = c("sujet01_Lwhite.shape.gii.gz",
                     "fmri_sujet01_Lwhite_projection.time.gii.gz",
                     "sujet01_Lwhite.surf.gii.gz",
                     "sujet01_Lwhite.inflated.surf.gii.gz"
  )
  expected_files = file.path("BV_GIFTI", "GzipBase64", expected_files)
  out_files = file.path(outdir, expected_files)
  if (!all(file.exists(out_files)) || overwrite) {
    # url = paste0( "http://www.nitrc.org/frs/download.php",
    #               "/411/BV_GIFTI_1.3.tar.gz")
    url = "http://johnmuschelli.com/gifti/BV_GIFTI.tar.gz"

    destfile = basename(url)
    destfile = file.path(outdir, destfile)
    download.file( url = url, destfile = destfile)

    efiles = gsub("[.]gz$", "", expected_files)
    untar(tarfile = destfile,
          exdir = outdir,
          files = efiles)
    # compress them
    files_to_zip = file.path(outdir, efiles)
    sapply(files_to_zip, R.utils::gzip, compression = 9)
    file.remove(destfile)
  }
  return(out_files)
}

#' @title Check Presence of GIFTI Test Data
#' @description Checks if GIFTI test data is downloaded
#'
#' @param outdir Output directory for test file directory
#'
#' @return Logical indicator
#' @export
#' @examples
#' have_gifti_test_data(outdir = NULL)
have_gifti_test_data = function(
  outdir = system.file(package = "gifti")
) {
  # simple workaround for vignettes
  if (is.null(outdir)) {
    outdir = ""
  }
  if (outdir == "") {
    outdir = tempdir()
  }
  expected_files = c("sujet01_Lwhite.shape.gii.gz",
                     "fmri_sujet01_Lwhite_projection.time.gii.gz",
                     "sujet01_Lwhite.surf.gii.gz",
                     "sujet01_Lwhite.inflated.surf.gii.gz"
  )
  expected_files = file.path("BV_GIFTI", "GzipBase64", expected_files)
  out_files = file.path(outdir, expected_files)
  res = all(file.exists(out_files))
  return(res)
}
