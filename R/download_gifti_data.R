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
download_gifti_data = function(
  outdir = system.file(package = "gifti"),
  overwrite = FALSE,
  ...
) {
  expected_files = c("sujet01_Lwhite.shape.gii",
                     "fmri_sujet01_Lwhite_projection.time.gii",
                     "sujet01_Lwhite.surf.gii",
                     "sujet01_Lwhite.inflated.surf.gii"
  )
  expected_files = file.path("BV_GIFTI", "GzipBase64", expected_files)
  out_files = file.path(outdir, expected_files)
  if (!all(file.exists(out_files)) || overwrite) {
    url = paste0( "https://www.nitrc.org/frs/download.php",
                  "/411/BV_GIFTI_1.3.tar.gz")
    destfile = basename(url)
    destfile = file.path(outdir, destfile)
    download.file( url = url, destfile = destfile)
    untar(tarfile = destfile,
                      exdir = outdir,
                      files = expected_files)
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
have_gifti_test_data = function(
  outdir = system.file(package = "gifti")
) {
  expected_files = c("sujet01_Lwhite.shape.gii",
                     "fmri_sujet01_Lwhite_projection.time.gii",
                     "sujet01_Lwhite.surf.gii",
                     "sujet01_Lwhite.inflated.surf.gii"
  )
  expected_files = file.path("BV_GIFTI", "GzipBase64", expected_files)
  out_files = file.path(outdir, expected_files)
  res = all(file.exists(out_files))
  return(res)
}