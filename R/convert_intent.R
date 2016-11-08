#' @title Convert Intent
#' @description Converts the \code{intent} field from a GIFTI image
#' to a more standard naming
#'
#' @param intent (character) string of intent type
#'
#' @return A character string
#' @export
convert_intent = function(intent) {
  namer = switch(
    intent,
    'NIFTI_INTENT_POINTSET' = "vertices",
    'NIFTI_INTENT_TRIANGLE' = "faces",
    'NIFTI_INTENT_NODE_INDEX' = "indices",
    'NIFTI_INTENT_VECTOR' = "normals",
    'NIFTI_INTENT_NORMAL' = "normals",
    'NIFTI_INTENT_NONE' = "unknown",
    'NIFTI_INTENT_SHAPE' = "shapes",
    "NIFTI_INTENT_LABEL" = "labels",
    "NIFTI_INTENT_TIME_SERIES" = "timeseries",
    "NIFTI_INTENT_RGB_VECTOR" = "rgb",
    "NIFTI_INTENT_RGBA_VECTOR" = "rgba",
    "NIFTI_INTENT_GENMATRIX" = "tensors"
  )
  if (is.null(namer)) {
    # stop("intent is wrong")
    namer = "unknown"
  }
  return(namer)
}