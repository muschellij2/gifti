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
    'NIFTI_INTENT_POINTSET' = "pointset",
    'NIFTI_INTENT_TRIANGLE' = "triangle",
    'NIFTI_INTENT_NODE_INDEX' = "node_index",
    'NIFTI_INTENT_VECTOR' = "vector",
    'NIFTI_INTENT_NORMAL' = "normal",
    'NIFTI_INTENT_NONE' = "unknown",
    'NIFTI_INTENT_SHAPE' = "shape",
    "NIFTI_INTENT_LABEL" = "labels",
    "NIFTI_INTENT_TIME_SERIES" = "timeseries",
    "NIFTI_INTENT_RGB_VECTOR" = "rgb",
    "NIFTI_INTENT_RGBA_VECTOR" = "rgba",
    "NIFTI_INTENT_GENMATRIX" = "genmatrix",

    "NIFTI_INTENT_CORREL" = "correl",
    "NIFTI_INTENT_TTEST" = "ttest",
    "NIFTI_INTENT_FTEST" = "ftest",
    "NIFTI_INTENT_ZSCORE" = "zscore",
    "NIFTI_INTENT_CHISQ" = "chisq",
    "NIFTI_INTENT_BETA" = "beta",
    "NIFTI_INTENT_BINOM" = "binom",
    "NIFTI_INTENT_GAMMA" = "gamma",
    "NIFTI_INTENT_POISSON" = "poisson",
    "NIFTI_INTENT_FTEST_NONC" = "ftest_nonc",
    "NIFTI_INTENT_CHISQ_NONC" = "chisq_nonc",
    "NIFTI_INTENT_LOGISTIC" = "logistic",
    "NIFTI_INTENT_LAPLACE" = "laplace",
    "NIFTI_INTENT_UNIFORM" = "uniform",
    "NIFTI_INTENT_TTEST_NONC" = "ttest_nonc",
    "NIFTI_INTENT_WEIBULL" = "weibull",
    "NIFTI_INTENT_CHI" = "chi",
    "NIFTI_INTENT_INVGAUSS" = "invgauss",
    "NIFTI_INTENT_EXTVAL" = "extval",
    "NIFTI_INTENT_PVAL" = "pval",
    "NIFTI_INTENT_LOGPVAL" = "logpval",
    "NIFTI_INTENT_LOG10PVAL" = "log10pval",
    "NIFTI_INTENT_ESTIMATE" = "estimate",

    "NIFTI_INTENT_NEURONAME" = "neuroname",

    "NIFTI_INTENT_SYMMATRIX" = "symmatrix",
    "NIFTI_INTENT_DISPVECT" = "dispvect",

    "NIFTI_INTENT_QUATERNION" = "quads",
    "NIFTI_INTENT_DIMLESS" = "dimless"
  )
  if (is.null(namer)) {
    # stop("intent is wrong")
    namer = "unknown"
  }
  return(namer)
}
