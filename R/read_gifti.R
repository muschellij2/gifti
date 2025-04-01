#' @title Read GIFTI File
#' @description Reads a GIFTI File and parses the output
#'
#' @param file Name of file to read
#'
#' @return List of values
#' @export
#' @importFrom xml2 xml_find_all read_xml xml_attr as_list
#' @importFrom xml2 xml_children xml_text xml_name
#' @importFrom R.utils gunzip
#' @importFrom tools file_ext
#' @examples
#' if (have_gifti_test_data(outdir = NULL)) {
#'    gii_files = download_gifti_data(outdir = NULL)
#'    gii_list = lapply(gii_files, readgii)
#'    surf_files = grep("white[.]surf[.]gii", gii_files, value = TRUE)
#'    surfs = lapply(surf_files, surf_triangles)
#'
#'    col_file = grep("white[.]shape[.]gii", gii_files, value = TRUE)
#'    cdata = readgii(col_file)
#'    cdata = cdata$data$shape
#'    mypal = grDevices::colorRampPalette(colors = c("blue", "black", "red"))
#'    n = 4
#'    breaks = quantile(cdata)
#'     ints = cut(cdata, include.lowest = TRUE, breaks = breaks)
#'     ints = as.integer(ints)
#'     stopifnot(!any(is.na(ints)))
#'     cols = mypal(n)[ints]
#'     cols = cols[surfs[[1]]$triangle]
#' }
#' \dontrun{
#' if (have_gifti_test_data(outdir = NULL)) {
#'
#'  if (requireNamespace("rgl", quietly = TRUE)) {
#'     rgl::rgl.open()
#'     rgl::rgl.triangles(surfs[[1]]$pointset, color = cols)
#'     rgl::play3d(rgl::spin3d(), duration = 5)
#'  }
#' }
#' }
#'
readgii = function(file){

  if (length(file) > 1) {
    res = lapply(file, readgii)
    return(res)
  }
  dn = dirname(file)
  ######################
  # Allow to read gii.gz
  ######################
  file = decompress_gii(file)
  doc = read_xml(file)
  n_data_arrays = xml2::xml_attr(doc, "NumberOfDataArrays")
  n_data_arrays = as.numeric(n_data_arrays)
  ver = xml2::xml_attr(doc, "Version")

  ###################################
  # Parsing Meta Data
  ###################################
  meta = xml2::xml_find_all(doc, "./MetaData")
  f = function(xpath) {
    xml2::xml_text(xml2::xml_find_all(xml2::xml_children(meta), xpath))
  }
  meta_names = f("./Name")
  meta = f("./Value")
  names(meta) = meta_names

  lab_tab_pre = xml2::xml_children(xml2::xml_find_all(doc, "./LabelTable"))
  lab_tab = do.call("rbind", xml2::xml_attrs(lab_tab_pre))
  if (!is.null(lab_tab)) {
    rownames(lab_tab) = xml2::xml_text(lab_tab_pre)
    }

  darray = xml2::xml_find_all(doc, "./DataArray")
  # darray = as_list(darray)
  stopifnot(length(darray) == n_data_arrays)

  MD = lapply(darray, function(x) {
    xml2::xml_children(xml2::xml_find_all(x, "./MetaData"))
  })
  MD = lapply(
    MD,
    function(x){
      names = xml2::xml_text(xml2::xml_find_all(x, "./Name"))
      vals = xml2::xml_text(xml2::xml_find_all(x, "./Value"))
      cbind(names = names,
            vals = vals)
    })

  # transformation matrix
  trans = lapply(
    darray,
    xml2::xml_find_all,
    xpath = "./CoordinateSystemTransformMatrix")

  parsed_trans = lapply(trans, function(x) {
    cx = xml2::xml_children(x)
    n = xml2::xml_name(cx)
    res = lapply(cx, xml2::xml_text)
    names(res) = n
    if ("MatrixData" %in% n) {
      ind = n %in% "MatrixData"
      res[ind] = lapply(res[ind], function(mat){
        mat = strsplit(mat, "\n")[[1]]
        mat = trimws(mat)
        mat = mat[ !mat %in% ""]
        mat = lapply(strsplit(mat, "\\s+"), as.numeric)
        mat = lapply(mat, function(q){q[!is.na(q)]})
        mat = do.call("rbind", mat)
        return(mat)
      })
    }
    return(res)
  })

  no_length_zero = function(x) {
    if (length(x) == 0) {
      return(NULL)
    } else {
      return(x)
    }
  }

  trans = lapply(trans, no_length_zero)
  parsed_trans = lapply(parsed_trans, no_length_zero)

  info = data_array_attributes(darray)
  dims = grep("^Dim\\d",
              colnames(info),
              value = TRUE)
  info$name = sapply(info$Intent, convert_intent)


  data = lapply(darray,
                xml2::xml_find_all,
                xpath = "./Data")
  vals = lapply(data,
                xml2::xml_text)

  ind = 1
  N = nrow(info)
  L = vector(mode = "list",
             length = N)

  for (ind in seq(N)) {

    encoding = info$Encoding[ind]
    datatype = info$DataType[ind]
    intent = info$Intent[ind]
    endian = info$Endian[ind]
    endian = convert_endian(endian)
    ext_filename = info$ExternalFileName[ind]
    ext_filename = file.path(dn, ext_filename)
    n = info$n[ind]

    dat = data_decoder(
      values = vals[[ind]],
      encoding = encoding,
      datatype = datatype,
      endian = endian,
      ext_filename = ext_filename,
      n = n)

    namer = convert_intent(intent)
    names(L)[ind] = namer

    stopifnot(length(dat) == n)

    mat_dims = info[ind, dims]
    mat_dims = unlist(mat_dims)
    ord = info$ArrayIndexingOrder[ind]

    arr = create_data_matrix(
      data = dat,
      dims = mat_dims,
      ordering = ord)
    L[[ind]] = arr

  }

  L = list(data = L,
           file_meta = meta,
           data_meta = MD,
           version = ver,
           transformations = trans,
           parsed_transformations = parsed_trans,
           label = lab_tab,
           data_info = info
  )
  class(L) = "gifti"
  return(L)
}

#' @rdname readgii
#' @export
readGIfTI = function(file){
  res = readgii(file)
  return(res)
}

#' @rdname readgii
#' @export
read_gifti = function(file){
  res = readgii(file)
  return(res)
}
