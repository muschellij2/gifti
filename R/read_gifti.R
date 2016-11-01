# rm(list = ls())
# library(xml2)
# library(rgl)
# library(base64enc)
# library(matrixStats)
# file = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.surf.gii"
# file = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.surf.gii"

#' @title Read GIFTI File
#' @description Reads a GIFTI File and parses the output
#'
#' @param file Name of file to read
#'
#' @return List of values
#' @export
#' @importFrom xml2 xml_find_all read_xml xml_attr as_list
#' @importFrom xml2 xml_children xml_text
readgii = function(file){

  doc = read_xml(file)
  n_data_arrays = xml_attr(doc, "NumberOfDataArrays")
  n_data_arrays = as.numeric(n_data_arrays)
  ver = xml_attr(doc, "Version")

  ###################################
  # Parsing Meta Data
  ###################################
  meta = xml_find_all(doc, "./MetaData")
  f = function(xpath) {
    xml_text(xml_find_all(xml_children(meta), xpath))
  }
  meta_names = f("./Name")
  meta = f("./Value")
  names(meta) = meta_names

  lab_tab = xml_find_all(doc, "./LabelTable")

  darray = xml_find_all(doc, "./DataArray")
  darray = as_list(darray)
  stopifnot(length(darray) == n_data_arrays)

  MD = lapply(darray, function(x) {
    xml_children(xml_find_all(x, "./MetaData"))
  })
  MD = lapply(
    MD,
    function(x){
      names = xml_text(xml_find_all(x, "./Name"))
      vals = xml_text(xml_find_all(x, "./Value"))
      cbind(names = names,
            vals = vals)
    })

  # transformation matrix
  trans = lapply(
    darray,
    xml_find_all,
    xpath = "./CoordinateSystemTransformMatrix")

  info = data_array_attributes(darray)
  dims = grep("^Dim\\d",
              names(info),
              value = TRUE)

  data = lapply(darray, xml_find_all, xpath = "./Data")
  vals = lapply(data, xml_text)

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

    dat = data_decoder(
      values = vals[[ind]],
      encoding = encoding,
      datatype = datatype,
      endian = endian)

    namer = convert_intent(intent)
    names(L)[ind] = namer

    n = info$n[ind]
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
           meta = meta,
           version = ver,
           transformations = trans,
           label = lab_tab
  )
  return(L)
}

# rgl.open()
# faces = as.vector(t(L$faces) + 1)
# verts = L$vertices[faces,]
# norms = L$normals[faces,]
# rgl.triangles(x = verts, normals = norms)

