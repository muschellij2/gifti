
rm(list = ls())
library(gifti)
library(RColorBrewer)
file1 = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.surf.gii"
file2 = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.shape.gii"
g = readgii(file1)
gg = readgii(file2)
rgl.open()
L = g$data
faces = as.vector(t(L$faces) + 1)
verts = L$vertices[faces,]
norms = L$normals[faces,]
cdata = gg$data$cdata

cols = brewer.pal(3, "Spectral")
mypal = colorRampPalette(colors = cols)
n = 10
breaks = seq(min(cdata), max(cdata), length.out = n)
ints = cut(cdata, include.lowest = TRUE, breaks = breaks)
ints = as.integer(ints)
stopifnot(!any(is.na(ints)))
cols = mypal(n)[ints]
cols = scales::alpha(cols, 1)
rgl.triangles(x = verts, normals = norms, color = cols)

