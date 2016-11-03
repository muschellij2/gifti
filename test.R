
rm(list = ls())
library(gifti)
library(rgl)
library(RColorBrewer)
# file1 = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.surf.gii"
# file2 = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.shape.gii"
file1 = "~/Downloads/100307.L.pial.164k_fs_LR.surf.gii"
# file2 = "~/Downloads/100307.L.atlasroi.164k_fs_LR.shape.gii"
file2 = "~/Downloads/100307.L.corrThickness.164k_fs_LR.shape.gii"


# file3 = "~/Downloads/BV_GIFTI/GzipBase64/sujet01_Lwhite.inflated.surf.gii"
g = readgii(file1)
gg = readgii(file2)
# g = readgii(file3)
L = g$data
faces = as.vector(t(L$faces) + 1)
verts = L$vertices[faces,]
norms = L$normals[faces,]
# cdata = gg$data$cdata[faces,]
cdata = gg$data$unknown[faces,]

cols = brewer.pal(3, "Spectral")
mypal = colorRampPalette(colors = cols)
n = 10
# breaks = seq(min(cdata), max(cdata), length.out = n)
# breaks = seq(-1, 1, by = 0.25)
breaks = seq(min(cdata), max(cdata) + 0.25, by = 0.25)
ints = cut(cdata, include.lowest = TRUE, breaks = breaks)
ints = as.integer(ints)
stopifnot(!any(is.na(ints)))
cols = mypal(n)[ints]
cols = scales::alpha(cols, 1)
rgl.open()
rgl.triangles(x = verts, normals = norms, color = cols)


file1 = "~/Downloads/100307.R.pial.164k_fs_LR.surf.gii"
file2 = "~/Downloads/100307.R.corrThickness.164k_fs_LR.shape.gii"
g = readgii(file1)
gg = readgii(file2)
# g = readgii(file3)
L = g$data
faces = as.vector(t(L$faces) + 1)
verts = L$vertices[faces,]
norms = L$normals[faces,]
cdata = gg$data$unknown[faces,]

cols = brewer.pal(3, "Spectral")
mypal = colorRampPalette(colors = cols)
n = 10
# breaks = seq(min(cdata), max(cdata), length.out = n)
breaks = seq(min(cdata), max(cdata) + 0.25, by = 0.25)
ints = cut(cdata, include.lowest = TRUE, breaks = breaks)
ints = as.integer(ints)
stopifnot(!any(is.na(ints)))
cols = mypal(n)[ints]
cols = scales::alpha(cols, 1)
rgl.triangles(x = verts, normals = norms, color = cols)