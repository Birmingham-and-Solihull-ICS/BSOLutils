# install.packages("magick")
library(magick)

img <- image_read("./dev/Signature_Strip_1.png")          # load PNG
image_write(img, path = "./inst/signature.svg", format = "svg")
