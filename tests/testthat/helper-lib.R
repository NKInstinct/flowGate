# helper-lib.R file to create variables once, 
#  avoiding repeatedly recreating objects during testing

# Loading internal .fcs files
path_to_fcs <- system.file("extdata", package = "flowGate")
fs <- read.flowSet(path = path_to_fcs,
   pattern = ".FCS$", full.names = TRUE)
gs <- GatingSet(fs)

# ggplot2 and ggcyto versions (for troubleshooting)
ggcytoVersion <- packageVersion("ggcyto")
ggplot2Version <- packageVersion("ggplot2")

sample <- 1
sample.gs <- gs[[sample]]


