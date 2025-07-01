
# Test script for testing functions ---------------------------------------

# Workflow:
# After updating the package, use devtools::document()
# To load package functions, use devtools::load_all()
# To install the package, use devtools::install()

library(langtropy)

?langtropy::surprisal

surprisal(0.01)

# pkgload::load_all()


## FrameNet data -----------------------------------------------------------

fn <- read.csv("data/FrameNet_data.csv")



# TODO --------------------------------------------------------------------

# pre-processing
# surprisal
# PMF + plots
# DKL, DJS
# MI
# ?

