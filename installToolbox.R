# install internal library
remotes::install_git(url = "https://github.com/Tomkess/generalToolboxR.git", 
                     git = "external",
                     build_vignettes = T, 
                     build_manual = T, 
                     build = T)

# initiate internal library
library("generalToolboxR")
