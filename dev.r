
#create compendium
install.packages("remotes")
remotes::install_github("benmarwick/rrtools")

rrtools::use_compendium("../reserveffect", open = FALSE)

#create file
dir.create("data")

