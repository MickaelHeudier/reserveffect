#create compendium
rrtools::use_compendium("../megafaunamap", open = FALSE)

#need to edit DESCRIPTION file

#create directories
dir.create("R")

dir.create("outputs")

dir.create("data")

dir.create("data/telemetry")

dir.create("outputs/poe_on_e ffort")

file.create("make.R")


#to be able to use pipes in functions
usethis::use_pipe()


#update NAMESPACE and add .Rd file for each function in man folder
devtools::document()

#load all functions
devtools::load_all()



#install all dependencies
remotes::install_deps(upgrade = "never")


#install parwiseadonis package
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")



#github tips
#command to stage files for git commit
git add -A
ls -al ~/.ssh

#uncommit
git reset --soft HEAD~1
