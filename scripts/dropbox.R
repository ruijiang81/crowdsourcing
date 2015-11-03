################################################################################
## Dropbox
## https://github.com/karthik/rdrop2
## library(help=rdrop2)
################################################################################
## 1. Setup for a new PC
# install.packages(c("rdrop2","httpuv"))
library(rdrop2)
drop_auth()

## 2. Create Dropbox inside the project (do not change)
dir.create('Dropbox',showWarnings=F)

## 4. Choose destination
dropbox.path = file.path("economic labelling","shared_results",Sys.Date()) 

## 5. Upload the files in the projects' Dropbox folder
dropbox.files2upload = list.files('./Dropbox', full.names=TRUE, recursive=FALSE, pattern=".*\\.csv")
for(file in dropbox.files2upload) drop_upload(file, dropbox.path)

## 6. Upload metadata
dropbox.files2upload = list.files('./Dropbox/metadata', full.names=TRUE, recursive=FALSE, pattern=".*\\.csv")
for(file in dropbox.files2upload) drop_upload(file, file.path(dropbox.path,'metadata'))
