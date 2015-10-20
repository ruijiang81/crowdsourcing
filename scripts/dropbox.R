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

## 3. Choose destination
# type `dropbox.path = file.path('Research','Reports',Sys.Date())` for daily backup
dropbox.path = file.path('Research','Reports') 

## 4. Upload the files in the projects' Dropbox folder
dropbox.files2upload = list.files('./Dropbox', full.names=TRUE, recursive=TRUE)
for(file in dropbox.files2upload) drop_upload(file, dropbox.path)
