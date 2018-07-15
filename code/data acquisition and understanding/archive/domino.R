################################################################################
## Domino
################################################################################
# http://help.dominodatalab.com/client
## creates new project.
# domino.init("Quick-and-Dirty")
## gets existing project from the server.
# domino.get("Quick-and-Dirty")

## 1. Login
require("domino")
domino.login("Tomer_Geva", "EconUse1!")

## 2. Upload new files to the project folder on Domino cloud
domino.download()
domino.upload()

## 3. Runs the project on Domino servers
# domino.run("Main.R")
