################################################################################
## Domino
################################################################################
## 0. Project Inception
# http://help.dominodatalab.com/client
# domino.init("Workforce-Based-Labeling")
# domino.get("Workforce-Based-Labeling")
## 1. Login
require("domino")
# domino.login("Tomer_Geva", "*****")  
## or
domino.login(Sys.getenv("Domino_User"), Sys.getenv("Domino_Pass"))  
## 2. Upload new files to the project folder on Domino cloud
domino.download()
domino.upload() 
## 3. Runs the project on Domino servers
#domino.run("Main.R") 
## 4. Download the results to local drive
domino.download() 


