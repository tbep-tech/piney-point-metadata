library(dataone)
library(datapack)
library(uuid)
library(here)

# authenticate
# token <- Sys.getenv('dataone_token')
# options(dataone_token = token)

token <- Sys.getenv('dataone_test_token')
options(dataone_test_token = token)

# # prod node for KNB
# cn <- CNode('PROD')
# mn <- getMNode(cn, 'urn:node:KNB')

# staging/test node for KNB
cn <- CNode('STAGING')
mn <- getMNode(cn, 'urn:node:mnTestKNB')

# initiate data package
dp <- new('DataPackage')

# xml metadata file
emlFile <- here('ppdat.xml')
doi <- generateIdentifier(mn, 'DOI')
metadataObj <- new('DataObject', format = 'eml://ecoinformatics.org/eml-2.1.1', filename = emlFile)
dp <- addMember(dp, metadataObj)

# csv data files, add to data package
fls <- list.files(here('data-raw'), full.names = T)
for(fl in fls){
  cat(fl, '\t')
  sourceObj <- new('DataObject', format='text/csv', filename = fl) 
  dp <- addMember(dp, sourceObj, metadataObj)
}

# set my access rules
myAccessRules <- data.frame(subject = 'https://orcid.org/0000-0002-4996-0059', permission = 'changePermission') 

# id member node, upload
cli <- D1Client(cn, mn)
packageId <- uploadDataPackage(cli, dp, public = TRUE, accessRules = myAccessRules, quiet = FALSE)
message(sprintf('Uploaded package with identifier: %s', packageId))