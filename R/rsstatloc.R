#Set library
library(EML)
library(emld)
library(tidyverse)

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                                               ~formatString, ~definition,        ~unit,   ~numberType,
    "source_lng", "Agency that took sample - Label",                                    NA,           "Source Label",     NA,       NA, 
    "source",     "Agency that took Sample - short hand",                               NA,            "Source Short hand",                 NA,       NA,
    "station",    "Station",                                                            NA,            "Station",          NA,       NA,
    "lng",        "Longitude (Decimal Degrees)",                                        NA,            "Longitude",               "dimensionless",       "real",
    "lat",        "Latitude (Decimal Degrees)",                                         NA,            "Latitude",                "dimensionless",       "real")

source <- c(
  cosp    = "City of St. Pete",
  epchc   = "Hillsborough County EPC",
  esa     = "ESA",
  fldep   = "Florida DEP",
  mpnrd   = "Manatee County",
  ncf     = "New College FL",
  pinco   = "Pinellas County",
  sbep    = "Sarasota Bay Estuary Program",
  tbep    = "Tampa Bay Estuary Program",
  uf      = "University of Florida",
  usf     = "University of South Florida"
)
## Write these into the data.frame format
factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  )
)
  
attributeList <- set_attributes(attributes, factors, col_classes = c("character", "factor", "character", "numeric", "numeric"))

physical <- set_physical("rsstatloc.csv")

dataTable <- list(
  entityName = "rsstatloc.csv",
  entityDescription = "Tampa Bay - Water Quality Monitoring Station Locations",
  physical = physical,
  attributeList = attributeList)

geographicDescription <- "Tampa Bay Florida, Middle Tampa Bay"


coverage <-
  set_coverage(begin = '2021-03-23', end = '2021-11-17',
               geographicDescription = geographicDescription,
               west = -82.8, east = -82.4,
               north = 27.9, south = 27.4, )

R_person <- person("Marcus", "Beck", email = "mbeck@tbep.org")
mbeck <- as_emld(R_person)
HF_address <- list(
  deliveryPoint = "263 13th Ave South",
  city = "St. Petersburg",
  administrativeArea = "FL",
  postalCode = "33701",
  country = "USA")
publisher <- list(
  organizationName = "Tampa Bay Estuary Program",
  address = HF_address)
contact <-
  list(
    individualName = mbeck$individualName,
    electronicMailAddress = mbeck$electronicMailAddress,
    address = HF_address,
    organizationName = "Tampa Bay Estuary Program",
    phone = "(727) 893-2765 x205")
keywordSet <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list("estuaries",
                   "oceans",
                   "algae",
                   "water quality",
                   "watersheds",
                   "phosphate",
                   "phosphorus",
                   "nitrogen",
                   "phytoplankton",
                   "plankton",
                   "seawater",
                   "seagrass",
                   "nutrients",
                   "geographic information systems",
                   "marine")
  ),
  list(
    keywordThesaurus = "LTER core area",
    keyword =  list("organic matter movement", "mineral cycling", "disturbance")
  ))
  
  title <- "Water Quality Sampling near Piney Point Station locations"
  
  abstract <- 'From March 30th to April 9th, approximately 215 million gallons of wastewater from Piney Point were released into Tampa Bay. The Tampa Bay Estuary Program is working with regional partners to coordinate and synthesize water quality, benthic, seagrass, and fisheries monitoring data. The primary pollutants of concern for this discharge are phosphorus and nitrogen (primarily ammonia nitrogen), which may stimulate an algae response and cause adverse effects on seagrass, fish, and other wildlife. 
  This dataset contains spatial data for each unique station.  This dataset relates gives spatial data for chemical and physical water quality measurements.'

  intellectualRights <- 'This dataset is released to the public and may be freely downloaded. Please keep the designated Contact person informed of any plans to use the dataset. Consultation or collaboration with the original investigators is strongly encouraged. Publications and data products that make use of the dataset must include proper acknowledgement. For more information on LTER Network data access and use policies, please see: http://www.lternet.edu/data/netpolicy.html.'
  
  # combine the metadata
  dataset <- list(
    title = title,
    creator = mbeck,
    intellectualRights = intellectualRights,
    abstract = abstract,
    keywordSet = keywordSet,
    coverage = coverage,
    contact = contact,
    dataTable = dataTable)
  
  eml <- list(
    packageId = uuid::UUIDgenerate(),
    system = "uuid", # type of identifier
    dataset = dataset)

    # write and validate the file
  write_eml(eml, "rsstatloc.xml")
  eml_validate("rsstatloc.xml")
  