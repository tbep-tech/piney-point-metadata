#Set library
library(EML)
library(emld)
library(tidyverse)

# character columns need definition
# numeric needs unit and numbertype (see EML::get_unitList()$units)
# factors need to be defined

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                                                 ~formatString, ~definition,        ~unit,   ~numberType,
    "station",    "Station",                                                            NA,            "Station",          NA,       NA,
    "date",       "Date",                                                               "YYYY-MM-DD",  NA,                 NA,       NA,
    "source",     "Agency that took Sample - short hand",                               NA,            NA,                 NA,       NA,
    "var",        "Variable - short hand",                                              NA,            NA,                 NA,       NA,
    "uni",        "Result Unit of measure - short hand",                                NA,            NA,                 NA,       NA,
    "val",        "Result Value",                                                       NA,            NA,             "dimensionless",       "real",
    "qual",       "Qualifier value using FDEP scheme (https://floridadep.gov/sites/default/files/62-160_help-document_0.pdf)", NA,            "Qual code",                 NA,       NA,
    "bswqstation", "Corresponding Long-term Reference Station",                         NA,            "Corresonding long-term refeference station",                 NA,       NA,
    "lbs",        "Variable - Label",                                                   NA,            "Variable label",                 NA,       NA,
    "nrmrng",     "Normal Value Range of Result",                                       NA,            "Normal value range",                 NA,       NA,
    "lbunis",     "Result Unit of measure - Label",                                     NA,            "Result unit",                 NA,       NA,
    "inrng",      "Result in Range of Normal value",                                    NA,            "Result in range",                 NA,       NA)


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

var <- c(
  bod    ="Biological Oxygen Demand",
  chla   ="Chlorophyl a",
  color  ="Color",
  do     ="Dissolved Oxygen (mg/L)",
  dosat  ="Dissolved Oxygen (Percent Saturation)",
  nh34   ="NH3 NH4+",
  no23 = "Nitrate-Nitrite",
  orthop ="Ortho-Phosphorus",
  ph     ="pH",
  sal    ="Salinity",
  secchi ="Secchi",
  temp   ="Temperature (C)",
  tkn    ="Total Kjeldahl Nitrogen",
  tn     ="Total Nitrogen",
  tp     ="Total Phosphorus",
  tss    ="Total Suspended Solids",
  turb   ="Turbidity"
) 

uni <- c(
  c = "celsius", 
  m = "meters", 
  mgl = "mg/L", 
  none = "no units", 
  ntu = "nephalometric turbidity units", 
  pcu = "platinum cobalt units", 
  per = "percent", 
  ppt = "parts per thousand", 
  ugl = "ug/L"
)

## Write these into the data.frame format
factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  ),
  data.frame(
    attributeName = "var",
    code = names(var),
    definition = unname(var)
  ),
  data.frame(
     attributeName = "uni",
     code = names(uni),
     definition = unname(uni)
  )
)

attributeList <- set_attributes(attributes, factors, 
                                col_classes = c("character", "Date", "factor", "factor", "factor", "numeric", "character", "character", "character", "character", "character", "character"))

physical <- set_physical("rswqdat.csv")

dataTable <- list(
  entityName = "rswqdat.csv",
  entityDescription = "Tampa Bay - Piney Point Water Quality Monitoring",
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
    phone = "(727) 893-2765")
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
                   "marine")
  ),
  list(
    keywordThesaurus = "LTER core area",
    keyword =  list("organic matter movement", "mineral cycling", "disturbance")
  ))
  
  title <- "Water Quality Sampling near Piney Point"
  
  abstract <- 'From March 30th to April 9th, approximately 215 million gallons of wastewater from Piney Point were released into Tampa Bay. The Tampa Bay Estuary Program is working with regional partners to coordinate and synthesize water quality, benthic, seagrass, and fisheries monitoring data. The primary pollutants of concern for this discharge are phosphorus and nitrogen (primarily ammonia nitrogen), which may stimulate an algae response and cause adverse effects on seagrass, fish, and other wildlife.'
  
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
  write_eml(eml, "rswqdat.xml")
  eml_validate("rswqdat.xml")
  