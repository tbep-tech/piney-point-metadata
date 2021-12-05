library(EML)
library(emld)
library(tidyverse)

# character columns need definition (in attributes)
# numeric needs unit and numbertype (see EML::get_unitList()$units) (in attributes)
# factors need to be defined (in attribute list)

# wqdat -------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                         ~formatString, ~definition,        ~unit,   ~numberType,
    "station",    "Station",                                      NA,            "Station",          NA,       NA,
    "date",       "Date",                                         "YYYY-MM-DD",  NA,                 NA,       NA,
    "source",     "Agency that took Sample - short hand",         NA,            NA,                 NA,       NA,
    "var",        "Variable - short hand",                        NA,            NA,                 NA,       NA,
    "uni",        "Result Unit of measure - short hand",          NA,            NA,                 NA,       NA,
    "val",        "Result Value",                                 NA,            NA,             "dimensionless",       "real",
    "qual",       "Qualifier value using FDEP scheme (https://floridadep.gov/lt/files/62-160_help-document_0.pdf)", NA,            "Qual code",                 NA,       NA,
    "bswqstation", "Corresponding Long-term Reference Station",   NA,            "Corresonding long-term refeference station",                 NA,       NA,
    "lbs",        "Variable - Label",                             NA,            "Variable label",      NA,       NA,
    "nrmrng",     "Normal Value Range of Result",                 NA,            "Normal value range",  NA,       NA,
    "lbunis",     "Result Unit of measure - Label",               NA,            "Result unit",         NA,       NA,
    "inrng",      "Result in Range of Normal value",              NA,            "Result in range",     NA,       NA)

source <- c(
  cosp = "City of St. Pete",
  epchc = "Hillsborough County EPC",
  esa = "ESA",
  fldep = "Florida DEP",
  mpnrd = "Manatee County",
  ncf = "New College FL",
  pinco = "Pinellas County",
  sbep = "Sarasota Bay Estuary Program",
  tbep = "Tampa Bay Estuary Program",
  uf = "University of Florida",
  usf = "University of South Florida"
)

var <- c(
  bod = "Biological Oxygen Demand",
  chla = "Chlorophyl a",
  color = "Color",
  do = "Dissolved Oxygen (mg/L)",
  dosat = "Dissolved Oxygen (Percent Saturation)",
  nh34 = "NH3 NH4+",
  no23 = "Nitrate-Nitrite",
  orthop = "Ortho-Phosphorus",
  ph = "pH",
  sal = "Salinity",
  secchi = "Secchi",
  temp = "Temperature (C)",
  tkn = "Total Kjeldahl Nitrogen",
  tn = "Total Nitrogen",
  tp = "Total Phosphorus",
  tss = "Total Suspended Solids",
  turb = "Turbidity"
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

# combine factors
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

attributeList <- set_attributes(attributes, factors, col_classes = c("character", "Date", "factor", "factor", "factor", "numeric", "character", "character", "character", "character", "character", "character"))

physical <- set_physical("wqdat.csv")

wqdatTable <- list(
  entityName = "wqdat.csv",
  entityDescription = "Water quality monitoring data",
  physical = physical,
  attributeList = attributeList)

# wqpts -------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                 ~formatString, ~definition,        ~unit,   ~numberType,
    "Source_lng", "Agency that took sample - Label",      NA,           "Source Label",       NA,       NA, 
    "source",     "Agency that took Sample - short hand", NA,            "Source Short hand", NA,       NA,
    "station",    "Station",                              NA,            "Station",           NA,       NA,
    "lng",        "Longitude (Decimal Degrees)",          NA,            "Longitude",         "dimensionless",      "real",
    "lat",        "Latitude (Decimal Degrees)",           NA,            "Latitude",          "dimensionless",      "real",)

source <- c(
  cosp = "City of St. Pete",
  epchc = "Hillsborough County EPC",
  esa = "ESA",
  fldep = "Florida DEP",
  mpnrd = "Manatee County",
  ncf = "New College FL",
  pinco = "Pinellas County",
  sbep = "Sarasota Bay Estuary Program",
  tbep = "Tampa Bay Estuary Program",
  uf = "University of Florida",
  usf = "University of South Florida"
)

# combine factors
factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  )
)

attributeList <- set_attributes(attributes, factors, col_classes = c("character", "factor", "character", "numeric", "numeric"))

physical <- set_physical("wqpts.csv")

wqptsTable <- list(
  entityName = "wqpts.csv",
  entityDescription = "Water quality monitoring station locations",
  physical = physical,
  attributeList = attributeList)

# trndat ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                 ~formatString, ~definition, ~unit, ~numberType,
    "date",        "Date",                                "YYYY-MM-DD",  NA,        NA,        NA,
    "station",     "Station",                             NA,            "Station", NA,        NA,
    "location",    "Location on the transect",            NA,            NA,        'meter',   'real', 
    "typ",         "Type of taxa",                        NA,            NA,        NA,        NA,
    "taxa",        "Taxa",                                NA,            NA,        NA,        NA,
    "abundance",   "Braun-Blanquet abundance as percent", NA,            NA,        NA,        NA,
    "bb",          "Braun-Blanquet abundance as integer", NA,            NA,        NA,        NA
  )

typ <- c(
  mcr = "Macroalgae",
  sav = "Submerged Aquatic Vegetation"
)

taxa <- c(
  Red = 'Red macroalgae (rhodophytes)', 
  Green = 'Green macroalgae (chlorophytes)', 
  Brown = 'Brown macroalgae', 
  Cyanobacteria = 'Cyanobacteria macroalgae, e.g., Dapis sp.', 
  `Thalassia testudinium` = 'Turtle grass', 
  `Halodule wrightii` = 'Shoal grass', 
  `Syringodium filiforme` = 'Manatee grass', 
  `Ruppia maritima` = 'Widgeon grass', 
  `Halophila elgelmanii` = 'Star grass', 
  `Halophila decipiens` = 'Carribbean grass'
)

abundance <- c(
  `<1%` = 'less than 1% cover', 
  `1-5%` = '1-5% cover', 
  `6-25%` = '6-25% cover', 
  `26-50%` = '26-50% cover', 
  `51-75%` = '51-75% cover', 
  `76-100%` = '76-100% cover'
)

bb <- c(
  `0` = 'less than 1% cover', 
  `1` = '1-5% cover', 
  `2` = '6-25% cover', 
  `3` = '26-50% cover', 
  `4` = '51-75% cover', 
  `5` = '76-100% cover'
)

# combine factors
factors <- rbind(
  data.frame(
    attributeName = "typ",
    code = names(typ),
    definition = unname(typ)
  ),
  data.frame(
    attributeName = "taxa",
    code = names(taxa),
    definition = unname(taxa)
  ),
  data.frame(
    attributeName = "abundance",
    code = names(abundance),
    definition = unname(abundance)
  ),
  data.frame(
    attributeName = "bb",
    code = names(bb),
    definition = unname(bb)
  )
)

attributeList <- set_attributes(attributes, factors, col_classes = c("Date", "character", "numeric", "factor", "factor", "factor", "factor"))

physical <- set_physical("trndat.csv")

trndatTable <- list(
  entityName = "trndat.csv",
  entityDescription = "Seagrass and macroalgae monitoring data",
  physical = physical,
  attributeList = attributeList)

# trnpts ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                 ~formatString, ~definition,   ~unit,          ~numberType,
    "source",     "Agency that surveyed the transect - short hand", NA,  "Source",      NA,              NA,
    "station",    "Station",                              NA,            "Station",     NA,              NA,
    "lng",        "Longitude (Decimal Degrees)",          NA,            NA,            "dimensionless", "real",
    "lat",        "Latitude (Decimal Degrees)",           NA,            NA,            "dimensionless", "real")

source <- c(
  esa = "ESA",
  sbep = "Sarasota Bay Estuary Program",
  TBEP = "Tampa Bay Estuary Program",
  uf = "University of Florida"
)

# combine factors
factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  )
)

attributeList <- set_attributes(attributes, factors, col_classes = c("factor", "character", "numeric", "numeric"))

physical <- set_physical("trnpts.csv")

trnptsTable <- list(
  entityName = "trnpts.csv",
  entityDescription = "Seagrass and macroalgae monitoring station locations",
  physical = physical,
  attributeList = attributeList)

# phydat ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                 ~formatString,  ~definition,  ~unit,           ~numberType,
    "date",       "Date",                                 'YYYY-MM-DD',   NA,           NA,              NA,
    "station",    "Station",                              NA,             "Station",    NA,              NA,
    "source",     "Agency that took Sample - short hand", NA,             "Source",     NA,              NA,
    "typ",        "Sample type",                          NA,             NA,           NA,              NA, 
    "species",    "Species",                              NA,             "Species",    NA,              NA, 
    "val",        "Cell concentration",                   NA,             NA,           "dimensionless", "real", 
    "valqual",    "Qualitative cell concentration",       NA,             NA,           NA,              NA, 
    "uni",        "Units (cells/L)",                      NA,             "Units",      NA,              NA,
  )

source <- c(
  epchc = "Hillsborough County EPC",
  fldep = "Florida DEP",
  fwri = "Fish and Wildlife Research Institute",
  pinco = "Pinellas County"
)
typ <- c(
  Qualitative = "Qualitative sample (present)",
  Quantitative = "Quantitative sample as cells/L" 
)
valqual <- c(
  `Very Low` = 'cells/L < 1e4', 
  Low = 'cells/L between 1e4 and 1e5', 
  Medium = 'cells/L between 1e5 and 1e6 ', 
  High = 'cells/L > 1e6'
)

# combine factors
factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  ),
  data.frame(
    attributeName = "typ",
    code = names(typ),
    definition = unname(typ)
  ),
  data.frame(
    attributeName = "valqual",
    code = names(valqual),
    definition = unname(valqual)
  )
)

attributeList <- set_attributes(attributes, factors, col_classes = c("Date", "character", "factor", "factor", "character", "numeric", "factor", "character"))

physical <- set_physical("phydat.csv")

phydatTable <- list(
  entityName = "phydat.csv",
  entityDescription = "Phytoplankton monitoring data",
  physical = physical,
  attributeList = attributeList)

# phypts ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                         ~formatString,  ~definition,          ~unit,          ~numberType,
    "source",     "Agency that surveyed the station - short hand", NA,            NA,                   NA,              NA,
    "source_lng", "Agency that surveyed the station - long hand",  NA,            "Source - long hand", NA,              NA,
    "station",    "Station",                                       NA,            "Station",            NA,              NA,
    "lng",        "Longitude (Decimal Degrees)",                   NA,            NA,                   "dimensionless", "real",
    "lat",        "Latitude (Decimal Degrees)",                    NA,            NA,                   "dimensionless", "real",
    )

source <- c(
  epchc = "Hillsborough County EPC",
  fldep = "Florida DEP",
  fwri = "Fish and Wildlife Research Institute",
  pinco = "Pinellas County"
)

factors <- rbind(
  data.frame(
    attributeName = "source",
    code = names(source),
    definition = unname(source)
  )
)

attributeList <- set_attributes(attributes, factors, col_classes = c("factor", "character", "character", "numeric", "numeric"))

physical <- set_physical("phypts.csv")

phyptsTable <- list(
  entityName = "phypts.csv",
  entityDescription = "Phytoplankton monitoring station locations",
  physical = physical,
  attributeList = attributeList)

# kbrdat ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,                ~formatString,  ~definition, ~unit, ~numberType,
    "date",       "Date",                                "YYYY-MM-DD",  NA,           NA,       NA,
    "station",    "Station",                             NA,            "Station",    NA,       NA,
    "var",        "Variable - short hand",               NA,            NA,           NA,       NA,
    "uni",        "Result unit of measure - short hand", NA,            NA,           NA,       NA,
    "val",        "Result value",                        NA,            NA,           "dimensionless",       "real",
  )

var <- c(
  kb = 'Karenia brevis', 
  sal = 'Salinity', 
  temp = 'Water temperature'
)
uni <- c(
  `100kcelll` = '100k cells/L', 
  `c` = 'Degrees celsius', 
  ppt = 'parts per thousand'
)

factors <- rbind(
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

attributeList <- set_attributes(attributes, factors, col_classes = c("Date", "character", "factor", "factor", "numeric"))

physical <- set_physical("kbrdat.csv")

kbrdatTable <- list(
  entityName = "kbrdat.csv",
  entityDescription = "karenia brevis monitoring data",
  physical = physical,
  attributeList = attributeList)

# kbrpts ------------------------------------------------------------------

attributes <-
  tibble::tribble(
    ~attributeName, ~attributeDefinition,         ~formatString, ~definition,  ~unit,           ~numberType,
    "station",    "Station",                      NA,            "Station",    NA,              NA,
    "lng",        "Longitude (Decimal Degrees)",  NA,            NA,           "dimensionless", "real",
    "lat",        "Latitude (Decimal Degrees)",   NA,            NA,           "dimensionless", "real",
  )

attributeList <- set_attributes(attributes, col_classes = c("character", "numeric", "numeric"))

physical <- set_physical("kbrpts.csv")

kbrptsTable <- list(
  entityName = "kbrpts.csv",
  entityDescription = "Karenia brevis monitoring station locations",
  physical = physical,
  attributeList = attributeList)

# universal metadata for all files ----------------------------------------

geographicDescription <- "Tampa Bay Florida, Middle/Lower Tampa Bay and portions of northern Sarasota Bay"

coverage <- set_coverage(
  begin = '2021-03-23', 
  end = '2021-10-01',
  geographicDescription = geographicDescription,
  west = -82.8, 
  east = -82.4,
  north = 27.9, 
  south = 27.4
)

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
contact <- list(
  individualName = mbeck$individualName,
  electronicMailAddress = mbeck$electronicMailAddress,
  address = HF_address,
  organizationName = "Tampa Bay Estuary Program",
  phone = "(727) 893-2765"
)

keywordSet <- list(
  list(
    keywordThesaurus = "Tampa Bay vocabulary",
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
  ))

title <- "2021 Piney Point Sampling in Tampa Bay"

abstract <- 'From March 30th to April 9th, approximately 215 million gallons of wastewater from Piney Point were released into Tampa Bay. The Tampa Bay Estuary Program is working with regional partners to coordinate and synthesize water quality, benthic, seagrass, and fisheries monitoring data. The primary pollutants of concern for this discharge are phosphorus and nitrogen (primarily ammonia nitrogen), which may stimulate an algae response and cause adverse effects on seagrass, fish, and other wildlife.'

intellectualRights <- 'This dataset is released to the public and may be freely downloaded. Please keep the designated Contact person informed of any plans to use the dataset. Consultation or collaboration with the original investigators is strongly encouraged. Publications and data products that make use of the dataset must include proper acknowledgement.'

# combine the metadata
dataset <- list(
  title = title,
  creator = mbeck,
  intellectualRights = intellectualRights,
  abstract = abstract,
  keywordSet = keywordSet,
  coverage = coverage,
  contact = contact,
  dataTable = list(wqdatTable, wqptsTable, trndatTable, trnptsTable, phydatTable, phyptsTable, kbrdatTable, kbrptsTable))

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset)

# write and validate the file
write_eml(eml, "ppdat.xml")
eml_validate("ppdat.xml")
