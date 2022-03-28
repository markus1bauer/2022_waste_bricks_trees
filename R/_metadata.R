# Waste bricks for tree substrates
# Write metadata for this project ####
# Markus Bauer
# 2022-03-15

### Packages ###
library(here)
library(EML)

### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Create infos about persons ###############################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



address_tum <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany"
)

address_uw <- list(
  deliveryPoint = "Hermann-Hesse-Strasse 1",
  city = "Pfaffenhofen a.d.Ilm",
  administrativeArea = "Bayern",
  postalCode = "85276",
  country = "Germany"
)

address_lb <- list(
  deliveryPoint = "Ziegeleistrasse 15",
  city = "Vatersdorf",
  administrativeArea = "Bayern",
  postalCode = "84172",
  country = "Germany"
)

creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus",
    surName = "Bauer"
  ),
  positionName = "PhD student",
  organizationName = "Technical University of Munich",
  address = address_tum,
  electronicMailAddress = "markus1.bauer@tum.de",
  phone = "0049-152-56391781",
  id = "https://orcid.org/0000-0001-5372-4174"
)

associatedParty <- list(
  
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Martin",
      surName = "Krause"
    ),
    role = "Researcher",
    electronicMailAddress = "mek187@hotmail.de"
  ),
  
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Valentin",
      surName = "Heizinger"
    ),
    role = "Researcher",
    organizationName = "Leipfinger-Bader",
    electronicMailAddress = "valentin.heizinger@leipfinger-bader.de"
  ),
  
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Johannes",
      surName = "Kollmann"
    ),
    role = "Professor",
    organizationName = "Technical University of Munich",
    address = address,
    electronicMailAddress = "johannes.kollmann@tum.de",
    phone = "0049-8161-714144",
    id = "https://orcid.org/0000-0002-4990-3636"
  )
)

contact <-
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address_tum,
    organizationName = "Technical University of Munich",
    onlineUrl = "DOI address to the database"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create site and experiment infos #########################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "This study focuses on potential effects of brick-based substrates
on survival, growth and functional traits of two urban trees (Acer platanoides,
Tilia cordata). We compared the effects of brick quantity (5% vs. 30%),
pre-treatment with phosphoric acid, nutrient-poor vs. -rich soil,
and mycorrhiza inoculation upon saplings in two greenhouse experiments."

keywordSet <- list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "substrates",
      "tree growth"
    )
)

geographicDescription <- "Greenhouse in Duernast near Freising"

coverage <- set_coverage(
  begin = "2018-11-01", end = "2020-07-31",
  sci_names = list(
    list(
      Subdivision = "Spermatophytina",
      Order = "Malvales",
      Family = "Malvaceae",
      Genus = "Tilia",
      Species = "cordata"
      ),
    list(
      Subdivision = "Spermatophytina",
      Order = "Malvales",
      Family = "Malvaceae",
      Genus = "Acer",
      Species = "platanoides"
    )),
  geographicDescription = geographicDescription,
  west = 11.68919, east = 11.68919,
  north = 48.40523, south = 48.40523,
  altitudeMin = 484, altitudeMaximum = 484,
  altitudeUnits = "meter"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C finalize EML #############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
    title = "Ecological application of waste bricks: brick-augmented substrates have no adverse effects on urban trees",
    creator = creator,
    pubDate = "2021",
    language = "English",
    intellectualRights = "CC BY 4.0",
    abstract = abstract,
    keywordSet = keywordSet,
    coverage = coverage,
    contact = contact
    )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  # type of identifier
  system = "uuid",
  dataset = dataset
  )

write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")
