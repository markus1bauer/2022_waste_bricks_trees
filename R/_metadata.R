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


creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus",
    surName = "Bauer"),
  electronicMailAddress = "markusbauer@mailbox.org"
)

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

contact <-
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    phone = "0049-152-56391781"
    )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create site and experiment infos #########################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "Coming up"

keywordSet <- list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "substrates",
      "tree growth"
    )
)

geographicDescription <- "Greenhouse in D?rnast near Freising"

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
    title = "Coming up",
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
