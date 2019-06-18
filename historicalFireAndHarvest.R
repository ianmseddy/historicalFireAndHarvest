
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "historicalFireAndHarvest",
  description = "a module that uses the high resolution national forest change product by White et al. (2017) to annually reconstruct past disturbances", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(c("Ian"), "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9002", historicalFireAndHarvest = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "historicalFireAndHarvest.Rmd"),
  reqdPkgs = list("raster", "data.table"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("firstYear", "numeric", 1985, 1985, 2010, "The first year for which to generate disturbance layers"),
    defineParameter("lastYear", "numeric", 2011, 1986, 2011, "the last year for which to generate disturbance layers"),
    defineParameter(name = "includeLowConfidence", class = "logical", TRUE, NA, NA,
                    desc = "include disturbance classifications of lower confidence"),
    defineParameter(name = "includeHarvest", class = "logical", TRUE, NA, NA, "output annual harvest raster"),
    defineParameter(name = "includeFire", class = "logical", TRUE, NA, NA, "output annual fire raster"),
    defineParameter(name = ".plotInitialTime", class = 'numeric', NA, NA, NA, "time of first plot"),
    defineParameter(name = ".plotInterval", class = 'numeric', 1, NA, NA, "interval between plot events")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", desc = "study Area to crop rasters"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "Annual historical disturbances output as a raster with identical properties to rasterToMatch"),
    expectsInput(objectName = "disturbanceYear", objectClass = "RasterLayer",
                 desc = "filepath to a raster layer representing year of disturbance occurence",
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_change_year_1985_2011.zip"),
    expectsInput(objectName = "disturbanceType", objectClass = "RasterLayer",
                 desc = "filepath to a raster layer representing type of forest disturbance",
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_change_type_1985_2011.zip")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'rstCurrentBurn', objectClass = "RasterLayer", desc = "annual historical burned area"),
    createsOutput(objectName = 'rstCurrentHarvest', objectClass = "RasterLayer", desc = "annual historical harvest")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.historicalFireAndHarvest = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)

      sim <- scheduleEvent(sim, P(sim)$firstYear, "historicalFireAndHarvest", "buildDisturbanceRasters")

      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "historicalFireAndHarvest", "plot")


    },

    buildDisturbanceRasters = {
      sim <- buildDisturbance(sim)
      if (time(sim) < P(sim)$lastYear) {
        sim <- scheduleEvent(sim, time(sim) + 1, "historicalFireAndHarvest", "buildDisturbanceRasters")
      }

    },
    plot = {

      if (!is.null(sim$rstCurrentHarvest)) {
        Plot(sim$rstCurrentHarvest, col = c("grey", "green"), new = TRUE)
      }

      if (!is.null(sim$rstCurrentBurn)) {
        Plot(sim$rstCurrentBurn, col = c("grey", "red"), new = TRUE)
      }

      if (time(sim) < P(sim)$lastYear) {
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "historicalFireAndHarvest", "plot")
      }
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  return(invisible(sim))
}


### template for your event1
buildDisturbance <- function(sim) {

  if (P(sim)$includeHarvest) {
    if (P(sim)$includeLowConfidence) {
      valsToUse <- c(2,4,6)
    } else {
      valsToUse <- 2
    }
    sim$rstCurrentHarvest <- Cache(reclassifyRasters,
                                   disturbanceType = sim$disturbanceType,
                                   disturbanceYear = sim$disturbanceYear,
                                   time = time(sim),
                                   types = valsToUse,
                                   rtm = sim$rasterToMatch,
                                   studyArea = sim$studyArea,
                                   userTags = c("historicalFireAndHarvest", "harvest", time(sim)))

    sim$rstCurrentHarvest@data@attributes <- list("Year" = time(sim))
  }

  if (P(sim)$includeFire) {
    if (P(sim)$includeLowConfidence) {
      valsToUse <- c(1,3,5)
    } else {
      valsToUse <- 1
    }

    sim$rstCurrentBurn <- Cache(reclassifyRasters,
                                disturbanceType = sim$disturbanceType,
                                disturbanceYear = sim$disturbanceYear,
                                time = time(sim),
                                types = valsToUse,
                                rtm = sim$rasterToMatch,
                                studyArea = sim$studyArea,
                                userTags = c("historicalFireAndHarvest", "burn", time(sim)))

    sim$rstCurrentBurn@data@attributes <- list("Year" = time(sim))
    }

  return(invisible(sim))
}


reclassifyRasters <- function(disturbanceType, disturbanceYear, time, types, rtm, studyArea) {

  yearDT <- setDT(list(getValues(disturbanceYear)))
  yearDT[!V1 == time - 1900, V1 := 0]
  yearDT[!V1 == 0, V1 := 1]
  #all disturbances at time(sim) now 1

  typeDT <- setDT(list(getValues(disturbanceType)))
  #gets rid of NAs....
  typeDT[!V1 %in% types, V1 := 0]
  typeDT[V1 %in% types, V1 := 1]
  #all valsToUse now 1

  typeDT$V1[yearDT$V1 == 0] <- 0
  #all valsToUse that aren't time sim are now zero

  disturbanceType <- setValues(disturbanceType, typeDT$V1)
  resampledRas <- postProcess(disturbanceType,
                                    rasterToMatch = rtm,
                                    method = "ngb",
                                    studyArea = studyArea,
                                    filename2 = NULL)
  return(resampledRas)
}


.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- dataPath(sim)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("studyArea", sim)) {
    message("study area not supplied. Using random polygon in Alberta")
    #TODO: remove LandR once this is confirmed working
    studyArea <- LandR::randomStudyArea(size = 15000000000, seed = 23654)
    sim$studyArea <- studyArea
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    message("rasterToMatch not supplied. generating from LCC2005 using studyArea CRS")

    sim$rasterToMatch <- LandR::prepInputsLCC(year = 2005,
                                          destinationPath = dataPath(sim),
                                          studyArea = sim$studyArea,
                                          useSAcrs = TRUE,
                                          filename2 = TRUE,
                                          overwrite = TRUE,
                                          userTags = c("cacheTags", "rasterToMatch"))
  }

  if (!suppliedElsewhere("disturbanceYear", sim)){
    sim$disturbanceYear <- Cache(prepInputs, url = extractURL(objectName = "disturbanceYear", sim),
                                 targetFile = "C2C_change_year_1985_2011.tif",
                                 destinationPath = dPath,
                                 studyArea = sim$studyArea,
                                 filename2 = TRUE,
                                 userTags = c("disturbanceYear", currentModule(sim)),
                                 overwrite = TRUE)
  }

  if (!suppliedElsewhere("disturbanceType", sim)) {

    sim$disturbanceType <- Cache(prepInputs, url = extractURL(objectName = "disturbanceType", sim),
                                 targetFile = "C2C_change_type_1985_2011.tif",
                                 destinationPath = dPath,
                                 studyArea = sim$studyArea,
                                 userTags = c("disturbanceType", currentModule(sim)),
                                 overwrite = TRUE)
  }

  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
