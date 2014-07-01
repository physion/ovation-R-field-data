library(plyr)

#' Initializes the Rovation environment
#' @export
InitRovation <- function() {
  if(Sys.info()['sysname'] == "Darwin") {
    Sys.setenv(NOAWT=1)  
  }
  
  library(Rovation)
  
  if(Sys.info()['sysname'] == "Darwin") {
    Sys.unsetenv("NOAWT")
  }
}

DateTimeZone <- J("org.joda.time.DateTimeZone")
time.zone <- function (tzone) {
  if(is.null(tzone)) {
    tzone <- DateTimeZone$getDefault()
  } else {
    tzone <- DateTimeZone$forID(tzone)
  }
}

MakeDate <- function(year, day_of_year, tzone=NULL) {
  tzone <- time.zone(tzone)
  
  d <- new(J('org.joda.time.DateTime'))$withZone(tzone)
  
  return(d$withYear(as.integer(year))$withDayOfYear(as.integer(day_of_year))$toDateMidnight()$toDateTime())
}


habitats <- list(m = 'mesic', d = 'dry', w = 'wet')

#' Imports field data from the "legacy" template
#' 
#' This import creates an Epoch for each year within a single Epoch container
#' 
#' @param csv.path Path to the CSV file
#' @param context Ovation DataContext
#' @param protocol.uri String containing the URI of the measurement Protocol
#' @param container.uri String containing the URI of the EpochContainer (i.e. Experiment or EpochGroup) that will hold the newly inserted data
#' @param tzone Data collection timezone name (String) [Default = local time zone]
#' @return list of inserted EpochGroups
#' @export
ImportLegacyCSV <- function(csv.path, context, protocol.uri, container.uri, tzone=NULL) {
  tzone <- time.zone(tzone)
  
  # Read the CSV file
  df <- read.csv(csv.path)
  
  # Retrieve protocol and container
  protocol <- context$getObjectWithURI(protocol.uri)
  container <- context$getObjectWithURI(container.uri)
  
  cat("\n\n")

  known.sites <- new(J("java.util.HashSet"))
  
  
  # Insert data group by YEAR
  cat("\n\n")
  cat("Importing data\n")
  cat("**************\n")
  swallow <- d_ply(df, .variables=.(YEAR), 
                   #.progress = progress_text(char = "."), 
                   .fun=function(g) {
                     
                     year = unique(g$YEAR)
                    
                     cat(sprintf("Importing %s... \n", as.character(year)))
                     start.doy = min(g$DOY)
                     end.doy = max(g$DOY)
                     
                     start.date <- MakeDate(year, start.doy, tzone$getID())
                     end.date <- MakeDate(year, end.doy, tzone$getID())
                     
                     plot.sites <- unique(g$PLOT)
                     
                     input.sources <- new(J("java.util.HashMap"))
                     lapply(as.character(plot.sites), function(plot) {
                       plot.label <- sprintf("Plot %s", plot)
                       plot.id <- sprintf("rmbl-inouye-phenology-%s", plot)
                       srcResult <- context$getOrInsertSource(plot.label, plot.id)
                       src <- srcResult$get()
                       known.sites$add(src)
                       input.sources$put(plot, src)
                     })
                     
                     # Insert an Epoch for these measurements
                     epochGroup <- container$insertEpochGroup(sprintf("%s", start.date$getYear()),
                                                              start.date,
                                                              NULL,
                                                              new(J("java.util.HashMap")),
                                                              new(J("java.util.HashMap")))
                     
                     epoch <- epochGroup$insertEpoch(input.sources,
                                                    NULL,
                                                    start.date,
                                                    end.date,
                                                    protocol,
                                                    new(J("java.util.HashMap")),
                                                    new(J("java.util.HashMap")))
                     
                     # Insert a measurement for this group
                     file.desc <- sprintf("%s", start.date$getYear())
                     file.name <- tempfile(pattern=file.desc, fileext=".csv")
                     write.csv(g, file=file.name, row.names=FALSE)
                     
                     source.names <- Vector2Set(c(as.character(unique(g$PLOT))))
                     measurement.name <- sprintf("%s.csv", file.desc)
                     cat(sprintf("    %s \n", measurement.name))
                     epoch$insertMeasurement(measurement.name,
                                             source.names,
                                             Vector2Set(c()),
                                             NewUrl(file.name),
                                             'text/csv');
                     
                     
                     
                     WaitForPendingUploads(context)
                   })
  
  # Update Plot tags
  cat("Updating tags\n")
  cat("*************\n")
  sources <- new(J("java.util.HashMap"))
  swallow <- d_ply(df, .variables=.(SPECIES, PLOT), 
                   #.progress = progress_text(char = "."), 
                   .fun=function(gg) {
                     
                     species <- as.character(unique(gg$SPECIES))
                     plot.name <- as.character(unique(gg$PLOT))
                     
                     cat(sprintf("      %s -> %s", species, plot.name))
                     plot.label <- sprintf("Plot %s", plot.name)
                     plot.id <- sprintf("rmbl-phenology-%s", plot.name)
                     
                     if(sources$containsKey(plot.name)) {
                       src <- sources$get(plot.name)
                     } else {
                       src <- context$getOrInsertSource(plot.label, plot.id)$get()  
                       sources$put(plot.name, src)
                     }
                     
                     src$addTag(species)
                   });
  return(container)
}

