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
  # Insert Sources if not already present
  cat("Checking Sources\n")
  cat("****************\n")
  known.sites <- new(J("java.util.HashSet"))
#   swallow <- apply(df, 1, function(r) {
#     plot.name <- r['PLOT']
#     if(!known.sites$contains(plot.name)) {
#       habitat <- habitats[[r['HABITAT']]]
#       plot.label <- sprintf("Plot %s", plot.name)
#       plot.id <- sprintf("rmbl-phenology-%s", plot.name)
#       cat(sprintf("  Checking %s...\n", plot.id))
#       
#       srcResult <- context$getOrInsertSource(plot.label, plot.id)
#       
#       if(srcResult$isNew()) {
#         srcResult$get()$addProperty('habitat', habitat)
#       }
#       
#       known.sites$add(plot.name)
#     }
#     
#   })
  
  
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
                       plot.id <- sprintf("rmbl-phenology-%s", plot)
                       srcResult <- context$getOrInsertSource(plot.label, plot.id)
                       input.sources$put(plot, srcResult$get())
                     })
                     
                     # Insert an Epoch for these measurements
                     
                     epoch <- container$insertEpoch(input.sources,
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
                     
                     
                     # Update Plot tags
                     cat("    Adding species tags...\n")
                     sources <- new(J("java.util.HashMap"))
                     apply(g, 1, function(r) {
                       species <- r['SPECIES']
                       plot.name <- r['PLOT']
                       plot.label <- sprintf("Plot %s", plot.name)
                       plot.id <- sprintf("rmbl-phenology-%s", plot.name)
                       if(sources$containsKey(plot.name)) {
                         src <- sources$get(plot.name)
                       } else {
                         src <- context$getOrInsertSource(plot.label, plot.id)$get()  
                         sources$put(plot.name, src)
                       }
                       
                       cat(sprintf("      %s\n", species))
                       src$addTag(species)
                     })
                     
                     
                     WaitForPendingUploads(context)
                   })
  
  return(container)
}

