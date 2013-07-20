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

DayEnds <- function(date.time) {
  midnight <- date.time$toDateMidnight()$toDateTime()
  return(list(start = midnight,
         end = midnight$plusDays(as.integer(1))$minusSeconds(as.integer(1))))
}

habitats <- list(m = 'mesic', d = 'dry', w = 'wet')

#' Imports field data from the "legacy" tempalte
#' 
#' This import creates an EpochGroup for each date, an Epoch for each site and a Measurement
#' for each species within the site.
#' 
#' @param csv.path Path to the CSV file
#' @param context Ovation DataContext
#' @param protocol.uri String containing the URI of the measurement Protocol
#' @param container.ui String containing the URI of the EpochGroupContainer (i.e. Experiment or EpochGroup) that will hold the newly inserted data
#' @param tzone Data collection timezone [Default = local time zone]
#' @return list of inserted EpochGroups
#' @export
ImportLegacyCSV <- function(csv.path, context, protocol.uri, container.uri, tzone=NULL) {
  tzone <- time.zone(tzone)
  
  # Read the CSV file
  df <- read.csv(csv.path)
  
  # Retrieve protocol and container
  protocol <- context$getObjectWithURI(protocol.uri)
  container <- context$getObjectWithURI(container.uri)
  
  # Insert Sources if not already present
  print("Checking Sources...")
  sites <- list()
  apply(df, 1, function(r) {
    plot.name <- r['PLOT']
    habitat <- habitats[[r['HABITAT']]]
    if(is.null(sites[[plot.name]])) {
      plot.label <- sprintf("Plot %s", plot.name)
      existing.sites <- as.list(context$getSourcesWithIdentifier(plot.name))
      
      if(length(existing.sites) == 0) {
        print(sprintf("  Adding plot %s...", plot.name))
        sites[[plot.name]] <<- context$insertSource(plot.label, plot.name)
      } else {
        sites[[plot.name]] <<- existing.sites[[1]]
      }
      
      sites[[plot.name]]$addProperty('habitat', habitat)
    }
    
  })
  
  # Insert data group by Plot,Date
  epoch.groups <- list()
  d_ply(df, .variables=.(PLOT, YEAR, DOY), 
        #.progress = progress_text(char = "."), 
        .fun=function(g) {
          date <- MakeDate(unique(g$YEAR), unique(g$DOY), tzone$getID())
          
          plot.name <- unique(g$PLOT)
          plot <- sites[[plot.name]]
          day.ends <- DayEnds(date)
          start <- day.ends$start
          group.name <- sprintf("%s-%s-%s", start$getYear(), start$getMonthOfYear(), start$getDayOfMonth())
          if(is.null(epoch.groups[[start$toString()]])) {
            print(sprintf("Adding EpochGroup %s", group.name))
            group <<- container$insertEpochGroup(group.name,
                                                 start,
                                                 protocol,
                                                 new(J("java.util.HashMap")),
                                                 new(J("java.util.HashMap")))
            epoch.groups[[start$toString()]] <<- group
          }
          
          
          # Insert an Epoch for these measurements
          input.sources <- new(J("java.util.HashMap"))
          input.sources$put(as.character(plot.name), plot)
          
          epoch <- epoch.groups[[start$toString()]]$insertEpoch(input.sources,
                                                                NULL,
                                                                start,
                                                                day.ends$end,
                                                                protocol,
                                                                new(J("java.util.HashMap")),
                                                                new(J("java.util.HashMap")))
          
          # Insert a measurement per row
          apply(g, 1, function(r) {
            species <- r['SPECIES']
            print(sprintf("  %s...", species))
            
            flower.count <- as.integer(r['X..FLOWERS'])
            measurement.df <- data.frame(Species = species,
                                         FLOWER_COUNT = flower.count)
            file.name <- tempfile(pattern=group.name, fileext=".csv")
            write.csv(measurement.df, file=file.name, row.names=FALSE)
            
            plot$addTag(as.character(species))
            
            source.names = Vector2Set(c(as.character(plot.name)))
            epoch$insertMeasurement(as.character(species),
                                    source.names,
                                    Vector2Set(c()),
                                    NewUrl(file.name),
                                    "text/csv")
          })
          
          
        })
  
  print("Don't forget to wait for data upload before quiting R!")
  
  return(epoch.groups)
}

