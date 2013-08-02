library(testthat)
Sys.setenv(NOAWT=1)
library(Rovation)
Sys.unsetenv("NOAWT")
library(rJava)


context("Utilities ")
test_that("makes dates from year and DOY", {
  year <- 2013
  day_of_year <- 302
  tzone <- "America/Chicago"
  expect_that(MakeDate(year, day_of_year, tzone=tzone)$getYear(), equals(year))
  expect_that(MakeDate(year, day_of_year, tzone=tzone)$getDayOfYear(), equals(day_of_year))
  expect_that(MakeDate(year, day_of_year, tzone=tzone)$getZone()$getID(), equals(tzone))
  expect_true(MakeDate(year, day_of_year, tzone=tzone)$equals(
    MakeDate(year, day_of_year, tzone=tzone)$toDateMidnight()$toDateTime()))
  
})

test_that("computes day bookends from DateTime", {
  
  d <- new(J("org.joda.time.DateTime"))
  ends <- DayEnds(d)
  midnight <- d$toDateMidnight()$toDateTime();
  expect_true(ends$start$equals(midnight))
  expect_true(ends$end$equals(midnight$plusDays(as.integer(1))$minusSeconds(as.integer(1))))    
  
})


context("Legacy CSV import")
test_that("imports legacy fixture", {
  TestWrapper("imports-legacy-fixture", function(ctx) {
    
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
    protocol <- ctx$insertProtocol("unused", "unused")
    
    protocol.parameters = list(param.1=1, param.2="abc")
    device.parameters = list(param.1=10, param.2="xyz")
    
    start.time <- Datetime(2013,1,1)
    end.time <- Datetime(2013,1,2)
    
    epochGroups <- ImportLegacyCSV('../fixtures/example_legacy_data.csv', 
                                   context=ctx, 
                                   protocol.uri=protocol$getURI()$toString(), 
                                   container.uri=exp$getURI()$toString(), 
                                   tzone="America/Denver")
    
    expect_that(is.null(epochGroups), is_false())
    expect_equal(length(epochGroups), 5)
  })
})

test_that("collates EpochGroups", {
  TestWrapper("collates-epoch-groups", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
    protocol <- ctx$insertProtocol("unused", "unused")
    
    protocol.parameters = list(param.1=1, param.2="abc")
    device.parameters = list(param.1=10, param.2="xyz")
    
    start.time <- Datetime(2013,1,1)
    end.time <- Datetime(2013,1,2)
    
    epochGroups1 <- ImportLegacyCSV('../fixtures/example_legacy_data.csv', 
                                   context=ctx, 
                                   protocol.uri=protocol$getURI()$toString(), 
                                   container.uri=exp$getURI()$toString(), 
                                   tzone="America/Denver")
    
    
    expected.count <- length(as.list(exp$getEpochGroups()))
    
    epochGroups2 <- ImportLegacyCSV('../fixtures/example_legacy_data.csv', 
                                   context=ctx, 
                                   protocol.uri=protocol$getURI()$toString(), 
                                   container.uri=exp$getURI()$toString(), 
                                   tzone="America/Denver")
    
#     cat(sprintf("epochGroups1: %d\n", length(epochGroups1)))
#     cat(sprintf("epochGroups2: %d\n", length(epochGroups2)))
#     cat(sprintf("expected: %d\n", expected.count))
    
    expect_equal(length(epochGroups1), length(epochGroups2))
    expect_equal(expected.count, length(as.list(exp$getEpochGroups())))
  })
})