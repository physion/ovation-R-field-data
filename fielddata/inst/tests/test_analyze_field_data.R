library(testthat)
Sys.setenv(NOAWT=1)
library(Rovation)
Sys.unsetenv("NOAWT")
library(rJava)

context("Collecting legacy measurements")
test_that("collects measurements", {
  TestWrapper("collects-measurements", function(ctx) {
    
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
    protocol <- ctx$insertProtocol("unused", "unused")
    
    protocol.parameters = list(param.1=1, param.2="abc")
    device.parameters = list(param.1=10, param.2="xyz")
    
    start.time <- Datetime(2013,1,1)
    end.time <- Datetime(2013,1,2)
    
    csv.path <- '../fixtures/example_legacy_data.csv'
    epochGroups <- ImportLegacyCSV(csv.path, 
                                   context=ctx, 
                                   protocol.uri=protocol$getURI()$toString(), 
                                   container.uri=exp$getURI()$toString(), 
                                   tzone="America/Denver")
    
    
    expected.df <- read.csv(csv.path)
    epochs <- CollectEpochs(exp)
    actual.df <- CollectLegacyMeasurements(epochs)
    
    expect_equal(nrow(expected.df), nrow(actual.df))
    for(col in colnames(expected.df)) {
      if(col == "X..FLOWERS") {
        expect_true("FLOWER_COUNT" %in% colnames(actual.df), "FLOWER_COUNT")
      }
      if(col != "X" && col != "X..FLOWERS") {
        expect_true(col %in% colnames(actual.df), col)
      }
    }
    
  })
})