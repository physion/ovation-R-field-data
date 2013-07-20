\name{ImportLegacyCSV}
\alias{ImportLegacyCSV}
\title{Imports field data from the "legacy" tempalte}
\usage{
  ImportLegacyCSV(csv.path, context, protocol.uri,
    container.uri, tzone = NULL)
}
\arguments{
  \item{csv.path}{Path to the CSV file}

  \item{context}{Ovation DataContext}

  \item{protocol.uri}{String containing the URI of the
  measurement Protocol}

  \item{container.ui}{String containing the URI of the
  EpochGroupContainer (i.e. Experiment or EpochGroup) that
  will hold the newly inserted data}

  \item{tzone}{Data collection timezone [Default = local
  time zone]}
}
\value{
  list of inserted EpochGroups
}
\description{
  This import creates an EpochGroup for each date, an Epoch
  for each site and a Measurement for each species within
  the site.
}

