\name{CollectEpochs}
\alias{CollectEpochs}
\title{Collects all Epochs in an Experiment or EpochGroup}
\usage{
  CollectEpochs(container)
}
\arguments{
  \item{container}{Experiment or EpochGroup}
}
\value{
  Vector of Epochs
}
\description{
  Recursively collects all Epochs in an Experiment or
  EpochGroup in to a Vector. Epoch order is not guaranteed.
  You may want to sort the resulting vector by Epoch
  \code{startTime}.
}

