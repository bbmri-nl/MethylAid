\docType{methods}
\name{visualize,summarizedData-method}
\alias{visualize}
\alias{visualize,summarizedData-method}
\title{visualize the summarized 450k data}
\usage{
\S4method{visualize}{summarizedData}(object)
}
\arguments{
  \item{object}{summarizedData object}
}
\value{
lauches a web browser with the shiny application and
returns a data.frame with detected outliers
}
\description{
launch a shiny app for visualization of the summarized 450k
data
}
\details{
details
}
\examples{
library(minfiData)
baseDir <- system.file("extdata", package = "minfiData")
targets <- read.450k.sheet(baseDir)
data <- summarize(targets)
\dontrun{
visualize(data)
}
}

