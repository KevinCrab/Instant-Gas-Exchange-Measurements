\name{AllValue}
\alias{AllValue}
\docType{data}
\title{
AllValue
%%   ~~ data name/kind ... ~~
}
\description{
AllValue is GXvalue object, example of GetValue function output. In each row it records the physiology measurements for each leaf sample as well as its identity.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("AllValue")}
\format{
  GXvalue class object
}
\details{
A data frame with 454 observations from part of my real measurements. "plot_name", "leaf_name", "name", "genotype", "range", "row", "block" columns record the identity of the leaf sample and the others are physiological measurements.
%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
data(AllValue)
str(AllValue)
}
\keyword{datasets}
