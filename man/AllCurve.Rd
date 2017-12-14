\name{AllCurve}
\alias{AllCurve}
\docType{data}
\title{
AllCurve
%%   ~~ data name/kind ... ~~
}
\description{
AllCurve is GXcurve object, example of read.GX function output. Each row is an recording point in a 4-min period measurement for a leaf sample. 


%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("AllCurve")}
\format{
GXcurve class object
}
\details{
A data frame with 26839 observations. "Photo", "Cond", "Ci/Ca" are short for photosythesis, water conductance and internal to atmospheric CO2 concentration. Other parameters record the identity of leaf sample.

%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
data(AllCurve)
str(AllCurve)
}
\keyword{datasets}
