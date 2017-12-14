\name{GetValue.GXcurve}
\alias{GetValue}
\alias{GetValue.GXcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Value
%%  ~~function to do ... ~~
}
\description{
Calculate the first-minute-value and last-minute-value for each measurement.First-minute-value is defined as the "Photosythesis","water conductance" and "Ci/Ca" values at the point when "water conductance" value achieves the biggest in the first minute. Last-minute-value is defined as the mean "Photosythesis","water conductance" and "Ci/Ca" values in the last minute.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetValue(table, ...)
\method{GetValue}{GXcurve}(table, first.minute = TRUE, last.minute = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
  The table consists all the observations for measurements.
%%     ~~Describe \code{table} here~~
}
  \item{first.minute}{
  If TRUE, include first-minute-value in the output. 
%%     ~~Describe \code{first.minute} here~~
}
  \item{last.minute}{
  If TRUE, include last-minute-value in the output. 
%%     ~~Describe \code{last.minute} here~~
}
}

\value{
Returns a data frame of GXvalue class. Each row are the values for desired parameters and leaf identity.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}

\examples{
# Assume "AllCurve" is a GXcurve dataset
data(AllCurve)
AllValue = GetValue.GXcurve(AllCurve, first.minute = TRUE, last.minute = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

