\name{Find.GXcurve}
\alias{Find}
\alias{Find.GXcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Measurement Index in Dataset
%%  ~~function to do ... ~~
}
\description{
Find.GXcurve finds the targeting measurement id in the table provided. If this table was run by CS in advance, then problematic measurements can be found all at once by setting "DuplicateName; MissRecording; TooSmall" arguments to TRUE.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Find(table, ...)
\method{Find}{GXcurve}(table, id = NULL, DuplicateName = FALSE, MissRecording = FALSE, TooSmall = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
  The table that contains the measurement you want to find.
%%     ~~Describe \code{table} here~~
}
  \item{id}{
  name of measurement
%%     ~~Describe \code{id} here~~
}
  \item{DuplicateName}{
  list of measurements with duplicate names. Exist in table attributes.
%%     ~~Describe \code{DuplicateName} here~~
}
  \item{TooSmall}{
    list of measurements with too few records in a single measurement. Exist in table attributes.
%%     ~~Describe \code{TooSmall} here~~
}
  \item{MissRecording}{
    list of measurements that include two measurements without a remark in between. Exist in table attributes.
%%     ~~Describe \code{MissRecording} here~~
}
  \item{\dots}{
  other inputs
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Returns the index of start row and end row of the measurement in the table. Multiple findings can be printed out. 
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


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# Assume "AllCurve" is a GXcurve dataset
data(AllCurve)
Find.GXcurve(AllCurve, id = "120_1")
Find.GXcurve(AllCurve, id = "111_1", DuplicateName = TRUE, TooSmall = TRUE, MissRecording = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

