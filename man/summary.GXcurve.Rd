\name{summary.GXcurve}
\alias{summary.GXcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for Class GXcurve
%%  ~~function to do ... ~~
}
\description{
Summarize the original dataset included, total measurement counts and counts for each replicates. A brief summary for parameters can be provided and measurement conditions can be checked.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{GXcurve}(object, value_summary = FALSE, check_condition = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  a class \code{GXcurve} object
%%     ~~Describe \code{object} here~~
}
  \item{value_summary}{
  If TRUE, print out a brief summary for values.
%%     ~~Describe \code{value_summary} here~~
}
  \item{check_condition}{
  If TRUE, give out the selected measurement condition. 
%%     ~~Describe \code{check_condition} here~~
}
  \item{\dots}{

}
%%     ~~Describe \code{\dots} here~~
}



\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}

\examples{
# Assume AllCurve has class GXcurve
data(AllCurve)
summary(AllCurve, value_summary = TRUE, check_condition = "flow")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

