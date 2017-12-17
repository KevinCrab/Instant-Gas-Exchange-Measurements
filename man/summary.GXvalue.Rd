\name{summary.GXvalue}
\alias{summary.GXvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for Class GXvalue
%%  ~~function to do ... ~~
}
\description{
Summarize total measurement counts and counts for each replicates. A brief summary and histogram can be provided for the selected trait.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{GXvalue}(object, trait, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  a \code{GXvalue} class object
%%     ~~Describe \code{object} here~~
}
  \item{trait}{
  the trait you want to summarize. Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last
%%     ~~Describe \code{trait} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}

\examples{
# Assume AllValue has class GXvalue
data(AllValue)
summary(AllValue, trait = "Photo_first")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

