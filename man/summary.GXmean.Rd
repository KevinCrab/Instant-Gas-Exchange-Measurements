\name{summary.GXmean}
\alias{summary.GXmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for Class GXmean
%%  ~~function to do ... ~~
}
\description{
Summarize the replicate number for each plot or genotype. A brief summary and histogram can be provided for the selected trait.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{GXmean}(object, trait,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  a \code{GXmean} class object
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
# Assume AllMean has class GXmean
data(AllMean)
summary(AllMean, trait = "Photo_first")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
