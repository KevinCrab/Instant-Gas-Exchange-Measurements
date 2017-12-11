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
summary.GXmean(table, trait)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
  a GXmean class table
%%     ~~Describe \code{table} here~~
}
  \item{trait}{
  the trait you want to summarize. Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last
%%     ~~Describe \code{trait} here~~
}
}

\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}

\examples{
# Assume AllMean has class GXmean
data(AllMean)
summary.GXmean(AllMean, trait = "Photo_first")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.