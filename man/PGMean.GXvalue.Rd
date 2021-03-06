\name{PGMean.GXvalue}
\alias{PGMean}
\alias{PGMean.GXvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot or Genotype Mean
%%  ~~function to do ... ~~
}
\description{
This function averages the replicates within plots or genotypes to get plot mean or genotype mean. The output table can be used for testing block or genotype effects or correlate with other physiology traits.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PGMean(table, ...)
\method{PGMean}{GXvalue}(table, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
  a class \code{GXvalue} table with first-minute-value or second-minute-value, or both.
%%     ~~Describe \code{table} here~~
}
  \item{type}{
  either "plot" or "genotype" depends on which mean you want to get.
%%     ~~Describe \code{type} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Returns a class \code{GXmean} dataframe with plot name or genotype name, replicate numbers within and mean parameter values and standard errors. 
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
# Assume AllValue is a GXvalue class table
data(AllValue)
All_plot_Mean = PGMean(table = AllValue, type = "plot")
All_geno_Mean = PGMean(table = AllValue, type = "genotype")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

