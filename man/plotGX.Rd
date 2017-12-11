\name{plotGX}
\alias{plotGX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Curve against Time
%%  ~~function to do ... ~~
}
\description{
plotGX finds the input id(s) and genotype name(s) in the dataset and plot the selected trait against measurement time. Replicates are categoried by genotypes. Input data can be 3 types: 1. table. Every measurement in this table will be ploted. 2.genotype + dataset. genotype must be in the dataset provided. 3. id + dataset. id must be in the dataset provided. PGN format Plots are saved into the working directory with a naming convention of "trait + genotype name.pgn". Plots are used for identifying outliers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotGX(trait, table = NULL, genotype = NULL, id = NULL, dataset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trait}{
  character of the trait you want to view. ( "Photo" / "Cond" / "Ci.Ca" )
%%     ~~Describe \code{trait} here~~
}
  \item{table}{
  Plot out all the measurements entries in this table.
%%     ~~Describe \code{table} here~~
}
  \item{genotype}{
  plot out the selected genotypes in corresponding dataset.
%%     ~~Describe \code{genotype} here~~
}
  \item{id}{
  plot out the selected plot names in corresponding dataset.
%%     ~~Describe \code{id} here~~
}
  \item{dataset}{
  if certain genotype or id are to be selected to plot, dataset containing those entries has to be provided.
%%     ~~Describe \code{dataset} here~~
}
}

\value{
Saves plots of selected physiological trait of selected entries over time into the working directory.
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
plotGX = function(trait = "Photo", table = GXcurve_table1 )
plotGX = function(trait = "Cond", genotype = "Z019E0032", dataset = GXcurve_table2 )
plotGX = function(trait = "Ci.Ca", id = "120_1" , dataset = GXcurve_table3 )
}
