\name{plotGX}
\alias{plotGX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Time Seires Trait Curve
%%  ~~function to do ... ~~
}
\description{
plotGX finds the input id(s) and genotype name(s) in the dataset and plot the selected trait against measurement time. Replicates are categoried by genotypes. Input data can be 3 types: table/id+dataset/genotype+dataset. PGN format Plots are saved into the working directory with a naming convention of trait+name.pgn
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotGX(trait, table = NULL, genotype = NULL, id = NULL, dataset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trait}{
  Character of the trait you want to view(Photo/Cond/Ci.Ca)
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
  If certain genotype or id are to be selected to plot, dataset containing those entries has to be provided.
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
Jiayang Xie
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{



}

