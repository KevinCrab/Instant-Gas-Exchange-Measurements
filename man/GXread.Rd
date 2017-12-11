\name{read.GX}
\alias{read.GX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in Licor-6400 raw Data
%%  ~~function to do ... ~~
}
\description{
read.GX reads the raw csv files generated from licor-6400 gas exchange measurement.It records measurement conditions, extracts each measurement observations, trims unnecessary parameters, matches genotype names and reformats name digit numbers. Data from machines with different output structure settings are unified and reorganized. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.GX(filename, leaf_rep, genolist, condition = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  Name character of a licor-6400 output csv file.
%%     ~~Describe \code{filename} here~~
}
  \item{leaf_rep}{
  Each day we sample a whole set of replication for each plot. Thus, each file comprise of same leaf replicate number.
%%     ~~Describe \code{leaf_rep} here~~
}
  \item{genolist}{
  A list of plot names and corresponding genotype names.
%%     ~~Describe \code{genolist} here~~
}
  \item{condition}{
  If TRUE, check whether measurement conditions are complete. (measurement_time/machine/lightsource/AD.avgtime/flow/par/CO2_mixer/Tblock)
%%     ~~Describe \code{condition} here~~
}
}
\value{
Returns a table that includes all observations in the raw dataset with measurements (Photo; Cond; Ci/Ca) and properties (plot name; leaf replicate number; genotype name; original dataset; range; row; block; time)
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://www.licor.com/documents/ifuhfcjga0wvh94lkysz
%% ~put references to the literature/web site here ~
}
\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}


\examples{
# Assume "GX_output.csv"" file is exported from Licor-6400 machine
# "genotypic imformation.csv" has matching of plot names with genotype names.
read.GX(filename = "GX_output.csv",leaf_rep = 1, genolist = "genotypic imformation.csv",condition = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
